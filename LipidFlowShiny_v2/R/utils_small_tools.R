# Helper functions for mod_small_tools.R - "Small Tools -> Peak Extraction".
#
# Giai doan 0 of the LipidFlow pipeline: extract QC peaks for each Internal
# Standard (IS) across candidate adduct forms, so the user can pick the
# adduct with the most symmetric/highest-intensity peak and get a real
# (measured) RT + Peak_Area to normalize against later in Step 3
# (Quantification). Output of this file's pipeline is "Y_IS_opt":
#   IS_ID, IS_Name, Selected_Adduct, Target_mz, Measured_RT, Peak_Area
#
# Design note on testability: raw-file EIC extraction (xcms::chromatogram()
# on an MSnbase raw object) cannot be meaningfully unit-tested without a real
# vendor raw file, so .lfs_extract_is_eic_from_raw() takes the chromatogram
# extraction and result-unpacking as injectable functions (chromatogram_fn,
# unpack_fn) - defaulting to the real xcms/MSnbase calls in production, but
# swappable for a synthetic stub in tests. Every other function in this file
# (adduct math, peak-shape scoring, adduct selection) is pure and operates on
# plain data.frames/vectors, so it is fully unit-testable with mock data with
# no MS packages involved at all.

# ---------------------------------------------------------------------------
# Reference adduct mass table. Delta masses are the standard monoisotopic
# adduct ion masses used across lipidomics MS software (MS-DIAL etc.):
# proton = 1.007276, Na+ = 22.989218, NH4+ = 18.033823, Cl- = 34.969402,
# HCOO- (formate) = 44.998201. m/z_calc = (Mass_IS + delta_mass) / |z|.
# ---------------------------------------------------------------------------
.lfs_is_adduct_table <- function(mode = c("positive", "negative")) {
  mode <- match.arg(mode)
  if (mode == "positive") {
    data.frame(
      adduct = c("+H", "+Na", "+NH4"),
      delta_mass = c(1.007276, 22.989218, 18.033823),
      z = c(1L, 1L, 1L),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      adduct = c("-H", "+Cl", "+HCOO"),
      delta_mass = c(-1.007276, 34.969402, 44.998201),
      z = c(1L, 1L, 1L),
      stringsAsFactors = FALSE
    )
  }
}

# ---------------------------------------------------------------------------
# Validate an Internal Standard table (already read into a data.frame) has
# the columns the rest of this pipeline needs: ID, Name, Formula,
# Accurate_Mass. Returns list(ok, message) rather than throwing, so callers
# (both Shiny UI and other utils_ functions) can decide how to react.
# ---------------------------------------------------------------------------
.lfs_validate_is_table_cols <- function(is_table) {
  required_cols <- c("ID", "Name", "Formula", "Accurate_Mass")

  if (is.null(is_table) || !is.data.frame(is_table)) {
    return(list(ok = FALSE, message = "Internal standard table must be a data.frame."))
  }
  if (nrow(is_table) == 0) {
    return(list(ok = FALSE, message = "Internal standard table is empty."))
  }
  missing_cols <- setdiff(required_cols, colnames(is_table))
  if (length(missing_cols) > 0) {
    return(list(ok = FALSE, message = paste0(
      "Missing required column(s): ", paste(missing_cols, collapse = ", "), "."
    )))
  }
  if (!is.numeric(is_table$Accurate_Mass)) {
    return(list(ok = FALSE, message = "Column 'Accurate_Mass' must be numeric."))
  }
  if (anyNA(is_table$Accurate_Mass) || any(is_table$Accurate_Mass <= 0)) {
    return(list(ok = FALSE, message = "Column 'Accurate_Mass' must contain positive, non-missing values."))
  }
  if (anyDuplicated(is_table$ID) > 0) {
    return(list(ok = FALSE, message = "Column 'ID' must contain unique values."))
  }
  list(ok = TRUE, message = NULL)
}

# ---------------------------------------------------------------------------
# Read + validate the uploaded IS CSV in one step (used by mod_small_tools.R).
# ---------------------------------------------------------------------------
.lfs_read_is_csv <- function(filepath) {
  if (is.null(filepath) || !nzchar(filepath) || !file.exists(filepath)) {
    return(list(ok = FALSE, data = NULL, message = paste0("File not found: ", filepath)))
  }
  is_table <- tryCatch(
    utils::read.csv(filepath, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) {
      message("[utils_small_tools] Failed to read IS CSV: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(is_table)) {
    return(list(ok = FALSE, data = NULL, message = "Could not parse the uploaded file as CSV."))
  }
  check <- .lfs_validate_is_table_cols(is_table)
  if (!isTRUE(check$ok)) return(list(ok = FALSE, data = NULL, message = check$message))
  list(ok = TRUE, data = is_table, message = NULL)
}

# ---------------------------------------------------------------------------
# Vectorized theoretical m/z calculation: cross-join every IS row against
# every requested adduct (base R's merge(by = character(0)) is the standard
# idiom for a Cartesian product), then m/z_calc = (Mass_IS + delta)/|z|.
# Does NOT mutate is_table - merge() always returns a new object.
# ---------------------------------------------------------------------------
.lfs_calc_target_mz <- function(is_table, adducts, mode = c("positive", "negative")) {
  mode <- match.arg(mode)

  check <- .lfs_validate_is_table_cols(is_table)
  if (!isTRUE(check$ok)) stop(".lfs_calc_target_mz: ", check$message)

  if (is.null(adducts) || length(adducts) == 0 || !is.character(adducts)) {
    stop(".lfs_calc_target_mz: 'adducts' must be a non-empty character vector.")
  }

  adduct_ref <- .lfs_is_adduct_table(mode)
  unknown <- setdiff(adducts, adduct_ref$adduct)
  if (length(unknown) > 0) {
    stop(".lfs_calc_target_mz: unsupported adduct(s) for ", mode, " mode: ",
         paste(unknown, collapse = ", "))
  }
  adduct_sel <- adduct_ref[adduct_ref$adduct %in% adducts, , drop = FALSE]

  is_small <- is_table[, c("ID", "Name", "Accurate_Mass"), drop = FALSE]
  combo <- merge(is_small, adduct_sel, by = character(0))
  combo$Target_mz <- (combo$Accurate_Mass + combo$delta_mass) / abs(combo$z)

  data.frame(
    IS_ID = combo$ID,
    IS_Name = combo$Name,
    Adduct = combo$adduct,
    z = combo$z,
    Target_mz = combo$Target_mz,
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------------
# Trapezoidal peak-area integration. Sorts by rt defensively, drops NA pairs,
# returns 0 for degenerate input (<2 usable points) rather than erroring -
# an EIC window with no signal is a legitimate real-world result, not a bug.
# ---------------------------------------------------------------------------
.lfs_trapz_area <- function(rt, intensity) {
  if (length(rt) != length(intensity)) {
    stop(".lfs_trapz_area: 'rt' and 'intensity' must be the same length.")
  }
  keep <- !is.na(rt) & !is.na(intensity)
  rt <- rt[keep]; intensity <- intensity[keep]
  if (length(rt) < 2) return(0)
  ord <- order(rt)
  rt <- rt[ord]; intensity <- intensity[ord]
  sum(diff(rt) * (utils::head(intensity, -1) + utils::tail(intensity, -1)) / 2)
}

# ---------------------------------------------------------------------------
# Peak-shape metrics for one EIC trace: apex RT, apex intensity, peak area,
# and a symmetry score in [0, 1] (1 = perfectly Gaussian-symmetric) based on
# the half-max crossing widths on either side of the apex - a standard,
# cheap proxy for "does this look like a real chromatographic peak" without
# needing a full Gaussian curve fit. Never throws on degenerate input; NA
# metrics signal "not enough signal to judge", left for the caller to rank.
# ---------------------------------------------------------------------------
.lfs_peak_shape_metrics <- function(rt, intensity) {
  empty_result <- list(apex_rt = NA_real_, max_intensity = NA_real_,
                        symmetry_score = NA_real_, peak_area = NA_real_)

  if (length(rt) == 0 || length(intensity) == 0) return(empty_result)
  if (length(rt) != length(intensity)) {
    stop(".lfs_peak_shape_metrics: 'rt' and 'intensity' must be the same length.")
  }

  keep <- !is.na(rt) & !is.na(intensity)
  rt <- rt[keep]; intensity <- intensity[keep]
  if (length(rt) == 0) return(empty_result)

  ord <- order(rt)
  rt <- rt[ord]; intensity <- intensity[ord]

  apex_idx <- which.max(intensity)
  apex_rt <- rt[apex_idx]
  max_intensity <- intensity[apex_idx]
  peak_area <- .lfs_trapz_area(rt, intensity)

  symmetry_score <- NA_real_
  if (is.finite(max_intensity) && max_intensity > 0 && length(rt) >= 3) {
    half_max <- max_intensity / 2
    left_rt <- if (apex_idx > 1) {
      suppressWarnings(stats::approx(x = intensity[1:apex_idx], y = rt[1:apex_idx], xout = half_max)$y)
    } else NA_real_
    right_rt <- if (apex_idx < length(rt)) {
      suppressWarnings(stats::approx(x = intensity[apex_idx:length(rt)], y = rt[apex_idx:length(rt)], xout = half_max)$y)
    } else NA_real_

    if (!is.na(left_rt) && !is.na(right_rt)) {
      left_width <- apex_rt - left_rt
      right_width <- right_rt - apex_rt
      total_width <- left_width + right_width
      if (is.finite(total_width) && total_width > 0) {
        symmetry_score <- 1 - abs(left_width - right_width) / total_width
      }
    }
  }

  list(apex_rt = apex_rt, max_intensity = max_intensity,
       symmetry_score = symmetry_score, peak_area = peak_area)
}

# ---------------------------------------------------------------------------
# Apply .lfs_peak_shape_metrics() to every row of an eic_data table (one row
# per IS x adduct combo, rt/intensity as list-columns of numeric vectors).
# purrr::pmap() + dplyr::bind_rows() instead of a manual for loop; returns a
# new data.frame, eic_data itself is never touched.
# ---------------------------------------------------------------------------
.lfs_score_eic <- function(eic_data) {
  required_cols <- c("IS_ID", "IS_Name", "Adduct", "Target_mz", "rt", "intensity")
  if (is.null(eic_data) || !is.data.frame(eic_data)) {
    stop(".lfs_score_eic: 'eic_data' must be a data.frame.")
  }
  missing_cols <- setdiff(required_cols, colnames(eic_data))
  if (length(missing_cols) > 0) {
    stop(".lfs_score_eic: missing required column(s): ", paste(missing_cols, collapse = ", "))
  }

  scored <- eic_data
  if (nrow(eic_data) == 0) {
    scored$apex_rt <- numeric(0)
    scored$max_intensity <- numeric(0)
    scored$symmetry_score <- numeric(0)
    scored$peak_area <- numeric(0)
    return(scored)
  }

  metrics_list <- purrr::pmap(
    list(rt = eic_data$rt, intensity = eic_data$intensity),
    function(rt, intensity) as.data.frame(.lfs_peak_shape_metrics(rt, intensity))
  )
  metrics <- dplyr::bind_rows(metrics_list)

  scored$apex_rt <- metrics$apex_rt
  scored$max_intensity <- metrics$max_intensity
  scored$symmetry_score <- metrics$symmetry_score
  scored$peak_area <- metrics$peak_area
  scored
}

# ---------------------------------------------------------------------------
# Pick the best adduct per IS: selection_score = symmetry_score *
# log1p(max_intensity) rewards both a clean Gaussian shape AND high
# intensity, rather than either alone (a huge but lopsided peak, or a tiny
# but perfectly symmetric one, both lose to a decent peak that's strong on
# both counts). Rows with unusable metrics (NA symmetry/intensity) are
# ranked last via -Inf, never chosen unless literally every candidate is
# unusable. Vectorized via order()/duplicated(), no explicit loop.
# ---------------------------------------------------------------------------
.lfs_select_optimal_adduct <- function(scored_eic) {
  required_cols <- c("IS_ID", "IS_Name", "Adduct", "Target_mz",
                      "apex_rt", "max_intensity", "symmetry_score", "peak_area")
  if (is.null(scored_eic) || !is.data.frame(scored_eic)) {
    stop(".lfs_select_optimal_adduct: 'scored_eic' must be a data.frame.")
  }
  missing_cols <- setdiff(required_cols, colnames(scored_eic))
  if (length(missing_cols) > 0) {
    stop(".lfs_select_optimal_adduct: missing required column(s): ", paste(missing_cols, collapse = ", "))
  }

  empty_out <- data.frame(
    IS_ID = character(), IS_Name = character(), Selected_Adduct = character(),
    Target_mz = numeric(), Measured_RT = numeric(), Peak_Area = numeric(),
    stringsAsFactors = FALSE
  )
  if (nrow(scored_eic) == 0) return(empty_out)

  df <- scored_eic
  df$selection_score <- ifelse(
    is.na(df$symmetry_score) | is.na(df$max_intensity), -Inf,
    df$symmetry_score * log1p(pmax(df$max_intensity, 0))
  )

  df <- df[order(df$IS_ID, -df$selection_score), , drop = FALSE]
  best <- df[!duplicated(df$IS_ID), , drop = FALSE]
  best <- best[order(best$IS_ID), , drop = FALSE]

  data.frame(
    IS_ID = best$IS_ID,
    IS_Name = best$IS_Name,
    Selected_Adduct = best$Adduct,
    Target_mz = best$Target_mz,
    Measured_RT = best$apex_rt,
    Peak_Area = best$peak_area,
    stringsAsFactors = FALSE
  )
}

# Convenience one-call pipeline: score every EIC trace, then pick the best
# adduct per IS. This is the function whose output is "Y_IS_opt".
.lfs_build_y_is_opt <- function(eic_data) {
  scored <- .lfs_score_eic(eic_data)
  .lfs_select_optimal_adduct(scored)
}

# ---------------------------------------------------------------------------
# Real backend: read a raw QC file (.mzXML/.mzML) via MSnbase. Kept separate
# from the extraction logic below so failures here (bad path, corrupt file,
# unsupported format) are reported with a clear, specific message.
# ---------------------------------------------------------------------------
.lfs_read_raw_ms_file <- function(path) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) {
    stop(".lfs_read_raw_ms_file: file not found: ", path)
  }
  ext <- tolower(tools::file_ext(path))
  if (!ext %in% c("mzxml", "mzml")) {
    stop(".lfs_read_raw_ms_file: unsupported file extension '.", ext,
         "' - expected .mzXML or .mzML.")
  }
  tryCatch({
    message("[utils_small_tools] Reading raw MS file: ", path)
    MSnbase::readMSData(path, mode = "onDisk", msLevel. = 1)
  }, error = function(e) {
    stop(".lfs_read_raw_ms_file: failed to read '", path, "': ", conditionMessage(e))
  })
}

# ---------------------------------------------------------------------------
# Extract an EIC per (IS, adduct) combo from an already-loaded raw object.
# chromatogram_fn/unpack_fn default to the real xcms::chromatogram() /
# MSnbase rtime()+intensity() calls, but can be swapped out (dependency
# injection) so this function's own logic - m/z window construction,
# per-row iteration, error containment, assembling the eic_data table - is
# unit-testable without ever touching xcms or a real raw file.
# ---------------------------------------------------------------------------
.lfs_extract_is_eic_from_raw <- function(raw_object, is_table, adducts,
                                          mode = c("positive", "negative"),
                                          ppm = 15, rt_window = NULL,
                                          chromatogram_fn = NULL, unpack_fn = NULL) {
  mode <- match.arg(mode)
  if (is.null(raw_object)) stop(".lfs_extract_is_eic_from_raw: 'raw_object' must not be NULL.")
  if (!is.numeric(ppm) || length(ppm) != 1 || is.na(ppm) || ppm <= 0) {
    stop(".lfs_extract_is_eic_from_raw: 'ppm' must be a single positive number.")
  }

  target <- .lfs_calc_target_mz(is_table, adducts, mode)

  if (is.null(chromatogram_fn)) {
    chromatogram_fn <- function(object, mz, rt) {
      xcms::chromatogram(object, mz = mz, rt = rt, aggregationFun = "sum")[[1]]
    }
  }
  if (is.null(unpack_fn)) {
    unpack_fn <- function(ch) {
      list(rt = as.numeric(MSnbase::rtime(ch)), intensity = as.numeric(MSnbase::intensity(ch)))
    }
  }

  rt_rng <- if (is.null(rt_window)) c(0, Inf) else rt_window
  delta <- target$Target_mz * ppm / 1e6
  mz_lo <- target$Target_mz - delta
  mz_hi <- target$Target_mz + delta

  traces <- purrr::pmap(list(lo = mz_lo, hi = mz_hi), function(lo, hi) {
    tryCatch({
      ch <- chromatogram_fn(raw_object, c(lo, hi), rt_rng)
      unpack_fn(ch)
    }, error = function(e) {
      message("[utils_small_tools] EIC extraction failed for m/z [",
              round(lo, 4), ", ", round(hi, 4), "]: ", conditionMessage(e))
      list(rt = numeric(0), intensity = numeric(0))
    })
  })

  eic_data <- target
  eic_data$rt <- lapply(traces, function(tr) tr$rt)
  eic_data$intensity <- lapply(traces, function(tr) tr$intensity)
  eic_data
}

# ---------------------------------------------------------------------------
# Unnest an eic_data table (rt/intensity list-columns) into one row per
# (IS, adduct, point) for plotting. Pure and testable; rows whose rt/
# intensity vectors are empty simply contribute no rows to the output.
# ---------------------------------------------------------------------------
.lfs_unnest_eic <- function(eic_data) {
  required_cols <- c("IS_ID", "IS_Name", "Adduct", "rt", "intensity")
  if (is.null(eic_data) || !is.data.frame(eic_data)) {
    stop(".lfs_unnest_eic: 'eic_data' must be a data.frame.")
  }
  missing_cols <- setdiff(required_cols, colnames(eic_data))
  if (length(missing_cols) > 0) {
    stop(".lfs_unnest_eic: missing required column(s): ", paste(missing_cols, collapse = ", "))
  }
  if (nrow(eic_data) == 0) {
    return(data.frame(IS_ID = character(), IS_Name = character(), Adduct = character(),
                       rt = numeric(), intensity = numeric(), stringsAsFactors = FALSE))
  }

  rows <- purrr::pmap(
    list(IS_ID = eic_data$IS_ID, IS_Name = eic_data$IS_Name, Adduct = eic_data$Adduct,
         rt = eic_data$rt, intensity = eic_data$intensity),
    function(IS_ID, IS_Name, Adduct, rt, intensity) {
      n <- length(rt)
      if (n == 0) return(NULL)
      data.frame(IS_ID = rep(IS_ID, n), IS_Name = rep(IS_Name, n), Adduct = rep(Adduct, n),
                 rt = rt, intensity = intensity, stringsAsFactors = FALSE)
    }
  )
  dplyr::bind_rows(rows)
}

# ---------------------------------------------------------------------------
# Top-level orchestration used by mod_small_tools.R: read the raw file,
# extract EICs, score + select the optimal adduct per IS. Every failure
# mode (bad file, bad params, extraction error) is caught here and turned
# into a structured result instead of an uncaught error reaching the Shiny
# session, per the "no uncaught exceptions in backend code" requirement.
# ---------------------------------------------------------------------------
.lfs_run_peak_extraction <- function(qc_file_path, is_table, adducts,
                                      mode = c("positive", "negative"),
                                      ppm = 15, rt_window = NULL) {
  mode <- match.arg(mode)
  tryCatch({
    raw_object <- .lfs_read_raw_ms_file(qc_file_path)
    eic_data <- .lfs_extract_is_eic_from_raw(raw_object, is_table, adducts, mode, ppm, rt_window)
    y_is_opt <- .lfs_build_y_is_opt(eic_data)
    list(ok = TRUE, eic_data = eic_data, y_is_opt = y_is_opt, message = NULL)
  }, error = function(e) {
    message("[utils_small_tools] Peak extraction failed: ", conditionMessage(e))
    list(ok = FALSE, eic_data = NULL, y_is_opt = NULL, message = conditionMessage(e))
  })
}
