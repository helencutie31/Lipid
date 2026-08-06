# utils_annotation.R - shared helper for Lipid Annotation.
#
# IMPORTANT: annotation results are kept as 2 SEPARATE mass_dataset objects
# (POS, NEG) all the way through - never merged via massdataset::merge_mass_dataset().
# Same root cause as the Peak Picking fix: merging 2 mass_dataset S4 objects
# has an edge case that corrupts expression_data with small/unusual data,
# and utils_S.R (the confirmed-working reference script) never merges
# mass_dataset objects either - it keeps POS/NEG separate throughout and
# only combines plain data.frame results at the very end.
#
# This function is the safe equivalent: extract_annotation_table() is called
# separately on each already-valid mass_dataset (POS, NEG), producing 2
# ordinary data.frames, which ARE safe to combine with rbind() - unlike
# merging the S4 objects themselves.
.lfs_combined_annotation_table <- function(annotation_result) {
  extract_one <- function(md) {
    if (is.null(md)) return(NULL)
    tryCatch(massdataset::extract_annotation_table(md), error = function(e) md@variable_info)
  }
  tbl_pos <- extract_one(annotation_result$pos)
  tbl_neg <- extract_one(annotation_result$neg)
  if (is.null(tbl_pos)) return(tbl_neg)
  if (is.null(tbl_neg)) return(tbl_pos)
  rbind(tbl_pos, tbl_neg)
}

# ---------------------------------------------------------------------------
# Giai doan 2 matching formula, as a pure, vectorized, unit-testable
# function: Error (ppm) = |mz_measured - mz_theoretical| / mz_theoretical * 1e6.
# 'mz_theoretical' may be length 1 (recycled) or the same length as
# 'mz_measured'. Never mutates its inputs (returns a new numeric vector).
# ---------------------------------------------------------------------------
.lfs_calc_ppm_error <- function(mz_measured, mz_theoretical) {
  if (!is.numeric(mz_measured) || !is.numeric(mz_theoretical)) {
    stop(".lfs_calc_ppm_error: both 'mz_measured' and 'mz_theoretical' must be numeric.")
  }
  if (length(mz_theoretical) == 1 && length(mz_measured) > 1) {
    mz_theoretical <- rep(mz_theoretical, length(mz_measured))
  }
  if (length(mz_measured) != length(mz_theoretical)) {
    stop(".lfs_calc_ppm_error: 'mz_measured' and 'mz_theoretical' must be the same length ",
         "(or 'mz_theoretical' length 1).")
  }
  if (any(mz_theoretical <= 0, na.rm = TRUE)) {
    stop(".lfs_calc_ppm_error: 'mz_theoretical' must be strictly positive.")
  }
  abs(mz_measured - mz_theoretical) / mz_theoretical * 1e6
}

# ---------------------------------------------------------------------------
# Pure MS1 nearest-match annotator: for each peak, find the database entry
# with the smallest ppm error, keeping the match only if it's within
# tolerance_ppm (Giai doan 2's "Chi chap nhan <= tolerance ppm" rule).
# Implemented via outer() to build the full peak x database ppm-error
# matrix in one vectorized call, then apply()/which.min() per row - no
# manual nested for loop. Real MS2-aware annotation (metid) stays the
# primary path in mod_annotation.R; this is a lightweight, fully-testable
# MS1-only fallback/sanity-check matcher against a database table
# (columns: mz_theoretical, Lipid_Name by default).
# ---------------------------------------------------------------------------
.lfs_annotate_by_mz <- function(peak_table, database_table, tolerance_ppm = 5,
                                 peak_mz_col = "mz", db_mz_col = "mz_theoretical",
                                 db_name_col = "Lipid_Name") {
  if (!is.data.frame(peak_table) || !is.data.frame(database_table)) {
    stop(".lfs_annotate_by_mz: 'peak_table' and 'database_table' must both be data.frames.")
  }
  if (!peak_mz_col %in% colnames(peak_table)) {
    stop(".lfs_annotate_by_mz: peak_table is missing column '", peak_mz_col, "'.")
  }
  if (!all(c(db_mz_col, db_name_col) %in% colnames(database_table))) {
    stop(".lfs_annotate_by_mz: database_table is missing column '", db_mz_col,
         "' or '", db_name_col, "'.")
  }
  if (!is.numeric(tolerance_ppm) || length(tolerance_ppm) != 1 || is.na(tolerance_ppm) || tolerance_ppm <= 0) {
    stop(".lfs_annotate_by_mz: 'tolerance_ppm' must be a single positive number.")
  }

  result <- peak_table
  result$Lipid_Name <- rep(NA_character_, nrow(peak_table))
  result$ppm_error <- rep(NA_real_, nrow(peak_table))

  if (nrow(peak_table) == 0 || nrow(database_table) == 0) return(result)

  peak_mz <- peak_table[[peak_mz_col]]
  db_mz <- database_table[[db_mz_col]]
  if (!is.numeric(peak_mz) || !is.numeric(db_mz)) {
    stop(".lfs_annotate_by_mz: '", peak_mz_col, "' and '", db_mz_col, "' must both be numeric.")
  }

  ppm_matrix <- outer(peak_mz, db_mz, function(p, d) abs(p - d) / d * 1e6)
  best_idx <- apply(ppm_matrix, 1, function(row) {
    if (all(is.na(row))) return(NA_integer_)
    idx <- which.min(row)
    if (isTRUE(row[idx] <= tolerance_ppm)) idx else NA_integer_
  })

  matched <- !is.na(best_idx)
  if (any(matched)) {
    result$Lipid_Name[matched] <- database_table[[db_name_col]][best_idx[matched]]
    result$ppm_error[matched] <- ppm_matrix[cbind(which(matched), best_idx[matched])]
  }
  result
}

# ---------------------------------------------------------------------------
# Load the bundled MS-DIAL-derived metid database (data/msdial_lipid_pos_db.rda
# or _neg_db.rda) for a given polarity. Defaults to LFS_DEFAULT_DB_DIR (set
# in global.R) if db_dir isn't given explicitly, falling back to "<cwd>/data"
# so this also works when global.R hasn't been sourced (e.g. in tests).
# ---------------------------------------------------------------------------
.lfs_load_msdial_db <- function(mode = c("positive", "negative"), db_dir = NULL) {
  mode <- match.arg(mode)
  if (is.null(db_dir)) {
    db_dir <- if (exists("LFS_DEFAULT_DB_DIR", inherits = TRUE)) {
      get("LFS_DEFAULT_DB_DIR", inherits = TRUE)
    } else {
      file.path(getwd(), "data")
    }
  }
  fname <- if (mode == "positive") "msdial_lipid_pos_db.rda" else "msdial_lipid_neg_db.rda"
  path <- file.path(db_dir, fname)
  if (!file.exists(path)) {
    stop(".lfs_load_msdial_db: database file not found: ", path)
  }
  tryCatch({
    e <- new.env()
    load(path, envir = e)
    obj_names <- ls(e)
    if (length(obj_names) == 0) stop("no objects found in .rda file")
    get(obj_names[1], envir = e)
  }, error = function(e) {
    stop(".lfs_load_msdial_db: failed to load '", path, "': ", conditionMessage(e))
  })
}
