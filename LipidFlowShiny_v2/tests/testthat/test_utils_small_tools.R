# Tests for R/utils_small_tools.R - Small Tools -> Peak Extraction (Giai doan 0).

# ---- mock data helpers -----------------------------------------------------

mock_is_table <- function() {
  data.frame(
    ID = c("IS1", "IS2"),
    Name = c("15:0-18:1(d7) PC", "d18:1-18:1(d9) SM"),
    Formula = c("C41H73D7NO8P", "C41H72D9N2O6P"),
    Accurate_Mass = c(700.500, 738.600),
    stringsAsFactors = FALSE
  )
}

# Clean, symmetric Gaussian EIC - what a good adduct peak should look like.
mock_gaussian_trace <- function(center = 10, amplitude = 1000, sigma = 1.5, n = 41) {
  rt <- seq(center - 5, center + 5, length.out = n)
  intensity <- amplitude * exp(-((rt - center)^2) / (2 * sigma^2))
  list(rt = rt, intensity = intensity)
}

# Deliberately weak + asymmetric (skewed, low amplitude) - a bad adduct.
mock_bad_trace <- function(n = 41) {
  rt <- seq(0, 20, length.out = n)
  intensity <- ifelse(
    rt < 8, 30 * exp(-((8 - rt)^2) / (2 * 0.3^2)),
    30 * exp(-((rt - 8)^2) / (2 * 6^2))
  )
  list(rt = rt, intensity = intensity)
}

mock_eic_data <- function() {
  good <- mock_gaussian_trace()
  bad <- mock_bad_trace()
  data.frame(
    IS_ID = c("IS1", "IS1"),
    IS_Name = c("Test IS", "Test IS"),
    Adduct = c("+H", "+Na"),
    Target_mz = c(701.51, 723.49),
    z = c(1L, 1L),
    stringsAsFactors = FALSE
  ) -> df
  df$rt <- list(good$rt, bad$rt)
  df$intensity <- list(good$intensity, bad$intensity)
  df
}

# ---- .lfs_is_adduct_table ---------------------------------------------------

test_that(".lfs_is_adduct_table returns the documented adducts per mode", {
  pos <- .lfs_is_adduct_table("positive")
  neg <- .lfs_is_adduct_table("negative")
  expect_setequal(pos$adduct, c("+H", "+Na", "+NH4"))
  expect_setequal(neg$adduct, c("-H", "+Cl", "+HCOO"))
  expect_true(all(c("adduct", "delta_mass", "z") %in% colnames(pos)))
})

test_that(".lfs_is_adduct_table rejects an unknown mode", {
  expect_error(.lfs_is_adduct_table("weird_mode"))
})

# ---- .lfs_validate_is_table_cols -------------------------------------------

test_that(".lfs_validate_is_table_cols: happy path accepts a well-formed table", {
  check <- .lfs_validate_is_table_cols(mock_is_table())
  expect_true(check$ok)
  expect_null(check$message)
})

test_that(".lfs_validate_is_table_cols: edge case - missing required column", {
  bad <- mock_is_table()
  bad$Accurate_Mass <- NULL
  check <- .lfs_validate_is_table_cols(bad)
  expect_false(check$ok)
  expect_match(check$message, "Accurate_Mass")
})

test_that(".lfs_validate_is_table_cols: edge case - NULL / non-data.frame / wrong type input", {
  expect_false(.lfs_validate_is_table_cols(NULL)$ok)
  expect_false(.lfs_validate_is_table_cols("not a data frame")$ok)
  expect_false(.lfs_validate_is_table_cols(data.frame())$ok)

  wrong_type <- mock_is_table()
  wrong_type$Accurate_Mass <- as.character(wrong_type$Accurate_Mass)
  expect_false(.lfs_validate_is_table_cols(wrong_type)$ok)
})

test_that(".lfs_validate_is_table_cols does not mutate its input", {
  original <- mock_is_table()
  snapshot <- mock_is_table()
  .lfs_validate_is_table_cols(original)
  expect_identical(original, snapshot)
})

# ---- .lfs_calc_target_mz ----------------------------------------------------

test_that(".lfs_calc_target_mz: happy path computes the documented formula", {
  out <- .lfs_calc_target_mz(mock_is_table(), c("+H", "+Na"), "positive")
  expect_equal(nrow(out), 4) # 2 IS x 2 adducts
  expect_true(all(c("IS_ID", "IS_Name", "Adduct", "z", "Target_mz") %in% colnames(out)))

  row <- out[out$IS_ID == "IS1" & out$Adduct == "+H", ]
  expect_equal(row$Target_mz, (700.500 + 1.007276) / 1, tolerance = 1e-6)
})

test_that(".lfs_calc_target_mz: edge case - unsupported adduct errors", {
  expect_error(.lfs_calc_target_mz(mock_is_table(), c("+Fake"), "positive"))
})

test_that(".lfs_calc_target_mz: edge case - NULL/empty inputs error informatively", {
  expect_error(.lfs_calc_target_mz(NULL, c("+H"), "positive"))
  expect_error(.lfs_calc_target_mz(mock_is_table(), character(0), "positive"))
  expect_error(.lfs_calc_target_mz(mock_is_table()[0, ], c("+H"), "positive"))
})

test_that(".lfs_calc_target_mz does not mutate its input is_table", {
  original <- mock_is_table()
  snapshot <- mock_is_table()
  .lfs_calc_target_mz(original, c("+H"), "positive")
  expect_identical(original, snapshot)
})

# ---- .lfs_trapz_area ---------------------------------------------------------

test_that(".lfs_trapz_area: happy path integrates a known triangle", {
  area <- .lfs_trapz_area(c(0, 1, 2), c(0, 10, 0))
  expect_equal(area, 10)
})

test_that(".lfs_trapz_area: edge case - empty / single-point input returns 0", {
  expect_equal(.lfs_trapz_area(numeric(0), numeric(0)), 0)
  expect_equal(.lfs_trapz_area(5, 100), 0)
})

test_that(".lfs_trapz_area: edge case - mismatched lengths error", {
  expect_error(.lfs_trapz_area(c(1, 2, 3), c(1, 2)))
})

# ---- .lfs_peak_shape_metrics -------------------------------------------------

test_that(".lfs_peak_shape_metrics: happy path scores a symmetric Gaussian highly", {
  g <- mock_gaussian_trace()
  m <- .lfs_peak_shape_metrics(g$rt, g$intensity)
  expect_equal(m$apex_rt, 10, tolerance = 0.3)
  expect_equal(m$max_intensity, 1000, tolerance = 1)
  expect_gt(m$symmetry_score, 0.9)
  expect_gt(m$peak_area, 0)
})

test_that(".lfs_peak_shape_metrics: edge case - empty vectors return all-NA metrics without erroring", {
  m <- .lfs_peak_shape_metrics(numeric(0), numeric(0))
  expect_true(is.na(m$apex_rt))
  expect_true(is.na(m$symmetry_score))
  expect_true(is.na(m$peak_area))
})

test_that(".lfs_peak_shape_metrics: edge case - single point and length mismatch", {
  m <- .lfs_peak_shape_metrics(5, 100)
  expect_equal(m$apex_rt, 5)
  expect_true(is.na(m$symmetry_score))
  expect_equal(m$peak_area, 0)

  expect_error(.lfs_peak_shape_metrics(c(1, 2, 3), c(1, 2)))
})

# ---- .lfs_score_eic / .lfs_select_optimal_adduct / .lfs_build_y_is_opt ------

test_that(".lfs_build_y_is_opt: happy path picks the clean Gaussian adduct over the weak/skewed one", {
  y_is_opt <- .lfs_build_y_is_opt(mock_eic_data())
  expect_equal(nrow(y_is_opt), 1)
  expect_equal(y_is_opt$IS_ID, "IS1")
  expect_equal(y_is_opt$Selected_Adduct, "+H")
  expect_equal(y_is_opt$Measured_RT, 10, tolerance = 0.3)
  expect_true(all(c("IS_ID", "IS_Name", "Selected_Adduct", "Target_mz", "Measured_RT", "Peak_Area") %in% colnames(y_is_opt)))
})

test_that(".lfs_build_y_is_opt: edge case - zero-row eic_data returns zero-row result, not an error", {
  empty <- mock_eic_data()[0, , drop = FALSE]
  out <- .lfs_build_y_is_opt(empty)
  expect_equal(nrow(out), 0)
  expect_true(all(c("IS_ID", "Selected_Adduct", "Peak_Area") %in% colnames(out)))
})

test_that(".lfs_score_eic / .lfs_select_optimal_adduct: edge case - missing required columns error", {
  bad <- mock_eic_data()
  bad$Target_mz <- NULL
  expect_error(.lfs_score_eic(bad))
  expect_error(.lfs_select_optimal_adduct(data.frame(IS_ID = "x")))
})

test_that(".lfs_build_y_is_opt does not mutate its input eic_data", {
  original <- mock_eic_data()
  snapshot <- mock_eic_data()
  .lfs_build_y_is_opt(original)
  expect_identical(original, snapshot)
})

# ---- .lfs_extract_is_eic_from_raw (dependency-injected, no real raw file) ---

test_that(".lfs_extract_is_eic_from_raw: happy path assembles eic_data via injected stubs", {
  stub_chromatogram <- function(object, mz, rt) {
    list(rt = seq(0, 5, by = 1), intensity = c(0, 10, 100, 10, 0, 0))
  }
  stub_unpack <- function(ch) ch # already in {rt, intensity} shape

  out <- .lfs_extract_is_eic_from_raw(
    raw_object = "dummy_raw_object", is_table = mock_is_table(),
    adducts = c("+H", "+Na"), mode = "positive", ppm = 15,
    chromatogram_fn = stub_chromatogram, unpack_fn = stub_unpack
  )

  expect_equal(nrow(out), 4) # 2 IS x 2 adducts
  expect_true(all(vapply(out$rt, length, integer(1)) == 6))
  expect_true(all(vapply(out$intensity, function(x) max(x), numeric(1)) == 100))
})

test_that(".lfs_extract_is_eic_from_raw: edge case - invalid ppm / NULL raw_object error", {
  stub_chromatogram <- function(object, mz, rt) list(rt = 1:3, intensity = c(1, 2, 1))
  expect_error(.lfs_extract_is_eic_from_raw(
    "dummy", mock_is_table(), c("+H"), "positive", ppm = -5,
    chromatogram_fn = stub_chromatogram, unpack_fn = identity
  ))
  expect_error(.lfs_extract_is_eic_from_raw(
    NULL, mock_is_table(), c("+H"), "positive", ppm = 15,
    chromatogram_fn = stub_chromatogram, unpack_fn = identity
  ))
})

test_that(".lfs_extract_is_eic_from_raw: edge case - chromatogram_fn errors are contained per-row, not fatal", {
  flaky_chromatogram <- function(object, mz, rt) stop("simulated instrument read failure")
  out <- .lfs_extract_is_eic_from_raw(
    "dummy", mock_is_table(), c("+H"), "positive", ppm = 15,
    chromatogram_fn = flaky_chromatogram, unpack_fn = identity
  )
  expect_equal(nrow(out), 2)
  expect_true(all(vapply(out$rt, length, integer(1)) == 0))
})

test_that(".lfs_extract_is_eic_from_raw does not mutate its input is_table", {
  original <- mock_is_table()
  snapshot <- mock_is_table()
  stub_chromatogram <- function(object, mz, rt) list(rt = 1:3, intensity = c(1, 2, 1))
  .lfs_extract_is_eic_from_raw("dummy", original, c("+H"), "positive", ppm = 15,
                                chromatogram_fn = stub_chromatogram, unpack_fn = identity)
  expect_identical(original, snapshot)
})

# ---- .lfs_run_peak_extraction (top-level orchestration error handling) -----

test_that(".lfs_run_peak_extraction: edge case - missing raw file is reported, not thrown", {
  result <- .lfs_run_peak_extraction(
    qc_file_path = "does_not_exist.mzXML", is_table = mock_is_table(),
    adducts = c("+H"), mode = "positive"
  )
  expect_false(result$ok)
  expect_match(result$message, "file not found", ignore.case = TRUE)
})

test_that(".lfs_run_peak_extraction: edge case - unsupported file extension is reported, not thrown", {
  tmp <- tempfile(fileext = ".txt")
  writeLines("not a raw ms file", tmp)
  on.exit(unlink(tmp))
  result <- .lfs_run_peak_extraction(
    qc_file_path = tmp, is_table = mock_is_table(), adducts = c("+H"), mode = "positive"
  )
  expect_false(result$ok)
  expect_match(result$message, "unsupported file extension", ignore.case = TRUE)
})

# ---- .lfs_unnest_eic ---------------------------------------------------------

test_that(".lfs_unnest_eic: happy path produces one row per EIC point", {
  long_df <- .lfs_unnest_eic(mock_eic_data())
  expect_equal(nrow(long_df), length(mock_gaussian_trace()$rt) + length(mock_bad_trace()$rt))
  expect_true(all(c("IS_ID", "IS_Name", "Adduct", "rt", "intensity") %in% colnames(long_df)))
})

test_that(".lfs_unnest_eic: edge case - zero-row / all-empty-trace input", {
  empty <- mock_eic_data()[0, , drop = FALSE]
  expect_equal(nrow(.lfs_unnest_eic(empty)), 0)

  all_empty <- mock_eic_data()
  all_empty$rt <- list(numeric(0), numeric(0))
  all_empty$intensity <- list(numeric(0), numeric(0))
  expect_equal(nrow(.lfs_unnest_eic(all_empty)), 0)
})
