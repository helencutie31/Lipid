# Tests for R/utils_annotation.R (Giai doan 2: Lipid Annotation).

mock_peak_table <- function() {
  data.frame(
    Peak_ID = c("P1", "P2", "P3"),
    mz = c(760.5851, 810.6012, 999.9999), # P3 deliberately far from any DB entry
    stringsAsFactors = FALSE
  )
}

mock_database_table <- function() {
  data.frame(
    Lipid_Name = c("PC(34:1)", "TG(52:2)"),
    mz_theoretical = c(760.5850, 810.6000),
    stringsAsFactors = FALSE
  )
}

# ---- .lfs_calc_ppm_error -----------------------------------------------------

test_that(".lfs_calc_ppm_error: happy path matches the documented formula", {
  err <- .lfs_calc_ppm_error(760.5851, 760.5850)
  expect_equal(err, abs(760.5851 - 760.5850) / 760.5850 * 1e6, tolerance = 1e-9)
})

test_that(".lfs_calc_ppm_error: edge case - non-numeric input errors", {
  expect_error(.lfs_calc_ppm_error("760.58", 760.585))
  expect_error(.lfs_calc_ppm_error(760.58, "760.585"))
})

test_that(".lfs_calc_ppm_error: edge case - zero/negative theoretical mass errors, mismatched length errors", {
  expect_error(.lfs_calc_ppm_error(760.58, 0))
  expect_error(.lfs_calc_ppm_error(760.58, -1))
  expect_error(.lfs_calc_ppm_error(c(1, 2, 3), c(1, 2)))
})

test_that(".lfs_calc_ppm_error recycles a length-1 theoretical mass and does not mutate inputs", {
  measured <- c(760.5851, 810.6012)
  measured_snapshot <- measured
  out <- .lfs_calc_ppm_error(measured, 760.5850)
  expect_equal(length(out), 2)
  expect_identical(measured, measured_snapshot)
})

# ---- .lfs_annotate_by_mz -----------------------------------------------------

test_that(".lfs_annotate_by_mz: happy path matches within tolerance, leaves out-of-tolerance peaks unmatched", {
  out <- .lfs_annotate_by_mz(mock_peak_table(), mock_database_table(), tolerance_ppm = 5)

  expect_equal(out$Lipid_Name[out$Peak_ID == "P1"], "PC(34:1)")
  expect_equal(out$Lipid_Name[out$Peak_ID == "P2"], "TG(52:2)")
  expect_true(is.na(out$Lipid_Name[out$Peak_ID == "P3"]))
  expect_true(out$ppm_error[out$Peak_ID == "P1"] <= 5)
})

test_that(".lfs_annotate_by_mz: edge case - empty peak_table or database_table doesn't error", {
  out1 <- .lfs_annotate_by_mz(mock_peak_table()[0, , drop = FALSE], mock_database_table())
  expect_equal(nrow(out1), 0)

  out2 <- .lfs_annotate_by_mz(mock_peak_table(), mock_database_table()[0, , drop = FALSE])
  expect_equal(nrow(out2), 3)
  expect_true(all(is.na(out2$Lipid_Name)))
})

test_that(".lfs_annotate_by_mz: edge case - missing columns / bad tolerance error", {
  bad_peak <- mock_peak_table()
  bad_peak$mz <- NULL
  expect_error(.lfs_annotate_by_mz(bad_peak, mock_database_table()))
  expect_error(.lfs_annotate_by_mz(mock_peak_table(), mock_database_table(), tolerance_ppm = -1))
  expect_error(.lfs_annotate_by_mz("not a df", mock_database_table()))
})

test_that(".lfs_annotate_by_mz does not mutate its inputs", {
  peak_tbl <- mock_peak_table()
  db_tbl <- mock_database_table()
  peak_snapshot <- mock_peak_table()
  db_snapshot <- mock_database_table()

  .lfs_annotate_by_mz(peak_tbl, db_tbl, tolerance_ppm = 5)

  expect_identical(peak_tbl, peak_snapshot)
  expect_identical(db_tbl, db_snapshot)
})

# ---- .lfs_load_msdial_db (real bundled database files) -----------------------

test_that(".lfs_load_msdial_db: happy path loads the real bundled POS/NEG databases", {
  skip_if_not(file.exists(file.path(LFS_DEFAULT_DB_DIR, "msdial_lipid_pos_db.rda")), "bundled POS db not present")
  pos_db <- .lfs_load_msdial_db("positive", db_dir = LFS_DEFAULT_DB_DIR)
  expect_false(is.null(pos_db))

  skip_if_not(file.exists(file.path(LFS_DEFAULT_DB_DIR, "msdial_lipid_neg_db.rda")), "bundled NEG db not present")
  neg_db <- .lfs_load_msdial_db("negative", db_dir = LFS_DEFAULT_DB_DIR)
  expect_false(is.null(neg_db))
})

test_that(".lfs_load_msdial_db: edge case - missing file / bad mode errors", {
  expect_error(.lfs_load_msdial_db("positive", db_dir = tempdir()))
  expect_error(.lfs_load_msdial_db("weird_mode", db_dir = LFS_DEFAULT_DB_DIR))
})
