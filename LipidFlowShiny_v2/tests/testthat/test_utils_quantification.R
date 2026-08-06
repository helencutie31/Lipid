# Tests for R/utils_quantification.R (Giai doan 3: Quantification).

mock_annotation_table <- function() {
  data.frame(
    Lipid_Name = c("PC(34:1)", "TG(52:2)", "999unparsable"),
    S1 = c(1000, 2000, 300),
    S2 = c(1200, 2400, 360),
    stringsAsFactors = FALSE
  )
}

mock_y_is_opt <- function() {
  data.frame(
    IS_ID = c("IS1", "IS2"),
    IS_Name = c("IS_PC", "IS_TG"),
    Selected_Adduct = c("+H", "+H"),
    Target_mz = c(760.58, 810.60),
    Measured_RT = c(120, 300),
    Peak_Area = c(500, 1000),
    stringsAsFactors = FALSE
  )
}

mock_is_table <- function() {
  data.frame(name = c("IS_PC", "IS_TG"), ug_ml = c(10, 20), stringsAsFactors = FALSE)
}

mock_match_item <- function() list(PC = "IS_PC", TG = "IS_TG")

# ---- .lfs_parse_lipid_class --------------------------------------------------

test_that(".lfs_parse_lipid_class: happy path extracts the leading class prefix", {
  out <- .lfs_parse_lipid_class(c("PC(34:1)", "TG(52:2)", "Cer(d18:1/24:0)"))
  expect_equal(out, c("PC", "TG", "Cer"))
})

test_that(".lfs_parse_lipid_class: edge case - NA / empty / no-letter-prefix input returns NA", {
  out <- .lfs_parse_lipid_class(c(NA, "", "123XYZ"))
  expect_true(all(is.na(out)))
})

test_that(".lfs_parse_lipid_class does not mutate its input", {
  x <- c("PC(34:1)", "TG(52:2)")
  x_snapshot <- x
  .lfs_parse_lipid_class(x)
  expect_identical(x, x_snapshot)
})

# ---- .lfs_calc_final_concentration -------------------------------------------

test_that(".lfs_calc_final_concentration: happy path matches the documented formula", {
  out <- .lfs_calc_final_concentration(1000, 500, 10)
  expect_equal(out, (1000 / 500) * 10)

  out_vec <- .lfs_calc_final_concentration(c(1000, 1200), 500, 10)
  expect_equal(out_vec, c(20, 24))
})

test_that(".lfs_calc_final_concentration: edge case - division by zero IS peak area yields NA, not an error/Inf", {
  out <- .lfs_calc_final_concentration(1000, 0, 10)
  expect_true(is.na(out))
})

test_that(".lfs_calc_final_concentration: edge case - non-numeric or non-recyclable input errors", {
  expect_error(.lfs_calc_final_concentration("1000", 500, 10))
  expect_error(.lfs_calc_final_concentration(c(1, 2, 3), c(1, 2), 10))
})

# ---- .lfs_detect_lipid_name_col ----------------------------------------------

test_that(".lfs_detect_lipid_name_col: happy path prefers 'Lipid_Name', falls back sensibly", {
  expect_equal(.lfs_detect_lipid_name_col(data.frame(Lipid_Name = "x", other = 1)), "Lipid_Name")
  expect_equal(.lfs_detect_lipid_name_col(data.frame(Compound.name = "x", val = 1)), "Compound.name")
  expect_equal(.lfs_detect_lipid_name_col(data.frame(some_char_col = "x", val = 1)), "some_char_col")
})

test_that(".lfs_detect_lipid_name_col: edge case - no usable column returns NA", {
  expect_true(is.na(.lfs_detect_lipid_name_col(data.frame(a = 1, b = 2))))
  expect_true(is.na(.lfs_detect_lipid_name_col(data.frame())))
})

# ---- .lfs_quantify_lipids (full Giai doan 3 pipeline) -------------------------

test_that(".lfs_quantify_lipids: happy path computes Final_Calculated correctly for matched lipids", {
  out <- .lfs_quantify_lipids(mock_annotation_table(), mock_y_is_opt(), mock_is_table(), mock_match_item())

  expect_true(all(c("Lipid_Name", "Class", "Sample", "Peak_Area_Sample", "Matched_IS",
                     "Peak_Area_IS", "Known_Conc_IS", "Final_Calculated") %in% colnames(out)))

  pc_s1 <- out[out$Lipid_Name == "PC(34:1)" & out$Sample == "S1", ]
  expect_equal(pc_s1$Final_Calculated, (1000 / 500) * 10)

  tg_s2 <- out[out$Lipid_Name == "TG(52:2)" & out$Sample == "S2", ]
  expect_equal(tg_s2$Final_Calculated, (2400 / 1000) * 20)

  # The unparsable lipid name has no matched class/IS - NA, not a crash.
  unparsable <- out[out$Lipid_Name == "999unparsable" & out$Sample == "S1", ]
  expect_true(is.na(unparsable$Final_Calculated))
})

test_that(".lfs_quantify_lipids: edge case - missing lipid_name_col / no numeric sample columns errors", {
  bad <- mock_annotation_table()
  bad$Lipid_Name <- NULL
  expect_error(.lfs_quantify_lipids(bad, mock_y_is_opt(), mock_is_table(), mock_match_item()))

  no_numeric <- data.frame(Lipid_Name = c("PC(34:1)"), stringsAsFactors = FALSE)
  expect_error(.lfs_quantify_lipids(no_numeric, mock_y_is_opt(), mock_is_table(), mock_match_item()))
})

test_that(".lfs_quantify_lipids: edge case - empty/NULL match_item or non-data.frame inputs error", {
  expect_error(.lfs_quantify_lipids(mock_annotation_table(), mock_y_is_opt(), mock_is_table(), list()))
  expect_error(.lfs_quantify_lipids(mock_annotation_table(), mock_y_is_opt(), mock_is_table(), NULL))
  expect_error(.lfs_quantify_lipids("not a df", mock_y_is_opt(), mock_is_table(), mock_match_item()))
})

test_that(".lfs_quantify_lipids does not mutate any of its input tables", {
  annot <- mock_annotation_table(); annot_snapshot <- mock_annotation_table()
  y_is_opt <- mock_y_is_opt(); y_is_opt_snapshot <- mock_y_is_opt()
  is_tbl <- mock_is_table(); is_tbl_snapshot <- mock_is_table()

  .lfs_quantify_lipids(annot, y_is_opt, is_tbl, mock_match_item())

  expect_identical(annot, annot_snapshot)
  expect_identical(y_is_opt, y_is_opt_snapshot)
  expect_identical(is_tbl, is_tbl_snapshot)
})
