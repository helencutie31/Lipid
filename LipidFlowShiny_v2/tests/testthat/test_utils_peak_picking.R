# Tests for R/utils_peak_picking.R (Giai doan 1: Peak Picking).
#
# .lfs_peak_table() itself needs a real S4 mass_dataset object (from
# massdataset), so it is not exercised directly here. Its pure, testable
# core - .lfs_merge_variable_expression() - is what's under test: matching
# variable_info (Peak_ID/m/z_mean/RT_peak) to expression_data (Peak_Area
# per sample) by variable_id, independent of any massdataset internals.

mock_variable_info <- function() {
  data.frame(
    variable_id = c("V1", "V2", "V3"),
    mz = c(700.51, 760.58, 810.60),
    rt = c(120.5, 300.2, 450.9),
    stringsAsFactors = FALSE
  )
}

mock_expression_data <- function() {
  m <- matrix(
    c(1000, 2000, 1500, 500, 800, 300, 4000, 4500, 4200),
    nrow = 3, byrow = TRUE,
    dimnames = list(c("V1", "V2", "V3"), c("Sample_A", "Sample_B", "Sample_C"))
  )
  m
}

test_that(".lfs_merge_variable_expression: happy path joins by variable_id, not row order", {
  var_info <- mock_variable_info()
  expr <- mock_expression_data()
  # Shuffle expression_data row order to prove the join isn't positional.
  expr_shuffled <- expr[c("V3", "V1", "V2"), , drop = FALSE]

  out <- .lfs_merge_variable_expression(var_info, expr_shuffled)

  expect_equal(nrow(out), 3)
  expect_equal(out$variable_id, c("V1", "V2", "V3"))
  expect_equal(out$Sample_A, c(1000, 500, 4000))
  expect_true(all(c("mz", "rt", "Sample_A", "Sample_B", "Sample_C") %in% colnames(out)))
})

test_that(".lfs_merge_variable_expression: edge case - NULL / non-data.frame variable_info errors", {
  expect_error(.lfs_merge_variable_expression(NULL, mock_expression_data()))
  expect_error(.lfs_merge_variable_expression("not a df", mock_expression_data()))
  expect_error(.lfs_merge_variable_expression(data.frame(x = 1), mock_expression_data()))
})

test_that(".lfs_merge_variable_expression: edge case - expression_data missing rows for some variable_id", {
  var_info <- mock_variable_info()
  expr_incomplete <- mock_expression_data()[c("V1", "V2"), , drop = FALSE] # V3 missing
  expect_error(.lfs_merge_variable_expression(var_info, expr_incomplete), "V3")
})

test_that(".lfs_merge_variable_expression: edge case - NULL expression_data errors", {
  expect_error(.lfs_merge_variable_expression(mock_variable_info(), NULL))
})

test_that(".lfs_merge_variable_expression does not mutate its inputs", {
  var_info <- mock_variable_info()
  expr <- mock_expression_data()
  var_info_snapshot <- mock_variable_info()
  expr_snapshot <- mock_expression_data()

  .lfs_merge_variable_expression(var_info, expr)

  expect_identical(var_info, var_info_snapshot)
  expect_identical(expr, expr_snapshot)
})
