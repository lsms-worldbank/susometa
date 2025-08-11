# ==============================================================================
# inputs
# ==============================================================================


# ==============================================================================
# outputs
# ==============================================================================

testthat::test_that("get_validations() returns data frame with expected columns", {

  # load input data
  qnr_df <- readRDS(testthat::test_path("fixtures", "qnr_df.rds"))
  validations_df <- get_validations(qnr_df = qnr_df)

  # is a data frame
  testthat::expect_s3_class(
    object = validations_df,
    class = "data.frame"
  )

  # names of the data frame
  # expected
  validations_expected_col_names <- c(
    "type",
    "varname",
    "text",
    "expression_number",
    "validation_expression",
    "validation_message",
    "severity"
  )
  # actual
  validations_col_names <- names(validations_df)

  # has expected attributes
  testthat::expect_true(
    all(validations_expected_col_names %in% validations_col_names)
  )

})
