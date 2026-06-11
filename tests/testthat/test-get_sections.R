# ==============================================================================
# inputs
# ==============================================================================

testthat::test_that("errors if `json_path` does not exist", {

  testthat::expect_error(
    get_sections(json_path = "path/dne/document.json")
  )

})

testthat::test_that("errors if `json_path` points to non-json file", {

  testthat::expect_error(
    get_sections(
      json_path = testthat::test_path(
        "fixtures", "qnr_metadata", "Categories",
        "0e00edf674ed45f4a93e6d226f2c069b.xlsx"
      )
    )
  )

})

# ==============================================================================
# outputs
# ==============================================================================

testthat::test_that("returns a data frame with the expected columns", {

  sections_df <- get_sections(
    json_path = testthat::test_path(
      "fixtures", "qnr_metadata", "document.json"
    )
  )

  testthat::expect_s3_class(
    object = sections_df,
    class = "data.frame"
  )

  section_cols_expected <- c(
    "object_type",
    "type",
    "title",
    "varname",
    "condition_expression",
    "public_key"
  )

  testthat::expect_true(
    all(section_cols_expected %in% names(sections_df))
  )

})
