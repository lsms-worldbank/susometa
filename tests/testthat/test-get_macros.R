# ==============================================================================
# inputs
# ==============================================================================

# ------------------------------------------------------------------------------
# path
# ------------------------------------------------------------------------------

testthat::test_that("errors if `json_path` does not exist", {

  testthat::expect_error(
    get_macros(json_path = "path/dne/document.json")
  )

})

testthat::test_that("errors if `json_path` points to non-json file", {

  testthat::expect_error(
    get_macros(
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

testthat::test_that("returns data frame with expected columns", {

  macros_df <- get_macros(
    json_path = testthat::test_path(
      "fixtures", "qnr_metadata", "document.json"
    )
  )

  # is a data frame
  testthat::expect_s3_class(
    object = macros_df,
    class = "data.frame"
  )

  macros_col_names_expected <- c(
    "object_type",
    # TODO: use a newer JSON fixture where this is present
    # "type",
    "macro_id",
    "macro_name",
    "macro_description",
    "macro_content"
  )

  # names of the data frame
  macros_col_names_found <- names(macros_df)

  # has core variable attributes
  testthat::expect_true(
    all(macros_col_names_expected %in% macros_col_names_found)
  )

})
