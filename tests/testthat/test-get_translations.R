# ==============================================================================
# inputs
# ==============================================================================

# ------------------------------------------------------------------------------
# path
# ------------------------------------------------------------------------------

testthat::test_that("errors if `json_path` does not exist", {

  testthat::expect_error(
    get_translations(json_path = "path/dne/document.json")
  )

})

testthat::test_that("errors if `json_path` points to non-json file", {

  testthat::expect_error(
    get_translations(
      json_path = testthat::test_path(
        "fixtures", "qnr_metadata", "Categories",
        "0e00edf674ed45f4a93e6d226f2c069b.xlsx"
      )
    )
  )

})

# ------------------------------------------------------------------------------
# translations in the target
# ------------------------------------------------------------------------------

testthat::test_that("errors if no translations present", {

  testthat::expect_error(
    get_translations(
      json_path = testthat::test_path(
        "fixtures", "qnr_metadata", "document.json"
      )
    )
  )

})

# ==============================================================================
# outputs
# ==============================================================================

testthat::test_that("returns data frame with expected columns", {

  translations_df <- get_translations(
    json_path = testthat::test_path(
      "fixtures", "qnr_metadata", "childcare_demand_qnr.json"
    )
  )

  # is a data frame
  testthat::expect_s3_class(
    object = translations_df,
    class = "data.frame"
  )

  translations_col_names_expected <- c(
    "type",
    "object_type",
    "Id",
    "Name"
  )

  # names of the data frame
  translations_col_names_found <- names(translations_df)

  # has core variable attributes
  testthat::expect_true(
    all(translations_col_names_expected %in% translations_col_names_found)
  )

})
