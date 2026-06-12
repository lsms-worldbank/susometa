# ==============================================================================
# inputs
# ==============================================================================

# ------------------------------------------------------------------------------
# path
# ------------------------------------------------------------------------------

testthat::test_that("errors if `json_path` does not exist", {

  testthat::expect_error(
    get_categories(json_path = "path/dne/document.json")
  )

})

testthat::test_that("errors if `json_path` points to non-json file", {

  testthat::expect_error(
    get_categories(
      json_path = testthat::test_path(
        "fixtures", "qnr_metadata", "Categories",
        "0e00edf674ed45f4a93e6d226f2c069b.xlsx"
      )
    )
  )

})

# ------------------------------------------------------------------------------
# add section ID
# ------------------------------------------------------------------------------


# ==============================================================================
# outputs
# ==============================================================================

testthat::test_that("returns data frame with expected columns", {

  categories_df <- get_categories(
    json_path = testthat::test_path(
      "fixtures", "qnr_metadata", "document.json"
    )
  )

  # is a data frame
  testthat::expect_s3_class(
    object = categories_df,
    class = "data.frame"
  )

  categories_col_names_expected <- c(
    # TODO: add a newer JSON fixture where this is present
    # "type",
    "object_type",
    "Id",
    "Name"
  )

  # names of the data frame
  categories_col_names_found <- names(categories_df)

  # has core variable attributes
  testthat::expect_true(
    all(categories_col_names_expected %in% categories_col_names_found)
  )

})


