# ==============================================================================
# inputs
# ==============================================================================

# ------------------------------------------------------------------------------
# path
# ------------------------------------------------------------------------------

testthat::test_that("errors if `json_path` does not exist", {

  testthat::expect_error(
    get_qnr_metadata(json_path = "path/dne/document.json")
  )

})

testthat::test_that("errors if `json_path` points to non-json file", {

  testthat::expect_error(
    get_qnr_metadata(
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

  qnr_metadata_list <- get_qnr_metadata(
    json_path = testthat::test_path(
      "fixtures", "qnr_metadata", "document.json"
    )
  )

  # is a list
  testthat::expect_type(
    object = qnr_metadata_list,
    type = "list"
  )

  # that is named
  testthat::expect_named(
    object = qnr_metadata_list
  )

  qnr_metadata_col_names_expected <- c(
    "Id",
    "Title",
    "VariableName"
  )

  # names of the data frame
  qnr_metadata_col_names_found <- names(qnr_metadata_list)

  # has core variable attributes
  testthat::expect_true(
    all(qnr_metadata_col_names_expected %in% qnr_metadata_col_names_found)
  )

})



