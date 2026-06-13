# ==============================================================================
# inputs
# ==============================================================================

# ------------------------------------------------------------------------------
# path
# ------------------------------------------------------------------------------

testthat::test_that("errors if `json_path` does not exist", {

  testthat::expect_error(
    get_lookup_tables(json_path = "path/dne/document.json")
  )

})

testthat::test_that("errors if `json_path` points to non-json file", {

  testthat::expect_error(
    get_lookup_tables(
      json_path = testthat::test_path(
        "fixtures", "qnr_metadata", "Categories",
        "0e00edf674ed45f4a93e6d226f2c069b.xlsx"
      )
    )
  )

})

# ------------------------------------------------------------------------------
# no lookup tables in the questionnaire
# ------------------------------------------------------------------------------

testthat::test_that("errors if no translations present", {

  testthat::expect_error(
    get_lookup_tables(
      json_path = testthat::test_path(
        "fixtures", "qnr_metadata", "childcare_demand_qnr.json"
      )
    )
  )

})


# ==============================================================================
# outputs
# ==============================================================================

testthat::test_that("returns data frame with expected columns", {

  lookup_tables_df <- get_lookup_tables(
    json_path = testthat::test_path(
      "fixtures", "qnr_metadata", "document.json"
    )
  )

  # is a data frame
  testthat::expect_s3_class(
    object = lookup_tables_df,
    class = "data.frame"
  )

  lookup_tables_col_names_expected <- c(
    "object_type",
    "lookup_table_id",
    "lookup_table_name",
    "lookup_table_file_name"
  )

  # names of the data frame
  lookup_tables_col_names_found <- names(lookup_tables_df)

  # has core variable attributes
  testthat::expect_true(
    all(lookup_tables_col_names_expected %in% lookup_tables_col_names_found)
  )

lookup_tables_col_names_expected[!lookup_tables_col_names_expected %in% lookup_tables_col_names_found]

})



