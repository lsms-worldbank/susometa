# ==============================================================================
# inputs
# ==============================================================================

# ------------------------------------------------------------------------------
# path
# ------------------------------------------------------------------------------

testthat::test_that("errors if `json_path` does not exist", {

  testthat::expect_error(
    get_variables(json_path = "path/dne/document.json")
  )

})

testthat::test_that("errors if `json_path` points to non-json file", {

  testthat::expect_error(
    get_variables(
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

  variables_df <- get_variables(
    json_path = testthat::test_path(
      "fixtures", "qnr_metadata", "document.json"
    )
  )

  # is a data frame
  testthat::expect_s3_class(
    object = variables_df,
    class = "data.frame"
  )

  variables_col_names_expected <- c(
    "object_type",
    "type",
    "label_variable",
    "public_key",
    "type_variable",
    "name_variable",
    "expression_variable",
    "do_not_export",
    "varname"
  )

  # names of the data frame
  variables_col_names_found <- names(variables_df)

  # has core variable attributes
  testthat::expect_true(
    all(variables_col_names_expected %in% variables_col_names_found)
  )

  testthat::expect_true(
    "section_id" %in% names(
      variables_df_w_section_id <- get_variables(
        json_path = testthat::test_path(
          "fixtures", "qnr_metadata", "document.json"
        ),
        add_section_id = TRUE
      )
    )
  )

})

