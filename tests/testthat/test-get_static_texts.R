# ==============================================================================
# inputs
# ==============================================================================

# ------------------------------------------------------------------------------
# path
# ------------------------------------------------------------------------------

testthat::test_that("errors if `json_path` does not exist", {

  testthat::expect_error(
    get_static_texts(json_path = "path/dne/document.json")
  )

})

testthat::test_that("errors if `json_path` points to non-json file", {

  testthat::expect_error(
    get_static_texts(
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

  static_texts_df <- get_static_texts(
    json_path = testthat::test_path(
      "fixtures", "qnr_metadata", "document.json"
    )
  )

  # is a data frame
  testthat::expect_s3_class(
    object = static_texts_df,
    class = "data.frame"
  )

  static_text_col_names_expected <- c(
    "object_type",
    "type",
    "public_key",
    "text",
    "attachment_name",
    "condition_expression",
    "hide_if_disabled"
  )

  # names of the data frame
  static_text_col_names_found <- names(static_texts_df)

  # has core variable attributes
  testthat::expect_true(
    all(
      c(
        static_text_col_names_expected
        # TODO: use a fixture where validations are actually present
          # while these may not always be present,
          # they are in the fixture JSON file
          # "validation_expression_1",
          # "validation_message_1",
          # "validation_severity_1"
      )

      %in%

      static_text_col_names_found

    )

  )

})
