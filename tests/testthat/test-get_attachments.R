# ==============================================================================
# inputs
# ==============================================================================

# ------------------------------------------------------------------------------
# path
# ------------------------------------------------------------------------------

testthat::test_that("errors if `json_path` does not exist", {

  testthat::expect_error(
    get_attachments(json_path = "path/dne/document.json")
  )

})

testthat::test_that("errors if `json_path` points to non-json file", {

  testthat::expect_error(
    get_attachments(
      json_path = testthat::test_path(
        "fixtures", "qnr_metadata", "attachments",
        "0e00edf674ed45f4a93e6d226f2c069b.xlsx"
      )
    )
  )

})

# ------------------------------------------------------------------------------
# no attachments in the questionnaire
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

  attachments_df <- get_attachments(
    json_path = testthat::test_path(
      "fixtures", "qnr_metadata", "childcare_demand_qnr.json"
    )
  )

  # is a data frame
  testthat::expect_s3_class(
    object = attachments_df,
    class = "data.frame"
  )

  attachments_col_names_expected <- c(
    "object_type",
    "type",
    "attachment_id",
    "attachment_name",
    "attachment_content_id"
  )

  # names of the data frame
  attachments_col_names_found <- names(attachments_df)

  # has core variable attributes
  testthat::expect_true(
    all(attachments_col_names_expected %in% attachments_col_names_found)
  )

})


