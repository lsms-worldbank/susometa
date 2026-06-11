# ==============================================================================
# inputs
# ==============================================================================

testthat::test_that("errors if `json_path` does not exist", {

  testthat::expect_error(
    get_rosters(json_path = "path/dne/document.json")
  )

})

testthat::test_that("errors if `json_path` points to non-json file", {

  testthat::expect_error(
    get_rosters(
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

testthat::test_that("get_rosters() returns data frame with expected columns", {

  rosters_df <- get_rosters(
    json_path = testthat::test_path(
      "fixtures", "qnr_metadata", "document.json"
    )
  )

  # is a data frame
  testthat::expect_s3_class(
    object = rosters_df,
    class = "data.frame"
  )

  roster_col_names_expected <- c(
    "object_type",
    "type",
    "condition_expression",
    "hide_if_disabled",
    "is_flat_mode",
    "is_plain_mode",
    "display_mode",
    "enabled",
    "description",
    "varname",
    "is_roster",
    "custom_roster_title",
    "roster_size_question_id",
    "roster_size_source",
    "public_key",
    "title"
  )

  # names of the data frame
  roster_col_names_found <- names(rosters_df)

  # has expected attributes
  testthat::expect_true(
    all(
      c(
        roster_col_names_expected,
        # this will not be the case in general
        # but for the JSON used as text fixture, there is a fixed roster
        "fixed_roster_value_1", "fixed_roster_title_1"
      )

      %in%

      roster_col_names_found
    )
  )

})
