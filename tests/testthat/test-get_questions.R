# ==============================================================================
# inputs
# ==============================================================================

# ------------------------------------------------------------------------------
# path
# ------------------------------------------------------------------------------

testthat::test_that("errors if `json_path` does not exist", {

  testthat::expect_error(
    get_questions(json_path = "path/dne/document.json")
  )

})

testthat::test_that("errors if `json_path` points to non-json file", {

  testthat::expect_error(
    get_questions(
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

  # questionnaire JSON fixture with all possible keys present
  questions_df <- get_questions(
    json_path = testthat::test_path(
      "fixtures", "qnr_metadata", "document.json"
    )
  )

  # questionnaire JSON fixture with some keys missing
  questions_df_wo_linked_cols <- get_questions(
    json_path = testthat::test_path(
      "fixtures", "qnr_metadata", "critical_rules_qnr.json"
    )
  )

  # is a data frame
  testthat::expect_s3_class(
    object = questions_df,
    class = "data.frame"
  )

  question_col_names_expected <- c(
    "object_type",
    "type",
    "question_type",
    "condition_expression",
    "hide_if_disabled",
    "featured",
    "instructions",
    "public_key",
    "question_scope",
    "question_text",
    "stata_export_caption",
    "variable_label",
    "is_timestamp",
    "varname",
    "hide_instructions",
    "use_formatting_properites",
    "geometry_type",
    "geometry_input_mode",
    "is_critical",
    "mask",
    "show_as_list",
    "categories_id",
    "is_filtered_combo_box",
    "show_as_list_threshold",
    "cascade_from_question_id",
    "max_answer_count",
    "linked_to_question_id"
  )

  # names of the data frame
  question_col_names_found <- names(questions_df)

  # has core variable attributes
  testthat::expect_true(
    all(
      c(
        question_col_names_expected,
        # while these may not always be present,
        # they are in the fixture JSON file
        "answer_text_1",
        "answer_value_1",
        "validation_expression_1",
        "validation_message_1",
        "validation_severity_1"
      )

      %in%

      question_col_names_found

    )

  )

  # has all core variable attributes, even those not present in the JSON
  testthat::expect_true(
    all(question_col_names_expected %in% names(questions_df_wo_linked_cols))
  )

  testthat::expect_true(
    "section_id" %in% names(
      questions_df_w_section_id <- get_questions(
        json_path = testthat::test_path(
          "fixtures", "qnr_metadata", "document.json"
        ),
        add_section_id = TRUE
      )
    )
  )

})
