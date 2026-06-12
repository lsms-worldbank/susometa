# ==============================================================================
# inputs
# ==============================================================================

# ------------------------------------------------------------------------------
# path
# ------------------------------------------------------------------------------

testthat::test_that("errors if `json_path` does not exist", {

  testthat::expect_error(
    get_questions_by_section(json_path = "path/dne/document.json")
  )

})

testthat::test_that("errors if `json_path` points to non-json file", {

  testthat::expect_error(
    get_questions_by_section(
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

  questions_by_section_df <- get_questions_by_section(
    json_path = testthat::test_path(
      "fixtures", "qnr_metadata", "document.json"
    )
  )

  # is a data frame
  testthat::expect_s3_class(
    object = questions_by_section_df,
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

  col_names_expected <- c(
    # common key
    "section_id",
    # question attributes
    question_col_names_expected,
    # while these may not always be present,
    # they are in the fixture JSON file
    "answer_text_1",
    "answer_value_1",
    "validation_expression_1",
    "validation_message_1",
    "validation_severity_1",
    # variable attributes
    variables_col_names_expected
  ) |>
  # remove duplicate attributes that appear for questions and variables
  # (e.g., `varname`)
	base::unique()

  # names of the data frame
  col_names_found <- names(questions_by_section_df)

  # has core variable attributes
  testthat::expect_true(
    all(col_names_expected %in% col_names_found)
  )

})

