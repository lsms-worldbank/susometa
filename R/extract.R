#' Get metadata for section.
#'
#' @description
#' Extract metadata about sections from the questionnaire JSON
#' as a data frame.
#'
#' @param json_path Character. Full path to the Survey Solutions questionnaire
#' JSON file (e.g., `~/my_proj/path/to/document.json`).
#'
#' @return Data frame with the following columns:
#'
#' - `object_type`. Character. Simplified object type. Value: `section`.
#' - `type`. Character. SuSo-provided type. Value: `Group.`
#' - `title`. Character.
#' - `varname`. Character.
#' - `condition_expression`. Character.
#' - `public_key`. Character. GUID.
#'
#' @importFrom jqr jq
#' @importFrom jsonlite fromJSON
#'
#' @export
get_sections <- function(json_path) {

  check_json_path(path = json_path)

  jq_expr <-
    paste0(
      # function definitions
      jq_rename_group_attribs,
      jq_rename_from_pascal_to_snake_case,
      # query
      '[
      # take the immediate children of the document: the sections
      .Children[]
      # select children whose type is group: sections
      | select(."$type" == "Group")
      # drop their immediate children (e.g., questions, rosters, etc)
      | del(.Children)
      # delete attributes that are irrelevant for sections
      | del(
        # roster attributes
        .IsFlatMode, .IsPlainMode, .DisplayMode,
        .IsRoster, .CustomRosterTitle, .RosterSizeSource,
        .RosterSizeQuestionId, .RosterTitleQuestionId, .FixedRosterTitles,
        # other attributes
        .Description, .HideIfDisabled, .Enabled
      )
      # rename known attributes
      | rename_group_attribs
      # rename all other attributes with snake case
      | rename_from_pascal_to_snake_case
      # add object type attribute
      | . + { "object_type" : "section" }
      ]'

    )

  sections_json <- base::file(json_path) |>
    jqr::jq(jq_expr)

  sections_df <- jsonlite::fromJSON(sections_json)

}

#' Get metadata for sub-sections
#'
#' @description
#' Extract metadata about sub-sections from the questionnaire JSON
#' as a data frame.
#'
#' @inheritParams get_sections
#'
#' @return Data frame with the following columns:
#'
#' - `object_type`. Character. Simplified object type. Value: `sub-section`.
#' - `type`. Character. SuSo provided object type. Value: Group.
#' - `title`. Character.
#' - `variable_name`. Character.
#' - `condition_expression`. Character.
#' - `public_key`. Character. GUID.
#'
#' @importFrom jqr jq
#' @importFrom jsonlite fromJSON
#'
#' @export
get_sub_sections <- function(json_path) {

  check_json_path(path = json_path)

  jq_expr <- paste0(
    # function definitions
    jq_rename_group_attribs,
    jq_rename_from_pascal_to_snake_case,
    # query
    '[
        # move into document content
        .Children[]
        # select sections
        | select(."$type" == "Group" and .IsRoster == false)
        # find sub-sections within those sections
        # descending until one is found
        | .Children[]
        | ..
        | objects
        | select(."$type" == "Group" and .IsRoster == false)
        # remove children of the sub-section
        | del(.Children)
        # delete attributes that are irrelevant for sections
        | del(
            # roster attributes
            .IsFlatMode, .IsPlainMode, .DisplayMode,
            .IsRoster, .CustomRosterTitle, .RosterSizeSource,
            .RosterSizeQuestionId, .RosterTitleQuestionId, .FixedRosterTitles,
            # other attributes
            .Description, .HideIfDisabled, .Enabled
        )
        # rename attributes
        # ... known sub-section attributes
        | rename_group_attribs
        # ... unknown other attributes from Pascal to snake case
        | rename_from_pascal_to_snake_case
        # add object type attribute
        | . + { "object_type" : "sub-section" }
    ]'
  )

  sub_sections_json <- base::file(json_path) |>
    jqr::jq(jq_expr)

  sub_sections_df <- jsonlite::fromJSON(sub_sections_json)

}

#' Get metadata about rosters.
#'
#' @description
#' Extract metadata about rosters from the questionnaire JSON
#' as a data frame.
#' 
#' @inheritParams get_sections
#'
#' @return Data frame with the following columns:
#'
#' - `object_type`. Character. Simplified object type. Value: `roster`.
#' - `type`. Character. SuSo-provided object type. Value: `Group`.
#' - `condition_expression`
#' - `hide_if_disabled`
#' - `is_flat_mode`
#' - `is_plain_mode`
#' - `display_mode`
#' - `enabled`
#' - `description`
#' - `varname`
#' - `is_roster`
#' - `custom_roster_title`
#' - `roster_size_question_id`
#' - `roster_size_source`
#' - `public_key`
#' - `title`
#' - `fixed_roster_title_*`
#' - `fixed_roster_value_*`
#'
#' @importFrom jqr jq
#' @importFrom jsonlite fromJSON
#'
#' @export
get_rosters <- function(json_path) {

  check_json_path(path = json_path)

  jq_expr <- paste0(
    # function definitions
    jq_def_flatten_fixed_roster_titles,
    jq_rename_group_attribs,
    jq_rename_from_pascal_to_snake_case,
    # query
    '
    [ 
      # collect all rosters
      ..
      | objects
      | select(.IsRoster == true)
      # remove their children
      | del(.Children)
      # flatten fixed roster titles
      | flatten_fixed_roster_titles 
      # rename keys
      # ... known keys
      | rename_group_attribs
      # ... unknown keys from Pascal to snake case
      | rename_from_pascal_to_snake_case
      # add object type attribute
      | . + { "object_type" : "roster" }
    ]
    '
  )

  rosters_json <- jqr::jq(
    x = base::file(json_path),
    jq_expr
  )

  rosters_df <- jsonlite::fromJSON(rosters_json)

}

#' Get metadata for questions
#'
#' @inheritParams get_sections
#' @param add_section_id Boolean. Whether to include the public key of the
#' section that contains the question.
#'
#' @return Data frame with the following columns:
#'
#' - `section_id`. Character. GUID. Present only if `add_section_id = TRUE`.
#' - `object_type`. Character. Simplified object type. Value: `question`.
#' - `type`. Character. SuSo-provided object type. Value: question type.
#' - `question_type`
#' - `condition_expression`     
#' - `hide_if_disabled`
#' - `featured`
#' - `instructions`
#' - `public_key`               
#' - `question_scope`
#' - `question_text`
#' - `stata_export_caption`
#' - `variable_label`
#' - `is_timestamp`
#' - `varname`
#' - `hide_instructions`
#' - `use_formatting_properites`
#' - `geometry_type`
#' - `geometry_input_mode`
#' - `is_critical`
#' - `mask`
#' - `answer_text_*`
#' - `answer_value_*`
#' - `validation_expression_*`
#' - `validation_message_*`
#' - `validation_severity_*`
#' - `show_as_list`
#' - `categories_id`
#' - `is_filtered_combo_box`
#' - `show_as_list_threshold`
#' - `cascade_from_question_id`
#' - `max_answer_count`
#' - `linked_to_question_id`
#'
#' @importFrom cli cli_abort
#' @importFrom glue glue
#' @importFrom jqr jq
#' @importFrom jsonlite fromJSON
#'
#' @export
get_questions <- function(
  json_path,
  add_section_id = FALSE
) {

  # ============================================================================
  # check inputs
  # ============================================================================

  # path
  check_json_path(path = json_path)

  # section ID toggle
  if (!is.logical(add_section_id)) {
    cli::cli_abort(
      message = c(
        "x" = "{.arg add_section_id} must be logical (TRUE / FALSE)."
      )
    )
  }

  # ============================================================================
  # construct query
  # ============================================================================

  # conditionally add content to the query
  if (add_section_id == TRUE) {

    jq_get_sections <- '
      # collect sections, the immediate children of the document
      | .Children[]
      | select(."$type" == "Group")
      | . as $section
    '

    jq_add_section_id <- '
      # construct an object for each question/variable that contains
      # the title of its ancestor section and
      # the attributres of the question
      | {
          "section_id": $section.PublicKey,
        }
        + .
    '
  # otherwise, set these conditional parts to be empty
  } else {

    jq_get_sections <- ''

    jq_add_section_id <- ''

  }

  jq_questions_expr <- glue::glue(
    '[
      .
      <jq_get_sections>
      # collect questions
      | ..
      | objects
      | select(has("QuestionType"))
      <jq_add_section_id>
      # hoist properties out of properties object
      | . + {
        "hide_instructions": .Properties.HideInstructions,
        "use_formatting_properites": .Properties.UseFormatting,
        # make "nullable" since these properties may not be present in
        # older versions of the JSON file
        "geometry_type": .Properties.GeometryType?,
        "geometry_input_mode": .Properties.GeometryInputMode?,
        "is_critical": .Properties.IsCritical?,
      }
      # remove the original Properties object
      | del(.Properties)
      # remove always-empty Children
      | del(.Children)
      # transform arrays of answer objects to
      # for answers
      # a numbered set of key-value pairs
      | flatten_answers
      # for validations
      | flatten_validations
      # rename keys
      # ... for known keys
      | rename_question_attribs
      # ... for unknown keys, from Pascal to snake case
      | rename_from_pascal_to_snake_case
      # add object type attribute
      | . + { "object_type" : "question" }
    ]',
    .open = "<",
    .close = ">"
  )

  jq_expr <-
    paste0(
      # function definitions
      jq_def_flatten_answers,
      jq_def_flatten_validations,
      jq_rename_question_attribs,
      jq_rename_from_pascal_to_snake_case,
      # query
      jq_questions_expr
    )

  questions_json <- base::file(json_path) |>
    jqr::jq(jq_expr)

  questions_df <- jsonlite::fromJSON(questions_json)

  return(questions_df)

}

#' Get metadata for computed variables.
#'
#' @description
#' Extract metadata about computed variables from the questionnaire JSON
#' as a data frame.
#'
#' @inheritParams get_questions
#'
#' @return Data frame with the following columns:
#'
#' - `section_id`. Character. GUID. Present only if `add_section_id = TRUE`.
#' - `object_type`. Character. Simplified object type. Value: `variable`.
#' - `type`. Character. SuSo-provided object type. Value: `Variable`.
#' - `label_variable`
#' - `public_key`
#' - `type_variable`
#' - `name_variable`
#' - `expression_variable`
#' - `do_not_export`
#' - `varname`
#'
#' @importFrom cli cli_abort
#' @import glue glue
#' @importFrom jqr jq
#' @importFrom jsonlite fromJSON
#'
#' @export
get_variables <- function(
  json_path,
  add_section_id = FALSE
) {

  # ============================================================================
  # check inputs
  # ============================================================================

  # path
  check_json_path(path = json_path)

  # section ID toggle
  if (!is.logical(add_section_id)) {
    cli::cli_abort(
      message = c(
        "x" = "{.arg add_section_id} must be logical (TRUE / FALSE)."
      )
    )
  }

  # ============================================================================
  # construct query
  # ============================================================================

  # conditionally add content to the query
  if (add_section_id == TRUE) {

    jq_get_sections <- '
      # collect sections, the immediate children of the document
      | .Children[]
      | select(."$type" == "Group")
      | . as $section
    '

    jq_add_section_id <- '
      # construct an object for each question/variable that contains
      # the title of its ancestor section and
      # the attributres of the question
      | {
          "section_id": $section.PublicKey,
        }
        + .
    '
  # otherwise, set these conditional parts to be empty
  } else {

    jq_get_sections <- ''

    jq_add_section_id <- ''

  }

  jq_variables_expr <- glue::glue('
    [
      .
      <jq_get_sections>
      # collect all variables whereve they may be found in the tree
      | ..
      | objects
      | select(."$type" == "Variable")
      <jq_add_section_id>
      # remove the always empty `Children` array
      | del(.Children)
      # rename attributes
      # ... known attributes
      | rename_variable_attribs
      # ... unknown attributes from Pascal to snake case
      | rename_from_pascal_to_snake_case
      # add object type attribute
      | . + { "object_type" : "variable" }
    ]',
    .open = "<",
    .close = ">"
  )

  jq_expr <- paste0(
    # function definitions
    jq_rename_variable_attribs,
    jq_rename_from_pascal_to_snake_case,
    # query
    jq_variables_expr
  )

  variables_json <- base::file(json_path) |>
    jqr::jq(jq_expr)

  variables_df <- jsonlite::fromJSON(variables_json)

}

#' Get questions with the section to which each belongs.
#'
#' @description
#' Extract metadata about questions and variables and their parent section
#' from the questionnaire JSON as a data frame.
#'
#' @inheritParams get_sections
#'
#' @return Data frame with the following columns
#'
#' - `title` and `section_id` (PublicKey) for sections
#' - All the columns from `get_questions()` and `get_variables()`
#'
#' @seealso get_sections get_questions get_variables
#' 
#' @importFrom dplyr bind_rows select left_join
#'
#' @export
get_questions_by_section <- function(json_path) {

  # ============================================================================
  # check inputs
  # ============================================================================

  # path
  check_json_path(path = json_path)

  # ============================================================================
  # retrieve
  # ============================================================================

  sections_df <- get_sections(json_path = json_path)

  questions_df <- get_questions(
    json_path = json_path,
    add_section_id = TRUE
  )

  variables_df <- get_variables(
    json_path = json_path,
    add_section_id = TRUE
  )

  # ============================================================================
  # construct data frame
  # ============================================================================

  questions_and_variables_df <- dplyr::bind_rows(
    questions_df, variables_df
  )

  questions_by_sections <- sections_df |>
    # select common attributes only
    dplyr::select(title, section_id = public_key) |>
    # join questions to the section to which each belongs
    dplyr::left_join(questions_and_variables_df, by = "section_id")

  return(questions_by_sections)

}

#' Get answer options
#'
#' @description
#' Obtain the answer options for a target variable as a set of name-value pairs
#' in labelled numeric vector.
#'
#' @inheritParams get_sections
#' @param varname Variable name. Bare name of variable whose answer options to
#' extract.
#' @param categories_dir Character. Path to the `Categories` directory
#' of metadata and/or where the `.xlsx` files containing reusable categories
#' can be found.
#' @param to_exclude Numeric vector. Code of answer options to exclude.
#'
#' @return Named numeric vector.
#' Values are answer codes. Names are answer labels.
#'
#' @importFrom rlang as_label expr .data
#' @importFrom jqr jq
#' @importFrom glue glue glue_collapse
#' @importFrom jsonlite fromJSON
#' @importFrom cli cli_abort
#' @importFrom dplyr filter
#' @importFrom stats setNames
#'
#' @export
get_answer_options <- function(
  json_path,
  categories_dir,
  varname,
  to_exclude = NULL
) {

  # capture variable name as text
  varname_txt <- rlang::as_label(rlang::expr({{varname}}))

  # ============================================================================
  # check inputs
  # ============================================================================

  # json_path
  check_json_path(path = json_path)

  # varname
  # TODO: check that it is a bare name

  # to_exclude
  if (
    !(
      # either default `NULL`
      is.null(to_exclude) |
      # or a numeric vector
      (
        is.numeric(to_exclude) &
        is.vector(to_exclude)
      )

    )
  ) {
    cli::cli_abort(
      message = c(
        "x" = "{.arg to_exclude} must be either `NULL` or a numeric vector."
      )
    )
  }


  # ============================================================================
  # check target's content in JSON
  # ============================================================================

  # ----------------------------------------------------------------------------
  # variable
  # ----------------------------------------------------------------------------

  varname_exists <- jqr::jq(
    x = base::file(json_path),
    glue::glue(
      'any(
        .. | objects | select(has("QuestionType"));
        (.VariableName == "<varname_txt>")
      )',
      .open = '<',
      .close = '>'
    )
  ) |>
	jsonlite::fromJSON()

  if (varname_exists == FALSE) {
    cli::cli_abort(
      message = c(
        "x" = "No variable named {.var {varname_txt}} found.",
        "i" = "Please check the name of the target variable.",
        "i" = paste(
          "Note: variable names should be specified",
          "as they appear in Designer.",
          sep = " "
        )
      )
    )
  }

  # ----------------------------------------------------------------------------
  # answer options
  # ----------------------------------------------------------------------------

  has_any_answer_options <- jqr::jq(
    x = base::file(json_path),
    glue::glue(
      'any(
        .. | objects;
        .VariableName == "<varname_txt>"
        and (
          ((.Answers // []) | length > 0)
          or
          (.CategoriesId != null)
        )
      )',
      .open = '<',
      .close = '>'
    )
  ) |>
	jsonlite::fromJSON()

  if (has_any_answer_options == FALSE) {

    cli::cli_abort(
      message = c(
        "x" = "The question ({varname_txt}) does not have any answer options."
      )
    )

  }

  if (!is.null(to_exclude)) {

    to_exclude_txt <- glue::glue_collapse(x = to_exclude, sep = ", ", last = ", ")

    has_remaining_answer_options <- base::file(json_path) |>
      jqr::jq(
        glue::glue(
          'any(
            (
              ..
              | objects
              | select(.VariableName == "<varname_txt>")
              | .Answers[]
            );
            # note: using `AnswerValue` since `AnswerCode` not in older JSON
            (.AnswerValue | tonumber | IN(<to_exclude_txt>) | not)
          )
          ',
          .open = '<',
          .close = '>'
        )
      ) |>
      jsonlite::fromJSON() 

    if (has_remaining_answer_options == FALSE) {

      cli::cli_abort(
        message = c(
          "x" = "No answers remain after excluding those in {.arg to_exclude}."
        )
      )

    }

  }

  # ============================================================================
  # determine how/where to retrieve answer options
  # ============================================================================

  has_answers <- jqr::jq(
    x = base::file(json_path),
    glue::glue(
      'any(
        .. | objects | select(.VariableName == "<varname_txt>");
        .Answers[]
      )
      ',
      .open = '<',
      .close = '>'
    )
  ) |>
	jsonlite::fromJSON()

  has_categories <- jqr::jq(
    x = base::file(json_path),
    glue::glue(
      'any(
        .. | objects | select(.VariableName == "<varname_txt>");
        .CategoriesId != null
      )
      ',
      .open = '<',
      .close = '>'
    )
  ) |>
	jsonlite::fromJSON()

  if (has_categories == TRUE & has_answers == TRUE) {
    cli::cli_abort(
      message = c(
        "x" = "Cannot resolve which answers to choose.",
        "*" = "{varname_txt} has both answers and reusable categories.",
        "*" = "The package currently does not handle this case.",
        "i" = "Create an issue on GitHub that includes `document.json`."
      )
    )
  }

  # ============================================================================
  # retrieve answer options
  # ============================================================================

  # ----------------------------------------------------------------------------
  # case 1: has answers
  # ----------------------------------------------------------------------------

  if (has_answers == TRUE) {

    jq_expr <- glue::glue(
      '
      ..
      | objects
      | select(.VariableName == "<varname_txt>")
      | .Answers
      ',
      .open = '<',
      .close = '>'
    )

    answers_json <- jqr::jq(
      x = base::file(json_path),
      jq_expr
    )

    answers_df <- jsonlite::fromJSON(answers_json)

    if (!is.null(to_exclude)) {

      answers_df <- answers_df |>
        dplyr::filter(!as.numeric(.data$AnswerValue) %in% to_exclude)

      if (nrow(answers_df) == 0) {
        cli::cli_abort(
          message = c(
            "x" = "No answers remain after excluding those in `to_exclude`."
          )
        )
      }

    }

    # note: using AnswerValue instead of AnswerCode
    # because the latter is present in newer files
    # while the former is present both in older and newer files
    answers <- stats::setNames(
      object = as.numeric(answers_df$AnswerValue),
      nm = answers_df$AnswerText
    )

    return(answers)

  }

  # ----------------------------------------------------------------------------
  # case 2: has categories
  # ----------------------------------------------------------------------------

  if (has_categories) {

    jq_expr <- glue::glue(
      '
      ..
      | objects
      | select(.VariableName == "<varname_txt>")
      | .CategoriesId
      ',
      .open = '<',
      .close = '>'
    )

    categories_id <- base::file(json_path) |>
      # extract categories
	    jqr::jq(jq_expr) |>
      # convert to R object
      jsonlite::fromJSON() |>
      # remove dashes
      # in JSON, `CategoriesID` uses `-` in the GUID
      # in the `Categories` directory, file names do not use the dash
      # need to harmonize the naming system for the two
      base::gsub(
        pattern = "-",
        replacement = ""
      )

    categories_df <- fs::path(categories_dir, paste0(categories_id, ".xlsx")) |>
      parse_categories_file()

    if (!is.null(to_exclude)) {

      categories_df <- categories_df |>
        dplyr::filter(!as.numeric(.data$AnswerValue) %in% to_exclude)

      if (nrow(categories_df) == 0) {
        cli::cli_abort(
          message = c(
            "x" = "No answers remain after excluding those in {.arg to_exclude}."
          )
        )
      }

    }

    answers <- stats::setNames(
      object = as.numeric(categories_df$value),
      nm = categories_df$text
    )

    return(answers)

  }

}

#' Get mutli-select answers as variable labels
#'
#' @description
#' When exported, multi-select questions become a set of variables whose
#' name is a combination of the variable name and answer option value in
#' Designer. This function extracts answer options in convenient form to
#' either label variables in data or columns in display tables,
#' where expected specification for labelling is of the form
#' `{variable} = {variable label}`.
#'
#' @inheritParams get_answer_options
#'
#' @return Named character variable.
#' Names are column names of the form `{variable}__{value}`.
#' Values are the answer options.
#'
#' @importFrom stats setNames
#' @importFrom rlang as_label expr
#'
#' @export
get_ms_answers_as_var_labels <- function(
    json_path,
    categories_dir = NULL,
    varname,
    to_exclude = NULL
) {

  # ============================================================================
  # check inputs
  # ============================================================================

  # json_path
  check_json_path(path = json_path)

  # ============================================================================
  # get answers
  # ============================================================================

  # extract answer options
  answer_options <- get_answer_options(
    json_path = json_path,
    categories_dir = categories_dir,
    varname = {{varname}},
    to_exclude = to_exclude
  )

  # ============================================================================
  # transform them into colum labels
  # ============================================================================

  # create named vector, where:
  # - names are variable names
  # - values are text 
  column_labels <- stats::setNames(
    object = names(answer_options),
    nm = paste0(
      # construct variable name from: 
      # - variable stub name
      rlang::as_label(rlang::expr({{varname}})),
      # - separator
      "__",
      # - value
      # replace negative value (-) with "n"
      gsub(x = unname(answer_options), pattern = "-", replacement = "n", fixed = TRUE)
    )
  )

  return(column_labels)

}

#' Get metadata for validations
#'
#' @inheritParams get_sections
#'
#' @return Data frame with following columns:
#'
#' - `public_key`. Character. GUID of object. Not available for static text.
#' - `object_type`. Character. Simplified object type. Value: `section`.
#' - `type`. Character.
#' - `varname`. Character.
#' - `text`. Character.
#' - `expression_number`. Numeric.
#' - `validation_message`. Character.
#' - `validation_expression`. Character.
#' - `severity`. Numeric.
#'
#' @importFrom jqr jq
#' @importFrom jsonlite fromJSON
#'
#' @export
get_validations <- function(json_path) {

  # ============================================================================
  # check inputs
  # ============================================================================

  check_json_path(path = json_path)

  # ============================================================================
  # get data from JSON
  # ============================================================================

  jq_expr <- '[
    # collect all objects with a non-emtpy array of validation conditions
    ..
    | objects
    | select((.ValidationConditions // []) | length > 0)
    # store that stream for later use
    | . as $q
    # convert the validation conditions array such as follows:
    # { "key": index_validation_object , "value: [ vals_in_validation_object ] }
    # and use this to construct a new set of objects that draw
    # from both the original stream of objects with validations
    # and the contents of the validations expressions objects
    | $q.ValidationConditions
    | to_entries[]
    | {
        "public_key": $q.PublicKey?,
        "object_type": "validation",
        "type": $q.["$type"],
        "varname": $q.VariableName?,
        "text": $q.Text?,
        "expression_number": (.key + 1),
        "validation_message": .value.Message,
        "validation_expression": .value.Expression,
        "severity": .value.Severity
      }
    ]'

  validations_json <- base::file(json_path) |>
    jqr::jq(jq_expr)

  validations_df <- jsonlite::fromJSON(validations_json)

}

#' Get metadata on static text.
#'
#' @description
#' Extract metadata about sections from the questionnaire JSON
#' as a data frame.
#'
#' @inheritParams get_sections
#'
#' @return Data frame with the following columns:
#'
#' - `object_type`. Character. Simplified object type. Value: `section`.
#' - `type`. Character. SuSo-provided type. Value: `Group.`
#' - `public_key`. Character. GUID for object.
#' - `text`. Character.
#' - `attachment_name`. Character.
#' - `condition_expression`. Character.
#' - `hide_if_disabled`. Logical.
#' - `validation_expression_*`. Character. Present if validations used.
#' - `validation_message_*`. Character. Present if validations used.
#' - `validation_severity_*`. Character. Present if validations used.
#'
#' @importFrom jqr jq
#' @importFrom jsonlite fromJSON
#'
#' @export
get_static_texts <- function(json_path) {

  # ============================================================================
  # check inputs
  # ============================================================================

  # path
  check_json_path(path = json_path)

  # ============================================================================
  # compose expression
  # ============================================================================

  jq_expr <- paste0(
    # function definitions
    jq_def_flatten_validations,
    jq_def_rename_static_text,
    jq_rename_from_pascal_to_snake_case,
    # query
    '[
      # collect all static text objects
      ..
      | objects
      | select(."$type" == "StaticText")
      # remove irrelvant attributes
      | del(.Children, .VariableName)
      # flatten validations
      | flatten_validations
      # rename attributes
      | rename_static_text
      # add object type attribute
      | . + { "object_type" : "static text" }
    ]'
  )

  # ============================================================================
  # get data
  # ============================================================================

  static_texts_json <- base::file(json_path) |>
    jqr::jq(jq_expr)

  static_texts_df <- jsonlite::fromJSON(static_texts_json)

}

#' Get metadata for macros.
#'
#' @description 
#' Extract metadata about macros from the questionnaire JSON
#' as a data frame.
#'
#' @inheritParams get_sections
#'
#' @return Data frame with the following columns:
#'
#' - `object_type`. Character. Type of object. Value: `macro`.
#' - `type`. Character. SuSo-provided object type. Value: `Macro`.
#' May not be present in older JSON versions.
#' - `macro_id`. Character.
#' - `macro_name`. Character.
#' - `macro_description`. Character.
#' - `macro_content`. Character.
#'
#' @importFrom jqr jq
#' @importFrom jsonlite fromJSON
#'
#' @export
get_macros <- function(json_path) {

  # ============================================================================
  # check inputs
  # ============================================================================

  # path
  check_json_path(path = json_path)

  # ============================================================================
  # compose expression
  # ============================================================================

  jq_expr <- paste0(
    # function definitions
    jq_def_rename_macros,
    jq_rename_from_pascal_to_snake_case,
    # query
    '
    # select top-level macros
    .Macros
    # remove `$type` key-value pair that begins the list of macro objects, if present
    # note: `del()` has no effect the key-value pair is not present
    | del(."$type")
    # note: each macro object is structured as follows:
    # "name_is_some_guid": { "$type": "type", "key1": "value1", "key2": "value2" }
    #
    # restructure the macro array so that it consists of a key and value entries
    # as follows:
    # [ "key": .key, "value": .value ]
    # where `.value` is the initial content of each element in the macro array
    | to_entries
    # restructure each element of the macro array as follows:
    # [ "macro_id": .key, .value ]
    # where `.value` is the value of the initial macro array
    | map({ macro_id: .key } + .value)
    # rename
    # ... for known properties
    | map(rename_macros)
    # ... for unknown properties, from Pascal to snake case
    | map(rename_from_pascal_to_snake_case)
    # add object type attribute
    | map(. + { "object_type" : "macro" })
    '
  )

  # ============================================================================
  # get data
  # ============================================================================

  macros_json <- base::file(json_path) |>
    jqr::jq(jq_expr)

  macros_df <- jsonlite::fromJSON(macros_json)

}

#' Get metadata on categories.
#'
#' @description
#' Extract metadata about categories from the questionnaire JSON
#' as a data frame.
#'
#' @inheritParams get_sections
#'
#' @return Data frame with the following columns:
#'
#' - `object_type`. Character. Simplified object type. Value: `reusable category`.
#' - `$type`. Character. May not be present in older JSON files.
#' - `Id`. Character.
#' - `Name`. Character.
#' 
#' @importFrom jqr jq
#' @importFrom jsonlite fromJSON
#' @importFrom cli cli_abort
#'
#' @export
get_categories <- function(json_path) {

  # ============================================================================
  # check inputs
  # ============================================================================

  # path
  check_json_path(path = json_path)

  # ============================================================================
  # check whether targets are present in the JSONS
  # ============================================================================

  # check whether any categories exist
  categories_exist <- base::file(json_path) |>
    jqr::jq('any(.Categories; length > 0)') |>
    jsonlite::fromJSON()

  if (categories_exist == FALSE){
    cli::cli_abort(
      message = c(
        "x" = "No reusable categories found in the questionnaire."
      )
    )
  }

  # ============================================================================
  # get data
  # ============================================================================

  categories_json <- jqr::jq(
    x = base::file(json_path),
    '
    .Categories
    # add object type attribute
    | map(. + { "object_type": "reusable category" })
    '
  )

  categories_df <- jsonlite::fromJSON(categories_json)

}

#' Get metadata about translations.
#'
#' @description
#' Extract metadata about translations from the questionnaire JSON
#' as a data frame.
#'
#' @inheritParams get_sections
#'
#' @return Data frame with the following columns:
#' 
#' - `object_type`. Character. Simplified object type. Value: `translation`.
#' - `$type`. Character. May not be present in older JSON files.
#' - `Id`. Character.
#' - `Name`. Character.
#'
#' @importFrom jqr jq
#' @importFrom cli cli_abort
#' @importFrom jsonlite fromJSON
#'
#' @export
get_translations <- function(json_path) {

  # ============================================================================
  # check inputs
  # ============================================================================

  # path
  check_json_path(path = json_path)

  # ============================================================================
  # check whether targets are present in the JSONS
  # ============================================================================

  # check whether any translations exist
  translations_exist <- base::file(json_path) |>
    jqr::jq('any(.Translations; length > 0)') |>
    jsonlite::fromJSON()

  if (translations_exist == FALSE){
    cli::cli_abort(
      message = c(
        "x" = "No translations found in the questionnaire."
      )
    )
  }

  # ============================================================================
  # get data
  # ============================================================================

  translations_json <- base::file(json_path) |>
    jqr::jq(
      paste0(
        # function definitions
        jq_def_rename_type,
        '
        .Translations
        # rename `$type` key to `type`
        | map(rename_type)
        # add object type attribute
        | map(. + { "object_type": "translation" })
        '
      )
    )

  translations_df <- jsonlite::fromJSON(translations_json)

}

#' Get metadata on attachments
#' 
#' @description
#' Extract metadata about attachments from the questionnaire JSON
#' as a data frame.
#'
#' @inheritParams get_sections
#'
#' @return Data frame with the following columns:
#'
#' - `object_type`. Character. Simplified object type. Value: `attachment`.
#' - `type`. Character. Value: `"Attachment"`.
#' - `attachment_id`. Character. GUID for the translation.
#' - `attachment_name`. Character. User-provided name for the attachment. This is how
#' Designer fields refer to it.
#' - `attachment_content_id`. Character. System-provided GUID for files in `Attachments`
#'
#' @importFrom jqr jq
#' @importFrom cli cli_abort
#' @importFrom jsonlite fromJSON
#'
#' @export
get_attachments <- function(json_path) {

  # ============================================================================
  # check inputs
  # ============================================================================

  # path
  check_json_path(path = json_path)

  # ============================================================================
  # check whether targets are present in the JSONS
  # ============================================================================

  # check target
  attachments_exist <- base::file(json_path) |>
    jqr::jq('any(.Attachments; length > 0)') |>
    jsonlite::fromJSON()

  if (attachments_exist == FALSE) {
    cli::cli_abort(
      message = c(
        "x" = "No attachments found for this questionnaire."
      )
    )
  }

  # ============================================================================
  # compose query
  # ============================================================================

  jq_expr <- paste0(
    # function definitions
    jq_def_rename_attachments,
    jq_rename_from_pascal_to_snake_case,
    '[
      # select top-level attachments key
      .Attachments[]
      # rename keys
      # ... for known keys
      | rename_attachments 
      # ... for unknown keys, from Pascal to snake case
      | rename_from_pascal_to_snake_case
      # add object type attribute
      | . + { "object_type" : "attachments" }
    ] '
  )

  # ============================================================================
  # get data
  # ============================================================================

  attachments_df <- base::file(json_path) |>
    jqr::jq(jq_expr) |>
    jsonlite::fromJSON()

}

#' Get metadata on lookup tables.
#'
#' @description
#' Extract metadata about translations from the questionnaire JSON
#' as a data frame.
#'
#' @inheritParams get_sections
#'
#' @return Data frame with the following columns:
#'
#' - `object_type`. Simple object type. Value: `lookup table`.
#' - `lookup_table_id`. Character. GUID.
#' - `lookup_table_name`. Character. Name as it appears in Designer.
#' - `lookup_table_file_name`. Character. Name of file uploaded in Designer.
#'
#' @importFrom jqr jq
#' @importFrom cli cli_abort
#' @importFrom jsonlite fromJSON
#'
#' @export
get_lookup_tables <- function(json_path) {

  # ============================================================================
  # check inputs
  # ============================================================================

  # path
  check_json_path(path = json_path)

  # ============================================================================
  # check whether targets are present in the JSONS
  # ============================================================================

  # check target has non-empty content
  lookup_tables_exist <- base::file(json_path) |>
    jqr::jq(
      '
      # select top-level property
      .LookupTables
      # remove `$type` key-value pair, if present
      # note: `del()` has no effect the key-value pair is not present
      | del(."$type")
      | any(length > 0)
      '
    ) |>
    jsonlite::fromJSON()

  if (lookup_tables_exist == FALSE) {
    cli::cli_abort(
      message = c(
        "x" = "No lookup tables found for this questionnaire."
      )
    )
  }

  # ============================================================================
  # compose query
  # ============================================================================

  jq_expr <- paste0(
    # function definitions
    jq_def_rename_lookup_tables,
    jq_rename_from_pascal_to_snake_case,
    # query
    '
    # select top-level property
    .LookupTables
    # remove `$type` key-value pair, if present
    # note: `del()` has no effect the key-value pair is not present
    | del(."$type")
    # restructure the lookup table array so that it consists of a key and value entries
    # as follows:
    # [ "key": .key, "value": .value ]
    # where `.value` is the initial content of each element in the lookup table array
    | to_entries
    # restructure each element of the lookup table array as follows:
    # [ "lookup_table_id": .key, .value ]
    # where `.value` is the value of the initial lookup table array
    | map({ lookup_table_id: .key } + .value)
    # rename keys
    # note: using `map()` to apply function since objects are nested in an array
    # because of earlier use of `map()`
    # ... for known keys
    | map(rename_lookup_tables)
    # ... for unknown keys, from Pascal to snake case
    | map(rename_from_pascal_to_snake_case)
    # add text
    | map(. + { "object_type": "lookup table" })
    '
  )

  # ============================================================================
  # get data
  # ============================================================================

  lookup_table_json <- base::file(json_path) |>
    jqr::jq(jq_expr)

  lookup_table_df <- jsonlite::fromJSON(lookup_table_json)

}

#' Get metadata on critical rules.
#'
#' @description
#' Extract metadata about critical rules from the questionnaire JSON
#' as a data frame.
#'
#' @inheritParams get_sections
#'
#' @return Data frame.
#'
#' - `object_type`. Character. Simplified object type. Value: `critical rule`.
#' - `type`. Character. Always `CriticalRule`.
#' - `rule_id`. Character. GUID
#' - `rule_message`. Character. Error message for the rule.
#' - `rule_expression`. Character. Validation expression for the rule.
#' - `rule_description`. Character. Optional description for the rule.
#'
#' @importFrom jqr jq
#' @importFrom cli cli_abort
#' @importFrom jsonlite fromJSON
#'
#' @export
get_critical_rules <- function(json_path) {

  # ============================================================================
  # check inputs
  # ============================================================================

  # path
  check_json_path(path = json_path)

  # ============================================================================
  # check whether targets are present in the JSONS
  # ============================================================================

  has_critical_rules <- base::file(json_path) |>
    jqr::jq('any(.CriticalRules // []; length > 0)') |>
    jsonlite::fromJSON()

  if (has_critical_rules == FALSE) {
    cli::cli_abort(
      message = c(
        "x" = "No critical rules found for the questionnaire."
      )
    )
  }

  # ============================================================================
  # compose query
  # ============================================================================

  jq_expr <- paste0(
    jq_def_rename_critical_rules,
    jq_rename_from_pascal_to_snake_case,
    '[
      .CriticalRules[]
      # rename keys
      # ... known keys
      | rename_critical_rules
      # ... unknown keys
      | rename_from_pascal_to_snake_case
      # add object type attribute
      | . + { "object_type": "critical rule" }
    ]'
  )

  # ============================================================================
  # get data
  # ============================================================================

  critical_rules_json <- base::file(json_path) |>
    jqr::jq(jq_expr)

  critical_rules_df <- jsonlite::fromJSON(critical_rules_json)

}

#' Get metadata on questionnaire document metadata.
#'
#' @description
#' Extract metadata about the questionnaire itself from the questionnaire JSON
#' file as a data frame.
#'
#' @inheritParams get_sections
#'
#' @return List with named elements corresponding the metadata attributes.
#'
#' @importFrom jqr jq
#' @importFrom jsonlite fromJSON
#'
#' @export
get_qnr_metadata <- function(json_path) {


  # ============================================================================
  # check inputs
  # ============================================================================

  # path
  check_json_path(path = json_path)

  # ============================================================================
  # compose query
  # ============================================================================

  jq_expr <- paste0(
    '
    .
    # delete immediate children
    # arrays
    | del(
      .Children, .Macros, .CriticalRules, .LookupTables,
      .Attachments, .Translations, .Categories, .FixedRosterTitles
    )
    # irrelevant keys (e.g., generic group keys)
    | del(
      .ConditionExpression, .IsRoster, .DisplayMode, .RosterSizeSource
    )
    # hoist keys from Metadata object to the main object
    | . + {
      "SubTitle": .Metadata.Subtitle?,
      "Version": .Metadata.Version?,
      "VersionNotes": .Metadata.VersionNotes?,
      "KindOfData": .Metadata.KindOfData?,
      "Country": .Metadata.Country?,
      "Year": .Metadata.Year?,
      "Language": .Metadata.Language?,
      "Coverage": .Metadata.Coverage?,
      "Universe": .Metadata.Universe?,
      "UnitOfAnalysis": .Metadata.UnitOfAnalysis?,
      "PrimaryInvestigator": .Metadata.PrimaryInvestigator?,
      "Funding": .Metadata.Funding?,
      "Consultant": .Metadata.Consultant?,
      "ModeOfDataCollection": .Metadata.ModeOfDataCollection?,
      "Notes": .Metadata.Notes?,
      "Keywords": .Metadata.Keywords?,
      "AgreeToMakeThisQuestionnairePublic": .Metadata.AgreeToMakeThisQuestionnairePublic?,
    }
    '
  )

  # ============================================================================
  # get data
  # ============================================================================

  qnr_meta_df <- base::file(json_path) |>
    jqr::jq(jq_expr) |>
    jsonlite::fromJSON()

}
