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

#' Get SuSo's computed variables from questionnaire metadata
#' 
#' @inheritParams get_sections
#' 
#' @returns Data frame of questions (`varname`), their JSON indices (`l_*`),
#' and other question attributes
#' 
#' @importFrom dplyr %>% filter select
#' @importFrom tidyselect any_of
#' @importFrom labelled set_value_labels
#' 
#' @export 
get_variables <- function(qnr_df) {

    variables <- qnr_df %>%
        # filter to objects that are of type `Variable`
        dplyr::filter(.data$type == "Variable") %>%
        # select attributes relevant for variables
        dplyr::select(
            # index IDs
            dplyr::starts_with("l_"),
            # name
            .data$varname,
            # other question properties
            tidyselect::any_of(variable_attribs),
        ) %>%
        labelled::set_value_labels(
            type_variable = variable_type_lbls
        )

    return(variables)

}

#' Get variables by section
#' 
#' @inheritParams get_sections
#' 
#' @returns Data frame of the mapping of variables to sections
#' 
#' @importFrom dplyr %>% full_join select
#' 
#' @export 
get_questions_by_section <- function(qnr_df) {

    sections <- get_sections(qnr_df = qnr_df)

    questions <- get_questions(qnr_df = qnr_df)

    qs_by_section <- sections %>%
        # select common attributes only
        dplyr::select(.data$title, .data$l_0) %>%
        # join the variables to the section to which they belong
        dplyr::left_join(questions, by = "l_0") %>%
        # rename vaiables for clarity
        dplyr::rename(
            section = .data$title,
            variable = .data$varname
        )

    return(qs_by_section)

}

#' Get answer options
#'
#' @description
#' Obtain the answer options for a target variable as a set of name-value pairs
#' in labelled numeric vector.
#'
#' @param qnr_df Data frame returned from `parse_questionnaire()`
#' @param categories_df Data frame returned from `parse_categories()`
#' @param varname Variable name. Bare name of variable whose answer options to
#' extract.
#' @param to_exclude Numeric vector. Code of answer options to exclude.
#'
#' @return Named numeric vector.
#' Values are answer codes. Names are answer labels.
#'
#' @importFrom dplyr %>% filter select starts_with pull if_else
#' @importFrom rlang .data as_label expr
#' @importFrom tidyr pivot_longer everything
#' @importFrom cli cli_abort
#' @importFrom stats setNames
#'
#' @export
get_answer_options <- function(
    qnr_df,
    categories_df = NULL,
    varname,
    to_exclude = NULL
) {

    # confirm that variable name is a bare name
    # varname_as_expr <- rlang::enexpr(varname)
    # is_name <- is_bare_name(varname_as_expr)

    # if (is_name == FALSE) {
    #     cli::cli_abort(
    #         message = c(
    #             "x" = "Invalid value provided for {.arg varname}.",
    #             "i" = paste(
    #                 "The function expects a bare variable name",
    #                 "(e.g. `varname` instead of {.str varname})."
    #             )
    #         )
    #     )
    # }

    # capture variable name as text
    varname_txt <- rlang::as_label(rlang::expr({{varname}}))

    # check that a variable named `varname` exists
    varname_exists <- any(qnr_df$varname == varname_txt)
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

    # check that the categories data frame exists
    categories_df_is_df <- is.data.frame(categories_df)
    if (categories_df_is_df == FALSE) {

        cli::cli_abort(
            message = c(
                "x" = "{.arg categories_df} is not a data frame.",
                "i" = "The function expects a data frame of categories.",
                sep = " "
            )
        )
    }

    # check that categories data frame is of expected form
    categories_col_names <- c("categories_id", "value", "text")
    categories_has_right_names <- all(
        categories_col_names %in% names(categories_df)
    )
    if (categories_has_right_names == FALSE) {

        cli::cli_abort(
            message = c(
                "x" = "Categories data frame does not have expected columns.",
                "i" = paste(
                    "The function expects the data frame produced by",
                    "{.fn parse_categories},",
                    "with column names",
                    "{.var categories_id}, {.var value}, {.var text}.",
                    sep = " "
                )
            )
        )
    }


    # filter down to metadata for the target variable
    var_metadata <- qnr_df %>%
        # find attributes for variable of interest
        dplyr::filter(varname == varname_txt)

    # determine whether `varname` has answer options and/or categories
    has_answer_options <- var_metadata |>
        dplyr::mutate(
            has_answers = dplyr::if_any(
                .cols = dplyr::starts_with("answer_value_"),
                .fns = ~ is.na(.x)
            )
        ) |>
        dplyr::pull(.data$has_answers)

    categories_id_value <- dplyr::pull(var_metadata, .data$categories_id) |>
        # remove hyphens so that GUID has same format across sources
        gsub(
            pattern = "-",
            replacement = ""
        )

    has_reusable_categories <- dplyr::if_else(
        condition = !is.na(categories_id_value) & (categories_id_value != ""),
        true = TRUE,
        false = FALSE,
        missing = FALSE
    )

    # case 1: has answer options only
    if (has_answer_options == TRUE & has_reusable_categories == FALSE) {
 
        # select answer and value columns from questionnaire data frame
        answer_options <- var_metadata %>%
            # select columns that capture answer text and values
            dplyr::select(
                dplyr::starts_with("answer_text"),
                dplyr::starts_with("answer_value")
            ) %>%
            # pivot so that option text and values occupy their own columns
            tidyr::pivot_longer(
                cols = tidyr::everything(),
                # create 1 column per each value found in pattern, 
                # named what is found in expression
                names_to = c(".value", "index"),
                # variable names: `answer_text_{n}` and `answer_value_{n}`
                names_pattern = "answer_([a-z]+)_([0-9]+)", 
                # drop rows that contain only NAs
                # that is, empty answer_text and answer_value columns
                values_drop_na = TRUE
            ) %>%
            # exclude label values
            {if (length(to_exclude) >= 1) filter(., !.data$value %in% to_exclude) else .}

        if (nrow(answer_options) == 0) {

            cli::cli_abort(
                message = c(
                    "x" = "No answer options.",
                    "i" = paste(
                        "After excluding the answers in {.arg to_exclude},",
                        "no answer options remain.",
                        sep = " "
                    )
                )
            )

        } else {

            # construct a named numeric vector
            # - values are answer option codes
            # - names are answer labels
            val_lbls <- stats::setNames(
                object = as.numeric(answer_options$value),
                nm = answer_options$text
            )

            return(val_lbls)

        }
       
    # case 2: has reusable categories
    } else if (
        # case 2a: has only reusable categories
        (has_answer_options == FALSE & has_reusable_categories == TRUE) |
        # case 2b: has only reusable categories and (stale) answer options
        (has_answer_options == TRUE & has_reusable_categories == TRUE)
    ) {

        if (is.null(categories_df)) {

            cli::cli_abort(
                message = c(
                    "x" = "No categories data frame provided in `categories_df`"
                )
            )

        } else {

            var_categories_df <- categories_df |>
                dplyr::filter(.data$categories_id == categories_id_value)
            
            if (nrow(var_categories_df) == 0) {

                cli::cli_abort(
                    message = c(
                        "x" = "Categories for `{varname_txt}` not found."
                    )
                )

            } else {

            # construct a named numeric vector
            # - values are answer option codes
            # - names are answer labels
            val_lbls <- stats::setNames(
                object = as.numeric(var_categories_df$value),
                nm = var_categories_df$text
            )

            return(val_lbls)

            }

        }

    # case 3: has no answer options, neither answers nor reusable categories
    } else if (has_answer_options == FALSE & has_reusable_categories == FALSE) {

        cli::cli_abort(
            message = c(
                "x" = "No answer options found.",
                "i" = paste(
                    "The questionnaire does not have any answer options",
                    "associated with {.var {varname_txt}}",
                    sep = " "
                )
            )
        )

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
#' @importFrom stringr str_replace_all
#'
#' @export
get_ms_answers_as_var_labels <- function(
    qnr_df,
    categories_df = NULL,
    varname,
    to_exclude = NULL
) {

    # extract answer options
    answer_options <- get_answer_options(
        qnr_df = qnr_df,
        varname = {{varname}},
        categories_df = categories_df,
        to_exclude = to_exclude
    )

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
            stringr::str_replace_all(unname(answer_options), "-", "n")
        )
    )

    return(column_labels)

}

#' Get validations from questionnaire metadata
#'
#' @param qnr_df Data frame produced by `susopara::parse_questionnaire()`
#'
#' @return Data frame of validated objects and their validation. Objects with
#' validations include questions and static text. The validation information
#' returned in the data frame is:
#' - `type`. Question type drawn directly from the JSON file.
#' - `varname`. Variable name. Present only for questions.
#' - `text`. Question text or static text. For static text objects, this
#' may serve as  practical identifier, for lack of the equivalent of a
#' unique variable name.
#' - `expression_number`. Sequential ID number of the validation defined by
#' Designer.
#' - `validation_expression`. Expression provided in the `validation condition`
#' field in Designer.
#' - `expression_message`. Text composed in the `error or warning message`
#' field in Designer
#' - `severity`. Severity level of the validation. Values of 0 denote an error
#' while values of 1 denote a warning.
#'
#' @export
#'
#' @importFrom dplyr %>% filter select starts_with
#' @importFrom tidyr pivot_longer
get_validations <- function(
  qnr_df
) {

  n_validations_in_qnr <- qnr_df %>%
    dplyr::filter(
      !(
        .data$validation_expression_1 == "" |
        is.na(.data$validation_expression_1)
      )
    ) %>%
	nrow()

  if (n_validations_in_qnr == 0) {
    cli::cli_abort(
      messages = c(
        "x" = "The questionnaire does not contain any validations."
      )
    )
  }

  validations <- qnr_df %>%
    dplyr::filter(
      !(
        .data$validation_expression_1 == "" |
        is.na(.data$validation_expression_1)
      )
    ) %>%
    dplyr::select(
      type, varname, text,
      dplyr::starts_with("validation_"), starts_with("severity_")
    ) %>%
    tidyr::pivot_longer(
      cols = c(
        dplyr::starts_with("validation_"),
        dplyr::starts_with("severity_")
      ),
      names_pattern = "(.+?)_([0-9]+)",
      names_to = c(".value", "expression_number"),
      values_drop_na = TRUE
    )

}
