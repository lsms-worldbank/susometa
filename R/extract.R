#' Section attributes
#' 
#' @description Attributes that are valid for sections in SuSo
#' 
#' @noRd 
section_attributes <- c(
    "condition_expression",
    "variable_name",
    "public_key"
)

#' Get sections from questionnaire metadata
#' 
#' @param qnr_df Data frame produced by `susopara::parse_questionnaire()`
#' 
#' @returns Data frame of sections (`title`), (JSON) indices (`l_0`),
#' and other section attributes
#' 
#' @importFrom dplyr `%>%` select
#' @importFrom tidyselect any_of
#' 
#' @export 
get_sections <- function(qnr_df) {

    sections <- qnr_df %>%
        # keep sections, defined as:
        # - a group
        # - without any child index
        dplyr::filter(
            .data$type == "Group" &
            is.na(.data$l_1)
        ) %>%
        # order by index
        dplyr::arrange(.data$l_0) %>%
        # keep relevant attributes
        dplyr::select(
            # index ID
            l_0, 
            # title
            title,
            # attributes
            tidyselect::any_of(section_attributes)
        )

    return(sections)

}

#' General attributes of questions
#' 
#' @description Attributes that are common to all question types
#' 
#' @noRd 
var_general <- c(
    "type",
    "public_key",
    "condition_expression",
    "hide_if_disabled",
    "featured",
    "hide_instructions",
    "use_formatting_properties",
    "filter_expression",
    "question_scope",
    "question_text",
    "instructions",
    "question_type",
    "stata_export_caption",
    "variable_label"
)

#' Single-select questions attributes
#' 
#' @description Attributes unique to single-select questions
#' 
#' @noRd 
var_single_select <- c(
    "show_as_list"
)

#' Multi-select questions attributes
#' 
#' @description Attributes unique to multi-select questions
#' 
#' @noRd 
var_multi_select <- c(
    "answer_order",
    "are_answered_ordered",
    "yes_no_view"
)

#' Attributes of either single- or multiple-select questions
#' 
#' @description Attributes that may be found for either single- or multiple-
#' select questions
#' 
#' @noRd 
var_single_or_multiple_select <- c(
    "is_filtered_combo_box",
    "linked_to_roster_id",
    "linked_filter_expression"
)

#' Date question attributes
#' 
#' @description Attributes unique to date questions
#' 
#' @noRd 
var_date <- c(
    "is_timestamp"
)

#' Numeric question attributes
#' 
#' @description Attributes unique to numeric questions
#' 
#' @noRd 
var_numeric <- c(
    "is_integer",
    "num_decimal_places",
    "use_formatting_numeric"
)

#' Text question attributes
#' 
#' @description Attributes unique to text questions
#' 
#' @noRd 
var_text <- c(
    "mask"
)

#' Get questons from questionnaire metadata
#' 
#' @inheritParams get_sections
#' 
#' @returns Data frame of questions (`varname`), their JSON indices (`l_*`),
#' and other question attributes
#' 
#' @importFrom dplyr %>% filter select
#' @importFrom tidyselect any_of
#' 
#' @export 
get_questions <- function(qnr_df) {

    variables <- qnr_df %>%
        # filter to objects that are variables
        # namely, entities that have a `question_type`
        dplyr::filter(!is.na(.data$question_type)) %>%
        # select the variable name
        dplyr::select(
            # index IDs
            dplyr::starts_with("l_"), 
            # name
            varname,
            # other question properties
            tidyselect::any_of(var_general),
            tidyselect::any_of(var_single_select),
            tidyselect::any_of(var_multi_select),
            tidyselect::any_of(var_single_or_multiple_select),
            tidyselect::any_of(var_date),
            tidyselect::any_of(var_numeric),
            tidyselect::any_of(var_text)
        )

    return(variables)

}

#' Get variables by section
#' 
#' @inheritParams get_sections
#' 
#' @returns Data frame of the mapping of variables to sections
#' 
#' @importFrom dplyr `%>%` full_join select
#' 
#' @export 
get_questions_by_section <- function(qnr_df) {

    sections <- get_sections(qnr_df = qnr_df)

    questions <- get_questions(qnr_df = qnr_df)

    qs_by_section <- sections %>%
        # select common attributes only
        dplyr::select(title, l_0) %>%
        # join the variables to the section to which they belong
        dplyr::left_join(questions, by = "l_0") %>%
        # rename vaiables for clarity
        dplyr::rename(
            section = .data$title,
            variable = .data$varname
        )

    return(qs_by_section)

}

#' Variable type value labels
#' 
#' @description Labels that describe the possible values of variable labels
#' 
#' @noRd 
variable_type_lbls <- c(
    `Boolean` = 1,
    `Double` = 2,
    `Date/Time` = 3,
    `Long Integer` = 4,
    `String` = 5
)

#' Question type labels
#' 
#' @description Labels that describe the possible values of question types
#' 
#' @noRd 
question_type_lbls <- c(
    `Categorical: Single-select` = 0,
    # 1?
    # 2?
    `Categorical: Multi-select` = 3, # MultyOptionsQuestion
    `Numeric` = 4, # NumericQuestion
    `Date` = 5, # DateTimeQuestion
    `GPS` = 6, # GpsCoordinateQuestion
    `Text` = 7, # TextQuestion
    # 8?
    `List` = 9 # TextListQuestion
)

#' Roster attributes
#' 
#' @description Attributes that are describe rosters
#' 
#' @noRd 
roster_attribs <- c(
    "public_key",
    "is_flat_mode",
    "is_plain_mode",
    "display_mode",
    "enabled",
    "description",
    "is_roster",
    "custom_roster_title",
    "roster_size_question_id",
    "roster_size_source",
    "fixed_roster_titles",
    "roster_title_question_id",
    "title"
)

#' Get rosters
#' 
#' @inheritParams get_sections
#' 
#' @returns Data frame of roster objects, their JSON indices, 
#' and attributes
#' 
#' @importFrom dplyr filter `%>%` select starts_with
#' @importFrom tidyselect any_of
#' 
#' @export 
get_rosters <- function(qnr_df) {

    roster_df <- qnr_df %>%
        # keep roster objects
        dplyr::filter(.data$is_roster == TRUE) %>%
        # keep relevant attributes
        dplyr::select(
            dplyr::starts_with("l_"), 
            tidyselect::any_of(roster_attribs)
        )

}

#' Get answer options
#' 
#' Obtain the answer options for a target variable as a set of name-value pairs in labelled numeric vector.
#' 
#' @param qnr_df Data frame returned from `parse_questionnaire`
#' @param varname Variable name. Bare name of variable whose answer options to extract.
#' @param to_exclude Numeric vector. Code of answer options to exclude.
#' 
#' @return Named numeric vector. Values are answer codes. Names are answer labels.
#' 
#' @importFrom dplyr `%>%` filter select starts_with
#' @importFrom rlang .data as_label expr
#' @importFrom tidyr pivot_longer
#' @importFrom stats setNames
#' @importFrom stringr str_replace
#' 
#' @export 
get_answer_options <- function(
    qnr_df,
    varname,
    to_exclude = NULL
) {

    # select answer and value columns from questionnaire data frame
    answer_options <- qnr_df %>%
        # find attributes for variable of interest
        dplyr::filter(varname == rlang::as_label(rlang::expr({{varname}}))) %>%
        # select columns that capture answer text and values
        dplyr::select(dplyr::starts_with("answer_text"), dplyr::starts_with("answer_value")) %>%
        # pivot so that option text and values occupy their own columns
        tidyr::pivot_longer(
            cols = tidyr::everything(),
            names_to = c(".value", "index"),            # create 1 column per each value found in pattern, named what is found in expression
            names_pattern = "answer_([a-z]+)_([0-9]+)", # variable names: `answer_text_{n}` and `answer_value_{n}`
            values_drop_na = TRUE                       # drop rows that contain only NAs--that is, empty answer_text and answer_value columns
        ) %>%
        # exclude label values
        {if (length(to_exclude) >= 1) filter(., !.data$value %in% to_exclude) else .}

    # create named vector, where:
    # - names are variable names
    # - values are text 
    column_labels <- stats::setNames(
        object = answer_options$text, 
        nm = paste0(
                # construct variable name from: variable stub name, separator, and value
                rlang::as_label(rlang::expr({{varname}})),   # variable stub name
                "__",           # separator
                # value
                # replace negative value (-) with "n"
                stringr::str_replace(answer_options$value, "-", "n")
            )
    )

    return(column_labels)

}
