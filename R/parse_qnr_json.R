#' Parses JSON questionnaire file into rectangular data frame
#'
#' Transforms the information found in the JSON questionnaire file, `document.json`, into a simpler rectangular file. 
#' The JSON file provides a (deeply) nested representation of the questionnaire. This function returns a rectangular data frame in the following format: 
#' 
#' - Rows: objects in the questionnaire (e.g., sections, sub-sections, rosters, questions, variables, static text, etc.)
#' - Columns: attributes. 
#' 
#' This format facilitates common needs such as: 
#' 
#' - Collecting all the validations
#' - Compiling keys and values for questions (e.g., PublicKey for questions to understand the paradata AnswerRemove event)
#' 
#' @param json_path Complete file path to the questionnaire JSON file, typically named `document.json`
#' 
#' @return Data frame that contains all attributes of all objects in the questionnaire.
#'
#' @importFrom dplyr bind_rows
#'
#' @export
parse_questionnaire <- function(json_path) {

  # ============================================================================
  # check inputs
  # ============================================================================

  # path
  check_json_path(path = json_path)

  # ============================================================================
  # extract data
  # ============================================================================

  sections <- get_sections(json_path = json_path)
  sub_sections <- get_sub_sections(json_path = json_path)
  rosters <- get_rosters(json_path = json_path)
  questions <- get_questions(json_path = json_path)
  variables <- get_variables(json_path = json_path)
  static_texts <- get_static_texts(json_path = json_path)

  # ============================================================================
  # combine data
  # ============================================================================

  qnr_df <- dplyr::bind_rows(
    sections,
    sub_sections,
    rosters,
    questions,
    variables,
    static_texts
  )

  return(qnr_df)

}
