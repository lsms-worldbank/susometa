#' Check whether arg is a bare name
#'
#' @param x Function argument
#'
#' @return Boolean. `TRUE` if `x` is a bare name; `FALSE` otherwise.
#'
#' @importFrom rlang is_symbol
#'
#' @noRd
is_bare_name <- function(x) {

    # check whether quoted expression `x` is a symbol
    is_name <- rlang::is_symbol(x)

    return(is_name)

}

#' Check the path provided for the Survey Solutions questionnaire JSON
#'
#' @param path Character. Full path to the file.
#'
#' @return Side-effect of throwing an error.
#'
#' @importFrom rlang caller_env
#' @importFrom fs file_exists path_ext
#' @importFrom cli cli_abort
#'
#' @noRd
check_json_path <- function(
  path,
  call = rlang::caller_env()
) {

  # path exists
  if (!fs::file_exists(path)) {
    cli::cli_abort(
      message = "The path provided does not exist.",
      call = call
    )
  }

  # file is json
  if (fs::path_ext(path) != "json") {
    cli::cli_abort(
      message =
        "The path provided does not point to a file with `.json` extension.",
      call = call
    )
  }

}

#' jq expression to rename keys from PascalCase to snake_case
#'
#' @noRd
jq_rename_from_pascal_to_snake_case <- '
def rename_from_pascal_to_snake_case:

# update the values of all keys at the current level
with_entries(
  .key |=
    (
      # insert an underscore in the boundary between Pascal-case words
      gsub("(?<=[a-z])(?=[A-Z])"; "_")
      # make these modified keys lowercase
      | ascii_downcase
    )
  )
;
'

#' jq expression to rename known keys for groups
#'
#' @noRd
jq_rename_group_attribs <- '
def rename_group_attribs:
# define a dictionary of old and new key names
{
  "$type": "type",
  "ConditionExpression": "condition_expression",
  "HideIfDisabled": "hide_if_disabled",
  "IsFlatMode": "is_flat_mode",
  "IsPlainMode": "is_plain_mode",
  "DisplayMode": "display_mode",
  "Enabled": "enabled",
  "Description": "description",
  "VariableName": "varname",
  "IsRoster": "is_roster",
  "CustomRosterTitle": "custom_roster_title",
  "RosterSizeSource": "roster_size_source",
  "RosterSizeQuestionId": "roster_size_question_id",
  "RosterTitleQuestionId": "roster_title_question_id", 
  "PublicKey": "public_key",
  "Title": "title",
}
# store them in a renames variable for easy access
as $renames |
  # if a key is in the dictionary, rename it
  # otherwise pass forward the key
  with_entries(
    if $renames[.key] != null then .key = $renames[.key] else . end
  )
;
'

