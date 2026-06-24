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

#' Variable type value labels
#' 
#' @description Labels that describe the possible values of variable labels
#' 
#' @noRd 
variable_type_lbls <- c(
  `Long Integer` = 1,
  `Double` = 2,
  `Boolean` = 3,
  `Date/Time` = 4,
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


#' `jq` function that flattens answers.
#'
#' @description
#' Transforms the array of answer objects into a set of key-value pairs
#' that are numbered with a 1-based index.
#'
#' @details
#' In `json`, answers are of this form:
#'
#' ```json
#'   "Answers": [
#'     {
#'       "$type": "Answer",
#'       "AnswerText": "Urban",
#'       "AnswerValue": "1",
#'       "AnswerCode": 1.0
#'     },
#'     {
#'       "$type": "Answer",
#'       "AnswerText": "Rural",
#'       "AnswerValue": "2",
#'       "AnswerCode": 2.0
#'     }
#'   ],
#' ```
#'
#' This function transforms them to this form:
#' 
#' ```json
#'  answer_text__1": "Urban",
#'  answer_value__1": 1,
#'  answer_text__2": "Rural",
#'  answer_value__2": 2,
#' ```
#' 
#' @noRd
jq_def_flatten_answers <- '
def flatten_answers:
  . + (
    [range(.Answers | length) as $i |
      {
        ("answer_text_\\($i + 1)"): .Answers[$i].AnswerText,
        ("answer_value_\\($i + 1)"): (.Answers[$i].AnswerValue | tonumber)
      }
    ] | add // {}
  )
  | del(.Answers)
;
'

#' `jq` function to flatten validations
#'
#' @description
#' Transforms the array of validation objects into a set of key-value pairs
#' that are numbered with a 1-based index.
#'
#' @noRd
jq_def_flatten_validations <- '
def flatten_validations:
  . + (
    [range(.ValidationConditions | length) as $i |
      {
        ("validation_expression_\\($i + 1)"): .ValidationConditions[$i].Expression,
        ("validation_message_\\($i + 1)"): (.ValidationConditions[$i].Message),
        ("validation_severity_\\($i + 1)"): (.ValidationConditions[$i].Severity | tonumber)
      }
    ] | add // {}
  )
  | del(.ValidationConditions)
;
'

#' jq expression to flatten fixed roster titles
#'
#' @noRd
jq_def_flatten_fixed_roster_titles <- '
def flatten_fixed_roster_titles:
  . + (
    [range(.FixedRosterTitles | length) as $i |
      {
        ("fixed_roster_title_\\($i + 1)"): .FixedRosterTitles[$i].Title,
        ("fixed_roster_value_\\($i + 1)"): (.FixedRosterTitles[$i].Value | tonumber)
      }
    ] | add // {}
  )
  | del(.FixedRosterTitles)
;
'

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
  # derive a null-filled template from the rename VALUES (snake_case names)
  ([$renames | to_entries[] | {(.value): null}] | add) as $template |
  # step 1: if a key is in the dictionary, rename it
  # otherwise pass forward the key
  with_entries(
    if $renames[.key] != null then .key = $renames[.key] else . end
  ) |
  # step 2: create entries with null values for keys present in the dictionary
  # but missing from the JSON file
  $template + .
;
'

#' jq expression for renaming known keys of questions
#'
#' @noRd
jq_rename_question_attribs <- '
def rename_question_attribs:
{

  # ======================================================================================
  # general object attributes
  # ======================================================================================

  "$type": "type",
  "VariableName": "varname",
  "PublicKey": "public_key",
  "ConditionExpression": "condition_expression",
  "HideIfDisabled": "hide_if_disabled",

  # ======================================================================================
  # general question attributes
  # ======================================================================================

  # those at the root of the question object
  "Featured": "featured",
  "QuestionScope": "question_scope",
  "QuestionText": "question_text",
  "Instructions": "instructions",
  "QuestionType": "question_type",
  "StataExportCaption": "stata_export_caption",
  "VariableLabel": "variable_label",
  # those within the Properties object
  "HideInstructions": "hide_instructions",
    # "UseFormatting" from properties is handled in the pipeline
  "GeometryType": "geometry_type",
  "GeometryInputMode": "geometry_input_mode",
  "IsCritical": "is_critical",

  # ======================================================================================
  # attributes by question type
  # ======================================================================================

  # ----------------------------------------------------------------------------
  # categorical, either single-select or multi-select
  # ----------------------------------------------------------------------------

  "IsFilteredCombobox": "is_filtered_combo_box",
  "LinkedToRosterId": "linked_to_roster_id",
  "LinkedToQuestionId": "linked_to_question_id",
  "LinkedFilterExpression": "linked_filter_expression",
  "CategoriesId": "categories_id",

  # ----------------------------------------------------------------------------
  # single-select
  # ----------------------------------------------------------------------------

  "ShowAsList": "show_as_list",

  # ----------------------------------------------------------------------------
  # multi-select
  # ----------------------------------------------------------------------------

  "AnswerOrder": "answer_order",
  "AreAnswersOrdered": "are_answers_ordered",
  "YesNoView": "yes_no_view",

  # ----------------------------------------------------------------------------
  # date
  # ----------------------------------------------------------------------------

  "IsTimestamp": "is_timestamp",

  # ----------------------------------------------------------------------------
  # numeric
  # ----------------------------------------------------------------------------

  "IsInteger": "is_integer",
  "CountOfDecimalPlaces": "num_decimal_places",
  "UseFormatting": "use_formatting_numeric",

  # ----------------------------------------------------------------------------
  # text
  # ----------------------------------------------------------------------------

  "Mask": "mask",

}
as $renames |
 # derive a null-filled template from the rename VALUES (snake_case names)
  ([$renames | to_entries[] | {(.value): null}] | add) as $template |
  # step 1: if a key is in the dictionary, rename it
  # otherwise pass forward the key
  with_entries(
    if $renames[.key] != null then .key = $renames[.key] else . end
  ) |
  # step 2: create entries with null values for keys present in the dictionary
  # but missing from the JSON file
  $template + .
;

'

#' jq expression to rename known keys of computed variables
#'
#' @noRd
jq_rename_variable_attribs <- '
def rename_variable_attribs:
{
  "$type": "type",
  "Label": "label_variable",
  "PublicKey": "public_key",
  "Type": "type_variable",
  "Name": "name_variable",
  "Expression": "expression_variable",
  "DoNotExport": "do_not_export",
  "VariableName": "varname"
}
as $renames |
  # derive a null-filled template from the rename VALUES (snake_case names)
  ([$renames | to_entries[] | {(.value): null}] | add) as $template |
  # step 1: if a key is in the dictionary, rename it
  # otherwise pass forward the key
  with_entries(
    if $renames[.key] != null then .key = $renames[.key] else . end
  ) |
  # step 2: create entries with null values for keys present in the dictionary
  # but missing from the JSON file
  $template + .
;

'

#' jq expression to rename known keys of static text
#'
#' @noRd
jq_def_rename_static_text <- '
def rename_static_text:
{
  "$type": "type",
  "PublicKey": "public_key",
  "Text": "text",
  "AttachmentName": "attachment_name",
  "ConditionExpression": "condition_expression",
  "HideIfDisabled": "hide_if_disabled"
}
as $renames |
  # derive a null-filled template from the rename VALUES (snake_case names)
  ([$renames | to_entries[] | {(.value): null}] | add) as $template |
  # step 1: if a key is in the dictionary, rename it
  # otherwise pass forward the key
  with_entries(
    if $renames[.key] != null then .key = $renames[.key] else . end
  ) |
  # step 2: create entries with null values for keys present in the dictionary
  # but missing from the JSON file
  $template + .
;

'

#' jq expression to rename known keys of macros
#'
#' @noRd
jq_def_rename_macros <- '
def rename_macros:
{
  # from ... to
  "$type": "type",
  "Name": "macro_name",
  "Description": "macro_description",
  "Content": "macro_content",
}
as $renames |
  # derive a null-filled template from the rename VALUES (snake_case names)
  ([$renames | to_entries[] | {(.value): null}] | add) as $template |
  # step 1: if a key is in the dictionary, rename it
  # otherwise pass forward the key
  with_entries(
    if $renames[.key] != null then .key = $renames[.key] else . end
  ) |
  # step 2: create entries with null values for keys present in the dictionary
  # but missing from the JSON file
  $template + .
;

'

#' jq expression to rename known keys of attachments
#'
#' @noRd
jq_def_rename_attachments <- '
def rename_attachments:
{
  # from ... to
  "$type": "type",
  "AttachmentId": "attachment_id",
  "Name": "attachment_name",
  "ContentId": "attachment_content_id",
}
as $renames |
  # derive a null-filled template from the rename VALUES (snake_case names)
  ([$renames | to_entries[] | {(.value): null}] | add) as $template |
  # step 1: if a key is in the dictionary, rename it
  # otherwise pass forward the key
  with_entries(
    if $renames[.key] != null then .key = $renames[.key] else . end
  ) |
  # step 2: create entries with null values for keys present in the dictionary
  # but missing from the JSON file
  $template + .
;

'

#' jq expression to rename known keys of lookup tables
#'
#' @noRd
jq_def_rename_lookup_tables <- '
def rename_lookup_tables:
{
  # from ... to
  "TableName": "lookup_table_name",
  "FileName": "lookup_table_file_name",
}
as $renames |
  # derive a null-filled template from the rename VALUES (snake_case names)
  ([$renames | to_entries[] | {(.value): null}] | add) as $template |
  # step 1: if a key is in the dictionary, rename it
  # otherwise pass forward the key
  with_entries(
    if $renames[.key] != null then .key = $renames[.key] else . end
  ) |
  # step 2: create entries with null values for keys present in the dictionary
  # but missing from the JSON file
  $template + .
;

'

#' jq expression to rename known keys of critical rules
#'
#' @noRd
jq_def_rename_critical_rules <- '
def rename_critical_rules:
{
  # from ... to
  "$type": "type",
  "Id": "rule_id",
  "Message": "rule_message",
  "Expression": "rule_expression",
  "Description": "rule_description",
}
as $renames |
  # derive a null-filled template from the rename VALUES (snake_case names)
  ([$renames | to_entries[] | {(.value): null}] | add) as $template |
  # step 1: if a key is in the dictionary, rename it
  # otherwise pass forward the key
  with_entries(
    if $renames[.key] != null then .key = $renames[.key] else . end
  ) |
  # step 2: create entries with null values for keys present in the dictionary
  # but missing from the JSON file
  $template + .
;

'

jq_def_rename_type <- '
def rename_type:
  # add `type`
  . + {"type": ."$type"}
  # remove old `$type`
  | del(."$type");
'
