# Get metadata for questions

Get metadata for questions

## Usage

``` r
get_questions(json_path, add_section_id = FALSE)
```

## Arguments

- json_path:

  Character. Full path to the Survey Solutions questionnaire JSON file
  (e.g., `~/my_proj/path/to/document.json`).

- add_section_id:

  Boolean. Whether to include the public key of the section that
  contains the question.

## Value

Data frame with the following columns:

- `section_id`. Character. GUID. Present only if
  `add_section_id = TRUE`.

- `object_type`. Character. Simplified object type. Value: `question`.

- `type`. Character. SuSo-provided object type. Value: question type.

- `question_type`

- `condition_expression`

- `hide_if_disabled`

- `featured`

- `instructions`

- `public_key`

- `question_scope`

- `question_text`

- `stata_export_caption`

- `variable_label`

- `is_timestamp`

- `varname`

- `hide_instructions`

- `use_formatting_properites`

- `geometry_type`

- `geometry_input_mode`

- `is_critical`

- `mask`

- `answer_text_*`

- `answer_value_*`

- `validation_expression_*`

- `validation_message_*`

- `validation_severity_*`

- `show_as_list`

- `categories_id`

- `is_filtered_combo_box`

- `show_as_list_threshold`

- `cascade_from_question_id`

- `max_answer_count`

- `linked_to_question_id`
