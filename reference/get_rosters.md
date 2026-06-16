# Get metadata about rosters.

Extract metadata about rosters from the questionnaire JSON as a data
frame.

## Usage

``` r
get_rosters(json_path)
```

## Arguments

- json_path:

  Character. Full path to the Survey Solutions questionnaire JSON file
  (e.g., `~/my_proj/path/to/document.json`).

## Value

Data frame with the following columns:

- `object_type`. Character. Simplified object type. Value: `roster`.

- `type`. Character. SuSo-provided object type. Value: `Group`.

- `condition_expression`

- `hide_if_disabled`

- `is_flat_mode`

- `is_plain_mode`

- `display_mode`

- `enabled`

- `description`

- `varname`

- `is_roster`

- `custom_roster_title`

- `roster_size_question_id`

- `roster_size_source`

- `public_key`

- `title`

- `fixed_roster_title_*`

- `fixed_roster_value_*`
