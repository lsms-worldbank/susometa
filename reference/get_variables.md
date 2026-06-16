# Get metadata for computed variables.

Extract metadata about computed variables from the questionnaire JSON as
a data frame.

## Usage

``` r
get_variables(json_path, add_section_id = FALSE)
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

- `object_type`. Character. Simplified object type. Value: `variable`.

- `type`. Character. SuSo-provided object type. Value: `Variable`.

- `label_variable`

- `public_key`

- `type_variable`

- `name_variable`

- `expression_variable`

- `do_not_export`

- `varname`
