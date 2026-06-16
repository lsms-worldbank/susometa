# Get metadata on static text.

Extract metadata about sections from the questionnaire JSON as a data
frame.

## Usage

``` r
get_static_texts(json_path)
```

## Arguments

- json_path:

  Character. Full path to the Survey Solutions questionnaire JSON file
  (e.g., `~/my_proj/path/to/document.json`).

## Value

Data frame with the following columns:

- `object_type`. Character. Simplified object type. Value: `section`.

- `type`. Character. SuSo-provided type. Value: `Group.`

- `public_key`. Character. GUID for object.

- `text`. Character.

- `attachment_name`. Character.

- `condition_expression`. Character.

- `hide_if_disabled`. Logical.

- `validation_expression_*`. Character. Present if validations used.

- `validation_message_*`. Character. Present if validations used.

- `validation_severity_*`. Character. Present if validations used.
