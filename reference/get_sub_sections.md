# Get metadata for sub-sections

Extract metadata about sub-sections from the questionnaire JSON as a
data frame.

## Usage

``` r
get_sub_sections(json_path)
```

## Arguments

- json_path:

  Character. Full path to the Survey Solutions questionnaire JSON file
  (e.g., `~/my_proj/path/to/document.json`).

## Value

Data frame with the following columns:

- `object_type`. Character. Simplified object type. Value:
  `sub-section`.

- `type`. Character. SuSo provided object type. Value: Group.

- `title`. Character.

- `variable_name`. Character.

- `condition_expression`. Character.

- `public_key`. Character. GUID.
