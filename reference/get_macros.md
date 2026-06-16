# Get metadata for macros.

Extract metadata about macros from the questionnaire JSON as a data
frame.

## Usage

``` r
get_macros(json_path)
```

## Arguments

- json_path:

  Character. Full path to the Survey Solutions questionnaire JSON file
  (e.g., `~/my_proj/path/to/document.json`).

## Value

Data frame with the following columns:

- `object_type`. Character. Type of object. Value: `macro`.

- `type`. Character. SuSo-provided object type. Value: `Macro`. May not
  be present in older JSON versions.

- `macro_id`. Character.

- `macro_name`. Character.

- `macro_description`. Character.

- `macro_content`. Character.
