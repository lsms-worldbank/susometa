# Get metadata for validations

Get metadata for validations

## Usage

``` r
get_validations(json_path)
```

## Arguments

- json_path:

  Character. Full path to the Survey Solutions questionnaire JSON file
  (e.g., `~/my_proj/path/to/document.json`).

## Value

Data frame with following columns:

- `public_key`. Character. GUID of object. Not available for static
  text.

- `object_type`. Character. Simplified object type. Value: `section`.

- `type`. Character.

- `varname`. Character.

- `text`. Character.

- `expression_number`. Numeric.

- `validation_message`. Character.

- `validation_expression`. Character.

- `severity`. Numeric.
