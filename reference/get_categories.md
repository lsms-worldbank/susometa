# Get metadata on categories.

Extract metadata about categories from the questionnaire JSON as a data
frame.

## Usage

``` r
get_categories(json_path)
```

## Arguments

- json_path:

  Character. Full path to the Survey Solutions questionnaire JSON file
  (e.g., `~/my_proj/path/to/document.json`).

## Value

Data frame with the following columns:

- `object_type`. Character. Simplified object type. Value:
  `reusable category`.

- `$type`. Character. May not be present in older JSON files.

- `Id`. Character.

- `Name`. Character.
