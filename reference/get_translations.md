# Get metadata about translations.

Extract metadata about translations from the questionnaire JSON as a
data frame.

## Usage

``` r
get_translations(json_path)
```

## Arguments

- json_path:

  Character. Full path to the Survey Solutions questionnaire JSON file
  (e.g., `~/my_proj/path/to/document.json`).

## Value

Data frame with the following columns:

- `object_type`. Character. Simplified object type. Value:
  `translation`.

- `$type`. Character. May not be present in older JSON files.

- `Id`. Character.

- `Name`. Character.
