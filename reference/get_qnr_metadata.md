# Get metadata on questionnaire document metadata.

Extract metadata about the questionnaire itself from the questionnaire
JSON file as a data frame.

## Usage

``` r
get_qnr_metadata(json_path)
```

## Arguments

- json_path:

  Character. Full path to the Survey Solutions questionnaire JSON file
  (e.g., `~/my_proj/path/to/document.json`).

## Value

List with named elements corresponding the metadata attributes.
