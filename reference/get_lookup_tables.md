# Get metadata on lookup tables.

Extract metadata about translations from the questionnaire JSON as a
data frame.

## Usage

``` r
get_lookup_tables(json_path)
```

## Arguments

- json_path:

  Character. Full path to the Survey Solutions questionnaire JSON file
  (e.g., `~/my_proj/path/to/document.json`).

## Value

Data frame with the following columns:

- `object_type`. Simple object type. Value: `lookup table`.

- `lookup_table_id`. Character. GUID.

- `lookup_table_name`. Character. Name as it appears in Designer.

- `lookup_table_file_name`. Character. Name of file uploaded in
  Designer.
