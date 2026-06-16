# Parse a categories file

Survey Solutions stores reusable categories in Excel files. This
function parses that file and returns a data frame.

## Usage

``` r
parse_categories_file(path = path, sheet = "Categories")
```

## Arguments

- path:

  Character. Path to a categories file.

- sheet:

  Character. Name of the Excel sheet where categories data are stored.
  By default, this is Survey Solutions' storage: `"Categories"`.

## Value

Data frame with the following columns:

- `categories_id`. GUID used in questionnaire JSON reference a reusable
  category.

- `value`. Numerical value input in Designer (e.g., `1`, `2`).

- `text`. Character label input in Designer (e.g., `"Yes"`, `"No"`)

- `parentid`. GUID used to identify parent question.
