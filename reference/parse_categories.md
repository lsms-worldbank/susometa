# Parse all categories files into a single data frame

Parse all categories files into a single data frame

## Usage

``` r
parse_categories(
  dir,
  file_pattern = "\\.xlsx",
  recurse = TRUE,
  sheet = "Categories"
)
```

## Arguments

- dir:

  Character. Directory where category files are located.

- file_pattern:

  Character. Default value is SuSo's default file name.

- recurse:

  Boolean. Default value assumes that `dir` is parent diectory that
  contains child directories that contain the categories.

- sheet:

  Character. Name of the Excel sheet where categories data are stored.
  By default, this is Survey Solutions' storage: `"Categories"`.

## Value

Data frame with the following columns:

- `category_id`. GUID used in questionnaire JSON reference a reusable
  category.

- `value`. Numerical value input in Designer (e.g., `1`, `2`).

- `text`. Character label input in Designer (e.g., `"Yes"`, `"No"`)

- `parentid`. GUID used to identify parent question.

Note: each group with the same `category_id` value represents the
contents of a single reusable category file.
