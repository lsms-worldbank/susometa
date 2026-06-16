# Get answer options

Obtain the answer options for a target variable as a set of name-value
pairs in labelled numeric vector.

## Usage

``` r
get_answer_options(json_path, categories_dir, varname, to_exclude = NULL)
```

## Arguments

- json_path:

  Character. Full path to the Survey Solutions questionnaire JSON file
  (e.g., `~/my_proj/path/to/document.json`).

- categories_dir:

  Character. Path to the `Categories` directory of metadata and/or where
  the `.xlsx` files containing reusable categories can be found.

- varname:

  Variable name. Bare name of variable whose answer options to extract.

- to_exclude:

  Numeric vector. Code of answer options to exclude.

## Value

Named numeric vector. Values are answer codes. Names are answer labels.
