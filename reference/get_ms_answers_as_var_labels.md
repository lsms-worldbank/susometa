# Get mutli-select answers as variable labels

When exported, multi-select questions become a set of variables whose
name is a combination of the variable name and answer option value in
Designer. This function extracts answer options in convenient form to
either label variables in data or columns in display tables, where
expected specification for labelling is of the form
`{variable} = {variable label}`.

## Usage

``` r
get_ms_answers_as_var_labels(
  json_path,
  categories_dir = NULL,
  varname,
  to_exclude = NULL
)
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

Named character variable. Names are column names of the form
`{variable}__{value}`. Values are the answer options.
