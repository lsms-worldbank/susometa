# Get questions with the section to which each belongs.

Extract metadata about questions and variables and their parent section
from the questionnaire JSON as a data frame.

## Usage

``` r
get_questions_by_section(json_path)
```

## Arguments

- json_path:

  Character. Full path to the Survey Solutions questionnaire JSON file
  (e.g., `~/my_proj/path/to/document.json`).

## Value

Data frame with the following columns

- `title` and `section_id` (PublicKey) for sections

- All the columns from
  [`get_questions()`](https://lsms-worldbank.github.io/susometa/reference/get_questions.md)
  and
  [`get_variables()`](https://lsms-worldbank.github.io/susometa/reference/get_variables.md)

## See also

get_sections get_questions get_variables
