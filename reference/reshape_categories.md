# Reshape categories data

Reshape categories data to be joined with metadata by `category_id`.

## Usage

``` r
reshape_categories(categories_df)
```

## Arguments

- categories_df:

  Data frame. categories data frame produced by
  [`parse_categories()`](https://lsms-worldbank.github.io/susometa/reference/parse_categories.md)
  or
  [`parse_categories_file()`](https://lsms-worldbank.github.io/susometa/reference/parse_categories_file.md).

## Value

Data frame with columns:

- `category_id`. Category ID for joining with questionnaire metadata.

- `answer_value_*`. Value of answer option.

- `answer_text_*`. Text label of answer option.
