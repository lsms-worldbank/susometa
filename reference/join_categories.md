# Join questionnaire metadata and categories metadata

Join questionnaire metadata and categories metadata

## Usage

``` r
join_categories(qnr_json_df, categories_df)
```

## Arguments

- qnr_json_df:

  Data frame. Type produced by
  [`parse_questionnaire()`](https://lsms-worldbank.github.io/susometa/reference/parse_questionnaire.md).

- categories_df:

  Data frame. Type produced by
  [`reshape_categories()`](https://lsms-worldbank.github.io/susometa/reference/reshape_categories.md).

## Value

Data frame. Same structure as `qnr_json_df`, but with categories
joined/updated by `categories_id` key.
