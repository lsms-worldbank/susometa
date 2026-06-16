# Changelog

## susometa 0.4.0

### Breaking changes

- **Workflow changes.** In previous versions, the workflow was two-step:
  first, parse the questionnaire JSON file into a data frame; then,
  extract target content from that questionnaire data frame. Now, the
  workflow is a single step: extract the target content from the JSON.
- **API changes.** In line with the workflow changes, the argument
  `qnr_df`, the questionnaire data frame, has now been changed to
  `json_path`, the path to the questionnaire JSON.

### Improvements

- Use a faster engine to parse JSON. In particular, move from `tidyjson`
  to `jqr`, a wrapper around `jq`, a library for filtering and mutating
  JSON data.
- Add functions to extract new content from the questionnaire JSON:
  - Static texts:
    [`get_static_texts()`](https://lsms-worldbank.github.io/susometa/reference/get_static_texts.md)
  - Macros:
    [`get_macros()`](https://lsms-worldbank.github.io/susometa/reference/get_macros.md)
  - Categories:
    [`get_categories()`](https://lsms-worldbank.github.io/susometa/reference/get_categories.md)
  - Translations:
    [`get_translations()`](https://lsms-worldbank.github.io/susometa/reference/get_translations.md)
  - Attachments:
    [`get_attachments()`](https://lsms-worldbank.github.io/susometa/reference/get_attachments.md)
  - Lookup tables:
    [`get_lookup_tables()`](https://lsms-worldbank.github.io/susometa/reference/get_lookup_tables.md)
  - Critical rules:
    [`get_critical_rules()`](https://lsms-worldbank.github.io/susometa/reference/get_critical_rules.md)
  - Questionnaire-level metadata:
    [`get_qnr_metadata()`](https://lsms-worldbank.github.io/susometa/reference/get_qnr_metadata.md)

## susometa 0.3.2

- Revert check that `varname` parameter in `get_answer_options` is a
  bare name, since not working correctly when called by other functions.
- Add variable type attributes extracted for computed varaibles.
  - Add variable type attributes to those exported
  - Correct labels for variable types (e.g., Boolean, string, etc.)
  - Apply labels to variable type

## susometa 0.3.1

Export
[`get_validations()`](https://lsms-worldbank.github.io/susometa/reference/get_validations.md),
a critical step somehow overlooked in the last release.

## susometa 0.3.0

- Add
  [`get_validations()`](https://lsms-worldbank.github.io/susometa/reference/get_validations.md)
  to return a data frame of validations and the objects to which they
  are associated.
- Add fixed roster titles to the questionnaire attributes extracted by
  [`parse_questionnaire()`](https://lsms-worldbank.github.io/susometa/reference/parse_questionnaire.md)
- Add fixed roster titles and values to the return value of
  [`get_rosters()`](https://lsms-worldbank.github.io/susometa/reference/get_rosters.md)

## susometa 0.2.0

- Improve
  [`get_answer_options()`](https://lsms-worldbank.github.io/susometa/reference/get_answer_options.md)
  in several user-facing ways:
  - Draw answers from both the questionnaire JSON and the reusable
    categories files.
  - Check that user inputs are complete and correct.
  - Issue informative error messages if not.
- Add attributes to the data for several questionnaire objects:
  - Add `categories_id` to questions data
  - Add `roster_variable_name`, `condition_expression`, and
    `hide_if_disabled` to rosters data
- Perform tests
  - Add testing infrastructure
  - Add tests for recently touched functions
  - Add text fixtures from the Resilient Futures questionnaire for
    current and future testing.
- Add LSMS Team as rights holder and funder.

## susometa 0.1.0

Split
[`get_answer_options()`](https://lsms-worldbank.github.io/susometa/reference/get_answer_options.md)
into two functions:

- [`get_answer_options()`](https://lsms-worldbank.github.io/susometa/reference/get_answer_options.md)
  returns a named numeric vector, where the values are answer codes and
  names are value labels.
- [`get_ms_answers_as_var_labels()`](https://lsms-worldbank.github.io/susometa/reference/get_ms_answers_as_var_labels.md)
  serves a use case for labelling multi-select variables in exported
  data or display tables.

## susometa 0.0.0.9000

Initial version. Preserved to lock in old functionality of
[`get_answer_options()`](https://lsms-worldbank.github.io/susometa/reference/get_answer_options.md)
for reproducibility.
