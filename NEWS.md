# susometa 0.3.0

- Add `get_validations()` to return a data frame of validations and the objects to which they are associated.
- Add fixed roster titles to the questionnaire attributes extracted by `parse_questionnaire()`
- Add fixed roster titles and values to the return value of `get_rosters()`

# susometa 0.2.0

- Improve `get_answer_options()` in several user-facing ways:
  -  Draw answers from both the questionnaire JSON and the reusable categories files.
  - Check that user inputs are complete and correct.
  - Issue informative error messages if not.
- Add attributes to the data for several questionnaire objects:
  - Add `categories_id` to questions data
  - Add `roster_variable_name`, `condition_expression`, and `hide_if_disabled` to rosters data
- Perform tests
  - Add testing infrastructure
  - Add tests for recently touched functions
  - Add text fixtures from the Resilient Futures questionnaire for current and future testing.
- Add LSMS Team as rights holder and funder.

# susometa 0.1.0

Split `get_answer_options()` into two functions:

- `get_answer_options()` returns a named numeric vector, where the values are answer codes and names are value labels.
- `get_ms_answers_as_var_labels()` serves a use case for labelling multi-select variables in exported data or display tables.

# susometa 0.0.0.9000

Initial version. Preserved to lock in old functionality of `get_answer_options()` for reproducibility.
