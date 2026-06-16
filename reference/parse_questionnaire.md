# Parses JSON questionnaire file into rectangular data frame

Transforms the information found in the JSON questionnaire file,
`document.json`, into a simpler rectangular file. The JSON file provides
a (deeply) nested representation of the questionnaire. This function
returns a rectangular data frame in the following format:

## Usage

``` r
parse_questionnaire(json_path)
```

## Arguments

- json_path:

  Complete file path to the questionnaire JSON file, typically named
  `document.json`

## Value

Data frame that contains all attributes of all objects in the
questionnaire.

## Details

- Rows: objects in the questionnaire (e.g., sections, sub-sections,
  rosters, questions, variables, static text, etc.)

- Columns: attributes.

This format facilitates common needs such as:

- Collecting all the validations

- Compiling keys and values for questions (e.g., PublicKey for questions
  to understand the paradata AnswerRemove event)
