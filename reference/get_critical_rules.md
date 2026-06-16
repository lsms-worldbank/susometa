# Get metadata on critical rules.

Extract metadata about critical rules from the questionnaire JSON as a
data frame.

## Usage

``` r
get_critical_rules(json_path)
```

## Arguments

- json_path:

  Character. Full path to the Survey Solutions questionnaire JSON file
  (e.g., `~/my_proj/path/to/document.json`).

## Value

Data frame.

- `object_type`. Character. Simplified object type. Value:
  `critical rule`.

- `type`. Character. Always `CriticalRule`.

- `rule_id`. Character. GUID

- `rule_message`. Character. Error message for the rule.

- `rule_expression`. Character. Validation expression for the rule.

- `rule_description`. Character. Optional description for the rule.
