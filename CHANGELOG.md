1) Version 0.1.0 - First version. Basic validations: 

- Range(min=, max=) # for floating 64
- Required # Any
- Custom = "\<function>\" # Any
- Regex = "\<regex>\" # String

2) Version 0.1.1

- Fixed problem with Option<String> for Custom and Regex validations. Now you can use Option or not. 
- Added Length(min=, max=) for Strings. 

2) Version 0.1.2

- Added 3 validations: `not_blank`, `one_of()`, and `not_in()`.

3) Version 0.1.3

- Migrated repo to https://github.com/cleuton/csv-schema-validator
- Code refactorings to improve maintainability.
- `required` can only be applied to `Option<T>` fields.
- Enforce field type / literal type check.
- Enforce validation type: 
    * Regex: Only for String or Option<String> fields. In case of Option<String> only validate if value is Some<>.
    * Custom: Only for String or Option<String> fields. In case of Option<String> only validate if value is Some<>.
    * Length: Only for String or Option<String> fields. In case of Option<String> only validate if value is Some<>.
    * Not blank: Only for String or Option<String> fields. In case of Option<String> only validate if value is Some<>.
    * Range: Only for numeric fields. 
- Range now can accept only `min` or only `max`, to check `greater-or-equal` or `less-or-equal`.

4) Version 0.2.0

- Added cross-column validation: `if_then("<conditional_column>", <conditional_value>, <expected_value>)`.
- The `if_then` rule defines a logical implication between two columns.
  If the conditional column equals `<conditional_value>`, the annotated column must equal `<expected_value>`.
- Both columns must be optional (`Option<T>` and `Option<R>`), and their inner types may differ (e.g., `Option<String>` with `Option<u32>`).
- Equality-only comparison. If the condition is not met, the annotated field is not validated (it may be `None` or any value).
- Example:
  ```rust
  use serde::Deserialize;
  use csv_schema_validator::ValidateCsv;

  #[derive(Deserialize, ValidateCsv, Debug)]
  struct Order {
      plan: Option<String>,

      // If plan == "P" → seats must be 100
      #[validate(if_then("plan", "P", 100))]
      seats: Option<u32>,
  }
  ```