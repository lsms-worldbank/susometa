# Get metadata on attachments

Extract metadata about attachments from the questionnaire JSON as a data
frame.

## Usage

``` r
get_attachments(json_path)
```

## Arguments

- json_path:

  Character. Full path to the Survey Solutions questionnaire JSON file
  (e.g., `~/my_proj/path/to/document.json`).

## Value

Data frame with the following columns:

- `object_type`. Character. Simplified object type. Value: `attachment`.

- `type`. Character. Value: `"Attachment"`.

- `attachment_id`. Character. GUID for the translation.

- `attachment_name`. Character. User-provided name for the attachment.
  This is how Designer fields refer to it.

- `attachment_content_id`. Character. System-provided GUID for files in
  `Attachments`
