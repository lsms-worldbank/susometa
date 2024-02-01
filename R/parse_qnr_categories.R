#' Find paradata files
#' 
#' Return path(s) of categories files found target path.
#' 
#' @param dir Character. Directory to scan for categories files
#' @param file_pattern Character. Default value is SuSo's default file name.
#' @param recurse Boolean. Default value assumes that `dir` is parent diectory
#' that contains child directories that contain the categories.
#' 
#' @return Character vector of paths to categories file(s).
#' 
#' @importFrom fs dir_ls
#' @importFrom cli cli_abort
#' 
#' @export
find_categories <- function(
    dir,
    file_pattern = "\\.xlsx",
    recurse = TRUE
) {

    # scan for paradata files
    categories_file_paths <- fs::dir_ls(
        path = dir,
        type = "file",
        regexp = file_pattern,
        recurse = recurse
    )

    # return either paths or an error if no files found
    if (length(categories_file_paths) == 0) {
        cli::cli_abort(
            message = c(
                "No paradata files found",
                "*" = "Check that {.var path} is correct.",
                "*" = "Check whether {.var recurse} scans child directories."
            )
        )
    } else {
        return(categories_file_paths)
    }

}

#' Parse all categories files into a single data frame
#' 
#' @param dir Character. Directory where category files are located.
#' @param file_pattern Character. Default value is SuSo's default file name.
#' @param recurse Boolean. Default value assumes that `dir` is parent diectory
#' that contains child directories that contain the categories.
#' @param sheet Character. Name of the Excel sheet where categories data are
#' stored. By default, this is Survey Solutions' storage: `"Categories"`.
#' 
#' @return Data frame with the following columns: 
#' 
#' - `category_id`. GUID used in questionnaire JSON reference a reusable 
#' category.
#' - `value`. Numerical value input in Designer (e.g., `1`, `2`).
#' - `text`. Character label input in Designer (e.g., `"Yes"`, `"No"`)
#' - `parentid`. GUID used to identify parent question.
#' 
#' Note: each group with the same `category_id` value represents the contents
#' of a single reusable category file.
#' 
#' @importFrom purrr map_dfr
#' 
#' @export
parse_categories <- function(
    dir,
    file_pattern = "\\.xlsx",
    recurse = TRUE,
    sheet = "Categories"
) {

    # compile paths to category files
    category_file_path <- find_categories(
        dir = dir,
        file_pattern = file_pattern,
        recurse = recurse
    )

    # collect all categories data frames in a single data frame
    categories_all <- purrr::map_dfr(
        .x = category_file_path,
        .f = ~ parse_categories_file(
            path = .x,
            sheet = sheet
        )
    )

    return(categories_all)

}

#' Parse a categories file
#' 
#' @description
#' Survey Solutions stores reusable categories in Excel files.
#' This function parses that file and returns a data frame.
#' 
#' @param path Character. Path to a categories file.
#' @param sheet Character. Name of the Excel sheet where categories data are
#' stored. By default, this is Survey Solutions' storage: `"Categories"`.
#' 
#' 
#' @return Data frame with the following columns:
#' 
#' - `category_id`. GUID used in questionnaire JSON reference a reusable 
#' category.
#' - `value`. Numerical value input in Designer (e.g., `1`, `2`).
#' - `text`. Character label input in Designer (e.g., `"Yes"`, `"No"`)
#' - `parentid`. GUID used to identify parent question.
#' 
#' @importFrom fs path_file path_ext_remove
#' @importFrom readxl read_excel
#' @importFrom dplyr %>% mutate rename
#' 
#' @export
parse_categories_file <- function(
    path = path,
    sheet = "Categories"
) {

    category_id <- fs::path_file(path) %>%
        fs::path_ext_remove()

    # injest questionnaire file
    categories_df <- readxl::read_excel(
            path = path,
            sheet = sheet
        ) %>%
        # add the category GUID before first column
        dplyr::mutate(
            categories_id = .env$category_id,
            .before = 1
        ) %>%
        # rename category ID column as value
        dplyr::rename(value = .data$id)

    return(categories_df)

}

#' Reshape categories data
#' 
#' @description
#' Reshape categories data to be joined with metadata by `category_id`.
#' 
#' @param categories_df Data frame. categories data frame produced by
#' `parse_categories()` or `parse_categories_file()`.
#' 
#' @return Data frame with columns:
#' 
#' - `category_id`. Category ID for joining with questionnaire metadata.
#' - `answer_value_*`. Value of answer option.
#' - `answer_text_*`. Text label of answer option.
#' 
#' @importFrom dplyr %>% mutate row_number
#' @importFrom tidyr pivot_wider
#' 
#' @export
reshape_categories <- function(categories_df) {

    df_reshaped <- categories_df %>%
        # add a within-group index
        dplyr::mutate(n = dplyr::row_number(), .by = .data$categories_id) %>%
        # pivot to a data frame composed of:
        # - category_id
        # - answer_value_*
        # - answer_text_*
        tidyr::pivot_wider(
            id_cols = .data$categories_id,
            names_from = .data$n,
            values_from = c(.data$text, .data$value),
            names_glue = "answer_{.name}"
        ) %>%
        # coerce columns to character so that they match the questionnaire JSON
        # if not done, `answer_value_*` will be numeric
        dplyr::mutate(
            dplyr::across(
                .cols = dplyr::starts_with("answer_"),
                .fns =  ~ as.character(.x)
            )
        )

    return(df_reshaped)

}

#' Join questionnaire metadata and categories metadata
#' 
#' @param qnr_json_df Data frame. Type produced by `parse_questionnaire()`.
#' @param categories_df Data frame. Type produced by `reshape_categories()`.
#' 
#' @return Data frame. Same structure as `qnr_json_df`, but with categories
#' joined/updated by `categories_id` key.
#' 
#' @importFrom dplyr %>% mutate select starts_with all_of rows_update left_join
#' @importFrom stringr str_replace_all
#' 
#' @export
join_categories <- function(
    qnr_json_df,
    categories_df
) {

    # create a `categories_id` in JSON file without the hyphens
    # so that this key can be used to merge with `categories_df`
    qnr_json_df <- qnr_json_df %>%
        dplyr::mutate(
            categories_id_original = .data$categories_id,
            categories_id = stringr::str_replace_all(
                string = .data$categories_id,
                pattern = "\\-",
                replacement = ""
            )
        )

    # construct df from JSON data that contains same family of columns
    # as categories_df, in order to facilitate comparison of column names
    json_categories <- qnr_json_df %>%
        dplyr::select(
            # entity ID
            .data$public_key,
            # category ID
            .data$categories_id, 
            # text-value pairs
            dplyr::starts_with("answer_text_"), 
            dplyr::starts_with("answer_value_")
        )

    # evaluate which columns appear where
    # ... in both
    cols_in_both <- base::intersect(names(json_categories), names(categories_df))
    # ... in categories file only
    cols_in_categories_only <- setdiff(names(categories_df), names(json_categories))
    # ... in JSON file only
    cols_in_json_only <- setdiff(names(json_categories), names(categories_df))

    # create data sets

    # core json cols that do not overlap with categories
    json_core <- qnr_json_df %>%
        dplyr::select(
            !c(
                dplyr::starts_with("answer_text_"), 
                dplyr::starts_with("answer_value_")
            )
        )

    # common data
    json_common_df <- qnr_json_df %>%
        dplyr::select(
            .data$public_key,
            dplyr::all_of(cols_in_both)
        )
    categories_common_df <- categories_df %>%
        dplyr::select(dplyr::all_of(cols_in_both))
    
    # unique data
    json_unique_df <- qnr_json_df %>%
        dplyr::select(
            .data$public_key,
            .data$categories_id,
            dplyr::all_of(cols_in_json_only)
        )
    categories_unique_df <- categories_df %>%
        dplyr::select(
            .data$categories_id,
            dplyr::all_of(cols_in_categories_only)
        )

    # patch common data
    patched_categories_df <- json_common_df %>%
        dplyr::rows_update(
            categories_common_df,
            by = "categories_id"
        )

    # join together all pieces by their common key: categories_id
    combined_df <- json_core %>%
        # json core and patched
        dplyr::left_join(
            patched_categories_df, 
            by = c("public_key", "categories_id")
        ) %>%
        # unique columns from json and categories data
        {
            if (ncol(json_unique_df) > 0)  {
                dplyr::left_join(
                    ., json_unique_df,
                    by = c("public_key", "categories_id")
                )
            } else {
                .
            }
            
        } %>%
        {
            if (ncol(categories_unique_df) > 0) {
                dplyr::left_join(
                    ., categories_unique_df, 
                    by = "categories_id",
                    relationship = "many-to-one"
                )
            } else {
                .
            }
        } %>%
        # revert to original categories_id column
        dplyr::mutate(
            # overwrite column with UUID containing dashes
            categories_id = .data$categories_id_original,
            # remove archival column
            categories_id_original = NULL
        )

    return(combined_df)

}
