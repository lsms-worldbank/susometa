# ==============================================================================
# ingest data
# ==============================================================================

# reusable categories metadata
reusable_categories_df <- susometa::parse_categories(
  dir = testthat::test_path("fixtures", "qnr_metadata", "Categories")
)

# ==============================================================================
# write data to disk for persistent storage
# ==============================================================================

saveRDS(
  object = reusable_categories_df,
  file = testthat::test_path("fixtures", "reusable_categories_df.rds")
)
