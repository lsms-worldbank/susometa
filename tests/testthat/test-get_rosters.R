# ==============================================================================
# inputs
# ==============================================================================


# ==============================================================================
# outputs
# ==============================================================================

testthat::test_that("get_rosters() returns data frame with expected columns", {

  # load input data
  qnr_df <- readRDS(testthat::test_path("fixtures", "qnr_df.rds"))

  rosters_df <- get_rosters(qnr_df = qnr_df)

  # is a data frame
  testthat::expect_s3_class(
    object = rosters_df,
    class = "data.frame"
  )

  # names of the data frame
  roster_col_names <- names(rosters_df)

  # has expected attributes
  testthat::expect_true(
    all(susometa:::roster_attribs %in% roster_col_names)
  )

})