# ==============================================================================
# inputs
# ==============================================================================


# ==============================================================================
# outputs
# ==============================================================================

testthat::test_that("returns data frame with expected columns", {

  # load input data
  qnr_df <- readRDS(testthat::test_path("fixtures", "qnr_df.rds"))

  questions_df <- get_questions(qnr_df = qnr_df)

  # is a data frame
  testthat::expect_s3_class(
    object = questions_df,
    class = "data.frame"
  )

  # names of the data frame
  question_col_names <- names(questions_df)

  # has core variable attributes
  testthat::expect_true(
    all(susometa:::var_general %in% question_col_names)
  )

})
