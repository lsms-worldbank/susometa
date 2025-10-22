# ==============================================================================
# inputs
# ==============================================================================

testthat::test_that("errors if `varname` is not a bare name", {

  # load input data
  qnr_df <- readRDS(testthat::test_path("fixtures", "qnr_df.rds"))
  reusable_categories_df <- readRDS(
    testthat::test_path("fixtures", "reusable_categories_df.rds")
  )

  # errors for inputs that are not bare names
  # testthat::expect_error(
  #   get_answer_options(
  #     qnr_df = qnr_df,
  #     categories_df = reusable_categories_df,
  #     varname = "reason_refusal"
  #   ),
  #   regexp = "Invalid value provided"
  # )

  testthat::expect_no_error(
    get_answer_options(
      qnr_df = qnr_df,
      categories_df = reusable_categories_df,
      varname = reason_refusal
    )
  )

})

testthat::test_that("errors if questionnaire does not contain variable", {

  # load input data
  qnr_df <- readRDS(testthat::test_path("fixtures", "qnr_df.rds"))
  reusable_categories_df <- readRDS(
    testthat::test_path("fixtures", "reusable_categories_df.rds")
  )

  testthat::expect_error(
    get_answer_options(
      qnr_df = qnr_df,
      categories_df = reusable_categories_df,
      varname = boo
    ),
    regexp = "No variable named"
  )

})

testthat::test_that("errors if categories are not a data frame", {

  # load input data
  qnr_df <- readRDS(testthat::test_path("fixtures", "qnr_df.rds"))
  reusable_categories_df <- c("wrong", "input")

  testthat::expect_error(
    get_answer_options(
      qnr_df = qnr_df,
      categories_df = reusable_categories_df,
      varname = reason_refusal
    ),
    regexp = "is not a data frame"
  )

})

testthat::test_that("errors if categories df does not have expected cols", {

  # load input data
  qnr_df <- readRDS(testthat::test_path("fixtures", "qnr_df.rds"))
  reusable_categories_df <- testthat::test_path(
    "fixtures", "reusable_categories_df.rds"
  ) |>
  readRDS() |>
  # remove column to trigger an error
	dplyr::select(categories_id)

  testthat::expect_error(
    get_answer_options(
      qnr_df = qnr_df,
      categories_df = reusable_categories_df,
      varname = reason_refusal
    ),
    regexp = "not have expected columns"
  )

})

# ==============================================================================
# outputs
# ==============================================================================

testthat::test_that(
  desc = "returns named vector of answer options when answer present",
  code = {

    # load input data
    qnr_df <- readRDS(testthat::test_path("fixtures", "qnr_df.rds"))
    reusable_categories_df <- readRDS(
      testthat::test_path("fixtures", "reusable_categories_df.rds")
    )

    # get answers
    answers_options <- susometa::get_answer_options(
      qnr_df = qnr_df,
      categories_df = reusable_categories_df,
      varname = reason_refusal
    )

    # check expectations
    # return value is a numeric vector
    testthat::expect_vector(
      object = answers_options,
      ptype = numeric()
    )
    # return value has expected names
    testthat::expect_named(
      object = answers_options,
      expected = c(
        "No time / Too busy",
        "No interest",
        "Other, specify"
      )
    )

  }
)
