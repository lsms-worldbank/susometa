# ==============================================================================
# inputs
# ==============================================================================

# ------------------------------------------------------------------------------
# json path
# ------------------------------------------------------------------------------

testthat::test_that("errors if `json_path` does not exist", {

  testthat::expect_error(
    get_answer_options(json_path = "path/dne/document.json")
  )

})

testthat::test_that("errors if `json_path` points to non-json file", {

  testthat::expect_error(
    get_answer_options(
      json_path = testthat::test_path(
        "fixtures", "qnr_metadata", "Categories",
        "0e00edf674ed45f4a93e6d226f2c069b.xlsx"
      )
    )
  )

})

# ------------------------------------------------------------------------------
# categories directory
# ------------------------------------------------------------------------------

# TODO

# ------------------------------------------------------------------------------
# varname
# ------------------------------------------------------------------------------

testthat::test_that("errors if questionnaire does not contain variable", {

  testthat::expect_error(
    get_answer_options(
      json_path = testthat::test_path(
        "fixtures", "qnr_metadata", "document.json"
      ),
      categories_dir = testthat::test_path(
        "fixtures", "qnr_metadata", "Categories"
      ),
      varname = boo
    ),
    regexp = "No variable named"
  )

  testthat::expect_no_error(
    get_answer_options(
      json_path = testthat::test_path(
        "fixtures", "qnr_metadata", "document.json"
      ),
      categories_dir = testthat::test_path(
        "fixtures", "qnr_metadata", "Categories"
      ),
      varname = s12q07
    )
  )

})

# TODO
# testthat::test_that("errors if `varname` is not a bare name", {
# })

# ------------------------------------------------------------------------------
# to_exclude
# ------------------------------------------------------------------------------

testthat::test_that("errors if `to_exclude` is not `NULL` or a numeric vector", {

  testthat::expect_error(
    get_answer_options(
      json_path = testthat::test_path(
        "fixtures", "qnr_metadata", "document.json"
      ),
      categories_dir = testthat::test_path(
        "fixtures", "qnr_metadata", "Categories"
      ),
      varname = s12q07,
      to_exclude = "foo"
    ),
    regexp = "must be either `NULL` or a numeric vector."
  )

  testthat::expect_no_error(
    get_answer_options(
      json_path = testthat::test_path(
        "fixtures", "qnr_metadata", "document.json"
      ),
      categories_dir = testthat::test_path(
        "fixtures", "qnr_metadata", "Categories"
      ),
      varname = s12q07,
      to_exclude = 1
    )
  )

  testthat::expect_no_error(
    get_answer_options(
      json_path = testthat::test_path(
        "fixtures", "qnr_metadata", "document.json"
      ),
      categories_dir = testthat::test_path(
        "fixtures", "qnr_metadata", "Categories"
      ),
      varname = s12q07,
      to_exclude = c(1, 96)
    )
  )

})

testthat::test_that("errors if no answers remain after excluding", {

  # answer options
  testthat::expect_error(
    get_answer_options(
      json_path = testthat::test_path(
        "fixtures", "qnr_metadata", "document.json"
      ),
      categories_dir = testthat::test_path(
        "fixtures", "qnr_metadata", "Categories"
      ),
      varname = s12q07,
      to_exclude = c(1:9, 96)
    ),
    regexp = "No answers remain"
  )

  # reusable categories
  testthat::expect_error(
    susometa::get_answer_options(
      json_path = testthat::test_path(
        "fixtures", "qnr_metadata", "document.json"
      ),
      categories_dir = testthat::test_path(
        "fixtures", "qnr_metadata", "Categories"
      ),
      varname = s22q1c,
      to_exclude = c(1, 2)
    ),
    regexp = "No answers remain"
  )

})

# ==============================================================================
# outputs
# ==============================================================================

testthat::test_that(
  desc = "returns named vector of answer options when answer present",
  code = {

    # get answers
    answers_options <- susometa::get_answer_options(
      json_path = testthat::test_path(
        "fixtures", "qnr_metadata", "document.json"
      ),
      categories_dir = testthat::test_path(
        "fixtures", "qnr_metadata", "Categories"
      ),
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

testthat::test_that(
  desc = "excluded answers are actually excluded",
  code = {

    # get answers
    answers_options <- susometa::get_answer_options(
      json_path = testthat::test_path(
        "fixtures", "qnr_metadata", "document.json"
      ),
      categories_dir = testthat::test_path(
        "fixtures", "qnr_metadata", "Categories"
      ),
      varname = reason_refusal,
      to_exclude = -96
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
        "No interest"
      )
    )

  }
)

testthat::test_that(
  desc = "returns answers when the question uses reusable categories",
  code = {

    # get answers
    answers_options <- susometa::get_answer_options(
      json_path = testthat::test_path(
        "fixtures", "qnr_metadata", "document.json"
      ),
      categories_dir = testthat::test_path(
        "fixtures", "qnr_metadata", "Categories"
      ),
      varname = s22q1c
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
        "YES",
        "NO"
      )
    )

  }
)
