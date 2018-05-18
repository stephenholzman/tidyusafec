context("Using search_candidates")

test_that("search_candidates returns both tidy and raw results.", {

  test_search <- search_candidates(data_structure = "both", state = "VA", election_year = 2018, office = "H")

  expect_equal(length(test_search), 2)

  expect_is(test_search$tidy, "data.frame")

  expect_is(test_search$raw_responses, "list")

})
