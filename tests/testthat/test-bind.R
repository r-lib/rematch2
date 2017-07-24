
context("bind_re_match")

test_that("normal cases", {
  match_cars <- tibble::rownames_to_column(mtcars)
  match_cars_nse <- bind_re_match(match_cars, rowname, "^(?<make>\\w+) ?(?<model>.+)?$")
  match_cars_se <- bind_re_match_(match_cars, "rowname", "^(?<make>\\w+) ?(?<model>.+)?$")
  match_cars_nse_with_match <- bind_re_match(match_cars, rowname, "^(?<make>\\w+) ?(?<model>.+)?$", keep_match = TRUE)

  second_match_cars <- bind_re_match(match_cars_nse_with_match, model, "(?<number>\\d+)", keep_match = TRUE)

  expect_equal(c(names(match_cars), "make", "model"), names(match_cars_nse))
  expect_equal(c(names(match_cars), "make", "model"), names(match_cars_se))
  expect_equal(c(names(match_cars), "make", "model", ".match"), names(match_cars_nse_with_match))
  expect_equal(c(names(match_cars_nse_with_match), "number", ".match1"), names(second_match_cars))
})
