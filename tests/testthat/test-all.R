test_that("corner cases", {
  res <- re_match_all(.text <- c("foo", "bar"), "")
  expect_equal(
    as.data.frame(res),
    asdf(.text = .text, .match = list(c("", "", ""), c("", "", "")))
  )

  res <- re_match_all(.text <- c("", "bar"), "")
  expect_equal(
    res,
    df(.text = .text, .match = list("", c("", "", "")))
  )

  res <- re_match_all(.text <- character(), "")
  expect_equal(res, df(.text = .text, .match = list()))

  res <- re_match_all(.text <- character(), "foo")
  expect_equal(as.data.frame(res), asdf(.text = .text, .match = list()))

  res <- re_match_all(.text <- "not", "foo")
  expect_equal(
    as.data.frame(res),
    asdf(.text = .text, .match = list(character()))
  )
})


test_that("capture groups", {
  pattern <- "([0-9]+)"

  res <- re_match_all(
    .text <- c("123xxxx456", "", "xxx", "1", "123"),
    pattern
  )
  expect_equal(
    as.data.frame(res),
    asdf(
      list(c("123", "456"), character(), character(), "1", "123"),
      .text = .text,
      .match = list(c("123", "456"), character(), character(), "1", "123")
    )
  )
})


test_that("scalar text with capure groups", {
  res <- re_match_all(.text <- "foo bar", "\\b(\\w+)\\b")
  expect_equal(
    res,
    df(list(c("foo", "bar")), .text = .text, .match = list(c("foo", "bar")))
  )

  res <- re_match_all(.text <- "foo bar", "\\b(?<word>\\w+)\\b")
  expect_equal(
    res,
    df(
      word = list(c("foo", "bar")),
      .text = .text,
      .match = list(c("foo", "bar"))
    )
  )
})
