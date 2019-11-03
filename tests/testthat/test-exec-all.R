
context("re_exec_all")

test_that("corner cases", {

  res <- re_exec_all(.text <- c("foo", "bar"), "")
  expect_equal(
    as.data.frame(res),
    asdf(
      .text = .text,
      .match = allreclist(
        list(
          match = c("", "", ""),
          start = c(1L, 2L, 3L),
          end   = c(0L, 1L, 2L)
        ),
        list(
          match = c("", "", ""),
          start = c(1L, 2L, 3L),
          end   = c(0L, 1L, 2L)
        )
      )
    )
  )

  res <- re_exec_all(.text <- c("", "bar"), "")
  expect_equal(
    as.data.frame(res),
    asdf(
      .text = .text,
      .match = allreclist(
        list(
          match = "",
          start = 1L,
          end   = 0L
        ),
        list(
          match = c("", "", ""),
          start = c(1L, 2L, 3L),
          end   = c(0L, 1L, 2L)
        )
      )
    )
  )

  res <- re_exec_all(.text <- character(), "")
  expect_equal(as.data.frame(res), asdf(.text = .text, .match = allreclist()))

  res <- re_exec_all(.text <- character(), "foo")
  expect_equal(as.data.frame(res), asdf(.text = .text, .match = allreclist()))

  res <- re_exec_all(.text <- "not", "foo")
  expect_equal(
    as.data.frame(res),
    asdf(
      .text = .text,
      .match = allreclist(mrec(character(), integer(), integer()))
    )
  )
})


test_that("capture groups", {

  pattern <- "([0-9]+)"

  res <- re_exec_all(
    .text <- c("123xxxx456", "", "xxx", "1", "123"),
    pattern
  )
  expect_equal(
    as.data.frame(res),
    asdf(
      ...1 = allreclist(
        mrec(c("123", "456"), c(1L, 8L), c(3L, 10L)), norec(), norec(),
        mrec("1", 1L, 1L), mrec("123", 1, 3)
      ),
      .text = .text,
      .match = allreclist(
        mrec(c("123", "456"), c(1L, 8L), c(3L, 10L)), norec(), norec(),
        mrec("1", 1L, 1L), mrec("123", 1, 3)
      )
    )
  )

})


test_that("scalar text with capure groups", {

  res <- re_exec_all(.text <- "foo bar", "\\b(\\w+)\\b")
  expect_equal(
    as.data.frame(res),
    asdf(
      ...1 = allreclist(mrec(c("foo", "bar"), c(1L, 5L), c(3L, 7L))),
      .text = .text,
      .match = allreclist(mrec(c("foo", "bar"), c(1L, 5L), c(3L, 7L)))
    )
  )

  res <- re_exec_all(.text <- "foo bar", "\\b(?<word>\\w+)\\b")
  expect_equal(
    as.data.frame(res),
    asdf(
      word = allreclist(mrec(c("foo", "bar"), c(1L, 5L), c(3L, 7L))),
      .text = .text,
      .match = allreclist(mrec(c("foo", "bar"), c(1L, 5L), c(3L, 7L)))
    )
  )

})
