test_that("corner cases", {

  res <- re_exec_val(.text <- c("foo", "bar"), "")
  expect_equal(
    as.data.frame(res),
    asdf(.text = .text, .match = reclist(mrec("", 1, 0), mrec("", 1, 0)))
  )

  res <- re_exec_val(.text <- c("foo", "", "bar"), "")
  expect_equal(
    as.data.frame(res),
    asdf(
      .text = .text,
      .match = reclist(mrec("", 1, 0), mrec("", 1, 0), mrec("", 1, 0))
    )
  )

  res <- re_exec_val(.text <- character(), "")
  expect_equal(as.data.frame(res), asdf(.text = .text, .match = reclist()))

  res <- re_exec_val(.text <- character(), "foo")
  expect_equal(as.data.frame(res), asdf(.text = .text, .match = reclist()))

  res <- re_exec_val(.text <- character(), "foo (g1) (g2)")
  expect_equal(
    as.data.frame(res),
    asdf(reclist(), reclist(), .text = .text, .match = reclist())
  )

  res <- re_exec_val(.text <- character(), "foo (g1) (?<name>g2)")
  expect_equal(
    as.data.frame(res),
    asdf(reclist(), name = reclist(), .text = .text, .match = reclist())
  )

  res <- re_exec_val(.text <- "not", "foo")
  expect_equal(
    as.data.frame(res),
    asdf(.text = .text, .match = reclist(mrec(NA, NA, NA)))
  )
})

test_that("not so corner cases", {

  dates <- c("2016-04-20", "1977-08-08", "not a date", "2016",
             "76-03-02", "2012-06-30", "2015-01-21 19:58")
  isodate <- "([0-9]{4})-([0-1][0-9])-([0-3][0-9])"
  expected <- asdf(
    reclist(
      mrec("2016", 1, 4), mrec("1977", 1, 4), narec(), narec(), narec(),
      mrec("2012", 1, 4), mrec("2015", 1, 4)
    ),
    reclist(
      mrec("04", 6, 7), mrec("08", 6, 7), narec(), narec(), narec(),
      mrec("06", 6, 7), mrec("01", 6, 7)
    ),
    reclist(
      mrec("20", 9, 10), mrec("08", 9, 10), narec(), narec(), narec(),
      mrec("30", 9, 10), mrec("21", 9, 10)
    ),
    .text = dates,
    .match = reclist(
      mrec("2016-04-20", 1, 10), mrec("1977-08-08", 1, 10), narec(),
      narec(), narec(), mrec("2012-06-30", 1, 10),
      mrec("2015-01-21", 1, 10)
    )
  )
  expect_equal(
    as.data.frame(re_exec_val(text = dates, pattern = isodate)),
    expected
  )

  isodaten <- "(?<year>[0-9]{4})-(?<month>[0-1][0-9])-(?<day>[0-3][0-9])"
  expected <- asdf(
    year = reclist(
      mrec("2016", 1, 4), mrec("1977", 1, 4), narec(), narec(), narec(),
      mrec("2012", 1, 4), mrec("2015", 1, 4)
    ),
    month = reclist(
      mrec("04", 6, 7), mrec("08", 6, 7), narec(), narec(), narec(),
      mrec("06", 6, 7), mrec("01", 6, 7)
    ),
    day = reclist(
      mrec("20", 9, 10), mrec("08", 9, 10), narec(), narec(), narec(),
      mrec("30", 9, 10), mrec("21", 9, 10)
    ),
    .text = dates,
    .match = reclist(
      mrec("2016-04-20", 1, 10), mrec("1977-08-08", 1, 10), narec(),
      narec(), narec(), mrec("2012-06-30", 1, 10),
      mrec("2015-01-21", 1, 10)
    )
  )

  expect_equal(
    as.data.frame(re_exec_val(text = dates, pattern = isodaten)),
    expected
  )
})


test_that("UTF8", {

  str <- "Gábor Csárdi"
  pat <- "Gábor"
  Encoding(str) <- Encoding(pat) <- "UTF-8"
  res <- re_exec_val(str, pat)
  expect_equal(
    as.data.frame(res),
    asdf(.text = str, .match = reclist(mrec(pat, 1, 5)))
  )

})


test_that("text is scalar & capture groups", {

  res <- re_exec_val(.text <- "foo bar", "(\\w+) (\\w+)")
  expect_equal(
    as.data.frame(res),
    asdf(
      reclist(mrec("foo", 1, 3)),
      reclist(mrec("bar", 5, 7)),
      .text = .text,
      .match = reclist(mrec("foo bar", 1, 7))
    )
  )

  res <- re_exec_val(.text <- "foo bar", "(?<g1>\\w+) (?<g2>\\w+)")
  expect_equal(
    as.data.frame(res),
    asdf(
      g1 = reclist(mrec("foo", 1, 3)),
      g2 = reclist(mrec("bar", 5, 7)),
      .text = .text,
      .match = reclist(mrec("foo bar", 1, 7))
    )
  )

})

test_that("perl argument", {
  # using perl=TRUE used to cause an error; not important in this case, but must
  # be supported if we want this to be a drop in replacement for other functions
  # (e.g. re-implenting `strsplit` with a rematch2 backend)

  res <- re_exec_val(.text <- "foo bar", "\\w+", perl = TRUE)
  expect_equal(
    as.data.frame(res),
    asdf(
      .text = .text,
      .match = reclist(mrec("foo", 1, 3))
    )
  )
  # actually check that the capture group doesn't show up

  res.tre <- re_exec_val(.text <- "foo bar", "\\w+ (\\w+)", perl = FALSE)
  res.perl <- re_exec_val(.text <- "foo bar", "\\w+ (\\w+)", perl= TRUE)
  expect_true(ncol(as.data.frame(res.perl)) == 3 && ncol(res.tre) == 2)
})
