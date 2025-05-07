test_that("re_exec_val intexing", {
  res <- re_exec_val(character(), "foo([0-9]+)")
  expect_identical(res[[1]]$match, character())
  expect_identical(res[[1]]$start, integer())
  expect_identical(res[[1]]$end, integer())

  expect_identical(res$.match$match, character())
  expect_identical(res$.match$start, integer())
  expect_identical(res$.match$end, integer())

  name_rex <- paste0(
    "(?<first>[[:upper:]][[:lower:]]+) ",
    "(?<last>[[:upper:]][[:lower:]]+)"
  )
  notables <- c(
    "  Ben Franklin and Jefferson Davis",
    "\tMillard Fillmore"
  )
  pos <- re_exec_val(notables, name_rex)

  expect_identical(pos$first$match, c("Ben", "Millard"))
  expect_identical(pos$first$start, c(3L, 2L))
  expect_identical(pos$first$end, c(5L, 8L))

  expect_identical(pos$last$match, c("Franklin", "Fillmore"))
  expect_identical(pos$last$start, c(7L, 10L))
  expect_identical(pos$last$end, c(14L, 17L))

  expect_identical(pos$.match$match, c("Ben Franklin", "Millard Fillmore"))
  expect_identical(pos$.match$start, c(3L, 2L))
  expect_identical(pos$.match$end, c(14L, 17L))
})

test_that("re_exec_all_val indexing", {
  name_rex <- paste0(
    "(?<first>[[:upper:]][[:lower:]]+) ",
    "(?<last>[[:upper:]][[:lower:]]+)"
  )
  notables <- c(
    "  Ben Franklin and Jefferson Davis",
    "\tMillard Fillmore"
  )
  allpos <- re_exec_all_val(notables, name_rex)

  expect_identical(
    allpos$first$match,
    list(c("Ben", "Jefferson"), "Millard")
  )
  expect_identical(allpos$first$start, list(c(3L, 20L), 2L))
  expect_identical(allpos$first$end, list(c(5L, 28L), 8L))

  expect_identical(
    allpos$last$match,
    list(c("Franklin", "Davis"), "Fillmore")
  )
  expect_identical(allpos$last$start, list(c(7L, 30L), 10L))
  expect_identical(allpos$last$end, list(c(14L, 34L), 17L))

  expect_identical(
    allpos$.match$match,
    list(c("Ben Franklin", "Jefferson Davis"), "Millard Fillmore")
  )
  expect_identical(allpos$.match$start, list(c(3L, 20L), 2L))
  expect_identical(allpos$.match$end, list(c(14L, 34L), 17L))
})

test_that("$ errors", {
  name_rex <- paste0(
    "(?<first>[[:upper:]][[:lower:]]+) ",
    "(?<last>[[:upper:]][[:lower:]]+)"
  )
  notables <- c(
    "  Ben Franklin and Jefferson Davis",
    "\tMillard Fillmore"
  )
  pos <- re_exec_val(notables, name_rex)
  allpos <- re_exec_all_val(notables, name_rex)

  expect_error(pos$first$foo)
  expect_error(allpos$first$foo)
})
