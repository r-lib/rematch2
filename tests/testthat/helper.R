
df <- function(...) {
  args <- list(...)
  structure(
    args,
    names = names(args),
    row.names = seq_along(args[[1]]),
    class = c("tbl_df", "tbl", "data.frame")
  )
}

asdf <- function(...) {
  as.data.frame(df(...))
}

mrec <- function(match, start, end) {
  list(
    match = as.character(match),
    start = as.integer(start),
    end = as.integer(end)
  )
}

narec <- function() {
  mrec(NA, NA, NA)
}

norec <- function() {
  mrec(character(), integer(), integer())
}

reclist <- function(...) {
  new_rematch_records(list(...))
}

allreclist <- function(...) {
  new_rematch_allrecords(list(...))
}

re_exec_val <- function(...) {
  res <- re_exec(...)
  expect_silent(tibble::validate_tibble(res))
  res
}

re_exec_all_val <- function(...) {
  res <- re_exec_all(...)
  expect_silent(tibble::validate_tibble(res))
  res
}
