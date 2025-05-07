empty_result <- function(text, pattern, perl = TRUE, ...) {
  match <- regexpr(pattern, text, perl = perl, ...)
  num_groups <- length(attr(match, "capture.names"))
  structure(
    c(
      replicate(num_groups, list(), simplify = FALSE),
      list(character()),
      list(list())
    ),
    names = c(attr(match, "capture.names"), ".text", ".match"),
    row.names = integer(0),
    class = c("tbl_df", "tbl", "data.frame")
  )
}
