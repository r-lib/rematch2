
#' Extract All Regular Expression Matches Into a Data Frame
#'
#' This function is a thin wrapper on the [base::gregexpr()]
#' base R function, to extract the matching (sub)strings as a data frame.
#' It extracts all matches, and potentially their capture groups as well.
#'
#' @inheritSection re_exec Tidy Data
#'
#' @note If the input text character vector has length zero,
#'   [base::regexpr()] is called instead of
#'   [base::gregexpr()], because the latter cannot extract the
#'   number and names of the capture groups in this case.
#'
#' @param ... Additional arguments to pass to
#'   [base::gregexpr()] (or [base::regexpr()] if
#'   `text` is of length zero).
#' @inheritParams re_match
#' @return A tidy data frame (see Section \dQuote{Tidy Data}).  The list columns
#'   contain character vectors with as many entries as there are matches for
#'   each input element.
#'
#' @family tidy regular expression matching
#' @export
#' @examples
#' name_rex <- paste0(
#'   "(?<first>[[:upper:]][[:lower:]]+) ",
#'   "(?<last>[[:upper:]][[:lower:]]+)"
#' )
#' notables <- c(
#'   "  Ben Franklin and Jefferson Davis",
#'   "\tMillard Fillmore"
#' )
#' re_match_all(notables, name_rex)

re_match_all <- function(text, pattern, perl=TRUE, ...) {

  text <- as.character(text)
  stopifnot(is.character(pattern), length(pattern) == 1, !is.na(pattern))

  ## Need to handle this case separately, as gregexpr effectively
  ## does not work for this.
  if (length(text) == 0) return(empty_result(text, pattern, perl=perl, ...))

  match <- gregexpr(pattern, text, perl=perl, ...)

  num_groups <- length(attr(match[[1]], "capture.names"))

  ## Non-matching strings have a rather strange special form,
  ## so we just treat them differently
  non <- vapply(match, function(m) m[1] == -1, TRUE)
  yes <- !non
  res <- replicate(length(text), list(), simplify = FALSE)
  if (any(non)) {
    res[non] <- list(replicate(num_groups + 1, character(), simplify = FALSE))
  }
  if (any(yes)) {
    res[yes] <- mapply(match1, text[yes], match[yes], SIMPLIFY = FALSE)
  }

  ## Need to assemble the final data frame "manually".
  ## There is apparently no function for this. rbind() is almost
  ## good, but simplifies to a matrix if the dimensions allow it....
  res <- lapply(seq_along(res[[1]]), function(i) {
    lapply(res, "[[", i)
  })

  res <- structure(
    res,
    names = c(attr(match[[1]], "capture.names"), ".match"),
    row.names = seq_along(text),
    class = c("tbl_df", "tbl", "data.frame")
  )

  res$.text <- text
  nc <- ncol(res)
  res[, c(seq_len(nc - 2), nc, nc - 1)]
}

match1 <- function(text1, match1) {

  matchstr <- substring(
    text1,
    match1,
    match1 + attr(match1, "match.length") - 1L
  )

  ## substring fails if the index is length zero,
  ## need to handle special case
  if (is.null(attr(match1, "capture.start"))) {
    list(.match = matchstr)

  } else {
    gstart  <- attr(match1, "capture.start")
    glength <- attr(match1, "capture.length")
    gend    <- gstart + glength - 1L

    groupstr <- substring(text1, gstart, gend)
    dim(groupstr) <- dim(gstart)

    c(lapply(seq_len(ncol(groupstr)), function(i) groupstr[, i]),
      list(.match = matchstr)
      )
  }
}
