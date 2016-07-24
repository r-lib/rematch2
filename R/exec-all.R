
#' All regular expression matches and positions
#'
#' Match a regular expression to a string, and return all matches and
#' match positions, plus capture groups as well, if any.
#'
#' This function is the counterpart of \code{\link{re_exec}}, but it
#' extracts multiple matching substrings.
#'
#' The function uses \code{\link[base]{gregexpr}} to extract all matching
#' substrings for a regular expression, but it returns the results in a
#' tidy data frame. The strings of the character vector correspond
#' to the rows of the data frame. The columns correspond to capture groups
#' and the first matching (sub)string. The columns of named capture groups
#' are named accordingly, and the column of the full match if the last
#' column and it is named \code{.match}.
#'
#' Each column of the result is a list, containing lists of match records.
#' A match record is a named list, with entries \code{match}, \code{start}
#' and \code{end}; the matching (sub) string, the start and end positions
#' (using one based indexing).
#'
#' If a string has no match, then an empty list is included in the list
#' column for it.
#'
#' @inheritParams re_match_all
#' @return A data frame with list columns, see details below.
#'
#' @family tidy regular expression matching
#' @export
#' @examples
#' TODO

re_exec_all <- function(text, pattern, ...) {

  text <- as.character(text)
  stopifnot(is.character(pattern), length(pattern) == 1, !is.na(pattern))

  if (length(text) == 0) return(empty_result(text, pattern, ...))

  match <- gregexpr(pattern, text, perl = TRUE, ...)

  rec_names <- c("match", "start", "end")
  colnames <- c(attr(match[[1]], "capture.names"), ".match")
  num_groups <- length(colnames)
  non_rec <- structure(
    list(character(0), integer(0), integer(0)),
    names = rec_names
  )

  ## Non-matching strings have a rather strange special form,
  ## so we just treat them differently
  non <- vapply(match, function(m) m[1] == -1, TRUE)
  yes <- !non
  res <- replicate(length(text), list(), simplify = FALSE)
  res[non] <- list(replicate(num_groups + 1, non_rec, simplify = FALSE))
  res[yes] <- mapply(exec1, text[yes], match[yes], SIMPLIFY = FALSE)

  res <- lapply(seq_along(res[[1]]), function(i) {
    lapply(res, "[[", i)
  })

  structure(
    res,
    names = colnames,
    row.names = seq_along(text),
    class = c("tbl_df", "tbl", "data.frame")
  )
}

exec1 <- function(text1, match1) {

  start    <- as.vector(match1)
  length   <- attr(match1, "match.length")
  end      <- start + length - 1L
  matchstr <- substring(text1, start, end)
  matchrec <- list(match = matchstr, start = start, end = end)
  colnames <- c(attr(match1, "capture.names"), ".match")

  ## substring fails if the index is length zero,
  ## need to handle special case
  res <- if (is.null(attr(match1, "capture.start"))) {
    replicate(length(colnames), matchrec, simplify = FALSE)

  } else {
    gstart  <- attr(match1, "capture.start")
    glength <- attr(match1, "capture.length")
    gend    <- gstart + glength - 1L

    groupstr <- substring(text1, gstart, gend)
    dim(groupstr) <- dim(gstart)

    c(
      lapply(
        seq_len(ncol(groupstr)),
        function(i) {
          list(match = groupstr[, i], start = gstart[, i], end = gend[, i])
        }
      ),
      list(.match = matchrec)
    )
  }

  names(res) <- colnames
  res
}