#' Extract Data From All Regular Expression Matches Into a Data Frame
#'
#' @inherit re_exec
#'
#' @description
#'
#' Match a regular expression to a string, and return matches, match positions,
#' and capture groups.  This function is like its
#' \code{\link[=re_match_all]{match}} counterpart, except it returns
#' match/capture group start and end positions in addition to the matched
#' values.
#'
#' @seealso \code{\link[base]{gregexpr}}, which this function wraps
#' @return A tidy data frame (see Section \dQuote{Tidy Data}).  The entries
#'   within the match records within the list columns will be one vectors
#'   as long as there are matches for the corresponding text element.
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
#' # All occurrences
#' allpos <- re_exec_all(notables, name_rex)
#' allpos
#'
#' # Custom $ to extract matches and positions
#' allpos$first$match
#' allpos$first$start
#' allpos$first$end
re_exec_all <- function(text, pattern, perl = TRUE, ...) {

  text <- as.character(text)
  stopifnot(is.character(pattern), length(pattern) == 1, !is.na(pattern))

  if (length(text) == 0) {
    res <- empty_result(text, pattern, perl = perl, ...)
    for (i in seq_along(res)) {
      if (is.list(res[[i]])) {
        res[[i]] <- new_rematch_allrecords(res[[i]])
      }
    }
    return(res)
  }

  match <- gregexpr(pattern, text, perl = perl, ...)

  rec_names <- c("match", "start", "end")
  colnames <- c(attr(match[[1]], "capture.names"), ".match")
  num_groups <- length(colnames) - 1L
  non_rec <- structure(
    list(character(0), integer(0), integer(0)),
    names = rec_names
  )

  ## Non-matching strings have a rather strange special form,
  ## so we just treat them differently
  non <- vapply(match, function(m) m[1] == -1, TRUE)
  yes <- !non
  res <- replicate(length(text), list(), simplify = FALSE)
  if (any(non)) {
    res[non] <- list(replicate(num_groups + 1, non_rec, simplify = FALSE))
  }
  if (any(yes)) {
    res[yes] <- mapply(exec1, text[yes], match[yes], SIMPLIFY = FALSE)
  }

  res <- lapply(seq_along(res[[1]]), function(i) {
    new_rematch_allrecords(lapply(res, "[[", i))
  })

  res <- structure(
    res,
    names = colnames,
    row.names = seq_along(text),
    class = c("tbl_df", "tbl", "data.frame")
  )

  res$.text <- text
  nc <- ncol(res)
  res[, c(seq_len(nc - 2), nc, nc - 1)]
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
    gstart  <- unname(attr(match1, "capture.start"))
    glength <- unname(attr(match1, "capture.length"))
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

  res
}
