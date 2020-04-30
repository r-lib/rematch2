#' Extract Data From First Regular Expression Match Into a Data Frame
#'
#' @description
#'
#' Match a regular expression to a string, and return matches, match positions,
#' and capture groups.  This function is like its
#' \code{\link[=re_match]{match}} counterpart, except it returns match/capture
#' group start and end positions in addition to the matched values.
#'
#' @section Tidy Data:
#'
#' The return value is a tidy data frame where each row
#' corresponds to an element of the input character vector \code{text}.  The
#' values from \code{text} appear for reference in the \code{.text} character
#' column.  All other columns are list columns containing the match data.  The
#' \code{.match} column contains the match information for full regular
#' expression matches while other columns correspond to capture groups if there
#' are any, and PCRE matches are enabled with \code{perl = TRUE} (this is on by
#' default).  If capture groups are named the corresponding columns will bear
#' those names.
#'
#' Each match data column list contains match records, one for each element in
#' \code{text}.  A match record is a named list, with entries \code{match},
#' \code{start} and \code{end} that are respectively the matching (sub) string,
#' the start, and the end positions (using one based indexing).
#'
#' @section Extracting Match Data:
#'
#' To make it easier to extract matching substrings or positions, a special
#' \code{$} operator is defined on match columns, both for the \code{.match}
#' column and the columns corresponding to the capture groups.  See examples
#' below.
#'
#' @inheritParams re_match_all
#' @seealso \code{\link[base]{regexpr}}, which this function wraps
#' @param x Object returned by \code{re_exec} or \code{re_exec_all}.
#' @param name \code{match}, \code{start} or \code{end}.
#' @return A tidy data frame (see Section \dQuote{Tidy Data}).  Match record
#'   entries are one length vectors that are set to NA if there is no match.
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
#' # Match first occurrence
#' pos <- re_exec(notables, name_rex)
#' pos
#'
#' # Custom $ to extract matches and positions
#' pos$first$match
#' pos$first$start
#' pos$first$end

re_exec <- function(text, pattern, perl=TRUE, ...) {

  stopifnot(is.character(pattern), length(pattern) == 1, !is.na(pattern))
  text <- as.character(text)

  match <- regexpr(pattern, text, perl = perl, ...)

  start  <- as.vector(match)
  length <- attr(match, "match.length")
  end    <- start + length - 1L

  matchstr <- substring(text, start, end)
  matchstr[ start == -1 ] <- NA_character_
  end     [ start == -1 ] <- NA_integer_
  start   [ start == -1 ] <- NA_integer_

  names <- c("match", "start", "end")

  matchlist <- new_rematch_records(
    lapply(seq_along(text), function(i) {
      structure(list(matchstr[i], start[i], end[i]), names = names)
    })
  )

  res <- new_tibble(
    list(text, matchlist),
    names = c(".text", ".match"),
    nrow = length(text)
  )

  if (!is.null(attr(match, "capture.start"))) {

    gstart  <- unname(attr(match, "capture.start"))
    glength <- unname(attr(match, "capture.length"))
    gend    <- gstart + glength - 1L

    groupstr <- substring(text, gstart, gend)
    groupstr[ gstart == -1 ] <- NA_character_
    gend    [ gstart == -1 ] <- NA_integer_
    gstart  [ gstart == -1 ] <- NA_integer_
    dim(groupstr) <- dim(gstart)

    grouplists <- lapply(
      seq_along(attr(match, "capture.names")),
      function(g) {
        new_rematch_records(
          lapply(seq_along(text), function(i) {
            structure(
              list(groupstr[i, g], gstart[i, g], gend[i, g]),
              names = names
            )
          })
        )
      }
    )

    res <- new_tibble(
      c(grouplists, res),
      names = c(attr(match, "capture.names"), ".text", ".match"),
      nrow = length(res[[1]])
    )
  }

  res
}

new_rematch_records <- function(x) {
  structure(x, class = c("rematch_records", "list"))
}
