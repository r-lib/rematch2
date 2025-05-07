#' Match Regular Expressions with a Nicer 'API'
#'
#' A small wrapper on 'regexpr' to extract the matches and captured
#' groups from the match of a regular expression to a character vector.
#' See [re_match()].
#'
#' @importFrom tibble tibble new_tibble
"_PACKAGE"

#' Extract Regular Expression Matches Into a Data Frame
#'
#' `re_match` wraps [base::regexpr()] and returns the
#' match results in a convenient data frame. The data frame has one
#' column for each capture group if `perl=TRUE`, and one final columns
#' called `.match` for the matching (sub)string. The columns of the capture
#' groups are named if the groups themselves are named.
#'
#' @note `re_match` uses PCRE compatible regular expressions by default
#' (i.e. `perl = TRUE` in [base::regexpr()]).  You can switch
#' this off but if you do so capture groups will no longer be reported as they
#' are only supported by PCRE.
#'
#' @param text Character vector.
#' @param pattern A regular expression. See [base::regex()] for more
#'   about regular expressions.
#' @param perl logical should perl compatible regular expressions be used?
#'   Defaults to TRUE, setting to FALSE will disable capture groups.
#' @param ... Additional arguments to pass to [base::regexpr()].
#' @return A data frame of character vectors: one column per capture
#'   group, named if the group was named, and additional columns for
#'   the input text and the first matching (sub)string. Each row
#'   corresponds to an element in the `text` vector.
#'
#' @export
#' @family tidy regular expression matching
#' @examples
#' dates <- c("2016-04-20", "1977-08-08", "not a date", "2016",
#'   "76-03-02", "2012-06-30", "2015-01-21 19:58")
#' isodate <- "([0-9]{4})-([0-1][0-9])-([0-3][0-9])"
#' re_match(text = dates, pattern = isodate)
#'
#' # The same with named groups
#' isodaten <- "(?<year>[0-9]{4})-(?<month>[0-1][0-9])-(?<day>[0-3][0-9])"
#' re_match(text = dates, pattern = isodaten)

re_match <- function(text, pattern, perl = TRUE, ...) {
  stopifnot(is.character(pattern), length(pattern) == 1, !is.na(pattern))
  text <- as.character(text)

  match <- regexpr(pattern, text, perl = perl, ...)

  start <- as.vector(match)
  length <- attr(match, "match.length")
  end <- start + length - 1L

  matchstr <- substring(text, start, end)
  matchstr[start == -1] <- NA_character_

  res <- data.frame(
    stringsAsFactors = FALSE,
    .text = text,
    .match = matchstr
  )

  if (!is.null(attr(match, "capture.start"))) {
    gstart <- attr(match, "capture.start")
    glength <- attr(match, "capture.length")
    gend <- gstart + glength - 1L

    groupstr <- substring(text, gstart, gend)
    groupstr[gstart == -1] <- NA_character_
    dim(groupstr) <- dim(gstart)

    res <- cbind(groupstr, res, stringsAsFactors = FALSE)
  }

  names(res) <- c(attr(match, "capture.names"), ".text", ".match")
  class(res) <- c("tbl_df", "tbl", class(res))
  res
}
