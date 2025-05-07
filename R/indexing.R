#' @rdname re_exec
#' @export $.rematch_records
#' @export

`$.rematch_records` <- function(x, name) {
  if (!name %in% c("match", "start", "end")) {
    stop("'$' match selector must refer to 'match', 'start' or 'end'")
  }

  vapply(x, "[[", name, FUN.VALUE = if (name == "match") "" else 1L)
}

#' @rdname re_exec
#' @export $.rematch_allrecords
#' @export

`$.rematch_allrecords` <- function(x, name) {
  if (!name %in% c("match", "start", "end")) {
    stop("'$' match selector must refer to 'match', 'start' or 'end'")
  }

  lapply(x, "[[", name)
}
