#' Common operators used internally by timefitteR
#'
#' @description
#' A small collection of operators used throughout **timefitteR** for
#' piping, membership testing, and NULL-coalescing. These are provided
#' primarily for internal use.
#'
#' @details
#' \itemize{
#'   \item `\%>\%`: the magrittr pipe operator.
#'   \item `\%notin\%`: negated membership operator.
#'   \item `\%||\%`: NULL-coalescing operator.
#' }
#'
#' @name timefitteR-operators
#' @keywords internal
NULL

#' @rdname timefitteR-operators
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
`%>%` <- magrittr::`%>%`

#' @rdname timefitteR-operators
#' @param x A vector to match.
#' @param table A vector to match against.
#' @return A logical vector indicating non-membership.
`%notin%` <- function(x, table) {
  !x %in% table
}

#' @rdname timefitteR-operators
#' @param a Any object.
#' @param b Value returned when `a` is `NULL`.
#' @return `a` if not `NULL`, otherwise `b`.
`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}
