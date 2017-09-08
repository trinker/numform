#' Abbreviate Strings
#'
#' A wrapper for \code{\link[base]{abbreviate}} for abbreviating strings.
#'
#' @param x A vector of text strings.
#' @param length The minimum length of the abbreviations.
#' @param \ldots Other arguments passed to \code{\link[base]{abbreviate}}.
#' @return Returns a string vector with strings abbreviated.
#' @rdname f_abbreviation
#' @export
#' @seealso \code{\link[base]{abbreviate}}
#' @examples
#' f_abbreviation(state.name)
#' f_abbreviation('Cool Variable')
f_abbreviation <- function (x, length = 5, ...) {

    abbreviate(x, minlength = length, named = FALSE, ...)

}


#' @export
#' @include utils.R
#' @rdname f_abbreviation
ff_abbreviation <- functionize(f_abbreviation)







