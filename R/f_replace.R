#' Replace Characters in Strings
#'
#' A wrapper for \code{\link[base]{gsub}} for replacing substrings that is
#' useful for \pkg{ggplot2} scales.  Useful for taking field names like
#' 'Cool_Variable' and turning it into 'Cool Variable'.
#'
#' @param x A vector of text strings.
#' @param pattern A character string defining search patterns.
#' @param replacement A character string defining replacement patterns.
#' @param \ldots Other arguments passed to \code{\link[base]{gsub}}.
#' @return Returns a string vector with characters replaced.
#' @rdname f_replace
#' @export
#' @seealso \code{\link[base]{strwrap}}
#' @examples
#' f_replace('Cool_Variable')
#' f_title(f_replace('cool_variable'))
#' f_replace('Cool_Variable', pattern = '([A-Z])', replacement = '\\L\\1')
#' cat(f_replace('really long label names are the pits',
#'     pattern = '\\s', replace = '\n'))
f_replace <- function (x, pattern = '_', replacement = ' ', ...) {

    gsub(pattern, replacement, x, perl = TRUE, ...)

}


#' @export
#' @include utils.R
#' @rdname f_replace
ff_replace <- functionize(f_replace)


