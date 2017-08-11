#' Format Numeric Signs
#'
#' \code{f_sign} - Formats numeric values to just their sign
#' ('-' == < 0, '+' == > 0, or '' == 0).
#'
#' @param x A vector of values.
#' @param positive A string/value to insert in for positive values.
#' @param negative A string/value to insert in for negative values.
#' @param zero A string/value to insert in for zero values.
#' @param \ldots ignored.
#' @return Returns a string of signs.
#' @export
#' @rdname f_sign
#' @seealso \code{\link[numform]{f_num}}
#' @examples
#' f_sign(c(-10, 0, 10))
#' f_sign(c(-10, 0, 10), zero = 0)
#' ## web based
#' f_sign(c(-10, 0, 10), '<b>+</b>', '<b>&ndash;</b>')
f_sign <- function(x, positive = '+', negative = '-', zero = '', ...) {

    out <- gsub("[01]", "", gsub("^1$", '+', as.character(sign(x))))
    out[out == "+"] <- positive
    out[out == "-"] <- negative
    out[out == ""] <- zero
    out

}


#' @export
#' @include utils.R
#' @rdname f_sign
ff_sign <- functionize(f_sign)

