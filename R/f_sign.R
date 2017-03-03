#' Format Numeric Signs
#'
#' \code{f_sign} - Formats numeric values to just their sign
#' ('-' == < 0, '+' == > 0, or '' == 0).
#'
#' @param x A vector of values.
#' @param zero An value to insert in for zero values.
#' @param \ldots ignored.
#' @return Returns a string of signs.
#' @export
#' @rdname f_sign
#' @seealso \code{\link[numform]{f_num}}
#' @examples
#' f_sign(c(-10, 0, 10))
#' f_sign(c(-10, 0, 10), 0)
f_sign <- function(x, zero = NULL, ...) {

    out <- gsub("[01]", "", gsub("^1$", "+", as.character(sign(x))))
    if (!is.null(zero)) out[out == ""] <- zero
    out

}


#' @export
#' @include utils.R
#' @rdname f_sign
ff_sign <- functionize(f_sign)

