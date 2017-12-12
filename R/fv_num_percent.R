#' Convert a Numeric Vector to Number and Parenthetical Percentages
#'
#' Convert a vector of numbers into a vector of strings with the number
#' followed by the relative percentage in parenthesis.
#'
#' @param x A numeric vector.
#' @param x_digits The number of digits to round the x vector.
#' @param y_digits The number of digits to round the y vector.
#' @param sep The separator between the first number and the leading parenthesis.
#' @param comma logical.  If \code{TRUE} the leading number is comma separated.
#' @param \ldots ignored.
#' @rdname fv_num_percent
#' @return Returns a vector of parenthesis combined strings using vector x
#' followed by the value as a relative percent in parenthesis.
#' @export
#' @examples
#' fv_num_percent(1:10)
#' fv_num_percent(1:10, x_digits = 0, y_digits = 1, sep = " ")
fv_num_percent <- function(x, x_digits = getOption("numformdigits"),
    y_digits = x_digits, sep = "", comma = TRUE, ...) {
    if(comma) cm <- f_comma else cm <- c
    paste(
        cm(f_num(x, x_digits)),
        paste0("(", f_prop2percent(x/sum(x, na.rm = TRUE), y_digits), ")"),
        sep = sep
    )
}


#' @export
#' @include utils.R
#' @rdname fv_num_percent
ffv_num_percent <- functionize(fv_num_percent)
