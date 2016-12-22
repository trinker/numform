#' Convert a Numeric Vector to Percentages
#'
#' Converts a numeric vector into a vector of relative percentages.
#'
#' @param x A numeric vector.
#' @param digits The number of digits to use.  Defaults to 1.  Can be set
#' globally via: \code{options(numformdigits = n)} where n is the number of
#' digits beyond the decimal point to include.
#' @param \ldots Other arguments passed to \code{\link[numform]{f_prop2percent}}.
#' @return Returns a string of publication ready relative percentages.
#' @export
#' @examples
#' fv_percent(1:4)
#' fv_percent(sample(1:100, 20))
fv_percent <- function (x, digits = getOption("numformdigits"), ...) {
    f_prop2percent(x/sum(x, na.rm = TRUE), digits = digits, ...)
}





