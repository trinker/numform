#' Format Percentages
#'
#' \code{f_percent} - A wraf_percenter for \code{\link[numform]{f_num}} that formats
#' percent values as labeled percentages.
#'
#' @param x A vector of proportions.
#' @param digits The number of digits to use.  Defaults to 1.  Can be set
#' globally via: \code{options(numformdigits = n)} where n is the number of
#' digits beyond the decimal point to include.
#' @param \ldots Other values passed to \code{\link[numform]{f_num}}.
#' @return Returns a string of publication ready digits.
#' @export
#' @rdname f_percent
#' @seealso \code{\link[numform]{f_num}}
#' @examples
#' f_percent(c(30, 33.45, .1))
#' f_percent(c(30, 33.45, .1), 1)
#' f_percent(c(0.0, 0, .2, -00.02, 1.122222, pi))
#' f_prop2percent(c(.30, 1, 1.01, .33, .222, .01))
f_percent <- function(x, digits = getOption("numformdigits"), ...) {

    f_num(x, digits = digits, s="%", ...)
}


#' Format Percentages
#'
#' \code{f_prop2percent} - A wrapper for \code{\link[numform]{f_num}} that formats
#' proportions as labeled percentages.
#'
#' @rdname f_percent
#' @export
f_prop2percent <- function(x, digits = getOption("numformdigits"), ...) {

    f_num(100*x, digits = digits, s="%", ...)
}
