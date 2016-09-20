#' Format Dollars
#'
#' \code{f_dollar} - A wrapper for \code{\link[numform]{f_num}} that formats
#' dollar values as labeled dollars.
#'
#' @param x A vector of values.
#' @param leading_zero logical.  If \code{TRUE} a leading zero will be added to
#' values from 0 up to 1.
#' @param digits The number of digits to use.  Defaults to 2.  Can be set
#' globally via: \code{options(numformdigits = n)} where n is the number of
#' digits beyond the decimal point to include.
#' @param \ldots Other values passed to \code{\link[numform]{f_num}}.
#' @return Returns a string of publication ready digits.
#' @export
#' @rdname f_dollar
#' @seealso \code{\link[numform]{f_num}}
#' @examples
#' f_dollar(c(30, 33.45, .1))
#' \dontrun{
#' library(dplyr)
#' f_dollar(c(0.0, 0, .2, -00.02, 1122222, pi)) %>% f_comma()
#' }
f_dollar <- function(x, leading_zero = TRUE, digits = 2, ...) {

    out <- f_num(x, digits = digits, p="$", ...)
    gsub("^\\$\\.", "$0.", out)

}

