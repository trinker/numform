#' Format Digits
#'
#' Remove leading zeros and standardize number of digits.  A workhorse for the
#' \pkg{numform} package.
#'
#' @param x A vector of numbers (or string equivalents).
#' @param digits The number of digits to use.  Defaults to 1.  Can be set
#' globally via: \code{options(numformdigits = n)} where n is the number of
#' digits beyond the decimal point to include.
#' @param p A string to paste at the begining of the output from \code{f_num}.
#' @param s A string to paste at the end of the output from \code{f_num}.
#' @return Returns a string of publication ready digits.
#' @export
#' @examples
#' f_num(c(0.0, 0, .2, -00.02, 1.122222, pi))
#' f_num(rnorm(10))
#' f_num(rnorm(20, 100, 200), 0)
#' f_num(c("-0.23", "0", ".23"))
#'
#' ## Percents
#' f_num(c(30, 33.45, .1), 3, s="%")
#'
#' ## Money
#' f_num(c(30, 33.45, .1), 2, p="$")
#'
#' ## Units
#' f_num(c(30, 33.45, .1), 2, s=" in.<sup>2</sup>")
#' f_num(c(30, 33.45, .1), 2, p="&Chi;<sup>2</sup>=")
f_num <- function(x, digits = getOption("numformdigits"), p, s) {

    if (is.null(digits)) digits <- 1

    if(length(digits) > 1) {
        digits <- digits[1]
        warning("Using only digits[1]")
    }

    x <- round(as.numeric(x), digits)

    if (digits > 0) x <- sprintf(paste0("%.", digits, "f"), x)
    out <- gsub("^0(?=\\.)|(?<=-)0", "", x, perl=TRUE)
    out[out == "NA"] <- NA
    if (!missing(p)) out <- paste0(p, out)
    if (!missing(s)) out <- paste0(out, s)
    out
}

