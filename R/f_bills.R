#' Abbreviate Numbers
#'
#' Use the K (thousands), M (millions), and B (billions) with abbreviated numbers.
#'
#' @param x A vector of large numbers.
#' @param relative A factor relative to the current \code{digits} being rounded.
#' For example \code{relative = -1} moves one to the left while
#' \code{relative = 1} moves one to the right.
#' @param digits The number of digits to round to.  Actual \code{digits}
#' calculated as \code{digits} +  \code{relative}.
#' @return Returns an abbreviated vector of numbers.
#' @export
#' @rdname number_abbreviation
#' @examples
#' f_thous(1234)
#' f_thous(12345)
#' f_thous(123456)
#' f_mills(1234567)
#' f_mills(12345678)
#' f_mills(123456789)
#' f_bills(1234567891)
#' f_bills(12345678912)
#' f_bills(123456789123)
#'
#' f_bills(123456789123, -1) # round to tens
#' f_bills(123456789123, -2) # round to hundreds
#' f_bills(123456789123, +1) # round to tenths
#' f_bills(123456789123, +2) # round to hundreths
#' \dontrun{
#' if (!require("pacman")) install.packages("pacman")
#' pacman::p_load(dplyr)
#'
#' f_bills(123456789123, -2) %>%
#'     f_prefix("$")
#' }
f_bills <- function(x, relative = 0, digits = -9) {

    digits <- digits + relative
    x <- gsub("^0.", ".", paste0(round(x, digits)/1000000000, "B"))
    ifelse(x == '.', '0B', x)

}


#' @export
#' @rdname number_abbreviation
f_mills <- function(x, relative = 0, digits = -6) {

    digits <- digits + relative

    x <- gsub("^0.", ".", paste0(round(x, digits)/1000000, "M"))

    digit_warn(x)
    ifelse(x == '.', '0M', x)

}

#' @export
#' @rdname number_abbreviation
f_thous <- function(x, relative = 0, digits = -3) {

    digits <- digits + relative

    x <- gsub("^0.", ".", paste0(round(x, digits)/1000, "K"))

    digit_warn(x)
    ifelse(x == '.', '0K', x)
}





digit_check <- function(x, digits = 3){
    any(nchar(gsub("(^\\d+)(\\.|[KBM])(.*$)", "\\1", x)) > digits)
}

digit_warn <- function(x, next_ver = "f_mill", digits = 3){
    if (digit_check(x)) {
        warning(paste0(
            "Detected one or more elements with a larger denomination.\n  Consider using `",
            next_ver,
            "` function instead."
        ))
    }
}





