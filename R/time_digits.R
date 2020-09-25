#' Compute Digits Needed for Quarter Hour Time Vector
#'
#' This tool computes the minimum number of digits required for a vector of times.
#' The defaults of the tool assumes your time is rounded to within the quarter
#' hour.
#'
#' @param x A numeric vector of times rounded tot he nearest quarter hour.
#' @param \ldots ignored
#' @return Returns integer 0-2
#' @export
#' @examples
#' time_digits(c(.5, .25, 6))
#' time_digits(c(.5, 3.5, 6))
#' time_digits(c(5, 25, 6))
#'
#' x <- c(.5, .25, 6)
#' numform::f_pad_left(numform::f_num(x, digits = numform::time_digits(x)))
#'
#' lapply(
#'     list(quarter = c(.5, .25, 6), half = c(.5, 3.5, 6), hour = c(5, 25, 6)),
#'     function(x) {numform::f_pad_left(numform::f_num(x, digits = numform::time_digits(x)))}
#' )
time_digits <- function(x, ...){

    digits = 0:2
    values = c(1, .5, .25)

    stopifnot(length(digits) == length(values))
    x <- na_omit(x)
    min((digits)[sapply(values, function(v){
          all(sapply(x, `%%`, v) == 0)
    })])

}


