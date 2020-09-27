#' Rounding
#'
#' \code{round2} - By default R's \code{round} function uses the 'round half to
#' even' method.  This function (taken from https://stackoverflow.com/a/12688836/1000343)
#' rounds half up.
#'
#' @param x A vector of digits.
#' @param digits The number of decimal places to round to.
#' @param \ldots ignored.
#' @return \code{round2} - Returns numeric vector half rounded up.
#' @references https://stackoverflow.com/a/12688836/1000343 \cr
#' https://stackoverflow.com/a/8665247/1000343
#' @export
#' @rdname round2
#' @examples
#' data.frame(
#'     orig = .5 + (0:8),
#'     round = round(.5 + (0:8)),
#'     round2 = round2(.5 + (0:8))
#' )
#'
#' round_any(c(.123, 1.234, 4, 4.715), .5)
#' round_any(c(.123, 1.234, 4, 4.715), .25)
round2 <- function(x, digits = 0, ...) {
    posneg <- sign(x)
    z <- abs(x) * 10^digits
    z <- z + 0.5 + sqrt(.Machine$double.eps)
    z <- trunc(z)
    z <- z/10^digits
    z * posneg
}


#' Rounding
#'
#' \code{round_any} - This tooling lets you round to fractional values, not
#' just whole numbers.  Code adapted from https://stackoverflow.com/a/8665247/1000343.
#'
#' @param accuracy Number to round to.
#' @param f A function to round (e.g., \code{round}, \code{ceiling}, \code{floor}).
#' efaults to \code{round2}.
#' @param \ldots ignored.
#' @return \code{round_any} - Returns a numeric vector or rounded fractional values.
#' @author Kohske Takahashi
#' @export
#' @rdname round2
round_any <- function(x, accuracy, f = round2, ...){
    f(x/accuracy)*accuracy
}

