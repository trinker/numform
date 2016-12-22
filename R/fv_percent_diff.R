#' Percent Difference
#'
#' Convert a vector of values to percent differences.
#'
#' @param x A numeric vector.
#' @param digits The number of digits to use.  Defaults to 1.  Can be set
#' globally via: \code{options(numformdigits = n)} where n is the number of
#' digits beyond the decimal point to include.
#' @param \ldots Other arguments passed to \code{\link[numform]{f_prop2percent}}.
#' @return Returns a string of publication ready relative percent differences.
#' @export
#' @examples
#' set.seed(10)
#' x <- sample(1:10)
#'
#' data.frame(
#'     original = x,
#'     perc_change = fv_percent_diff(x)
#' )
#'
#' \dontrun{
#' library(dplyr)
#'
#' CO2 %>%
#'     group_by(Plant) %>%
#'     mutate(
#'         `Percent` = fv_percent(conc),
#'         `Percent Diff` = fv_percent_diff(conc)
#'     ) %>%
#'     print(n=Inf)
#' }
fv_percent_diff <- function(x, digits = getOption("numformdigits"), ...){
    f_prop2percent(c(0, (x[-1] - x[-length(x)])/x[-length(x)]), digits = digits, ...)
}
