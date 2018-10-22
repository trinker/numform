#' Percent Difference
#'
#' \code{fv_percent_lead} - Convert a vector of values to percent relative to
#' prior value in the vector (i.e., T2/T1).
#'
#' @param x A numeric vector.
#' @param digits The number of digits to use.  Defaults to 1.  Can be set
#' globally via: \code{options(numformdigits = n)} where n is the number of
#' digits beyond the decimal point to include.
#' @param \ldots Other arguments passed to \code{\link[numform]{f_prop2percent}}.
#' @return Returns a string of publication ready relative percent differences.
#' @rdname fv_percent_lead
#' @export
#' @examples
#' set.seed(10)
#' x <- sample(1:10)
#'
#' data.frame(
#'     original = x,
#'     perc_change = fv_percent_lead(x)
#' )
#'
#' \dontrun{
#' library(dplyr)
#'
#' CO2 %>%
#'     group_by(Plant) %>%
#'     mutate(
#'         `Percent` = fv_percent(conc),
#'         `Percent Diff` = fv_percent_diff(conc),
#'         `Percent Relative` = fv_percent_lead(conc)
#'     ) %>%
#'     print(n=Inf)
#'
#' CO2 %>%
#'     group_by(Type, Treatment) %>%
#'     mutate(
#'         `Percent` = fv_percent(conc),
#'         `Percent Diff` = fv_percent_diff(conc),
#'         `Percent Relative` = fv_percent_lead(conc)
#'     ) %>%
#'     print(n=Inf)
#' }
fv_percent_lead <- function(x, digits = getOption("numformdigits"), ...){
    f_prop2percent(c(0, x[-1]/x[-length(x)]), digits = digits, ...)
}


#' \code{fv_percent_lead_fixed_relative} - Similar to \code{fv_percent_lead} but
#' the comparison year is a constant rather than the prior year.  This is
#' particularly useful for seeing changes relative to time 1.
#' @param fixed.relative The position of the element to be used for comparison.
#' Default is the first element.
#' @export
#' @include utils.R
#' @rdname fv_percent_lead
fv_percent_lead_fixed_relative <- function(x, fixed.relative = 1, digits = getOption("numformdigits"), ...){
    f_prop2percent(c(0, x[-1])/x[fixed.relative], digits = digits, ...)
}



#' @export
#' @include utils.R
#' @rdname fv_percent_lead
ffv_percent_lead <- functionize(fv_percent_lead)


#' @export
#' @include utils.R
#' @rdname fv_percent_lead
ffv_percent_lead_fixed_relative <- functionize(fv_percent_lead_fixed_relative)




