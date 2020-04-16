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
#' @rdname fv_percent
#' @examples
#' fv_percent(1:4)
#' fv_percent(sample(1:100, 20))
#' \dontrun{
#' library(tidyverse)
#'
#' mtcars %>%
#'     count(cyl, gear) %>%
#'     group_by(cyl) %>%
#'     mutate(perc = fv_percent(n, digits = 0))
#'
#' mtcars %>%
#'     count(cyl, gear) %>%
#'     group_by(cyl) %>%
#'     mutate(perc = fv_percent(n, digits = 0)) %>%
#'     ggplot(aes(gear, n)) +
#'         geom_bar(stat = 'identity') +
#'         facet_wrap(~cyl, ncol = 1) +
#'         geom_text(aes(y = n + 1, label = perc))
#' }
fv_percent <- function (x, digits = getOption("numformdigits"), ...) {
    f_prop2percent(x/sum(x, na.rm = TRUE), digits = digits, ...)
}



#' @export
#' @include utils.R
#' @rdname fv_percent
ffv_percent <- functionize(fv_percent)

