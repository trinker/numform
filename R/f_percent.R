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
#'
#' \dontrun{
#' if (!require("pacman")) install.packages("pacman")
#' pacman::p_load(ggplot2, dplyr)
#'
#' mtcars %>%
#'     count(cyl, gear) %>%
#'     group_by(cyl) %>%
#'     mutate(prop = n/sum(n)) %>%
#'     ggplot(aes(gear, prop)) +
#'         geom_bar(stat = 'identity') +
#'         facet_wrap(~cyl, ncol = 1) +
#'         scale_y_continuous(labels = ff_prop2percent(digits = 0))
#' }
f_percent <- function(x, digits = getOption("numformdigits"), ...) {

    f_num(x, digits = digits, s="%", ...)
}

#' @export
#' @include utils.R
#' @rdname f_percent
ff_percent <- functionize(f_percent)


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


#' @export
#' @include utils.R
#' @rdname f_percent
ff_prop2percent <- functionize(f_prop2percent)

