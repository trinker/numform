#' Format Percentages
#'
#' \code{f_percent} - A wraf_percenter for \code{\link[numform]{f_num}} that formats
#' percent values as labeled percentages.
#'
#' @param x A vector of proportions.
#' @param digits The number of digits to use.  Defaults to 1.  Can be set
#' globally via: \code{options(numformdigits = n)} where n is the number of
#' digits beyond the decimal point to include.
#' @param less.than.replace logical.  If \code{TRUE} values lower than lowest
#' place value, specified by \code{digits}, will be replaced with a less than
#' sign followed by the \code{double} representation of the place value
#' specified by \code{digits}.  For example, if \code{digits = 0} then
#' replacement will be \code{"<1\%"} or if \code{digits = 2} then replacement will
#' be \code{"<.01\%"}.
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
#' f_pp(c(.30, 1, 1.01, .33, .222, .01))
#'
#' f_percent(c(30, 33.45, .1), digits = 0, less.than.replace = TRUE)
#' f_prop2percent(c(.30, 1, 1.01, .33, .222, .01, .0001, NA), digits = 0,
#'     less.than.replace = TRUE)
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
f_percent <- function(x, digits = getOption("numformdigits"), less.than.replace = FALSE, ...) {

    out <- f_num(x, digits = digits, s="%", ...)

    if (isTRUE(less.than.replace)){
        if (is.null(digits)) digits <- 1
        repl <- replace_less_than(digits, percent = TRUE)
        out[x < repl[['prop_cut']][1] & x >= 0] <- repl[['replacement']][1]
        out[x > repl[['prop_cut']][2] & x < 0] <- repl[['replacement']][2]
    }

    out
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
f_prop2percent <- function(x, digits = getOption("numformdigits"), less.than.replace = FALSE, ...) {

    out <- f_num(100*x, digits = digits, s="%", ...)

    if (isTRUE(less.than.replace)){
        if (is.null(digits)) digits <- 1
        repl <- replace_less_than(digits, percent = FALSE)
        out[x < repl[['prop_cut']][1] & x >= 0] <- repl[['replacement']][1]
        out[x > repl[['prop_cut']][2] & x < 0] <- repl[['replacement']][2]
    }

    out

}


#' @export
#' @include utils.R
#' @rdname f_percent
ff_prop2percent <- functionize(f_prop2percent)


#' @export
#' @include utils.R
#' @rdname f_percent
f_pp <- hijack(f_prop2percent, digits = 0)


#' @export
#' @include utils.R
#' @rdname f_percent
ff_pp <- functionize(f_pp)




replace_less_than <- function(digits = 0, prefix = c("<", ">-"), percent = FALSE, ...){

    if(percent) div <- 1 else div <- 1e2
    cut <- 1/(10^digits)
    list(prop_cut = c((cut)/div, -(cut)/div), replacement = f_percent(cut, digits = digits, prefix = prefix))

}





