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
#' @param prefix A string to append to the front of elements.
#' @param \ldots ignored.
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
#' pacman::p_load(tidyverse, magrittr)
#'
#' f_bills(123456789123, -2) %>%
#'     f_prefix("$")
#'
#'
#' data_frame(
#'     revenue = rnorm(100, 500000, 50000),
#'     deals = sample(20:50, 100, TRUE)
#' ) %>%
#'     mutate(
#'         dollar = f_dollar(revenue, digits = -3),
#'         thous = f_thous(revenue),
#'         thous_dollars = f_thous(revenue, prefix = '$')
#'     ) %T>%
#'     print() %>%
#'     ggplot(aes(deals, revenue)) +
#'         geom_point() +
#'         geom_smooth() +
#'         scale_y_continuous(label = function(x) x %>% f_thous(x, prefix = '$') )
#'
#' data_frame(
#'     revenue = rnorm(10000, 500000, 50000),
#'     date = sample(seq(as.Date('1999/01/01'), as.Date('2000/01/01'), by="day"), 10000, TRUE),
#'     site = sample(paste("Site", 1:5), 10000, TRUE)
#' ) %>%
#'     mutate(
#'         dollar = f_dollar(revenue, digits = -3),
#'         thous = f_thous(revenue),
#'         thous_dollars = f_thous(revenue, prefix = '$'),
#'         abb_month = f_month(date),
#'         abb_week = factor(f_weekday(date, distinct = TRUE), levels = c('Su', 'M', 'T', 'W', 'Th', 'F', 'S'))
#'     ) %T>%
#'     print() %>%
#'     ggplot(aes(abb_week, revenue)) +
#'         geom_jitter(width = .2, height = 0, alpha = .2) +
#'         scale_y_continuous(label = function(x) x %>% f_thous(x, prefix = '$'))+
#'         facet_wrap(~site)
#' }
f_bills <- function(x, relative = 0, digits = -9, prefix = "", ...) {

    digits <- digits + relative
    x <- gsub("^0.", ".", paste0(round(x, digits)/1000000000, "B"))
    paste0(prefix, ifelse(x == '.', '0B', x))

}


#' @export
#' @rdname number_abbreviation
f_mills <- function(x, relative = 0, digits = -6, prefix = "", ...) {

    digits <- digits + relative

    x <- gsub("^0.", ".", paste0(round(x, digits)/1000000, "M"))

    digit_warn(x)
    paste0(prefix, ifelse(x == '.', '0M', x))

}

#' @export
#' @rdname number_abbreviation
f_thous <- function(x, relative = 0, digits = -3, prefix = "", ...) {

    digits <- digits + relative

    x <- gsub("^0.", ".", paste0(round(x, digits)/1000, "K"))

    digit_warn(x)
    paste0(prefix, ifelse(x == '.', '0K', x))
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





