#' Format Years
#'
#' Format 4 digit integer, date, or POSIXlt formats to 2 or 4 digit years.
#'
#' @param x A vector of 4 digits integers, dates, or POSIXlt.
#' @param digits Either 2 or 4 for the number of digits to make the year.
#' @param \ldots ignored.
#' @return Returns a vector of two or four digit years.
#' @export
#' @rdname f_year
#' @examples
#' f_year(as.Date(paste0(1998:2016, '-12-12')))
#' f_year(c(NA, 1998:2016, 21345))
#' \dontrun{
#' library(tidyverse)
#'
#' dat <- data_frame(
#'     year = 1998:2016,
#'     year2 = as.POSIXct(sample(seq_len(1e4), 12), origin = '1970-01-01') +
#'         (365 * 24 * 3600 * seq_len(19)),
#'     val = sample(1:20, length(year), TRUE)
#' ) %>%
#'     mutate(prop = val/sum(val))
#'
#' dat %>%
#'     ggplot(aes(year, prop)) +
#'         geom_line() +
#'         scale_x_continuous(labels = ff_year(digits = 2), breaks = 1998:2016) +
#'         scale_y_continuous(labels = ff_prop2percent(digits = 0))
#'
#' dat %>%
#'     ggplot(aes(year2, prop)) +
#'         geom_line() +
#'         scale_x_time(labels = ff_year(digits = 2), breaks = dat$year2) +
#'         scale_y_continuous(labels = ff_prop2percent(digits = 0))
#' }
f_year <- function(x, digits = 2, ...) {
    UseMethod('f_year')
}




#' @export
#' @rdname f_year
#' @method f_year numeric
f_year.numeric <- function(x, digits = 2, ...) {

    x[!grepl('^\\d{4}$', as.character(x))] <- NA

    switch(ifelse(digits == 2, 'two', ifelse(digits == 4, 'four', 'three')),
        two = gsub('(^\\d{2})(\\d{2}$)', '\\2', as.character(as.integer(x))),
        four = gsub('(^\\d{2})(\\d{2}$)', '\\1\\2', as.character(as.integer(x))),
        stop('`digits` must be either 2 or 4')
    )
}




#' @export
#' @rdname f_year
#' @method f_year Date
f_year.Date <- function(x, digits = 2, ...) {

    f_year(as.integer(format(x, '%Y')), digits = digits)
}


#' @export
#' @rdname f_year
#' @method f_year POSIXt
f_year.POSIXt <- function(x, digits = 2, ...) {
    f_year.Date(x, digits = digits)
}

#' @export
#' @rdname f_year
#' @method f_year hms
f_year.hms <- function(x, digits = 2, ...) {
    f_year(as.integer(format(as.POSIXct(x), '%Y')), digits = digits)
}



#' @export
#' @rdname f_year
ff_year <- function(digits = 2, ...) {
    function(x) {f_year(x, digits = digits)}
}

