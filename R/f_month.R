#' Format Months to One Letter Abbreviation
#'
#' Format long month name, integer, or date formats to a single capital letter.
#' Useful for plot scales as a way to save space.
#'
#' @param x A vector of month names, integers 1-12, or dates.
#' @param \ldots ignored.
#' @return Returns a single letter month abbreviation atomic vector.
#' @export
#' @rdname f_month
#' @examples
#' f_month(month.name)
#'
#' f_month(1:12)
#'
#' dates <- seq(as.Date("2000/1/1"), by = "month", length.out = 12)
#' f_month(dates)
#' \dontrun{
#' if (!require("pacman")) install.packages("pacman")
#' pacman::p_load(tidyverse)
#'
#' set.seed(10)
#' dat <- data_frame(
#'     month = sample(month.name, 1000, TRUE),
#'     area =  sample(LETTERS[1:5], 1000, TRUE)
#' ) %>%
#'     count(month, area) %>%
#'     ungroup() %>%
#'     mutate(month = factor(month, levels = month.name))
#'
#' ## without date formatting
#' ggplot(dat, aes(month, n)) +
#'     geom_bar(stat = 'identity') +
#'     facet_wrap(~ area)
#'
#' ## with date formatting
#' ggplot(dat, aes(month, n)) +
#'     geom_bar(stat = 'identity') +
#'     facet_wrap(~ area) +
#'     scale_x_discrete(labels = f_month)
#' }
f_month <- function(x, ...) {
    UseMethod('f_month')
}



#' @export
#' @rdname f_month
#' @method f_month default
f_month.default <- function(x, ...) {
    toupper(gsub("(^.)(.+)", "\\1", as.character(x)))
}

#' @export
#' @rdname f_month
#' @method f_month numeric
f_month.numeric <- function(x, ...) {
    toupper(gsub("(^.)(.+)", "\\1", month.abb[x]))
}


#' @export
#' @rdname f_month
#' @method f_month Date
f_month.Date <- function(x, ...) {
    toupper(gsub("(^.)(.+)", "\\1", as.character(format(x, "%b"))))
}


#' @export
#' @rdname f_month
#' @method f_month POSIXlt
f_month.POSIXlt <- function(x, ...) {
    toupper(gsub("(^.)(.+)", "\\1", as.character(format(x, "%b"))))
}
