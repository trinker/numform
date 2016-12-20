#' Format Months to One Letter Abbreviation
#'
#' Format long month name or integer formats to a single capital letter.  Useful
#' for plot scales as a way to save space.  \code{f_month} is used for
#' nominal data where as  \code{f_number2month} is reserved for integers
#' 1-12.
#'
#' @param x A vector of month names or integers 1-12.
#' @return Returns a single letter month abbreviation atomic vector.
#' @export
#' @rdname f_month
#' @examples
#' f_month(month.name)
#' f_number2month(1:12)
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
#' ggplot(dat, aes(month, n)) +
#'     geom_bar(stat = 'identity') +
#'     facet_wrap(~ area)
#'
#' ggplot(dat, aes(month, n)) +
#'     geom_bar(stat = 'identity') +
#'     facet_wrap(~ area) +
#'     scale_x_discrete(labels = f_month)
#' }
f_month <- function(x) {
    toupper(gsub("(^.)(.+)", "\\1", as.character(x)))
}


#' @export
#' @rdname f_month
f_number2month <- function(x) {
    toupper(gsub("(^.)(.+)", "\\1", month.abb[x]))
}

