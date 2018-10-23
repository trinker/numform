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
#' set.seed(11)
#' data_frame(
#'     date = sample(seq(as.Date("1990/1/1"), by = "day", length.out = 2e4), 12)
#' ) %>%
#'     mutate(
#'         year_4 = f_year(date, 2),
#'         year_2 = f_year(date, 4),
#'         quarter = f_quarter(date),
#'         month_name = f_month_name(date) %>%
#'             as_factor(),
#'         month_abbreviation = f_month_abbreviation(date) %>%
#'             as_factor(),
#'         month_short = f_month(date),
#'         weekday_name = f_weekday_name(date),
#'         weekday_abbreviation = f_weekday_abbreviation(date),
#'        weekday_short = f_weekday(date),
#'         weekday_short_distinct = f_weekday(date, distinct = TRUE)
#'     )
#'
#'
#' set.seed(10)
#' dat <- data_frame(
#'     month = sample(month.name, 1000, TRUE),
#'     area =  sample(LETTERS[1:5], 1000, TRUE)
#' ) %>%
#'     count(month, area) %>%
#'     ungroup() %>%
#'     mutate(month = factor(month, levels = constant_months))
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
#' @method f_month POSIXt
f_month.POSIXt <- function(x, ...) {
    toupper(gsub("(^.)(.+)", "\\1", as.character(format(x, "%b"))))
}



#' @export
#' @rdname f_month
#' @method f_month hms
f_month.hms <- function(x, ...) {
    f_month.POSIXt(as.POSIXct(x))
}



#' @export
#' @rdname f_month
ff_month <- function(...) {
    function(x) {f_month(x)}
}












#' @export
#' @rdname f_month
f_month_name <- function(x, ...) {
    UseMethod('f_month_name')
}



#' @export
#' @rdname f_month
#' @method f_month_name default
f_month_name.default <- function(x, ...) {
    gsub("(^.)(.+)", "\\U\\1\\L\\2", as.character(x), perl = TRUE)
}

#' @export
#' @rdname f_month
#' @method f_month_name numeric
f_month_name.numeric <- function(x, ...) {
    month.name[x]
}


#' @export
#' @rdname f_month
#' @method f_month_name Date
f_month_name.Date <- function(x, ...) {
    format(x, "%B")
}


#' @export
#' @rdname f_month
#' @method f_month_name POSIXt
f_month_name.POSIXt <- function(x, ...) {
    format(x, "%B")
}



#' @export
#' @rdname f_month
#' @method f_month_name hms
f_month_name.hms <- function(x, ...) {
    f_month_name.POSIXt(as.POSIXct(x))
}



#' @export
#' @rdname f_month
ff_month_name <- function(...) {
    function(x) {f_month_name(x)}
}








#' @export
#' @rdname f_month
f_month_abbreviation <- function(x, ...) {
    UseMethod('f_month_abbreviation')
}



#' @export
#' @rdname f_month
#' @method f_month_abbreviation default
f_month_abbreviation.default <- function(x, ...) {
    gsub("(^.)(.{2})()", "\\U\\1\\L\\2", as.character(x), perl = TRUE)
}

#' @export
#' @rdname f_month
#' @method f_month_abbreviation numeric
f_month_abbreviation.numeric <- function(x, ...) {
    month.abb[x]
}


#' @export
#' @rdname f_month
#' @method f_month_abbreviation Date
f_month_abbreviation.Date <- function(x, ...) {
    format(x, "%b")
}


#' @export
#' @rdname f_month
#' @method f_month_abbreviation POSIXt
f_month_abbreviation.POSIXt <- function(x, ...) {
    format(x, "%b")
}



#' @export
#' @rdname f_month
#' @method f_month_abbreviation hms
f_month_abbreviation.hms <- function(x, ...) {
    f_month_abbreviation.POSIXt(as.POSIXct(x))
}



#' @export
#' @rdname f_month
ff_month_abbreviation <- function(...) {
    function(x) {f_month_abbreviation(x)}
}












