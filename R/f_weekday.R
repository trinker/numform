#' Format Weekdays to One Letter Abbreviation
#'
#' Format long weekday name, integer, or date formats to a single capital letter.
#' Useful for plot scales as a way to save space.
#'
#' @param x A vector of weekday names, integers 1-12, or dates.
#' @param distinct logical.  If \code{TRUE} Sunday will be presented as \code{Su}
#' and Thursday as \code{Th}.
#' @param \ldots ignored.
#' @return Returns a single letter month abbreviation atomic vector.
#' @export
#' @rdname f_weekday
#' @examples
#' f_weekday(weekdays(x=as.Date(seq(7), origin="1950-01-07")))
#' f_weekday(weekdays(x=as.Date(seq(7), origin="1950-01-07")), TRUE)
#'
#' f_weekday(1:7)
#' f_weekday(1:7, TRUE)
#'
#' days <- seq(as.Date("2000/1/2"), by = "day", length.out = 7)
#' f_weekday(days)
#' f_weekday(days, TRUE)
#'
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
#'     day = sample(weekdays(days), 10000, TRUE),
#'     area =  sample(LETTERS[1:15], 10000, TRUE)
#' ) %>%
#'     count(day, area) %>%
#'     ungroup() %>%
#'     mutate(
#'         day = factor(day, levels = weekdays(days))
#'     )
#'
#' ## without date formatting
#' ggplot(dat, aes(day, n)) +
#'     geom_bar(stat = 'identity') +
#'     facet_wrap(~area)
#'
#' ## with date formatting
#' ggplot(dat, aes(day, n)) +
#'     geom_bar(stat = 'identity') +
#'     facet_wrap(~area) +
#'     scale_x_discrete(labels = f_weekday)
#'
#' ## with date formatting
#' ggplot(dat, aes(day, n)) +
#'     geom_bar(stat = 'identity') +
#'     facet_wrap(~area) +
#'     scale_x_discrete(labels = ff_weekday(distinct = TRUE))
#' }
f_weekday <- function(x, distinct = FALSE, ...) {
    UseMethod('f_weekday')
}




#' @export
#' @rdname f_weekday
#' @method f_weekday default
f_weekday.default <- function(x, distinct = FALSE, ...) {
    if (distinct){
        locs <- match(
            gsub("(^.)(.)(.+)", "\\U\\1\\L\\2", as.character(x), perl = TRUE),
            short_weekdays_key
        )
        return(names(short_weekdays_key)[locs])
    }
    toupper(gsub("(^.)(.+)", "\\1", as.character(x)))
}

#' @export
#' @rdname f_weekday
#' @method f_weekday numeric
f_weekday.numeric <- function(x, distinct = FALSE, ...) {
    if (distinct) return(names(short_weekdays_key)[x])
    c("S", "M", "T", "W", "T", "F", "S")[x]
}




#' @export
#' @rdname f_weekday
#' @method f_weekday Date
f_weekday.Date <- function(x, distinct = FALSE, ...) {
    if (distinct){
        locs <- match(
            gsub("(^.)(.)(.+)", "\\U\\1\\L\\2", weekdays(x), perl = TRUE),
            short_weekdays_key
        )
        return(names(short_weekdays_key)[locs])
    }
    toupper(gsub("(^.)(.+)", "\\1", weekdays(x)))
}


#' @export
#' @rdname f_weekday
#' @method f_weekday POSIXt
f_weekday.POSIXt <- function(x, distinct = FALSE, ...) {
    if (distinct){
        locs <- match(
            gsub("(^.)(.)(.+)", "\\U\\1\\L\\2", weekdays(x), perl = TRUE),
            short_weekdays_key
        )
        return(names(short_weekdays_key)[locs])
    }
    toupper(gsub("(^.)(.+)", "\\1", weekdays(x)))
}

#' @export
#' @rdname f_weekday
#' @method f_weekday hms
f_weekday.hms <- function(x, distinct = FALSE, ...) {
    f_weekday.POSIXt(as.POSIXct(x))
}


#' @export
#' @rdname f_weekday
ff_weekday <- function(distinct = FALSE, ...) {
    function(x) {f_weekday(x, distinct = distinct)}
}



short_weekdays_key <- structure(c("Su", "Mo", "Tu", "We", "Th", "Fr", "Sa"), .Names = c("Su",
"M", "T", "W", "Th", "F", "S"))


#' @export
#' @rdname f_weekday
f_weekday_name <- function(x, ...) {
    UseMethod('f_weekday_name')
}




#' @export
#' @rdname f_weekday
#' @method f_weekday_name default
f_weekday_name.default <- function(x, ...) {

            gsub("(^.)(.+)", "\\U\\1\\L\\2", as.character(x), perl = TRUE)

}

#' @export
#' @rdname f_weekday
#' @method f_weekday_name numeric
f_weekday_name.numeric <- function(x, ...) {

    constant_weekdays[x]
}




#' @export
#' @rdname f_weekday
#' @method f_weekday_name Date
f_weekday_name.Date <- function(x, ...) {

    weekdays(x)

}


#' @export
#' @rdname f_weekday
#' @method f_weekday_name POSIXt
f_weekday_name.POSIXt <- function(x, ...) {

    weekdays(x)
}

#' @export
#' @rdname f_weekday
#' @method f_weekday_name hms
f_weekday_name.hms <- function(x, ...) {
    f_weekday_name.POSIXt(as.POSIXct(x))
}


#' @export
#' @rdname f_weekday
ff_weekday_name <- function(...) {
    function(x) {f_weekday_name(x)}
}




#' @export
#' @rdname f_weekday
f_weekday_abbreviation <- function(x, ...) {
    UseMethod('f_weekday_abbreviation')
}




#' @export
#' @rdname f_weekday
#' @method f_weekday_abbreviation default
f_weekday_abbreviation.default <- function(x, ...) {

            gsub("(^.)(.{2})", "\\U\\1\\L\\2", as.character(x), perl = TRUE)

}

#' @export
#' @rdname f_weekday
#' @method f_weekday_abbreviation numeric
f_weekday_abbreviation.numeric <- function(x, ...) {

    constant_weekdays_abbreviation[x]
}




#' @export
#' @rdname f_weekday
#' @method f_weekday_abbreviation Date
f_weekday_abbreviation.Date <- function(x, ...) {

    substring(weekdays(x), 1, 3)

}


#' @export
#' @rdname f_weekday
#' @method f_weekday_abbreviation POSIXt
f_weekday_abbreviation.POSIXt <- function(x, ...) {

    substring(weekdays(x), 1, 3)

}

#' @export
#' @rdname f_weekday
#' @method f_weekday_abbreviation hms
f_weekday_abbreviation.hms <- function(x, ...) {
    f_weekday_abbreviation.POSIXt(as.POSIXct(x))
}


#' @export
#' @rdname f_weekday
ff_weekday_abbreviation <- function(...) {
    function(x) {f_weekday_abbreviation(x)}
}


