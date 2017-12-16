#' Convert Select \pkg{numform} Outputs to Factor
#'
#' Convert month and weekday and weekday types to factor with correctly ordered
#' levels.
#'
#' @param x A vector of weekdays or months.
#' @param shift Shift the levels to the right or left.  Useful for setting the
#' week beginning to something besides Sunday.  Use -1 to set to Monday instead.
#' @param \ldots ignored.
#' @return Returns a factor vector with levels set.
#' @export
#' @examples
#' dat <- structure(list(month1 = c("Jan", "Nov", "Mar", "Jul", "Aug",
#' "Jan", "Aug", "May", "Dec", "Apr"), month2 = c("March", "May",
#' "March", "July", "May", "October", "March", "November", "April",
#' "January"), weekday1 = c("Th", "F", "M", "Su", "Th", "Su", "M",
#' "Th", "W", "T"), weekday2 = c("We", "Th", "Fr", "Sa", "We", "Su",
#' "Tu", "Su", "Su", "Th"), weekday3 = c("Sat", "Wed", "Mon", "Wed",
#' "Wed", "Wed", "Wed", "Sun", "Fri", "Thu"), weekday4 = c("Sunday",
#' "Sunday", "Thursday", "Saturday", "Monday", "Wednesday", "Friday",
#' "Thursday", "Sunday", "Saturday")), .Names = c("month1", "month2",
#' "weekday1", "weekday2", "weekday3", "weekday4"))
#'
#' as_factor(dat$month1)
#' as_factor(dat$month2)
#' as_factor(dat$weekday1)
#' as_factor(dat$weekday2)
#' as_factor(dat$weekday3)
#' as_factor(dat$weekday4)
#'
#' ## shift levels
#' as_factor(dat$weekday4, -1)
#' as_factor(dat$weekday4, -2)
#' as_factor(dat$weekday4, 1)
#' as_factor(dat$weekday4, 2)
#'
#' \dontrun{
#' library(tidyverse)
#'
#' data_frame(
#'     revenue = rnorm(10000, 500000, 50000),
#'     date = sample(seq(as.Date('1999/01/01'), as.Date('2000/01/01'), by="day"), 10000, TRUE),
#'     site = sample(paste("Site", 1:5), 10000, TRUE)
#' ) %>%
#'     mutate(
#'         dollar = f_comma(f_dollar(revenue, digits = -3)),
#'         thous = f_thous(revenue),
#'         thous_dollars = f_thous(revenue, prefix = '$'),
#'         abb_month = f_month(date),
#'         abb_week = as_factor(f_weekday(date, distinct = TRUE))
#'     ) %T>%
#'     print() %>%
#'     ggplot(aes(abb_week, revenue)) +
#'         geom_jitter(width = .2, height = 0, alpha = .2) +
#'         scale_y_continuous(label = ff_thous(prefix = '$'))+
#'         facet_wrap(~site) +
#'         theme_bw()
#' }
as_factor <- function(x, shift = 0, ...){
    type <- check_type(x)
    factor(x, levels = shiftit(fact_check[[type]], shift))
}

shiftit <- function(x, n) {

    if (n == 0) return(x)
    if (n > 0) {
        c(x[(length(x) - (n - 1)):length(x)], x[1:(length(x) - n)])
    } else {
        c(x[(abs(n) + 1):length(x)], x[1:abs(n)])
    }

}

fact_check <- list(
    month_3letter = month.abb,
    month_long = month.name,
    weekday_1letter = c("Su", "M", "T", "W", "Th", "F", "S"),
    weekday_2letter = c("Su", "Mo", "Tu", "We", "Th", "Fr", "Sa"),
    weekday_3_letter = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"),
    weekday_long = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"),
    quarter = paste0('Q', 1:14),
    response = c("Yes", "No")
)

check_type <- function(x){

    type <- NA
    i <- 1
    matches <- FALSE
    mi <- length(fact_check) + 1

    while(!matches) {
        matches <- all(rm_na(x) %in% fact_check[[i]])
        type <- names(fact_check)[i]
        i <- i + 1

        if (!matches & i == mi) {
            stop('The provided vector does not appear to match any known type of `numform` factor.')
        }
    }

    return(type)
}

