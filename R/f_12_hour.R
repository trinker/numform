#' Format 12 Hour Times
#'
#' Format times to the typical 12 hour '%I:%M %p' in-text format.
#'
#' @param x A vector of coercable times.
#' @param format A character string specifying the time output format.
#' @param pad.char A character to use for leading padding if lengths of output
#' are unequal.
#' @param \ldots Other arguments passed to \code{\link[base]{as.POSIXct}}.
#' @return Returns a string of publication ready 12 hour time stamps.
#' @export
#' @rdname f_12_hour
#' @examples
#' f_12_hour(Sys.time())
#' f_12_hour(Sys.time(), pad.char ='0')
#' f_12_hour(Sys.time(), pad.char =' ')
#' f_12_hour(Sys.time(), '%I:%M:%S %p')
#' f_12_hour(c(NA, 0:24), '%I %p')
#' set.seed(10)
#' times <- as.POSIXct(sample(seq_len(1e4), 12), origin = '1970-01-01')
#' paste(f_12_hour(range(times)), collapse = ' to ')
#' \dontrun{
#' library(tidyverse)
#'
#' set.seed(10)
#' data_frame(
#'     time = as.POSIXct(sample(seq_len(1e4), 12), origin = '1970-01-01'),
#'     val = sample(1:20, length(time), TRUE)
#' ) %>%
#'     mutate(prop = val/sum(val)) %>%
#'     ggplot(aes(time, prop)) +
#'         geom_line() +
#'         scale_x_time(labels = ff_12_hour(format = '%I %p')) +
#'         scale_y_continuous(labels = ff_prop2percent(digits = 0))
#' }
f_12_hour <- function(x = Sys.time(), format = '%I:%M %p', pad.char = '', ...){
    UseMethod('f_12_hour')
}


#' @export
#' @rdname f_12_hour
#' @method f_12_hour default
f_12_hour.default <- function(x, format = '%I:%M %p', pad.char = '', ...){

    x <- as.POSIXct(x, ..., origin = "1960-01-01")
    nalocs <- is.na(x)
    x[nalocs] <- as.POSIXct(Sys.Date())

    out <- format(x, format = format)
    out <- gsub('^0', pad.char, out)
    out[nalocs] <- NA
    out

}


#' @export
#' @rdname f_12_hour
#' @method f_12_hour integer
f_12_hour.integer <- function(x, format = '%I:%M %p', pad.char = '', ...){

    nalocs <- is.na(x)
    x[nalocs] <- 1

    out <- format(as.POSIXct(paste0("2017-08-18 ", ifelse(nchar(x) == 1, '0', ''), x, ":00:00"), origin = "1960-01-01"), format=format)

    out <- gsub('^0', pad.char, out)
    out[nalocs] <- NA
    out

}


#' @export
#' @rdname f_12_hour
#' @method f_12_hour numeric
f_12_hour.numeric <- function(x, format = '%I:%M %p', pad.char = '', ...){

    x <- as.integer(x)
    nalocs <- is.na(x)
    x[nalocs] <- 1

    out <- format(as.POSIXct(paste0("2017-08-18 ", ifelse(nchar(x) == 1, '0', ''), x, ":00:00"), origin = "1960-01-01"), format=format)

    out <- gsub('^0', pad.char, out)
    out[nalocs] <- NA
    out

}

#' @export
#' @rdname f_12_hour
#' @method f_12_hour hms
f_12_hour.hms <- function(x, format = '%I:%M %p', pad.char = '', ...){

    f_12_hour.default(as.POSIXct(x), format = '%I:%M %p', pad.char = '', ...)

}


#' @export
#' @rdname f_12_hour
ff_12_hour <- function(format = '%I:%M %p', pad.char = '', ...) {
    function(x) {f_12_hour(x, format = format, pad.char = pad.char)}
}





