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
#' f_12_hour(0:24, '%I %p')
#' set.seed(10)
#' times <- as.POSIXct(sample(seq_len(1e4), 12), origin = '1970-01-01')
#' paste(f_12_hour(range(times)), collapse = ' to ')
f_12_hour <- function(x = Sys.time(), format = '%I:%M %p', pad.char = '', ...){
    UseMethod('f_12_hour')
}


#' @export
#' @rdname f_12_hour
#' @method f_12_hour default
f_12_hour.default <- function(x, format = '%I:%M %p', pad.char = '', ...){

    out <- format(as.POSIXct(x, ..., origin = "1960-01-01"), format = format)
    gsub('^0', pad.char, out)

}


#' @export
#' @rdname f_12_hour
#' @method f_12_hour integer
f_12_hour.integer <- function(x, format = '%I:%M %p', pad.char = '', ...){


    out <- format(as.POSIXct(paste0("2017-08-18 ", ifelse(nchar(x) == 1, '0', ''), x, ":00:00"), origin = "1960-01-01"), format=format)

    gsub('^0', pad.char, out)

}


#' @export
#' @rdname f_12_hour
#' @method f_12_hour numeric
f_12_hour.numeric <- function(x, format = '%I:%M %p', pad.char = '', ...){

    x <- as.integer(x)
    out <- format(as.POSIXct(paste0("2017-08-18 ", ifelse(nchar(x) == 1, '0', ''), x, ":00:00"), origin = "1960-01-01"), format=format)

    gsub('^0', pad.char, out)

}

#' @export
#' @include utils.R
#' @rdname f_12_hour
ff_12_hour <- functionize(f_12_hour)






