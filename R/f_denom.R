#' Abbreviate Numbers
#'
#' Use the denomination abbreviations K (thousands), M (millions), and
#' B (billions) with abbreviated numbers.\cr\code{f_denom} - Auto-detect the
#' maximum denomination and attempt to use it.
#'
#' @param x A vector of large numbers.
#' @param relative A factor relative to the current \code{digits} being rounded.
#' For example \code{relative = -1} moves one to the left while
#' \code{relative = 1} moves one to the right.
#' @param digits The number of digits to round to.  Actual \code{digits}
#' calculated as \code{digits} +  \code{relative}.
#' @param prefix A string to append to the front of elements.
#' @param pad.char A character to use for leading padding if lengths of output
#' are unequal.  Use \code{NA} to forgo padding.
#' @param \ldots ignored.
#' @return Returns an abbreviated vector of numbers.
#' @export
#' @rdname f_denom
#' @examples
## f_denom(c(12345, 12563, 191919), prefix = '$')
## f_denom(c(12345, 12563, 191919), prefix = '$', pad.char = '')
## f_denom(c(1234365, 122123563, 12913919), prefix = '$')
## f_denom(c(12343676215, 122126763563, 1291673919), prefix = '$')
## f_denom(c(NA, 2, 12343676215, 122126763563, 1291673919), prefix = '$')
## f_denom(c(NA, 2, 12343676215, 122126763563, 1291673919), relative = 1, prefix = '$')
## f_denom(c(NA, 2, 12343676215, 122126763563, 1291673919), relative = 9, prefix = '$')
#'
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
#'
#' x <- c(3886902.8696, 4044584.0424, 6591893.2104, 591893.2104)
#' f_mills(x)
#' f_mills(x, 1)
#' f_mills(x, 1, prefix = '$')
#' f_mills(x, 1, prefix = '$', pad.char = '0')
#'
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
#'         scale_y_continuous(label = ff_thous(prefix = '$') )
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
#'         abb_week = factor(f_weekday(date, distinct = TRUE),
#'         levels = c('Su', 'M', 'T', 'W', 'Th', 'F', 'S'))
#'     ) %T>%
#'     print() %>%
#'     ggplot(aes(abb_week, revenue)) +
#'         geom_jitter(width = .2, height = 0, alpha = .2) +
#'         scale_y_continuous(label = ff_thous(prefix = '$'))+
#'         facet_wrap(~site)
#' }
f_denom <- function(x, relative = 0, prefix = "", pad.char = ifelse(prefix == "", NA, " "), ...) {

    #if (missing(pad.char)) pad.char <- ifelse(prefix == "", NA, " ")

    md <- max(nchar(round(x, 0)), na.rm = TRUE)
    digs <- ifelse(md <= 6, 'thous', ifelse(md <= 9, 'mills', ifelse(md <= 12, 'bills', NA)))
    if (is.na(digs)) stop("Element(s) in `x` are greater than 12 digits.")

    fun <- switch(digs,
        thous = {ff_thous(relative = relative, prefix =prefix, pad.char = pad.char)},
        mills = {ff_mills(relative = relative, prefix =prefix, pad.char = pad.char)},
        bills = {ff_bills(relative = relative, prefix =prefix, pad.char = pad.char)}
    )

    fun(x)

}

#' @export
#' @include utils.R
#' @rdname f_denom
ff_denom <- functionize(f_denom)


#' @description \code{f_bills} - Force the abbreviation to the billions
#' denomination (B).
#' @export
#' @include utils.R
#' @rdname f_denom
f_bills <- function(x, relative = 0, digits = -9, prefix = "",
    pad.char = ifelse(prefix == '', NA, ' '), ...) {

    #if (missing(pad.char)) pad.char <- ifelse(prefix == '', NA, ' ')

    digits <- digits + relative
    nas <- is.na(x)

    if (relative > 0) {
        x <- sprintf(paste0("%.", 9 + digits, "f"), round(x, digits)/1e+09)
        x <- gsub("^0.", ".", paste0(x, "B"))
    } else {
        x <- gsub("^0.", ".", paste0(round(x, digits)/1e+09, "B"))
    }

    x <- ifelse(x == '.', '0B', x)
    if (!is.na(pad.char)) x <- f_pad_zero(x, width = max(nchar(x)), pad.char = pad.char)
    out <- paste0(prefix, x)
    out[nas] <- NA
    out
}


#' @export
#' @include utils.R
#' @rdname f_denom
ff_bills <- functionize(f_bills)

#' @description \code{f_mills} - Force the abbreviation to the millions
#' denomination (B).
#' @export
#' @rdname f_denom
f_mills <- function(x, relative = 0, digits = -6, prefix = "",
    pad.char = ifelse(prefix == '', NA, ' '), ...) {

    #if (missing(pad.char)) pad.char <- ifelse(prefix == '', NA, ' ')

    digits <- digits + relative
    nas <- is.na(x)

    if (relative > 0) {
        x <- sprintf(paste0("%.", 6 + digits, "f"), round(x, digits)/1e+06)
        x <- gsub("^0.", ".", paste0(x, "M"))
    } else {
        x <- gsub("^0.", ".", paste0(round(x, digits)/1e+06, "M"))
    }

    digit_warn(x, 'f_bills', 6)

    x <- ifelse(x == '.', '0M', x)
    if (!is.na(pad.char)) x <- f_pad_zero(x, width = max(nchar(x)), pad.char = pad.char)
    out <- paste0(prefix, x)
    out[nas] <- NA
    out
}


#' @export
#' @include utils.R
#' @rdname f_denom
ff_mills <- functionize(f_mills)


#' @description \code{f_thous} - Force the abbreviation to the thousands
#' denomination (B).
#' @export
#' @rdname f_denom
f_thous <- function(x, relative = 0, digits = -3, prefix = "",
    pad.char = ifelse(prefix == '', NA, ' '), ...) {

    #if (missing(pad.char)) pad.char <- ifelse(prefix == '', NA, ' ')

    digits <- digits + relative
    nas <- is.na(x)

    if (relative > 0) {
        x <- sprintf(paste0("%.", 3 + digits, "f"), round(x, digits)/1e+03)
        x <- gsub("^0.", ".", paste0(x, "K"))
    } else {
        x <- gsub("^0.", ".", paste0(round(x, digits)/1e+03, "K"))
    }

    digit_warn(x, 'f_mills', 3)

    x <- ifelse(x == '.', '0k', x)
    if (!is.na(pad.char)) x <- f_pad_zero(x, width = max(nchar(x)), pad.char = pad.char)
    out <- paste0(prefix, x)
    out[nas] <- NA
    out
}



#' @export
#' @include utils.R
#' @rdname f_denom
ff_thous <- functionize(f_thous)




digit_check <- function(x, digits = 3){
    any(nchar(gsub("(^\\d+)(\\.|[KBM])(.*$)", "\\1", x)) > digits)
}

digit_warn <- function(x, next_ver = "f_mills", digits = 3){
    if (digit_check(x, digits)) {
        warning(paste0(
            "Detected one or more elements with a larger denomination.\n  Consider using `",
            next_ver,
            "` function instead."
        ))
    }
}




