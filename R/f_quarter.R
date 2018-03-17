#' Format Quarters
#'
#' Format long/abbreviation month name, integer, or date formats to a quarter
#' format (i.e., Q1, Q2, Q3, Q4).
#'
#' @param x A vector of month names, integers 1-12, or dates.
#' @param prefix A quarter prefix (defaults to \code{'Q'}).
#' @param space A string to place between `Q` and quarter number.
#' @param max A maximum in the \code{x} vector, if \code{x} is \code{numeric},
#' corresponding to months (12) or quarters (4).
#' @param \ldots ignored.
#' @return Returns a quarter formatted atomic vector.
#' @export
#' @rdname f_quarter
#' @examples
#' f_quarter(month.name)
#'
#' f_quarter(1:12)
#'
#' dates <- seq(as.Date("2000/1/1"), by = "month", length.out = 12)
#' f_quarter(dates)
#' \dontrun{
#' if (!require("pacman")) install.packages("pacman")
#' pacman::p_load(tidyverse)
#'
#' set.seed(10)
#' dat <- data_frame(
#'     month = sample(month.name, 1000, TRUE),
#'     area =  sample(LETTERS[1:5], 1000, TRUE)
#' ) %>%
#'     mutate(quarter = factor(f_quarter(month), levels = constant_quarters)) %>%
#'     count(quarter, area)
#'
#' ggplot(dat, aes(quarter, n)) +
#'     geom_bar(stat = 'identity') +
#'     facet_wrap(~ area)
#' }
f_quarter <- function(x, prefix = 'Q', space = '', max = 12, ...) {
    UseMethod('f_quarter')
}



#' @export
#' @rdname f_quarter
#' @method f_quarter default
f_quarter.default <- function(x, prefix = 'Q', space = '', max = 12, ...) {
    out <- switch(check_month_type(x),
        month = {names(mnthqrt)[match(x, mnthqrt)]},
        month_abbreviated = {names(mnthqrt2)[match(x, mnthqrt2)]},
        stop('No method available for this class of `x`')
    )

    nas <- is.na(out)
    out <- paste0(prefix, space = space, out)
    out[nas] <- NA
    out
}

#' @export
#' @rdname f_quarter
#' @method f_quarter numeric
f_quarter.numeric <- function(x, prefix = 'Q', space = '',
    max = ifelse(all(x %in% c(1:4, NA)), 4, 12), ...) {

    if (!max %in% c(4, 12)) stop('`max` must be either 4 or 12')
    if (any(x > max) | any(x < 1) | any(x %% 1 != 0)) {
        x <- as.integer(x)
        x[x > max | x < 1] <- NA
        warning(sprintf('`x` has been coerced to integer and values > %s OR < 1 replaced with `NA`.', max))
    }
    if (max == 12){
        out <- names(mnthqrt3)[match(x, mnthqrt3)]
    } else {
        out <- x
    }
    nas <- is.na(out)
    out <- paste0(prefix, space = space, out)
    out[nas] <- NA
    out
}


#' @export
#' @rdname f_quarter
#' @method f_quarter Date
f_quarter.Date <- function(x, prefix = 'Q', space = '', max = 12, ...) {
    f_quarter(as.character(format(x, "%b")), prefix = prefix, space = space, max = max, ...)
}


#' @export
#' @rdname f_quarter
#' @method f_quarter POSIXt
f_quarter.POSIXt <- function(x, prefix = 'Q', space = '', max = 12, ...) {
     f_quarter(as.character(format(x, "%b")), prefix = prefix, space = space, max = max, ...)
}



#' @export
#' @rdname f_quarter
#' @method f_quarter hms
f_quarter.hms <- function(x, prefix = 'Q', space = '', max = 12, ...) {
    f_quarter.POSIXt(as.POSIXct(x), prefix = prefix, space = space, max = max, ...)
}



#' @export
#' @rdname f_quarter
ff_quarter <- function(prefix = 'Q', space = '', max = 12, ...) {
    function(x) {f_quarter(x, prefix = prefix, max = max, space = space, ...)}
}









