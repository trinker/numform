#' Format Dates
#'
#' Format dates to the typical '%B %d, %Y in-text format.
#'
#' @param x A vector of coercible dates.
#' @param format A character string specifying the date output format.
#' @param \ldots Other arguments passed to \code{\link[base]{as.Date}}.
#' @return Returns a string of publication ready dates.
#' @export
#' @rdname f_date
#' @examples
#' f_date(Sys.Date())
#' f_date(Sys.time())
#' f_date(Sys.time(), '%b-%y')
#' set.seed(10)
#' dates <- as.Date(sample(1:10000, 12), origin = '1970-01-01')
#' paste(f_date(range(dates)), collapse = ' to ')
f_date <- function(x = Sys.Date(), format = '%B %d, %Y', ...){

    format(as.Date(x, ...), format = format)

}

#' @export
#' @include utils.R
#' @rdname f_date
ff_date <- functionize(f_date)

