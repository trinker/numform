#' Format List Series
#'
#' \code{f_list} - Format a vector of elements as a list series
#' (e.g., \code{c('A', 'B', 'C')} becomes \code{"A, B, and C"}).
#'
#' @param x A vector of values to turn into a collapsed series.
#' @param and The value to use for the 'and'.  Commonly \code{'and'} and
#' \code{'&'} are used.
#' @param oxford logical.  If \code{TRUE} an oxford comma is used.  If you use
#' \code{FALSE} you are a monster.
#' @param \ldots ignored.
#' @return Returns a string that is a list series.
#' @export
#' @rdname f_list
#' @examples
#' f_list(1)
#' f_list(1:2)
#' f_list(1:3)
#' f_list(1:5)
#'
#' x <- c("parents", "Lady Gaga",  "Humpty Dumpty")
#' ## Three things you love
#' sprintf('I love my %s.', f_list(x))
#' ## Your parents are lady Gaga & Humpty Dumpty?????
#' sprintf('I love my %s.', f_list(x, oxford = FALSE))
#'
#' sprintf('I love my %s.', f_list(x, and = '&'))
#' sprintf('I love my %s.', f_list_amp(x))
f_list <- function(x, and = 'and', oxford = TRUE, ...){

    if (length(x) == 1) return(x)
    if (length(x) == 2) return(paste(x, collapse = paste0(' ', and, ' ')))

    comma <- ifelse(isTRUE(oxford), ',', '')

    len <- length(x)

    paste0(
        paste(x[-len], collapse = ', '),
        comma,
        ' ', and, ' ',
        x[len]
    )

}

#' Format List Series
#'
#' \code{f_list_amp} - A ampersand wrapper for \code{f_list} with
#' \code{and = '&'} set by default.
#'
#' @export
#' @rdname f_list
f_list_amp <- function(x, and = '&', oxford = TRUE, ...){

    f_list(x, and = and, oxford = oxford, ...)
}



#' @export
#' @include utils.R
#' @rdname f_list
ff_list <- functionize(f_list)


