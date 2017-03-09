#' Pad Numbers with Leading Zeros
#'
#' Add leading zeros to numbers.
#'
#' @param x A vector of numbers (or string equivalents).
#' @param width The width to make the stings.  Defaults to one more character
#' than the maximum number of characters for all elements in \code{x}.
#' @param pad.char A character to pad the string with.
#' @param \ldots ignored.
#' @return Returns a string of zero padded digits.
#' @rdname f_pad_zero
#' @export
#' @examples
#' f_pad_zero(c(NA, 1, 12))
#' f_pad_zero(c(NA, 1, 100, 10, 1000))
#' pad_zero(as.character(c(NA, 1, 100, 10, 1000)))
#' f_pad_zero(c(NA, 1, 100, 10, 1000, "B", "BB"))
f_pad_zero <- function(x, width = NULL, pad.char = '0', ...){

    is_na <- is.na(x)
    lens <- nchar(x)
    maxd <- max(lens, na.rm = TRUE) + 1
    if (is.null(width)) width <- maxd
    stopifnot(width >= maxd)
    zeros <- width - lens


    x[!is_na] <- paste0(unlist(lapply(zeros[!is.na(zeros)], function(x) paste(rep(pad.char, x), collapse = ""))), x[!is_na])
    x
}


#' @export
#' @include utils.R
#' @rdname f_pad_zero
ff_pad_zero <- functionize(f_pad_zero)




