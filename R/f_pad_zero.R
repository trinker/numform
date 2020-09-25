#' Pad Numbers with Leading Zeros
#'
#' \code{f_pad_zero} - Add leading zeros to numbers.
#'
#' @param x A vector of numbers (or string equivalents).
#' @param width The width to make the stings.  Defaults to the maximum number of
#' characters for all elements in \code{x}.
#' @param pad.char A character to pad the string with.
#' @param \ldots ignored.
#' @return Returns a padded string.
#' @rdname f_pad_zero
#' @export
#' @examples
#' f_pad_zero(c(NA, 1, 12))
#' f_pad_zero(c(NA, 1, 100, 10, 1000))
#' f_pad_zero(as.character(c(NA, 1, 100, 10, 1000)))
#' f_pad_zero(c(NA, 1, 100, 10, 1000, "B", "BB"))
#' f_pad_left(c(NA, 1, 100, 10, 1000, "B", "BB"), '-')
#' f_pad_right(c(NA, 1, 100, 10, 1000, "B", "BB"), '-')
#' f_pad_left(c(NA, 1, 12))
f_pad_zero <- function(x, width = NULL, pad.char = '0', ...){

    f_pad_left(x, pad.char = pad.char, width = width)
}



#' Pad Numbers with Leading Zeros
#'
#' \code{f_pad_left} - Add leading character to strings.
#'
#' @rdname f_pad_zero
#' @export
f_pad_left <- function(x, pad.char = ' ', width = NULL, ...){

    stopifnot(nchar(pad.char) == 1 | length(pad.char) == 1)
    is_na <- is.na(x)
    lens <- nchar(x)
    maxd <- max(lens, na.rm = TRUE)
    if (is.null(width)) width <- maxd
    stopifnot(width >= maxd)
    zeros <- width - lens

    x[!is_na] <- paste0(strrep(pad.char, zeros[!is.na(zeros)]), x[!is_na])

    x

}


#' Pad Numbers with Leading Zeros
#'
#' \code{f_pad_right} - Add trailing character to strings.
#'
#' @rdname f_pad_zero
#' @export
f_pad_right <- function(x, pad.char = ' ', width = NULL, ...){

    stopifnot(nchar(pad.char) == 1 | length(pad.char) == 1)
    is_na <- is.na(x)
    lens <- nchar(x)
    maxd <- max(lens, na.rm = TRUE)
    if (is.null(width)) width <- maxd
    stopifnot(width >= maxd)
    zeros <- width - lens

    x[!is_na] <- paste0(x[!is_na], strrep(pad.char, zeros[!is.na(zeros)]))

    x
}



#' @export
#' @include utils.R
#' @rdname f_pad_zero
ff_pad_zero <- functionize(f_pad_zero)



#' @export
#' @include utils.R
#' @rdname f_pad_zero
ff_pad_left <- functionize(f_pad_left)


#' @export
#' @include utils.R
#' @rdname f_pad_zero
ff_pad_right <- functionize(f_pad_right)











