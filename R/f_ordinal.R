#' Add Ordinal Suffixes (-st, -nd, -rd, -th) to Numbers
#'
#' Add ordinal suffixes (-st, -nd, -rd, -th) to numbers.
#'
#' @param x A vector of numbers (or string equivalents).
#' @param \ldots ignored.
#' @return Returns a string vector with ordinal suffixes.
#' @rdname f_ordinal
#' @export
#' @examples
#' f_ordinal(1:25)
f_ordinal <- function(x, ...){

    if (is.numeric(x) & any(x < 1)) warning("Values below 1 found.\nMay yield incorrect results")

    x <- as.character(x)

    regs <- c(th = '^1[1:2]$|[0456789]$', st = '(?<!^1)1$', nd = '(?<!^1)2$', rd = '(?<!^1)3$')

    for (i in seq_along(regs)){
        locs <- grepl(regs[i], x, perl = TRUE)
        x[locs] <- paste0(x[locs] , names(regs)[i])
    }

    x
}


#' @export
#' @include utils.R
#' @rdname f_ordinal
ff_ordinal <- functionize(f_ordinal)

