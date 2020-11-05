#' Format Digits
#'
#' Remove leading zeros and standardize number of digits.  A workhorse for the
#' \pkg{numform} package.
#'
#' @param x A vector of numbers (or string equivalents).
#' @param digits The number of digits to use.  Defaults to 1.  Can be set
#' globally via: \code{options(numformdigits = n)} where n is the number of
#' digits beyond the decimal point to include.
#' @param p A string to paste at the beginning of the output from \code{f_num}.
#' @param s A string to paste at the end of the output from \code{f_num}.
#' @param pad.char A character to use for leading padding if lengths of output
#' are unequal.
#' @param zero A value to insert in for zero values.
#' @param retain.leading.zero logical.  If \code{TRUE} then leading zeros before
#' a decimal place are retained.
#' @param \ldots ignored.
#' @return Returns a string of publication ready digits.
#' @export
#' @rdname f_num
#' @examples
#' f_num(c(0.0, 0, .2, -00.02, 1.122222, pi))
#' f_num(rnorm(10))
#' f_num(rnorm(20, 100, 200), 0)
#' f_num(c("-0.23", "0", ".23"))
#'
#' ## Percents
#' f_num(c(30, 33.45, .1), 3, s="%")
#'
#' ## Money
#' f_num(c(30, 33.45, .1), 2, p="$")
#'
#' ## Units
#' f_num(c(30, 33.45, .1), 2, s=" in.<sup>2</sup>")
#' f_num(c(30, 33.45, .1), 2, p="&Chi;<sup>2</sup>=")
#'
#' \dontrun{
#' library(dplyr)
#'
#' is.int <- function(x) !all(x %% 1 == 0)
#'
#' mtcars %>%
#'     mutate_if(.funs = f_num, is.int)
#'
#' df <- data.frame(x = -10:10, y = (-10:10)/10)
#'
#' ggplot(df, aes(x, y))+
#'     geom_point() +
#'     scale_y_continuous(labels = ff_num(zero = 0))
#' }
f_num <- function(x, digits = getOption("numformdigits"), p, s, pad.char = NA, zero = NULL, retain.leading.zero = FALSE, ...) {

    ldots <- list(...)
    if (length(ldots) > 0) {
        if (!is.null(ldots[['prefix']]) & missing(p)) p <- ldots[['prefix']]
        if (!is.null(ldots[['suffix']]) & missing(s)) s <- ldots[['suffix']]
    }


    if (is.null(digits)) digits <- 1

    if(length(digits) > 1) {
        digits <- digits[1]
        warning("Using only digits[1]")
    }

    x <- round(as.numeric(x), digits)
    na_locs <- which(is.na(x))

    if (digits > 0) x <- sprintf(paste0("%.", digits, "f"), x)
    if (!retain.leading.zero) out <- gsub("^0(?=\\.)|(?<=-)0", "", x, perl=TRUE) else out <- x
    if (!is.null(zero)) out <- gsub('^-?0?\\.?0+$', zero, out)

    if (!is.na(pad.char)) out <- f_pad_zero(out, width = max(nchar(out)), pad.char = pad.char)

    if (!missing(p)) out <- paste0(p, out)
    if (!missing(s)) out <- paste0(out, s)

    out[na_locs] <- NA
    out
}

#' @export
#' @include utils.R
#' @rdname f_num
ff_num <- functionize(f_num)


