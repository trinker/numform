#' Format P-Values
#'
#' Format p-values for reporting using a < or = sign if greater than alpha level.
#'
#' @param x A p-value.
#' @param alpha The alpha cut off to use.  Defaults to .05.  Can be set
#' globally via: \code{options(numformalpha = n)} where n is the alpha level.
#' @param digits The number of digits to use.  Defaults to 3.  Can be set
#' globally via: \code{options(numformdigits = n)} where n is the number of
#' digits beyond the decimal point to include.
#' @param \ldots Other values passed to \code{\link[numform]{f_num}}.
#' @return Returns a string of publication ready p-values.
#' @rdname f_pval
#' @export
#' @seealso \code{\link[numform]{f_num}}
#' @examples
#' f_pval(.05)
#' f_pval(.04999999999999999)
#' f_pval(.0002)
#' f_pval(.0002, .001)
#'
#' mod1 <- t.test(1:10, y = c(7:20))
#' f_pval(mod1$p.value)
#'
#' mod2 <- t.test(1:10, y = c(7:20, 200))
#' f_pval(mod2$p.value)
f_pval <- function(x, alpha = getOption("numformalpha"),
    digits = getOption("numformdigits"), ...) {

    if (is.null(alpha)) alpha <- .05
    if (is.null(digits)) digits <- 3

    if(length(alpha) > 1) {
        alpha <- alpha[1]
        warning("Using only alpha[1]")
    }

    if (length(x) > 1) {
        x <- x[1]
        warning("Using only x[1]")
    }

    if(alpha < 0 | alpha > 1) {
        alpha <- .05
        warning("alpha not between 0 and 1; using `alpha = .05`")
    }

    less <- as.numeric(x) < alpha

    op <- ifelse(isTRUE(less), "<", "=")
    comp <- ifelse(isTRUE(less),
        sub("^0+(?=\\.)", "", as.numeric(alpha), perl = TRUE), f_num(x, digits, ...))
    sprintf("p %s %s", op, comp)

}


#' @export
#' @include utils.R
#' @rdname f_pval
ff_pval <- functionize(f_pval)



