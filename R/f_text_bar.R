#' Format Text Based Bar Plots
#'
#' Use a text symbol to create scaled horizontal bar plots of numeric vectors.
#' Note that you will have to coerce the table to a \code{data.frame} in order
#' for the output to look pretty.
#'
#' @param x A numeric vector.
#' @param symbol A sumbol to use for the bars.
#' @param width The max width of the bar.
#' @param \ldots ignored.
#' @return Returns a vector of concatenated symbols as a string that represent x% of the bar.
#' @export
#' @rdname f_text_bar
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' mtcars %>%
#'     count(cyl, gear) %>%
#'     group_by(cyl) %>%
#'     mutate(
#'         p = numform::f_pp(n/sum(n))
#'     ) %>%
#'     ungroup() %>%
#'     mutate(
#'         cyl = numform::fv_runs(cyl),
#'         ` ` = f_text_bar(n)  ## Overall
#'     ) %>%
#'     as.data.frame()
#'
#' mtcars %>%
#'     count(cyl, gear) %>%
#'     group_by(cyl) %>%
#'     mutate(
#'         p = numform::f_pp(n/sum(n)),
#'         ` ` = f_text_bar(n) ## within groups
#'     ) %>%
#'     ungroup() %>%
#'     mutate(
#'         cyl = numform::fv_runs(cyl),
#'         ` ` = f_text_bar(n)
#'     ) %>%
#'     as.data.frame()
#'
#' mtcars %>%
#'     count(cyl, gear) %>%
#'     group_by(cyl) %>%
#'     mutate(
#'         p = numform::f_pp(n/sum(n)),
#'         `within` = f_text_bar(n, width = 3, symbol = '#')
#'     ) %>%
#'     ungroup() %>%
#'     mutate(
#'         cyl = numform::fv_runs(cyl),
#'         `overall` = f_text_bar(n, width = 30, symbol = '*')
#'     ) %>%
#'     as.data.frame() %>%
#'     pander::pander(split.tables = Inf, justify = alignment(.), style = 'simple')
#'
#' ## Drop the headers
#' mtcars %>%
#'     count(cyl, gear) %>%
#'     group_by(cyl) %>%
#'     mutate(
#'         p = numform::f_pp(n/sum(n)),
#'         `   ` = f_text_bar(n, symbol = '=')
#'     ) %>%
#'     ungroup() %>%
#'     mutate(
#'         cyl = numform::fv_runs(cyl),
#'         ` ` = f_text_bar(n, symbol = '#')
#'     ) %>%
#'     as.data.frame()
#' }
f_text_bar <- function(x, symbol = '_', width = 9, ...){

    stopifnot(is.numeric(x))
    stri_pad_right(strrep(symbol, round(width * x/max(x), 0)), width = width)

}



#' @export
#' @include utils.R
#' @rdname f_text_bar
ff_text_bar <- functionize(f_text_bar)





stri_pad_right <- function(x, width = floor(0.9 * getOption("width")), pad = " "){

    r <- width - nchar(x)
    r[r < 0] <- 0
    paste0(x, strrep(pad, r))
}



