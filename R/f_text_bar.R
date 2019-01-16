#' Format Text Based Bar Plots
#'
#' Use a text symbol to create scaled horizontal bar plots of numeric vectors.
#'
#' @param x A numeric vector.
#' @param symbol A sumbol to use for the bars.
#' @param width The max width of the bar.
#' @param \ldots ignored.
#' @return Returns a vector of concatenated symbols as a string that represent x% of the bar.
#' @export
#' @rdname f_text_bar
#' @examples
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
#'         ` ` = text_bar(n)  ## Overall
#'     ) %>%
#'     as.data.frame()
#'
#' mtcars %>%
#'     count(cyl, gear) %>%
#'     group_by(cyl) %>%
#'     mutate(
#'         p = numform::f_pp(n/sum(n)),
#'         ` ` = text_bar(n) ## within groups
#'     ) %>%
#'     ungroup() %>%
#'     mutate(
#'         cyl = numform::fv_runs(cyl),
#'         ` ` = text_bar(n)
#'     ) %>%
#'     as.data.frame()
#'
#'
#' mtcars %>%
#'     count(cyl, gear) %>%
#'     group_by(cyl) %>%
#'     mutate(
#'         p = numform::f_pp(n/sum(n)),
#'         `within groups` = text_bar(n, width = 8, symbol = '#')
#'     ) %>%
#'     ungroup() %>%
#'     mutate(
#'         cyl = numform::fv_runs(cyl),
#'         `overall` = text_bar(n, width = 30, symbol = '-')
#'     ) %>%
#'     as.data.frame()
#'
#' ## Drop the ehaders
#' mtcars %>%
#'     count(cyl, gear) %>%
#'     group_by(cyl) %>%
#'     mutate(
#'         p = numform::f_pp(n/sum(n)),
#'         `   ` = text_bar(n, symbol = '#')
#'     ) %>%
#'     ungroup() %>%
#'     mutate(
#'         cyl = numform::fv_runs(cyl),
#'         ` ` = text_bar(n, symbol = '-')
#'     ) %>%
#'     as.data.frame()
f_text_bar <- function(x, symbol = '_', width = 20, ...){

    stopifnot(is.numeric(x))
    stringi::stri_pad_right(strrep(symbol, round(width * x/max(x), 0)), width = width)

}



#' @export
#' @include utils.R
#' @rdname f_text_bar
ff_text_bar <- functionize(f_text_bar)
