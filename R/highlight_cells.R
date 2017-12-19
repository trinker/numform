#' Highlight Cells
#'
#' A lightweight cell highlighter that uses non-standard evaluation.  This
#' function is designed for interactive use.  It's behavior outside of this
#' context is not gaurenteed.  For finer contral use an \code{ifelse} with
#' \code{paste} within a \code{?dplyr::mutate} statement.
#'
#' @param data A data.frame.
#' @param rows An expression that evaluates to logical and is equal in length to
#' the number of rows.
#' @param columns A vector of either integer positions or character names
#' corresponding to columns that should be highlighted.  Defaults to all columns.
#' @param left A highlighting tag for the left side of the cell value.
#' @param right A highlighting tag for the right side of the cell value.
#' Attempts to use the \code{left} input to create a corresponding \code{right}
#' HTML based tag.
#' @param \ldots ignored.
#' @return Returns a data.frame with the chosen cell values wrapped in highlight
#' tags.
#' @export
#' @examples
#' highlight_cells(mtcars, rows = hp > 230 | qsec > 20)
#' highlight_cells(mtcars, rows = hp > 230, columns = 'hp')
#'
#' \dontrun{
#' library(dplyr); library(tibble); library(pander)
#' mtcars %>%
#'     highlight_cells(rows = hp > 230, columns = 'hp') %>%
#'     highlight_cells(rows = qsec > 20, columns = 'qsec', left = '<b style="color:blue;">')  %>%
#'     rownames_to_column('car') %>%
#'     data.frame(stringsAsFactors = FALSE, check.names = FALSE) %>%
#'     pander::pander(split.tables = Inf, justify = alignment(.))
#' }
#'
#' \dontrun{
#' if (!require("pacman")) install.packages("pacman")
#' pacman::p_load(tidyverse, magrittr)
#'
#' set.seed(10)
#' data_frame(
#'     w = paste(constant_months, rep(2016:2017, each = 12))[1:20] ,
#'     x = rnorm(20, 200000, 75000)
#' ) %>%
#'     {
#'         a <- .
#'         rbind(
#'             a,
#'             a %>%
#'                 mutate(w = 'Total') %>%
#'                 group_by(w) %>%
#'                 summarize(x = sum(x))
#'         )
#'     } %>%
#'     mutate(
#'         y = f_denom(x, prefix = '$'),
#'         z = f_denom(x, mix.denom = TRUE, prefix = '$'),
#'         x = f_comma(f_dollar(x, 2))
#'     )  %>%
#'     highlight_cells(w == 'Total') %>%
#'     data.frame(stringsAsFactors = FALSE, check.names = FALSE) %>%
#'     pander::pander(split.tables = Inf, justify = alignment(.))
#' }
highlight_cells <- function(data, rows, columns = seq_len(ncol(data)),
    left = '<b>', right = gsub('(<)([^> ]+)([^>]*>)', '\\1/\\2>', left), ...){

    ## check data is data.frame
    stopifnot(is.data.frame(data))

    obs <- eval(substitute(rows), data)

    ## check that is
    if (!is.logical(obs)) stop('`rows` did not evaluate to logical')

    data[obs, columns] <- lapply(data[obs, columns, drop = FALSE], function(x) {
        paste0(left, x, right)
    })

    data

}


