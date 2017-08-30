#' Wrap Strings
#'
#' Wrap strings by splitting n width, and paste collapsing with new line
#' characters.
#'
#' @param x A vector of text strings.
#' @param width A positive integer giving the target column for wrapping lines
#' in the output.
#' @param exdent A non-negative integer specifying the indentation of subsequent
#' lines in paragraphs.
#' @param indent A non-negative integer giving the indentation of the first line
#' in a paragraph.
#' @param \ldots Other arguments passed to \code{\link[base]{strwrap}}.
#' @return Returns a string vector with wrapped new line characters.
#' @rdname f_wrap
#' @export
#' @seealso \code{\link[base]{strwrap}}
#' @examples
#' cat(f_wrap('really long label names are the pits'))
#' cat(f_wrap('really long label names are the pits', width = 20, exdent = 2))
#'
#' \dontrun{
#' library(tidyverse); library(gridExtra)
#'
#' set.seed(10)
#' dat <- data_frame(
#'     level = c('Not Involved', 'Somewhat Involved Single Group',
#'         'Somewhat Involved Multiple Groups', 'Very Involved One Group',
#'         'Very Involved Multiple Groups'
#'     ),
#'     n = sample(1:10, length(level))
#' ) %>%
#'     mutate(
#'         level = factor(level, levels = unique(level)),
#'         `%` = n/sum(n)
#'     )
#'
#' gridExtra::grid.arrange(
#'     dat %>%
#'         ggplot(aes(level, `%`)) +
#'             geom_col() +
#'             labs(title = 'Yucky Labels', y = NULL),
#'
#'     dat %>%
#'         ggplot(aes(level, `%`)) +
#'             geom_col() +
#'             scale_x_discrete(labels = f_wrap) +
#'             scale_y_continuous(labels = ff_prop2percent(digits = 0)) +
#'             labs(title = 'Happy Labels', y = NULL),
#'
#'     ncol = 1, heights = c(.45, .55)
#' )
#'
#' }
f_wrap <- function (x, width = 15, exdent = 0, indent = 0, ...) {

    nas <- is.na(x)
    out <- unlist(lapply(x, function(y) {
        paste(
            strwrap(y, width = width, exdent = exdent, indent = indent, ...),
            collapse = "\n"
        )
    }))
    out[nas] <- NA
    out

}


#' @export
#' @include utils.R
#' @rdname f_wrap
ff_wrap <- functionize(f_wrap)
