#' Convert First Letter of Words to Title Case
#'
#' A wrapper for \code{\link[tools]{toTitleCase}} converting text to title case.
#'
#' @param x A vector of text strings.
#' @param \ldots ignored.
#' @return Returns a string vector with characters replaced.
#' @rdname f_title
#' @export
#' @seealso \code{\link[tools]{toTitleCase}}
#' @examples
#' f_title('i love this title')
#' f_title(f_title('Cool_Variable'))
#'
#' \dontrun{
#' library(tidyverse)
#'
#' set.seed(10)
#' dat <- data_frame(
#'     level = c("not_involved", "somewhat_involved_single_group",
#'         "somewhat_involved_multiple_groups", "very_involved_one_group",
#'         "very_involved_multiple_groups"
#'     ),
#'     n = sample(1:10, length(level))
#' ) %>%
#'     mutate(
#'         level = factor(level, levels = unique(level)),
#'         `%` = n/sum(n)
#'     )
#'
#' gridExtra::grid.arrange(
#'
#'     gridExtra::arrangeGrob(
#'
#'         dat %>%
#'             ggplot(aes(level, `%`)) +
#'                 geom_col() +
#'                 labs(title = 'Very Sad', y = NULL) +
#'                 theme(
#'                     axis.text = element_text(size = 7),
#'                     title = element_text(size = 9)
#'                 ),
#'
#'        dat %>%
#'             ggplot(aes(level, `%`)) +
#'                 geom_col() +
#'                 scale_x_discrete(labels = function(x) f_replace(x, '_', '\n')) +
#'                 scale_y_continuous(labels = ff_prop2percent(digits = 0))  +
#'                 labs(title = 'Underscore Split (Readable)', y = NULL) +
#'                 theme(
#'                     axis.text = element_text(size = 7),
#'                     title = element_text(size = 9)
#'                 ),
#'
#'
#'         ncol = 2
#'
#'     ),
#'     gridExtra::arrangeGrob(
#'
#'        dat %>%
#'             ggplot(aes(level, `%`)) +
#'                 geom_col() +
#'                 scale_x_discrete(labels = function(x) f_title(f_replace(x))) +
#'                 scale_y_continuous(labels = ff_prop2percent(digits = 0))  +
#'                 labs(title = 'Underscore Replaced & Title (Capitalized Sadness)', y = NULL) +
#'                 theme(
#'                     axis.text = element_text(size = 7),
#'                     title = element_text(size = 9)
#'                 ),
#'
#'         dat %>%
#'             ggplot(aes(level, `%`)) +
#'                 geom_col() +
#'                 scale_x_discrete(labels = function(x) f_wrap(f_title(f_replace(x)))) +
#'                 scale_y_continuous(labels = ff_prop2percent(digits = 0))  +
#'                 labs(title = 'Underscore Replaced, Title, & Wrapped (Happy)', y = NULL) +
#'                 theme(
#'                     axis.text = element_text(size = 7),
#'                     title = element_text(size = 9)
#'                 ),
#'
#'         ncol = 2
#'
#'     ), ncol = 1
#'
#' )
#'
#' }
f_title <- function (x, ...) {

    nas <- is.na(x)
    out <- gsub('\\bi\\b', 'I', gsub('(^.)', '\\U\\1', tools::toTitleCase(x), perl = TRUE))

    out[nas] <- NA
    out

}


#' @export
#' @include utils.R
#' @rdname f_title
ff_title <- functionize(f_title)







