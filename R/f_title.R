#' Convert First Letter of Words to Title Case
#'
#' A wrapper for \code{\link[tools]{toTitleCase}} converting text to title case.
#'
#' @param x A vector of text strings.
#' @param upper A vector of regular expression to convert to upper case that
#' would otherwise be lower cased (this should be targetted at the initial output,
#' not the input).
#' @param lower A vector of regular expression to convert to lower case that
#' would otherwise be upper cased (this should be targetted at the initial output,
#' not the input).
#' @param \ldots ignored.
#' @return Returns a string vector with characters replaced.
#' @rdname f_title
#' @export
#' @seealso \code{\link[tools]{toTitleCase}}
#' @examples
#' f_title('i love this title')
#' f_title(f_replace('Cool_Variable'))
#'
#' f_title(c('select', 'group by', 'My ascii'))
#' f_title(c('select', 'group by', 'My ascii'), upper = c('Ascii'))
#' f_title(c('select', 'group by', 'My ascii'), upper = c('Ascii', 'b(?=y\\b)'))
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
f_title <- function (x, upper = NULL, lower = NULL, ...) {

    nas <- is.na(x)
    out <- gsub('\\bi\\b', 'I', gsub('(^.)', '\\U\\1', tools::toTitleCase(x), perl = TRUE))

    if (!is.null(upper)) {
       out <- gsub(paste0('(', paste(upper, collapse = '|'), ')'), '\\U\\1', out, perl = TRUE)
    }

    if (!is.null(lower)) {
       out <- gsub(paste0('(', paste(lower, collapse = '|'), ')'), '\\U\\1', out, perl = TRUE)
    }

    out[nas] <- NA
    out

}


#' @export
#' @include utils.R
#' @rdname f_title
ff_title <- functionize(f_title)







