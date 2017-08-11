#' Yes/No Convert Logical/Dummy Code
#'
#' Coerce logical (\code{TRUE}, \code{FALSE}) or or dummy coded elements (0/1)
#' to "Yes"/"No" elements.  This function is most useful in plot scales.
#'
#' @param x A vector of logical or dummy integers.  This vector will be coerced
#' to logical.
#' @param true A value for \code{TRUE} elements.
#' @param false A value for  \code{FALSE} elements.
#' @param \ldots ignored.
#' @return Returns a string of either "Yes" or "No" elements.
#' @export
#' @rdname f_affirm
#' @seealso \code{\link[base]{prettyNum}}
#' @examples
#' f_affirm(c(TRUE, TRUE, FALSE))
#' f_affirm(c(1, 1, 0, 1, 0, 0, NA))
#' f_affirm(c(1, 0, 2, .3, -3))
#' f_affirm(rnorm(20) > 0)
#' f_affirm(rnorm(20) > 0, "A", "B")
#'
#' \dontrun{
#' library(ggplot2)
#' library(dplyr)
#'
#' ## Without labels
#' data_frame(dummy = sample(c(TRUE, FALSE), 30, TRUE)) %>%
#'     count(dummy) %>%
#'     ggplot(aes(dummy, n)) +
#'         geom_bar(stat = 'identity')
#'
#' ## With labels
#' data_frame(dummy = sample(c(TRUE, FALSE), 30, TRUE)) %>%
#'     count(dummy) %>%
#'     ggplot(aes(dummy, n)) +
#'         geom_bar(stat = 'identity') +
#'         scale_x_discrete(labels = f_affirm)
#' }
f_affirm <- function(x, true = 'Yes', false = 'No', ...) {

    ifelse(as.logical(x), true, false)

}

#' @export
#' @include utils.R
#' @rdname f_affirm
ff_affirm <- functionize(f_affirm)


