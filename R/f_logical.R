#' True/False Convert Logical/Dummy Code
#'
#' Coerce logical (\code{TRUE}, \code{FALSE}) or or dummy coded elements (0/1)
#' to "True"/"False" elements.  This function is most useful in plot scales.
#'
#' @param x A vector of logical or dummy integers.  This vector will be coerced
#' to logical.
#' @param true A value for \code{TRUE} elements.
#' @param false A value for  \code{FALSE} elements.
#' @param \ldots ignored.
#' @return Returns a string of either "True"/"False" elements.
#' @export
#' @rdname f_logical
#' @seealso \code{\link[base]{prettyNum}}
#' @examples
#' f_logical(c(TRUE, TRUE, FALSE))
#' f_logical(c(1, 1, 0, 1, 0, 0, NA))
#' f_logical(c(1, 0, 2, .3, -3))
#' f_logical(rnorm(20) > 0)
#' f_logical(rnorm(20) > 0, "A", "B")
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
#'         scale_x_discrete(labels = f_logical)
#' }
f_logical <- function(x, true = 'True', false = 'False', ...) {

    ifelse(as.logical(x), true, false)

}

#' @export
#' @include utils.R
#' @rdname f_logical
ff_logical <- functionize(f_logical)


#' @export
#' @rdname f_logical
#' @param yes A value for \code{TRUE} elements.
#' @param no A value for  \code{FALSE} elements.
f_response <- function(x, yes = 'Yes', no = 'No', ...) {

    ifelse(as.logical(x), yes, no)

}

#' @export
#' @include utils.R
#' @rdname f_logical
ff_response <- functionize(f_response)

