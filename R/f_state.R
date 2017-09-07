#' Format State Names as Abbreviations
#'
#' Formats a state name as the abbreviated form.
#'
#' @param x A vector of states.
#' @param \ldots ignored.
#' @return Returns a string of abbreviated states.
#' @export
#' @rdname f_state
#' @examples
#' f_state(c('Texas', 'New York', NA, 'New Jersey', 'Washington', 'Europe'))
f_state <- function(x, ...){

    names(nms)[match(as.character(x), nms)]

}

nms <- datasets::state.name
names(nms) <- datasets::state.abb


#' @export
#' @include utils.R
#' @rdname f_state
ff_state <- functionize(f_state)

