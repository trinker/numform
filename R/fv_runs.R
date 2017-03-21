#' Remove Subsequent Runs from a Vector
#'
#' Remove subsequent runs from a vector.
#'
#' @param x A vector with runs.
#' @param fill What to fill in subsequent runs with.
#' @param missing What to fill in missing values with.
#' @param \ldots ignored.
#' @return Returns a vector of strings with subsequent runs removed.
#' @export
#' @rdname fv_runs
#' @examples
#' x <- c(1, 1 , 2, 3, 4, 4, 1, 1, 3, 3, NA, 5)
#' fv_runs(x)
#' fv_runs(x, fill = '-')
#' fv_runs(x, fill = '-', missing = 'X')
#'
#' \dontrun{
#' library(dplyr)
#' set.seed(10)
#' data.frame(
#'     state = sort(sample(state.name[c(1, 5, 9, 12)], 12, TRUE)),
#'     val = rnorm(12)
#' ) %>%
#'     mutate(state2 = fv_runs(state))
#' }
fv_runs <- function(x, fill = "", missing = NA, ...){
    y <- as.numeric(as.factor(x))
    runs <- rle(y)
    out <- rep(fill, length(x))
    locs <- c(1, cumsum(runs[['lengths']])[-length(runs[['lengths']])] +1)
    out[locs] <- as.character(x[locs])
    if (!is.na(missing)) out[is.na(out)] <- missing
    out
}



#' @export
#' @include utils.R
#' @rdname fv_percent
ffv_percent <- functionize(fv_percent)

