#' Parenthesis Formatting of Two Vectors
#'
#' \code{f_parenthesis} - Form two vectors of numbers as a leading number
#' followed by a second number in parenthesis.
#'
#' @param x Vector 1 (in \code{f_mean_sd} the mean values and in
#' \code{f_num_percent} the leading number vector).
#' @param y Vector 2 (in \code{f_mean_sd} the standard deviation values and in
#' \code{f_num_percent} the percent/proportion vector).
#' @param x_digits The number of digits to round the x vector.
#' @param y_digits The number of digits to round the y vector.
#' @param sep The separator between the first number and the leading parenthesis.
#' @param x_prefix A constant to place before each value in the x vector.
#' @param y_prefix A constant to place before each value in the y
#' vector inside of the parenthesis.
#' @param prop_fun The proportion function to convert the y y vector in
#' \code{f_num_percent}.  Default is \code{f_prop2percent}.  \code{f_percent} is
#' used for when the values are already percentages.
#' @param \ldots ignored.
#' @return Returns a vector of parenthesis combined strings using vector x and y.
#' @rdname f_parenthesis
#' @export
#' @examples
#' f_parenthesis(
#'     f_num(sample(50:100, 5), 1),
#'     f_num(rnorm(5, 5:15, 5), 1),
#'     prefix = 'mean = ',
#'     parenthesis_prefix = 'sd = ',
#'     sep = " "
#' )
#'
#' f_mean_sd(rnorm(5, 100, 20), rnorm(5, 20, 5))
#'
#' f_num_percent(rnorm(5, 100, 20), rnorm(5, .5, .1))
#'
#' f_parenthesis(
#'     sample(50:100, 5),
#'     f_prop2percent(rnorm(5, .5, .1), 0)
#' )
#'
#'  \dontrun{
#' library(tidyverse)
#' mtcars %>%
#'     group_by(cyl) %>%
#'     summarize(
#'         mean = mean(hp),
#'         sd = sd(hp),
#'         n = n()
#'     ) %>%
#'     mutate(
#'         prop = n /sum(n),
#'         mean_sd = f_mean_sd(mean, sd),
#'         n_perc = f_num_percent(n, prop, 0)
#'     )
#' }
f_parenthesis <- function(x, y, sep = "", x_prefix = "", y_prefix = "", ...){
    paste(
        paste0(x_prefix, x),
        paste0("(", y_prefix, y, ")"),
        sep = sep
    )
}

#' @export
#' @include utils.R
#' @rdname f_parenthesis
ff_parenthesis <- functionize(f_parenthesis)


#' Parenthesis Formatting of Two Vectors
#'
#' \code{f_mean_sd} - Wrapper for \code{f_parenthesis} optimized for formatting
#' vectors of means and standard deviations.
#'
#' @rdname f_parenthesis
#' @export
f_mean_sd <- function(x, y, x_digits = 1, y_digits = x_digits, sep = "", ...) {
    paste(
        f_num(x, x_digits),
        f_num(y, y_digits, "(", ")"),
        sep = sep
    )
}

#' @export
#' @include utils.R
#' @rdname f_parenthesis
ff_mean_sd <- functionize(f_mean_sd)


#' Parenthesis Formatting of Two Vectors
#'
#' \code{f_num_percent}  - Wrapper for \code{f_parenthesis} optimized for formatting
#' vectors of numbers and percentages deviations.
#'
#' @rdname f_parenthesis
#' @export
f_num_percent <- function(x, y, x_digits = 1, y_digits = x_digits, sep = "",
    prop_fun = numform::f_prop2percent, ...) {
    paste(
        f_num(x, x_digits),
        paste0("(", match.fun(prop_fun)(y, y_digits), ")"),
        sep = sep
    )
}


#' @export
#' @include utils.R
#' @rdname f_parenthesis
ff_num_percent <- functionize(f_num_percent)


