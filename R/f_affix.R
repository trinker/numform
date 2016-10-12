#' Add String Affixes
#'
#' Convenience function to add affixes to strings (prefixes & suffixes).
#'
#' @param x A vector of elements to append with an affix.
#' @param prefix A string to append to the front of elements.
#' @param suffix A string to append to the back of elements.
#' @return
#' @export
#' @rdname f_affix
#' @seealso
#' @examples
#' f_affix(1:5, "-", "%")
#' f_affix(f_num(1:5, 2), "-", "%")
#'
#' f_prefix(LETTERS[1:5], "_")
#' f_prefix(f_bills(123456789123, -2), "$")
#'
#' f_suffix(LETTERS[1:5], "_")
#' f_suffix(f_num(1:5, 2), "%")
#'
#' \dontrun{
#' f_bills(123456789123, -2) %>%
#'     f_prefix("$")
#' }
f_affix <- function(x, prefix = "", suffix = ""){
    paste0(prefix, x, suffix)
}

#' @export
#' @rdname f_affix
f_prefix <- function(x, prefix = "$"){
    paste0(prefix, x)
}

#' @export
#' @rdname f_affix
f_suffix <- function(x, suffix = "%"){
    paste0(x, suffix)
}
