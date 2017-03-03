#' Comma Format Large Integers
#'
#' Add commas to larger integers.
#'
#' @param x A vector opc numbers (or string equivalents).
#' @param mark The character to include every n places.
#' @param \ldots Other arguments passed to \code{\link[base]{prettyNum}}.
#' @return Returns a comma separted string of publication ready digits.
#' @export
#' @rdname f_comma
#' @seealso \code{\link[base]{prettyNum}}
#' @examples
#' set.seed(4)
#' f_comma(sample(4:10, 5)^5)
#' f_comma(c(1234.12345, 1234567890, .000034034, 123000000000, -1234567))
f_comma <- function(x, mark = ",", ...) {
    #x <- gsub("^0+\\.", ".", sapply(x, format, scientific = FALSE))
    # ifelse(
    #     grepl("\\.", x),
    #     # stringi::stri_replace_all_regex(x, thousands_regex, paste0("$1", mark)),
    #     gsub(thousands_regex, paste0("\\1", mark), x, perl = TRUE),
        #gsub('(\\d)(?=(\\d{3})+$)', paste0("\\1", mark), as.character(x), perl = TRUE)
        gsub("^0+\\.", ".", unname(prettyNum(x, mark, preserve.width = 'none',
            scientific = FALSE, ...)))
    # )
}

#' @export
#' @include utils.R
#' @rdname f_comma
ff_comma <- functionize(f_comma)


# thousands_regex <- "(\\d)(?:(?=\\d+(?=[^\\d.]))(?=(?:[0-9]{3})+\\b)|(?=\\d+(?=\\.))(?=(?:[0-9]{3})+(?=\\.)))"
