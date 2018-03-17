#' Convert Binned Intervals to Readable Form
#'
#'  \code{f_bin} - Convert binned intervals to symbol form (e.g., \code{"1 < x <= 3"}).
#'
#' @param x A vector of binned numbers from \code{cut}.
#' @param l Less than symbol.
#' @param le Less than or equal to symbol.
#' @param parse logical.  If \code{TRUE} is parsed for \pkg{ggplot2} facet labels.
#' @param \ldots ignored.
#' @return \code{f_bin} - Returns human readable intervals in symbol form.
#' @rdname f_bin
#' @export
#' @examples
#' x <- cut(-1:5, 3, right = FALSE)
#' y <- cut(-4:10, c(-5, 2, 6, 10), right = TRUE)
#' z <- cut(-4:10, c(-4, 2, 6, 11), right = FALSE)
#'
#' f_bin(x)
#' f_interval(x) #`_interval` and `_bin` are interchangeable aliases in the function names
#' f_bin(y)
#' f_bin(z)
#' ## HTML
#' f_bin(z, le = '&le;')
#'
#' f_bin_text(x)
#' f_bin_text(y)
#' f_bin_text(z)
#' f_bin_text(x, middle = 'but')
#' f_bin_text(x, greater = 'Above', middle = '', equal = '', less = 'to')
#' f_bin_text(z, greater = 'From', middle = '', equal = '', less = 'up to')
#'
#' f_bin_text_right(x)
#' f_bin_text_right(y)
#' f_bin_text_right(cut(-4:10, c(-3, 2, 6, 11)))
#' f_bin_text_right(x, equal.digits = TRUE)
#'
#' f_bin_right(x)
#' f_bin_right(y)
#' f_bin_right(x, equal.digits = TRUE)
#' ## HTML
#' f_bin_right(y, le = '&le;')
#'
#' \dontrun{
#' library(tidyverse)
#'
#' mtcars %>%
#'     mutate(mpg2 = cut(mpg, 3)) %>%
#'     ggplot(aes(disp, hp)) +
#'         geom_point() +
#'         facet_wrap(~ mpg2,
#'             labeller = ff_bin()
#'         )
#'
#' mtcars %>%
#'     mutate(mpg2 = cut(mpg, 3)) %>%
#'     ggplot(aes(disp, hp)) +
#'         geom_point() +
#'         facet_wrap(~ mpg2,
#'             labeller = function(x) f_bin_right(x, parse = TRUE)
#'         )
#'
#' mtcars %>%
#'     mutate(mpg2 = cut(mpg, 3, right = FALSE)) %>%
#'     ggplot(aes(disp, hp)) +
#'         geom_point() +
#'         facet_wrap(~ mpg2,
#'             labeller = function(x) f_bin_right(x, parse = TRUE)
#'         )
#'
#' mtcars %>%
#'     mutate(mpg2 = cut(mpg, 5, right = FALSE)) %>%
#'     ggplot(aes(mpg2)) +
#'         geom_bar() +
#'         scale_x_discrete(labels = ff_bin_text_right(l = 'Up to')) +
#'         coord_flip()
#'
#' mtcars %>%
#'     mutate(mpg2 = cut(mpg, 10, right = FALSE)) %>%
#'     ggplot(aes(mpg2)) +
#'         geom_bar(fill = '#33A1DE') +
#'         scale_x_discrete(labels = function(x) f_wrap(f_bin_text_right(x, l = 'up to'), width = 8)) +
#'         scale_y_continuous(breaks = seq(0, 14, by = 2), limits = c(0, 7)) +
#'         theme_minimal() +
#'         theme(
#'             panel.grid.major.x = element_blank(),
#'             axis.text.x = element_text(size = 14, margin = margin(t = -12)),
#'             axis.text.y = element_text(size = 14),
#'             plot.title = element_text(hjust = .5)
#'         ) +
#'         labs(title = 'Histogram', x = NULL, y = NULL)
#' }

f_bin <- function(x, l = '<', le = '<=', parse = FALSE, ...){

    x <- as.character(unlist(x))
    check_binned(x)
    is_na <- is.na(x)

    key <- f_bins_level_replace(x)

    x <- key[['to']][match(as.character(x), key[['from']])]

    x <- gsub(' < ', paste0(' ', l, ' '), x, fixed = TRUE)
    out <- gsub(' <= ', paste0(' ', le, ' '), x, fixed = TRUE)
    out[is_na] <- NA
    parse_bin(out, parse = parse)
}
parse_bin <- function(x, parse = TRUE, ...){

    if (!parse) return(x)

    if (grepl('(^[0-9.-]+\\s<=\\sx )(<\\s[0-9.-]+)', na_omit(x)[1])) {
        z <- gsub('(^[0-9.-]+\\s<=\\sx )(<\\s[0-9.-]+)', "\\1 ~ '\\2'", x)
    } else {
        if (grepl('(^[0-9.-]+\\s<)(\\sx\\s<=\\s[0-9.-]+)', na_omit(x)[1])) {
            z <- gsub('<=', ' <= ', gsub('(^[0-9.-]+\\s<)(\\sx\\s<=\\s[0-9.-]+)', "'\\1' ~ \\2", x))
        } else {
            z <- x
        }
    }

    list(lapply(unlist(z), function(y) {
        parse(text = y)
    }))

}

parse_bin2 <- function(x, parse = TRUE, ...){

    if (!parse) return(x)
#browser()

    list(lapply(unlist(x), function(y) {
        parse(text = paste('symbol("\40")', y))
    }))

}


check_binned <- function(x, ...){
    x <- as.character(x)
    if (any(!(is.na(x) | grepl('^(\\[|\\()[0-9.-]+,[0-9.-]+(\\]|\\))$', x)))){
        stop('`x` doesn\'t appear to be a product of the `cut` function.\nPlease use `cut` to make intervals/bins')
    }
}



#' Convert Binned Intervals to Readable Form
#'
#'  \code{f_bin_text} - Convert binned intervals to text form (e.g., \code{"Greater than or equal to 1 to less than 3"}).
#'
#' @param greater String to use for greater.
#' @param middle String to use for middle  (defaults to \code{'to'}).
#' @param less String to use for less.
#' @param equal String to use for equal to.  This is combined with the \code{less} or \code{greater}.
#' @return \code{f_bin} - Returns human readable intervals in word form.
#' @rdname f_bin
#' @export
f_bin_text <- function(x, greater = 'Greater than', middle = 'to',
    less = 'less than', equal = 'or equal to', ...){

    x <- as.character(unlist(x))
    check_binned(x)
    is_na <- is.na(x)

    key <- f_bins_level_replace(x)

    x <- key[['to']][match(as.character(x), key[['from']])]

    out <- unlist(lapply(strsplit(x, '\\s+'), function(y){

        prefix <- trimws(paste(greater, ifelse(grepl('=', y[2]), equal, '')))
        suffix <- trimws(paste(less, ifelse(grepl('=', y[4]), equal, '')))

        trimws(gsub('\\s+', ' ', paste(prefix, y[1], middle, suffix, y[5])))

    }))

    out[is_na] <- NA
    out
}

#' @param equal.digits logical.  If \code{TRUE} digits are given equal number of decimal places.
#' @return \code{f_bin_text_right} - Returns human readable right hand of intervals in word form.
#' @rdname f_bin
#' @export
f_bin_text_right <- function(x, l = 'up to', le = 'to', equal.digits = FALSE, ...){

    x <- as.character(unlist(x))
    check_binned(x)
    is_na <- is.na(x)

    key <- f_bins_level_replace(x)
    key[['to']] <- gsub('^<', l, gsub('^<=', le, gsub('^.+?x\\s', '', key[['to']])))

    if (isTRUE(equal.digits)) {
        numbs <- gsub('(^.+?\\s)([0-9.-]+)', '\\2', key[['to']])
        nc <- max(nchar(gsub('^-?[0-9]+(\\.|$)', '', numbs)))
        numbs <- formatC(as.numeric(numbs), format = 'f', flag='0', digits = nc)
        key[['to']] <- paste0(gsub('(^.+?\\s)([0-9.-]+)', '\\1', key[['to']]), numbs)
    }

    out <- key[['to']][match(as.character(x), key[['from']])]
    out[is_na] <- NA
    out

}

#' @return \code{f_bin_right} - Returns human readable right hand intervals in symbol form.
#' @rdname f_bin
#' @export
f_bin_right <- function(x, l = '<', le = '<=', equal.digits = FALSE, parse = FALSE, ...){

    x <- as.character(unlist(x))
    check_binned(x)
    is_na <- is.na(x)

    key <- f_bins_level_replace(x)
    key[['to']] <- gsub('^<', l, gsub('^<=', le, gsub('^.+?x\\s', '', key[['to']])))

    if (isTRUE(equal.digits)) {
        numbs <- gsub('(^.+?\\s)([0-9.-]+)', '\\2', key[['to']])
        nc <- max(nchar(gsub('^-?[0-9]+(\\.|$)', '', numbs)))
        numbs <- formatC(as.numeric(numbs), format = 'f', flag='0', digits = nc)
        key[['to']] <- paste0(gsub('(^.+?\\s)([0-9.-]+)', '\\1', key[['to']]), numbs)
    }

    x <- key[['to']][match(as.character(x), key[['from']])]

    x <- gsub(' < ', paste0(' ', l, ' '), x, fixed = TRUE)
    out <- gsub(' <= ', paste0(' ', le, ' '), x, fixed = TRUE)
    out[is_na] <- NA
    parse_bin2(out, parse = parse)
}



#' @export
#' @rdname f_bin
ff_bin <- function(l = '<', le = '<=', parse = TRUE, ...) {
    function(x) {f_bin(x, l = l, le = le, parse = parse, ...)}
}

#' @export
#' @rdname f_bin
ff_bin_text <- function(greater = 'Greater than', middle = 'to',
    less = 'less than', equal = 'or equal to', ...) {
    function(x) {f_bin_text(x, greater = greater, middle = middle, less = less, equal = equal, ...)}
}


#' @export
#' @rdname f_bin
ff_bin_right <- function(l = '<', le = '<=', equal.digits = FALSE, parse = TRUE, ...) {
    function(x) {f_bin_right(x, l = l, le = le, equal.digits = equal.digits, parse = parse, ...)}
}



#' @export
#' @rdname f_bin
ff_bin_text_right <- function(l = 'up to', le = 'to', equal.digits = FALSE, ...) {
    function(x) {f_bin_text_right(x, l = l, le = le, equal.digits = equal.digits, ...)}
}



#' @rdname f_bin
#' @export
f_interval <- f_bin

#' @rdname f_bin
#' @export
f_interval_text <- f_bin_text

#' @rdname f_bin
#' @export
f_interval_text_right <- f_bin_text_right

#' @rdname f_bin
#' @export
f_interval_right <- f_bin_right

#' @rdname f_bin
#' @export
ff_interval <- ff_bin

#' @rdname f_bin
#' @export
ff_interval_text <- ff_bin_text

#' @rdname f_bin
#' @export
ff_interval_text_right <- ff_bin_text_right

#' @rdname f_bin
#' @export
ff_interval_right <- ff_bin_right



f_bins_level_replace <- function(x, ...){

    y <- as.character(unique(x))
    is_ge <- grepl('\\[', y[1])

    intervals <- lapply(gsub('(\\]|\\))', ')', gsub('^(\\[|\\()', 'c(', y)), function(x) as.character(eval(parse(text = x))))
    pairs <- do.call(rbind, intervals)
    ##nc <- max(nchar(gsub('^-?[0-9]+(\\.|$)', '', pairs)))
    ##formatC(pairs, format = 'f', flag='0', digits = nc)

    templ <- ifelse(is_ge, '%s <= x < %s', '%s < x <= %s')

    list(from = y, to = unlist(apply(pairs, 1, function(a) sprintf(templ, a[1], a[2]))))

}
