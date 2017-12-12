#' Format Degrees (e.g., Temperature, Coordinates)
#'
#' Format numbers into degree format for strings, text, titles, and scales.
#'
#' @param x A vector of values.
#' @param type One of \code{c('scale', 'text', 'title', 'string')}:
#' \describe{
#'   \item{scale}{To be used for \pkg{ggplot2} scales (i.e., axis or legend)}
#'   \item{text}{To be used for \pkg{ggplot2} text (i.e., geom_text, annotate; note that \code{parse = TRUE} must be set}
#'   \item{title}{To be used for \pkg{ggplot2} titles (e.g., main title, axis title, legend title); ignores \code{x} values}
#'   \item{string}{To be used for plain text, especially table formatting and allows control over the degree symbol used}
#' }
#' @param digits The number of digits to use.  Defaults to 1.  Can be set
#' globally via: \code{options(numformdigits = n)} where n is the number of
#' digits beyond the decimal point to include.
#' @param prefix A prefix to use before the parenthesis + units when
#' \code{type = 'title'}.
#' @param suffix logical.  If \code{TRUE} a suffix will be added corresponding
#' to the \code{measure}:
#' \describe{
#'   \item{celcius}{A capital C will be used}
#'   \item{fahrenheit}{A capital F will be used}
#'   \item{longitude}{Capital W and E will be used}
#'   \item{latitude}{Capital S and N will be used}
#' }
#' @param absolute.value logical.  If \code{TRUE} the absolute value of \code{x}
#' will be used.  This is useful for coordinates when E/W or N/S indicate direction.
#' @param symbol A symbol to use for degree when \code{type = 'string'}.
#' @param \ldots ignored.
#' @param measure One of \code{c('fahrenheit', 'celcius', 'C', 'F', 'longitude',
#' 'latitude')}.  There are functions by these names (e.g., \code{f_celcius}) but
#' not C or F.  These functions may be clearer than using \code{f_degree} and
#' then specifying \code{measure}.
#' @note Note that this function differs a bit from other \code{f_} functions
#' in that in needs a \code{type}.  This is because other \code{f_} functions
#' return a plain text representation that is generalizable across usages (titles,
#' tables, axis, geom_text, etc).  This function has notation that requires
#' special parsing by various usages hence requiring the \code{type} argument.
#' @return Returns number string(s) with degree symbols.
#' @export
#' @rdname f_degree
#' @examples
#' ## used for ggplot2 axis.text & legend scale
#' f_celcius(37, type = 'scale')
#'
#' ## used for ggplot2 geom_text
#' f_celcius(37, type = 'text')
#'
#' ## used for ggplot2 titles
#' f_celcius(prefix = "My Title",  type = 'title')
#'
#' ## used for table and string formatting
#' f_celcius(37, type = 'string')
#' f_celcius(37, type = 'string', symbol = '\\textdegree')  # LaTeX
#'
#' \dontrun{
#' if (!require("pacman")) install.packages("pacman")
#' pacman::p_load(tidyverse, maps, viridis, mapproj)
#'
#' states <- map_data("state")
#' arrests <- USArrests
#' names(arrests) <- tolower(names(arrests))
#' arrests$region <- tolower(rownames(USArrests))
#' choro <- merge(states, arrests, sort = FALSE, by = "region")
#' choro <- choro[order(choro$order), ]
#'
#' ggplot(choro, aes(long, lat)) +
#'     geom_polygon(aes(group = group, fill = assault)) +
#'     coord_map("albers",  at0 = 45.5, lat1 = 29.5) +
#'     scale_y_continuous(labels = f_latitude) +
#'     scale_x_continuous(labels = f_longitude)
#'
#' ggplot(choro, aes(long, lat)) +
#'     geom_polygon(aes(group = group, fill = assault)) +
#'     coord_map("albers",  at0 = 45.5, lat1 = 29.5) +
#'     scale_y_continuous(labels = ff_latitude(suffix = FALSE)) +
#'     scale_x_continuous(labels = ff_longitude(suffix = FALSE))
#'
#'
#' world <- map_data(map="world")
#'
#' ggplot(world, aes(map_id = region, x = long, y = lat)) +
#'     geom_map(map = world, aes(map_id = region), fill = "grey40",
#'         colour = "grey70", size = 0.25) +
#'     scale_y_continuous(labels = f_latitude) +
#'     scale_x_continuous(labels = f_longitude)
#'
#'
#' data_frame(
#'     Event = c('freezing water', 'room temp', 'body temp', 'steak\'s done',
#'         'hamburger\'s done', 'boiling water'),
#'     F = c(32, 70, 98.6, 145, 160, 212)
#' ) %>%
#'     mutate(
#'         C = (F - 32) * (5/9),
#'         Event = f_title(Event),
#'         Event = factor(Event, levels = unique(Event))
#'     ) %>%
#'     ggplot(aes(Event, F, fill = F)) +
#'         geom_col() +
#'         geom_text(aes(y = F + 4, label = f_fahrenheit(F, digits = 1, type = 'text')),
#'             parse = TRUE, color = 'grey60') +
#'         scale_y_continuous(
#'             labels = f_fahrenheit, limits = c(0, 220), expand = c(0, 0),
#'             sec.axis = sec_axis(trans = ~(. - 32) * (5/9), labels = f_celcius,
#'             name = f_celcius(prefix = 'Temperature ', type = 'title'))
#'         ) +
#'         scale_x_discrete(labels = ff_replace(pattern = ' ', replacement = '\n')) +
#'         scale_fill_viridis(option =  "magma", labels = f_fahrenheit, name = NULL) +
#'         theme_bw() +
#'         labs(
#'             y = f_fahrenheit(prefix = 'Temperature ', type = 'title'),
#'             title = f_fahrenheit(prefix = 'Temperature of Common Events ', type = 'title')
#'         ) +
#'         theme(
#'             axis.ticks.x = element_blank(),
#'             panel.border = element_rect(fill = NA, color = 'grey80'),
#'             panel.grid.minor.x = element_blank(),
#'             panel.grid.major.x = element_blank()
#'         )
#'
#'
#' data_frame(
#'     Event = c('freezing water', 'room temp', 'body temp', 'steak\'s done',
#'         'hamburger\'s done', 'boiling water', 'sun surface', 'lighting'),
#'     F = c(32, 70, 98.6, 145, 160, 212, 9941, 50000)
#' ) %>%
#'     mutate(
#'         Event = f_title(Event),
#'         C = (F - 32) * (5/9)
#'     ) %>%
#'     mutate(
#'         F = f_degree(F, measure = 'F', type = 'string'),
#'         C = f_degree(C, measure = 'C', type = 'string', zero = '0.0')
#'     )  %>%
#'     data.frame(stringsAsFactors = FALSE, check.names = FALSE) %>%
#'     pander::pander(split.tables = Inf, justify = alignment(.))
#' }
f_fahrenheit <- function(x,
    digits = getOption("numformdigits"),
    prefix = NULL,
    suffix = TRUE,
    absolute.value = suffix,
    type = 'scale',
    symbol = '&deg;',
    ...
){
    f_degree(x = x, measure = 'F', digits = digits, prefix = prefix, suffix = suffix,
        absolute.value = absolute.value, type = type, symbol = symbol, ...)
}

#' @export
#' @rdname f_degree
f_celcius <- function(x,
    digits = getOption("numformdigits"),
    prefix = NULL,
    suffix = TRUE,
    absolute.value = suffix,
    type = 'scale',
    symbol = '&deg;',
    ...
){
    f_degree(x = x, measure = 'C', digits = digits, prefix = prefix, suffix = suffix,
        absolute.value = absolute.value, type = type, symbol = symbol, ...)
}

#' @export
#' @rdname f_degree
f_longitude <- function(x,
    digits = getOption("numformdigits"),
    prefix = NULL,
    suffix = TRUE,
    absolute.value = suffix,
    type = 'scale',
    symbol = '&deg;',
    ...
){
    f_degree(x = x, measure = 'longitude', digits = digits, prefix = prefix, suffix = suffix,
        absolute.value = absolute.value, type = type, symbol = symbol, ...)
}

#' @export
#' @rdname f_degree
f_latitude <- function(x,
    digits = getOption("numformdigits"),
    prefix = NULL,
    suffix = TRUE,
    absolute.value = suffix,
    type = 'scale',
    symbol = '&deg;',
    ...
){
    f_degree(x = x, measure = 'latitude', digits = digits, prefix = prefix, suffix = suffix,
        absolute.value = absolute.value, type = type, symbol = symbol, ...)
}

#' @export
#' @rdname f_degree
f_degree <- function(x,
    type = c('scale', 'text', 'scale','title', 'string'),
    digits = getOption("numformdigits"),
    prefix = NULL,
    suffix = TRUE,
    absolute.value = suffix,
    symbol = '&deg;',
    measure = c('fahrenheit', 'celcius', 'C', 'F', 'longitude', 'latitude'),
    ...
){

    if (is.null(digits)) digits <- 1

    switch(type,
        scale = {f_degree.scale(x, measure = measure, digits = digits, prefix = prefix,
                    suffix = suffix, absolute.value = absolute.value, type = type,
                    symbol = symbol, ...
        )},
        text = {f_degree.text(x, measure = measure, digits = digits, prefix = prefix,
                    suffix = suffix, absolute.value = absolute.value, type = type,
                    symbol = symbol, ...
        )},
        title = {f_degree.title(x, measure = measure, digits = digits, prefix = prefix,
                    suffix = suffix, absolute.value = absolute.value, type = type,
                    symbol = symbol, ...
        )},
        string = {f_degree.string(x, measure = measure, digits = digits, prefix = prefix,
                    suffix = suffix, absolute.value = absolute.value, type = type,
                    symbol = symbol, ...
        )},
        stop("`type` must be one of `c('scale', 'text', 'title', 'string')`")

    )

}



f_degree.scale <- function(x,
    type = c('scale', 'text', 'scale','title', 'string'),
    digits = getOption("numformdigits"),
    prefix = NULL,
    suffix = TRUE,
    absolute.value = suffix,
    symbol = '&deg;',
    measure = c('fahrenheit', 'celcius', 'C', 'F', 'longitude', 'latitude'),
    ...
){

    nas <- is.na(x)
    if (measure == 'C') measure <- 'celcius'
    if (measure == 'F') measure <- 'fahrenheit'

    suf <- make_suffix(measure, suffix)

    if (absolute.value) {a <- function(x, ...) {f_num(abs(x), digits = digits, ...)}} else {a <- function(x, ...) {f_num(c(x), digits = digits, ...)}}

    switch(measure,
        fahrenheit = {
            out <- unlist(lapply(x, function(y) parse(text = paste0(a(y, ...), "^o", suf))))
        },
        celcius = {
            out <- unlist(lapply(x, function(y) parse(text = paste0(a(y, ...), "^o", suf))))
        },
        longitude = {
            out <- unlist(lapply(x, function(y) {
                ifelse(y < 0, parse(text = paste0(a(y, ...), "^o", suf[1])), ifelse(y > 0, parse(text = paste0(a(y, ...), "^o", suf[2])), parse(text = paste0(a(y, ...), "^o"))))
            }))
        },

        latitude = {
            out <- unlist(lapply(x, function(y) {
                ifelse(y < 0, parse(text = paste0(a(y, ...), "^o", suf[1])), ifelse(y > 0, parse(text = paste0(a(y, ...), "^o", suf[2])), parse(text = paste0(a(y, ...), "^o"))))
            }))
        }
    )

    out[nas] <- NA
    out
}

f_degree.text <- function(x,
    type = c('scale', 'text', 'scale','title', 'string'),
    digits = getOption("numformdigits"),
    prefix = NULL,
    suffix = TRUE,
    absolute.value = suffix,
    symbol = '&deg;',
    measure = c('fahrenheit', 'celcius', 'C', 'F', 'longitude', 'latitude'),
    ...
){

    nas <- is.na(x)
    if (measure == 'C') measure <- 'celcius'
    if (measure == 'F') measure <- 'fahrenheit'

    suf <- make_suffix(measure, suffix)

    if (absolute.value) {a <- function(x, ...) {f_num(abs(x), digits = digits, ...)}} else {a <- function(x, ...) {f_num(c(x), digits = digits, ...)}}

    switch(measure,
        fahrenheit = {
            out <- unlist(lapply(x, function(y) {paste0(a(y, ...), "^o", suf)}))
        },
        celcius = {
            out <- unlist(lapply(x, function(y) {paste0(a(y, ...), "^o", suf)}))
        },
        longitude = {
            out <- unlist(lapply(x, function(y) {
                ifelse(y < 0, paste0(a(y, ...), "^o", suf[1]), ifelse(y > 0, paste0(a(y, ...), "^o", suf[2]), paste0(a(y, ...), "^o")))
            }))
        },

        latitude = {
            out <- unlist(lapply(x, function(y) {
                ifelse(y < 0, paste0(a(y, ...), "^o", suf[1]), ifelse(y > 0, paste0(a(y, ...), "^o", suf[2]), paste0(a(y, ...), "^o")))
            }))
        }
    )

    out[nas] <- NA
    out
}

f_degree.title <- function(x,
    type = c('scale', 'text', 'scale','title', 'string'),
    digits = getOption("numformdigits"),
    prefix = NULL,
    suffix = TRUE,
    absolute.value = suffix,
    symbol = '&deg;',
    measure = c('fahrenheit', 'celcius', 'C', 'F', 'longitude', 'latitude'),
    ...
){
    if (measure == 'C') measure <- 'celcius'
    if (measure == 'F') measure <- 'fahrenheit'

    suf <- make_suffix(measure, suffix)

    parse(text=paste0('`', prefix, '`(', 'degree', suf, ')'))

}

f_degree.string <- function(x,
    type = c('scale', 'text', 'scale','title', 'string'),
    digits = getOption("numformdigits"),
    prefix = NULL,
    suffix = TRUE,
    absolute.value = suffix,
    symbol = '&deg;',
    measure = c('fahrenheit', 'celcius', 'C', 'F', 'longitude', 'latitude'),
    ...
){

    nas <- is.na(x)
    if (measure == 'C') measure <- 'celcius'
    if (measure == 'F') measure <- 'fahrenheit'

    suf <- gsub('^\\*', '', make_suffix(measure, suffix))
    if (absolute.value) {a <- function(x, ...) {f_num(abs(x), digits = digits, ...)}} else {a <- function(x, ...) {f_num(c(x), digits = digits, ...)}}

    switch(measure,
        fahrenheit = {
            out <- paste0(a(x, ...), symbol, suf)
        },
        celcius = {
            out <- paste0(a(x, ...), symbol, suf)
        },
        longitude = {
            out <- ifelse(x < 0, paste0(a(x, ...), symbol, suf[1]), ifelse(x > 0, paste0(a(x, ...), symbol, suf[2]), paste0(a(x, ...), symbol)))

        },
        latitude = {
            out <- ifelse(x < 0, paste0(a(x, ...), symbol, suf[1]), ifelse(x > 0, paste0(a(x, ...), symbol, suf[2]), paste0(a(x, ...), symbol)))

        }
    )

    out[nas] <- NA
    out

}



make_suffix <- function(measure, suffix, ...){

    switch(measure,
        fahrenheit = {
            suf <- "*F"
        },
        celcius = {
            suf <- "*C"
        },
        longitude = {
            suf <- c("*W", "*E")
        },
        latitude = {
            suf <- c("*S", "*N")
        },
        stop("`measure` muste be one of `c('fahrenheit', 'celcius', 'C', 'F', 'longitude', 'latitude')`")
    )

    if (!isTRUE(suffix)) {
        if (length(suf) == 2) {
            suf <- c('', '')
        } else {
            suf <- ''
        }
    }

    suf
}


#' @export
#' @include utils.R
#' @rdname f_degree
ff_degree <- functionize(f_degree)


#' @export
#' @include utils.R
#' @rdname f_degree
ff_celcius <- functionize(f_celcius)

#' @export
#' @include utils.R
#' @rdname f_degree
ff_fahrenheit <- functionize(f_fahrenheit)

#' @export
#' @include utils.R
#' @rdname f_degree
ff_longitude <- functionize(f_longitude)

#' @export
#' @include utils.R
#' @rdname f_degree
ff_latitude <- functionize(f_latitude)

