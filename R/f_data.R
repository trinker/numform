#' Convert and Abbreviate Units of Data.
#'
#' Convert numeric data to shorter form with unit abbreviations attached.  For
#' example, move from  10,000,000,000 (Bytes) to 10GB (Gigabytes) instead.
#'
#' @param x A vector of data units.
#' @param binary logical.  If \code{TRUE} the result uses binary conversion,
#' otherwise decimal conversion is used.
#' See \url{https://en.wikipedia.org/wiki/Binary_prefix} for additional
#' information on standards.
#' @param digits The number of digits to round to. .
#' @param pad.char A character to use for leading padding if lengths of output
#' are unequal.  Use \code{NA} to forgo padding.
#' @param less.than.replace logical.  If \code{TRUE} values lower than lowest
#' place value will be replaced with a less than sign followed by the
#' \code{integer} representation of the place value.  For example, if \code{"0GB"}
#' then replacement will be \code{"<1GB"}.
#' @param sep The separator to use between the number and data unit abbreviation.
#' @param mix.units logical.  If \code{TRUE} then units can be mixed.
#' Typically, this is not a good idea for the sake of comparison.  It is most
#' useful when there is a total row which is a sum of the column and this value's
#' unit exceeds the unit of the rest of the column.
#' @param from The starting unit.  Typically, this is assumed to be 'Bytes' ('B').
#' Must be one of c("Bit", "Byte", "Kilobyte", "Megabyte", "Gigabyte", "Terabyte",
#' "Petabyte", "Exabyte", "Zettabyte", "Yottabyte") or c("b", "B", "KB", "MB",
#' "GB", "TB", "PB", "EB", "ZB", "YB").  These are case sensitive.
#' @param \ldots ignored.
#' @param to The units to convert to.  See the \code{from} parameter for accepted
#' units.
#' @param suffix A suffix to use for the units at the end of the numeric string.
#' Typically the user will not interact with this argument.  Meant for internal
#' modularity of functions.
#' @return Returns a converted and abbreviated vector of units of data.
#' @export
#' @rdname f_data
#' @examples
#' \dontrun{
#' x <- c(NA, '3', '-', -233456789, -2334567890, 10^(0:10))
#' f_data(x)
#' f_data(x, pad.char = NA)
#' f_data(x, mix.units = TRUE)
#' f_data(x, mix.units = TRUE, binary = TRUE)
#' f_data(x, mix.units = TRUE, binary = TRUE, digits = 2)
#' f_byte(100000000, from = 'GB', binary = TRUE)
#' f_giga(10000000000)
#' f_giga(10000000000, suffix = 'Gb')
#'
#' library(tidyverse)
#' set.seed(15)
#' dat <- data_frame(
#'     bytes = round(rnorm(7, 1e7, 7.95e6), 0),
#'     days = constant_weekdays %>%
#'         as_factor()
#' )
#'
#' dat %>%
#'     mutate(
#'         data = f_data(bytes, less.than.replace = TRUE),
#'         weekday = f_weekday(days, distinct = TRUE) %>%
#'             as_factor()
#'     )
#'
#' dat %>%
#'     mutate(days = days %>% as_factor()) %>%
#'     ggplot(aes(days, bytes, group = 1)) +
#'         geom_line() +
#'         geom_point() +
#'         scale_y_continuous(labels = f_data) +
#'         scale_x_discrete(labels = ff_weekday(distinct = TRUE))
#' }
f_data <- function(x, binary = FALSE, digits = 0, pad.char = ' ',
    less.than.replace = FALSE, sep = '', mix.units = FALSE, from = 'B', ...) {

    ## convert to bits
    x <- f_byte_conversion(as.numeric(x), from = from, to = 'b')
    from <- 'b'

    if (mix.units) {

        chars <- nchar(drop_sci_not(abs(x)))
        is_na <- is.na(x)
        chars[is_na] <- 1

        to <- data_conversion[['Abbreviation']][unlist(lapply(chars, function(x) max(which(x > data_conversion[['Chars']]))))]

        out <- unlist(Map(function(y, u){

            if (is.na(y)) return(NA)

            f_data_default(
                x = y, to = u, binary = binary, digits = digits,
                suffix =  f_data_abbreviation(u),
                from = from, sep = sep
            )

        }, x, to))

        if (!is.na(pad.char)) {

            chars <- nchar(out)
            chars <- max(chars, na.rm = TRUE) - chars
            out <- paste0(unlist(lapply(chars, function(x) {
                    if (is.na(x)) return('')
                    paste(rep(pad.char, x), collapse = '')
                })), out)

        }

        out[is_na] <- NA

        out

    } else {
        chars <- nchar(drop_sci_not(max(abs(rm_na(x)))))
        to <- data_conversion[['Abbreviation']][max(which(chars > data_conversion[['Chars']]))]

        f_data_default(x = x, to = to, binary = binary, digits = digits, suffix =  f_data_abbreviation(to),
            pad.char = pad.char, less.than.replace = less.than.replace, from = from, sep = sep
        )

    }

}


data_conversion <- read.table(text =
"Unit    Abbreviation    Binary     Decimal   Step  Chars
Bit      b     		      1           1      0      0
Byte     B	   			8           8      1      1
Kilobyte KB 	         1024        1000      2      4
Megabyte MB			   1024        1000      3      7
Gigabyte GB			   1024        1000      4     10
Terabyte TB			   1024        1000      5     13
Petabyte PB			   1024        1000      6     16
Exabyte  EB			   1024        1000      7     19
Zettabyte ZB		   1024        1000      8     22
Yottabyte  YB		   1024        1000      9     25",
    header = TRUE,
    stringsAsFactors = FALSE
)


f_byte_conversion <- function(x, from, to, binary = FALSE, ..){

    tocol <- ifelse(nchar(to) > 2, 'Unit', "Abbreviation")
    if (!to %in% data_conversion[[tocol]]) error_data_conv('to')

    fromcol <- ifelse(nchar(from) > 2, 'Unit', "Abbreviation")
    if (!from %in% data_conversion[[fromcol]]) error_data_conv('from')

    convcol <- ifelse(binary, 'Binary', 'Decimal')

    fromloc <- match(from, data_conversion[[fromcol]])
    toloc <- match(to, data_conversion[[tocol]])

    if (fromloc == toloc) return(x)
    if (fromloc > toloc) dirfun <- `*` else dirfun <- `/`


    convfactor <- prod(data_conversion[[convcol]][(fromloc:toloc)[-which.min(fromloc:toloc)]])

    dirfun(x, convfactor)

}

error_data_conv <- function(var){

    stop(sprintf(
        '`%s` must match one of:\n\n    (%s)\n\n -OR-    \n\n    (%s) ',
        var,
        paste(shQuote(data_conversion[['Unit']]), collapse = ', '),
        paste(shQuote(data_conversion[['Abbreviation']]), collapse = ', ')
    ), call. = FALSE)

}


f_data_default <- function(x, to, binary = FALSE, digits = 0,
    suffix = f_data_abbreviation(to),
    pad.char = ' ', less.than.replace = FALSE, from = 'B', sep = '', ...) {

    x <- as.numeric(x)
    nas <- is.na(x)

    out <- f_byte_conversion(x, to = to, from = from, binary = binary)
    out <- f_num(out, digits = digits, s = paste0(sep, suffix))


    if (isTRUE(less.than.replace)){
        out <- gsub(
            '(^\\.?0+)([^0-9].*$)',
            paste0('<', gsub('.$', '1', f_num(0, digits = digits)), '\\2'),
            out
        )
    }

    if (!is.na(pad.char)) {

        chars <- nchar(out)
        chars <- max(chars, na.rm = TRUE) - chars
        out <- paste0(unlist(lapply(chars, function(x) {
                if (is.na(x)) return('')
                paste(rep(pad.char, x), collapse = '')
            })), out)

    }

    out[nas] <- NA

    out
}


#binary_factors <- decimal_factors <- rep(NA, 10)
#for (i in seq_len(nrow(data_conversion))){
#
#    if (i == 1) {
#        binary_factors[i] <- decimal_factors[i] <- 1
#    } else {
#        binary_factors[i] <- binary_factors[i - 1] * data_conversion[['Binary']][i]
#        decimal_factors[i] <- decimal_factors[i - 1] * data_conversion[['Decimal']][i]
#    }
#
#}

drop_sci_not <- function(x, ...){

    x <- as.character(as.numeric(x))
    locs <- grepl('e\\+', x)
    x[locs] <- paste0(
        gsub('e\\+.+', '', x[locs]),
        unlist(lapply(gsub('^.+?e\\+', '', x[locs]), function(x){paste(rep('0', x), collapse = '')}))
    )
    x
}


#' @export
#' @include utils.R
#' @rdname f_data
ff_data <- functionize(f_data)



#' @description \code{f_byte} - Force the abbreviation to bytes
#' unit (B).
#' @export
#' @include utils.R
#' @rdname f_data
f_byte <- hijack(f_data_default, to = 'B')

#' @export
#' @include utils.R
#' @rdname f_data
ff_byte <- functionize(f_byte)




#' @description \code{f_kilo} - Force the abbreviation to kilobytes
#' unit (KB).
#' @export
#' @include utils.R
#' @rdname f_data
f_kilo <- hijack(f_data_default, to = 'KB')

#' @export
#' @include utils.R
#' @rdname f_data
ff_kilo <- functionize(f_kilo)




#' @description \code{f_mega} - Force the abbreviation to megabytes
#' unit (MB).
#' @export
#' @include utils.R
#' @rdname f_data
f_mega <- hijack(f_data_default, to = 'MB')

#' @export
#' @include utils.R
#' @rdname f_data
ff_mega <- functionize(f_mega)




#' @description \code{f_giga} - Force the abbreviation to gigabytes
#' unit (GB).
#' @export
#' @include utils.R
#' @rdname f_data
f_giga <- hijack(f_data_default, to = 'GB')

#' @export
#' @include utils.R
#' @rdname f_data
ff_giga <- functionize(f_giga)




#' @description \code{f_tera} - Force the abbreviation to terabytes
#' unit (TB).
#' @export
#' @include utils.R
#' @rdname f_data
f_tera <- hijack(f_data_default, to = 'TB')

#' @export
#' @include utils.R
#' @rdname f_data
ff_tera <- functionize(f_tera)




#' @description \code{f_peta} - Force the abbreviation to petabytes
#' unit (PB).
#' @export
#' @include utils.R
#' @rdname f_data
f_peta <- hijack(f_data_default, to = 'PB')

#' @export
#' @include utils.R
#' @rdname f_data
ff_peta <- functionize(f_peta)




#' @description \code{f_exa} - Force the abbreviation to exabytes
#' unit (EB).
#' @export
#' @include utils.R
#' @rdname f_data
f_exa <- hijack(f_data_default, to = 'EB')

#' @export
#' @include utils.R
#' @rdname f_data
ff_exa <- functionize(f_exa)




#' @description \code{f_zetta} - Force the abbreviation to zettabytes
#' unit (ZB).
#' @export
#' @include utils.R
#' @rdname f_data
f_zetta <- hijack(f_data_default, to = 'ZB')

#' @export
#' @include utils.R
#' @rdname f_data
ff_zetta <- functionize(f_zetta)




#' @description \code{f_yotta} - Force the abbreviation to yottabytes
#' unit (YB).
#' @export
#' @include utils.R
#' @rdname f_data
f_yotta <- hijack(f_data_default, to = 'YB')

#' @export
#' @include utils.R
#' @rdname f_data
ff_yotta <- functionize(f_yotta)


#' Convert Data (byte) Labels to an Abbreviated Form
#'
#' Convert a data label such as Gigabyte to an abbreviated form
#' like 'GB'.
#'
#' @param x A vector of data labels.  One of:
#" c("Bit", "Byte", "Kilobyte", "Megabyte", "Gigabyte", "Terabyte",
#' "Petabyte", "Exabyte", "Zettabyte", "Yottabyte") ignoring case or
#' retaining c("b", "B", "KB", "MB", "GB", "TB", "PB", "EB", "ZB", "YB")
#' with proper case.
#' @param \ldots ignored.
#' @return Returns avector of abbreviated data units.
#' @export
#' @examples
#' x <- c("Exabyte", "terabyte", "ZB", "PetaByte", "KB", "byte", "Gigabyte",
#' "Bit", "GB", "b")
#'
#' f_data_abbreviation(x)
f_data_abbreviation <- function(x, ...){

    out <- data_conversion[['Abbreviation']][match(tolower(x), tolower(data_conversion[['Unit']]))]
    out[is.na(out)] <- data_conversion[['Abbreviation']][match(x[is.na(out)], data_conversion[['Abbreviation']])]
    out
}





## to <- 'GB'
## x <- c(NA, '3', '-', -233456789, -2334567890, 10^(0:10))
##
##
## sprintf("%.100f", f_byte_conversion(1, from = 'GB', to = 'B'))
## sprintf("%.100f", f_byte_conversion(x, 'B', 'GB'))
## sprintf("%.100f", f_byte_conversion(x, 'B', 'GB', binary = TRUE))
## sprintf("%.100f", f_byte_conversion(x, from = 'B', to = 'b'))
##
## f_data_default(x, 'GB')
## f_data_default(x, 'GB', less.than.replace = TRUE)
## f_data_default(x, 'GB', sep = ' ')
## f_data_default(x, 'GB', less.than.replace = TRUE, sep = ' ')
## f_data_default(x, 'GB', less.than.replace = TRUE, sep = ' ', pad.char = ' ')
##
## hijack <- numform:::hijack
## functionize <- numform:::functionize
## hijack(f_data_default, to = 'GB')
## rm_na <- numform:::rm_na


##===============================================================================
## ## USED TO MAKE THE f_xxx & ff_xxx in a modular way
## cat(sprintf(
## paste0(
## "#' @description \\code{f_%s} - Force the abbreviation to %ss\n#' unit (%s).\n#' @export\n#' @include utils.R\n#' @rdname f_data\nf_%s <- hijack(f_data_default, to = '%s')\n\n",
##
## "#' @export\n#' @include utils.R\n#' @rdname f_data\nff_%s <- functionize(f_%s)\n\n\n\n"
##
## ),
##
## tolower(gsub('byte', '',  data_conversion[[1]])),
## tolower(data_conversion[[1]]),
## data_conversion[[2]],
## tolower(gsub('byte', '',  data_conversion[[1]])),
## data_conversion[[2]],
## tolower(gsub('byte', '',  data_conversion[[1]])),
## tolower(gsub('byte', '',  data_conversion[[1]]))
##
## ), sep='\n', file = 'clipboard')

