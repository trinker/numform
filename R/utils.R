## Hijack a function
## see: http://stackoverflow.com/a/25366322/1000343
hijack <- function(FUN, ...){

    .FUN <- FUN

    args <- list(...)
    invisible(lapply(seq_along(args), function(i) {
        formals(.FUN)[[names(args)[i]]] <<- args[[i]]
    }))

    .FUN
}


## Hijack a function
functionize <- function(FUN, ...){

    function(...){
        hijack(FUN, ...)
    }
}

## remove NA elements
rm_na <- function(x) {
    x[!is.na(x)]
}


## determine if month char vector is month, month_abbreviated, or other (NA)
check_month_type <- function(x, threshold = .6, ...){
    if (mean(as.character(rm_na(x)) %in% mnthqrt) > threshold) {
        return('month')
    }
    if (mean(as.character(rm_na(x)) %in% mnthqrt2) > threshold) {
        return('month_abbreviated')
    }
    return(NA)
}

mnthqrt <- month.name
names(mnthqrt) <- rep(1:4, each = 3)

mnthqrt2 <- month.abb
names(mnthqrt2) <- rep(1:4, each = 3)

mnthqrt3 <- 1:12
names(mnthqrt3) <- rep(1:4, each = 3)


na_omit <- function(x, ...){
    x[!is.na(x)]
}



drop_sci_note <- function(x, ...){

    if (!is.numeric(x)) return(x)

    x <- as.character(as.numeric(x))

    locs <- grepl('e\\+', x)

    x[locs] <- unlist(Map(function(b, e) {

            subs <- nchar(gsub('^.*\\.', '', b))

            paste0(gsub('[.]', '', b), paste(rep('0', e - subs), collapse = ''))

        }, gsub('e\\+.+', '', x[locs]), as.integer(gsub('^.+?e\\+', '', x[locs]))
    ))


    x
}
