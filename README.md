numform
============


[![Build
Status](https://travis-ci.org/trinker/numform.svg?branch=master)](https://travis-ci.org/trinker/numform)
[![Coverage
Status](https://coveralls.io/repos/trinker/numform/badge.svg?branch=master)](https://coveralls.io/r/trinker/numform?branch=master)
<a href="https://img.shields.io/badge/Version-0.0.1-orange.svg"><img src="https://img.shields.io/badge/Version-0.0.1-orange.svg" alt="Version"/></a>
</p>
**numform** contains tools to assist in the formatting of numbers for
publication. Tools include the removal of leading zeros, standardization
of number of digits, and a p-value formatter.


Table of Contents
============

-   [[Installation](#installation)](#[installation](#installation))
-   [[Contact](#contact)](#[contact](#contact))
-   [[Available Functions](#available-functions)](#[available-functions](#available-functions))
-   [[Demonstration](#demonstration)](#[demonstration](#demonstration))
    -   [[Load Packages](#load-packages)](#[load-packages](#load-packages))
    -   [[Numbers](#numbers)](#[numbers](#numbers))
    -   [[Abbreviated Numbers](#abbreviated-numbers)](#[abbreviated-numbers](#abbreviated-numbers))
    -   [[Commas](#commas)](#[commas](#commas))
    -   [[Percents](#percents)](#[percents](#percents))
    -   [[Dollars](#dollars)](#[dollars](#dollars))
    -   [[Plotting](#plotting)](#[plotting](#plotting))
    -   [[Modeling](#modeling)](#[modeling](#modeling))

Installation
============


To download the development version of **numform**:

Download the [zip
ball](https://github.com/trinker/numform/zipball/master) or [tar
ball](https://github.com/trinker/numform/tarball/master), decompress and
run `R CMD INSTALL` on it, or use the **pacman** package to install the
development version:

    if (!require("pacman")) install.packages("pacman")
    pacman::p_load_current_gh("trinker/numform")
    pacman::p_load(tidyverse)

Contact
=======

You are welcome to: 
* submit suggestions and bug-reports at: <https://github.com/trinker/numform/issues> 
* send a pull request on: <https://github.com/trinker/numform> 
* compose a friendly e-mail to: <tyler.rinker@gmail.com>


Available Functions
===================

Below is a table of available **numform** functions. Note that `f_` is
read as "format" whereas `fv_` is read as "format vector". The former
formats individual values in the vector while the latter uses the vector
to compute a calculation on each of the values and then formats them.  Additionally, all **numform** non-methods functions have a functional return version that is prefixed with an additional `f`.  For example, `f_num` has `ff_num` which has the same arguments but returns a function instead.  This is useful for passing in to **ggplot2** `scale_x/y_type` functions (see [[Plotting](#plotting)](#[plotting](#plotting)) for usage).

<!-- html table generated in R 3.3.2 by xtable 1.8-2 package -->
<!-- Thu Dec 22 11:39:56 2016 -->
<table>
<tr>
<td>
f_affix
</td>
<td>
f_mills
</td>
<td>
f_parenthesis
</td>
<td>
f_sign
</td>
<td>
fv_percent
</td>
</tr>
<tr>
<td>
f_bills
</td>
<td>
f_month
</td>
<td>
f_percent
</td>
<td>
f_suffix
</td>
<td>
fv_percent_diff
</td>
</tr>
<tr>
<td>
f_comma
</td>
<td>
f_num
</td>
<td>
f_prefix
</td>
<td>
f_thous
</td>
<td>
</td>
</tr>
<tr>
<td>
f_dollar
</td>
<td>
f_num_percent
</td>
<td>
f_prop2percent
</td>
<td>
f_weekday
</td>
<td>
</td>
</tr>
<tr>
<td>
f_mean_sd
</td>
<td>
f_ordinal
</td>
<td>
f_pval
</td>
<td>
fv_num_percent
</td>
<td>
</td>
</tr>
</table>
<p class="caption">
<b><em>Available Formatting Functions</em></b>
</p>
Demonstration
=============

Load Packages
-------------

    if (!require("pacman")) install.packages("pacman")
    pacman::p_load_gh("trinker/numform")
    pacman::p_load(dplyr)

Numbers
-------

    f_num(c(0.0, 0, .2, -00.02, 1.122222, pi, "A"))

    ## Warning in f_num(c(0, 0, 0.2, -0.02, 1.122222, pi, "A")): NAs introduced by
    ## coercion

    ## [1] ".0"  ".0"  ".2"  "-.0" "1.1" "3.1" NA

Abbreviated Numbers
-------------------

    f_thous(1234)

    ## [1] "1K"

    f_thous(12345)

    ## [1] "12K"

    f_thous(123456)

    ## [1] "123K"

    f_mills(1234567)

    ## [1] "1M"

    f_mills(12345678)

    ## [1] "12M"

    f_mills(123456789)

    ## [1] "123M"

    f_bills(1234567891)

    ## [1] "1B"

    f_bills(12345678912)

    ## [1] "12B"

    f_bills(123456789123)

    ## [1] "123B"

Commas
------

    f_comma(c(1234.12345, 1234567890, .000034034, 123000000000, -1234567))

    ## [1] "1,234.123"       "1,234,567,890"   ".000034034"      "123,000,000,000"
    ## [5] "-1,234,567"

Percents
--------

    f_percent(c(30, 33.45, .1), 1)

    ## [1] "30.0%" "33.5%" ".1%"

    f_percent(c(0.0, 0, .2, -00.02, 1.122222, pi))

    ## [1] ".0%"  ".0%"  ".2%"  "-.0%" "1.1%" "3.1%"

    f_prop2percent(c(.30, 1, 1.01, .33, .222, .01))

    ## [1] "30.0%"  "100.0%" "101.0%" "33.0%"  "22.2%"  "1.0%"

Dollars
-------

    f_dollar(c(0, 30, 33.45, .1))

    ## [1] "$0.00"  "$30.00" "$33.45" "$0.10"

    f_dollar(c(0.0, 0, .2, -00.02, 1122222, pi)) %>% 
        f_comma()

    ## [1] "$0.00"         "$0.00"         "$0.20"         "$-.02"        
    ## [5] "$1,122,222.00" "$3.14"

Plotting
--------

    library(tidyverse)

    data_frame(
        revenue = rnorm(10000, 500000, 50000),
        date = sample(seq(as.Date('1999/01/01'), as.Date('2000/01/01'), by="day"), 10000, TRUE),
        site = sample(paste("Site", 1:5), 10000, TRUE)
    ) %>%
        mutate(
            dollar = f_dollar(revenue, digits = -3),
            thous = f_thous(revenue),
            thous_dollars = f_thous(revenue, prefix = '$'),
            abb_month = f_month(date),
            abb_week = factor(f_weekday(date, distinct = TRUE), levels = c('Su', 'M', 'T', 'W', 'Th', 'F', 'S'))
        ) %T>%
        print() %>%
        ggplot(aes(abb_week, revenue)) +
            geom_jitter(width = .2, height = 0, alpha = .2) +
            scale_y_continuous(label = ff_thous(x, prefix = '$'))+
            facet_wrap(~site) +
            theme_bw()

    ## # A tibble: 10,000 × 8
    ##     revenue       date   site  dollar thous thous_dollars abb_month
    ##       <dbl>     <date>  <chr>   <chr> <chr>         <chr>     <chr>
    ## 1  452330.0 1999-10-15 Site 1 $452000  452K         $452K         O
    ## 2  593915.8 1999-08-15 Site 3 $594000  594K         $594K         A
    ## 3  413368.6 1999-04-17 Site 1 $413000  413K         $413K         A
    ## 4  530987.9 1999-09-24 Site 3 $531000  531K         $531K         S
    ## 5  501330.1 1999-02-28 Site 4 $501000  501K         $501K         F
    ## 6  481456.4 1999-05-23 Site 2 $481000  481K         $481K         M
    ## 7  441079.1 1999-08-29 Site 4 $441000  441K         $441K         A
    ## 8  549676.1 1999-01-11 Site 1 $550000  550K         $550K         J
    ## 9  496630.2 1999-11-12 Site 4 $497000  497K         $497K         N
    ## 10 393438.1 1999-09-17 Site 2 $393000  393K         $393K         S
    ## # ... with 9,990 more rows, and 1 more variables: abb_week <fctr>

![](inst/figure/unnamed-chunk-10-1.png)

Modeling
--------

We can see its use in actual model reporting as well:

    mod1 <- t.test(1:10, y = c(7:20))

    sprintf(
        "t = %s (%s)",
        f_num(mod1$statistic),
        f_pval(mod1$p.value)
    )

    ## [1] "t = -5.4 (p < .05)"

    mod2 <- t.test(1:10, y = c(7:20, 200))

    sprintf(
        "t = %s (%s)",
        f_num(mod2$statistic, 2),
        f_pval(mod2$p.value, digits = 2)
    )

    ## [1] "t = -1.63 (p = .12)"

We can build a function to report model statistics:

    report <- function(mod, stat = NULL, digits = c(0, 2, 2)) {
        
        stat <- if (is.null(stat)) stat <- names(mod[["statistic"]])
        sprintf(
            "%s(%s) = %s, %s", 
            gsub('X-squared', '&Chi;<sup>2</sup>', stat),
            paste(f_num(mod[["parameter"]], digits[1]), collapse = ", "),
            f_num(mod[["statistic"]], digits[2]),
            f_pval(mod[["p.value"]], digits = digits[3])
        )

    }

    report(mod1)

    ## [1] "t(22) = -5.43, p < .05"

    report(oneway.test(count ~ spray, InsectSprays))

    ## [1] "F(5, 30) = 36.07, p < .05"

    report(chisq.test(matrix(c(12, 5, 7, 7), ncol = 2)))

    ## [1] "&Chi;<sup>2</sup>(1) = .64, p = .42"

This enables in-text usage as well. First set up the models in a code
chunk:

    mymod <- oneway.test(count ~ spray, InsectSprays)
    mymod2 <- chisq.test(matrix(c(12, 5, 7, 7), ncol = 2))

And then use <code class="r">`` `r report(mymod)` ``</code> resulting in
a report that looks like this: F(5, 30) = 36.07, p &lt; .05. For
&Chi;<sup>2</sup> using proper HTML leads to &Chi;<sup>2</sup>(1) = .64,
p = .42.
