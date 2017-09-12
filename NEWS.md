NEWS
====

Versioning
----------

Releases will be numbered with the following semantic versioning format:

&lt;major&gt;.&lt;minor&gt;.&lt;patch&gt;

And constructed with the following guidelines:

* Breaking backward compatibility bumps the major (and resets the minor
  and patch)
* New additions without breaking backward compatibility bumps the minor
  (and resets the patch)
* Bug fixes and misc changes bumps the patch


numform 0.3.1 -
----------------------------------------------------------------

**BUG FIXES**

* `f_12_hour` did not format correctly if the vector contained missing (`NA`)
  values; see <a href="https://github.com/trinker/numform/issues/12">issue #12</a>.

**NEW FEATURES**

* `f_year`, `f_12_hour`, `f_month`, and `f_weekday` (methods based functions)
  did not previously have closure, function retuning, forms (i.e., `ff_` form)
  making them inconstent in usage.  These functions all have a closure, function
  retuning, form.

* `f_quarter` added for working with business quarters.

**MINOR FEATURES**

* `f_wrap` picks up an `equal.lines` argument to add additional lines to make
  each element have the same number of new line characters.  This is useful for
  legend spacing.

**IMPROVEMENTS**

**CHANGES**




numform 0.0.6 - 0.3.0
----------------------------------------------------------------

**NEW FEATURES**

* `f_affirm` and `f_logical` added to convert logical and dummy values into
  yes/No or True/False strings respectively.

* alignment` added to easily align columns in table formatting functions from
  add-on R packages which may be confused by **numform**'s conversion of numeric
  to character class, though right table alignment should be maintained.

* `f_date` and `f_12_hour` added to convert dates and times into common in-text
  form.

* `f_replace` added as a ggplot2 scale `gsub` convenience function.  Defaults
  to replace '_' with ' '.

* `f_title` added as a ggplot2 scale `tools::toTitleCase` convenience function.

* `f_wrap` added as a ggplot2 scale `strwrap` + `paste(collapse =TRUE)`
  convenience function.

* `f_state` added for converting state names to their abbreviated form for plots.

* `f_year` added as a ggplot2 scale to convert to 2 digit year form convenience
  function.

* `f_abbreviation` added for converting string names to their abbreviated form.

* constants added for weekdays, months, and month abbreviations in order to make
  factor conversion of levels easier.  See `?constant_months` for more.

**MINOR FEATURES**

* `f_sign` picks up `negative` and `positive` assignments allowing for more
  control in format (previously +/- were assigned).  This enables the ability to
  give other characters tailored for document formats such as html, LaTeX, etc.

* `f_prop2percent` and `f_percent` pick up a logical `less_than_replace`
  argument that replaces values lower than this with a less than sign and the
  cut point specified by the `digits` argument.

**CHANGES**

* `f_denom` now returns `x` if the `max` &lt; 1K.



numform 0.0.1 - 0.0.5
----------------------------------------------------------------

Format numbers for publication; includes the removal of leading zeros,
standardization of number of digits, addition of affixes, and a p-value
formatter.  These tools combine the functionality of several 'base' functions
such as paste(), format(), and sprintf() into specific use case functions that
are named in a way that is consistent with usage, making their names easy to
remember and easy to deploy.