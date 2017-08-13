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


numform 0.0.6 -
----------------------------------------------------------------

**BUG FIXES**

**NEW FEATURES**

* `f_affirm` and `f_logical` added to convert logical and dummy values into
  yes/No or True/False strings respectively.

* alignment` added to easily align columns in table formatting functions from
  add-on R packages which may be confused by **numform**'s conversion of numeric
  to character class, though right table alignment should be maintained.

* `f_date` and `f_12_hour` added to convert dates and times into common in-text
  form.

**MINOR FEATURES**

* `f_sign` picks up `negative` and `positive` assignments allowing for more
  control in format (previously +/- were assigned).  This enables the ability to
  give other characters tailored for document formats such as html, LaTeX, etc.

IMPROVEMENTS

**CHANGES**

numform 0.0.1 - 0.0.5
----------------------------------------------------------------

Format numbers for publication; includes the removal of leading zeros,
standardization of number of digits, addition of affixes, and a p-value
formatter.  These tools combine the functionality of several 'base' functions
such as paste(), format(), and sprintf() into specific use case functions that
are named in a way that is consistent with usage, making their names easy to
remember and easy to deploy.