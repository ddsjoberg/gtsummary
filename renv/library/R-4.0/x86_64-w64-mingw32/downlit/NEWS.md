# downlit 0.2.1

* When auto-linking `vignette(foo)`, downlit now looks for a vignette named
  foo in the packages it knows to be attached (#61).

* Can now force highlighting of any `<pre>` by wrapping it inside a `<div>`
  with `class = "downlit"`. This is useful in cases where it may otherwise
  be hard to set the class of the `<pre>`.

* In comments, `\u2029` is converted to `\033` to make it possible to preserve
  ANSI escapes when passed through xml2.

* No longer errors on library calls with named but empty arguments.

# downlit 0.2.0

* Autolinking can use metadata stored in package itself with pkgdown setting
  `deploy.install_metadata`; this is useful for packages that only have 
  private websites (@matthewstrasiotto, #29)

* Autolinking guesses reference and article urls for pkgdown sites that haven't
  set url (@krlmlr, #44).

* R6 classes are autolinked when a new object is created i.e. in 
  `r6_object$new()`, `r6_object` will link to its docs (#59, @maelle). 

* R6 methods are no longer autolinked as if they were functions of the same 
  name (#54, @maelle).

* `classes_pandoc()` and `classes_chroma()` have been thoroughly revieweed to
  produce syntax highlighting as similar as possible to RStudio.

* `downlit_html_path()` has a more flexible XPath identifying R code blocks, 
  and a `classes` argument (#53, @maelle, @cderv)

* Trailing `/` are no longer stripped from URLs (#45, @krlmlr).

* Removed extra newline in `<pre>` output (#42, @krlmlr).

# downlit 0.1.0

* Added a `NEWS.md` file to track changes to the package.
