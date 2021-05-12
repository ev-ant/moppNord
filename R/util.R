pkg_file <- function(..., package = "thesisNord", mustWork = FALSE) {
  if (is.null(devtools_meta(package))) {
    system.file(..., package = package, mustWork = mustWork)
  } else {
    # used only if package has been loaded with devtools or pkgload
    file.path(getNamespaceInfo(package, "path"), "inst", ...)
  }
}

get_base_format = function(format, options = list()) {
  if (is.character(format)) format = eval(parse(text = format))
  if (!is.function(format)) stop('The output format must be a function')
  # make sure named elements in `options` have corresponding named arguments in
  # the format function, unless the function has the ... argument
  nms = names(formals(format))
  if (!('...' %in% nms)) options = options[names(options) %in% c(nms, '')]
  do.call(format, options)
}
devtools_meta <- function(package) {
  ns <- .getNamespace(package)
  ns[[".__DEVTOOLS__"]]
}
read_utf8 <- function(file) {
  if (inherits(file, 'connection')) con <- file else {
    con <- base::file(file, encoding = 'UTF-8'); on.exit(close(con), add = TRUE)
  }
  enc2utf8(readLines(con, warn = FALSE))
}
write_utf8 <- function(text, con, ...) {
  opts <- options(encoding = "native.enc"); on.exit(options(opts), add = TRUE)
  writeLines(enc2utf8(text), con, ..., useBytes = TRUE)
}
common_format_config = function(
  config, format, file_scope = getOption('bookdown.render.file_scope', FALSE)
) {

  # provide file_scope if requested
  if (file_scope) config$file_scope = md_chapter_splitter

  # prepend the custom-environment filter
  config$pandoc$lua_filters = c(
    lua_filter("custom-environment.lua"), config$pandoc$lua_filters
  )
  # and add bookdown metadata file for the filter to work
  config$pandoc$args = c(bookdown_yml_arg(), config$pandoc$args)

  # set output format
  config$bookdown_output_format = format

  # use labels of the form (\#label) in knitr
  config$knitr$opts_knit$bookdown.internal.label = TRUE
  # when the output is LaTeX, force LaTeX tables instead of default Pandoc tables
  # http://tex.stackexchange.com/q/276699/9128
  config$knitr$opts_knit$kable.force.latex = TRUE
  config
}
md_chapter_splitter = function(file) {
  x = read_utf8(file)

  # get positions of the chapter delimiters (r_chap_pattern defined in html.R)
  if (length(pos <- grep(r_chap_pattern, x)) <= 1) return()
  pos = c(0, pos)

  # get the filenames
  names = gsub(r_chap_pattern, '\\1', x[pos])

  # extract the chapters and pair them w/ the names
  lapply(seq_along(names), function(i) {
    i1 = pos[i] + 1
    i2 = pos[i + 1]
    list(name = names[i], content = x[i1:i2])
  })
}
ua_filter = function (filters = NULL) {
  rmarkdown::pkg_file_lua(filters, package = 'bookdown')
}
# pass _bookdown.yml to Pandoc's Lua filters
bookdown_yml_arg = function(config = load_config(), path = tempfile()) {
  # this is supported for Pandoc >= 2.3 only
  if (!rmarkdown::pandoc_available('2.3') || length(config) == 0) return()
  yaml::write_yaml(list(bookdown = config), path)
  c("--metadata-file", rmarkdown::pandoc_path_arg(path))
}
load_config = function() {
  if (length(opts$get('config')) == 0 && file.exists('_bookdown.yml')) {
    # store the book config
    opts$set(config = rmarkdown:::yaml_load_file('_bookdown.yml'))
  }
  opts$get('config')
}
lua_filter = function (filters = NULL) {
  rmarkdown::pkg_file_lua(filters, package = 'bookdown')
}
r_chap_pattern = '^<!--chapter:end:(.+)-->$'
pandoc_args2 = function(args) {
  if (pandoc2.0() && !length(grep('--wrap', args))) c('--wrap', 'preserve', args) else args
}
label_names = list(fig = 'Figure ', tab = 'Table ', eq = 'Equation ')
theorem_abbr = c(
  theorem = 'thm', lemma = 'lem', corollary = 'cor', proposition = 'prp', conjecture = 'cnj',
  definition = 'def', example = 'exm', exercise = 'exr', hypothesis = 'hyp'
)
# numbered math environments
label_names_math = setNames(list(
  'Theorem ', 'Lemma ', 'Corollary ', 'Proposition ', 'Conjecture ', 'Definition ', 'Example ', 'Exercise ',
  'Hypothesis '
), theorem_abbr)
# unnumbered math environments
label_names_math2 = list(proof = 'Proof. ', remark = 'Remark. ', solution = 'Solution. ')

label_names = c(label_names, label_names_math)

# types of labels currently supported, e.g. \(#fig:foo), \(#tab:bar)
label_types = names(label_names)
reg_label_types = paste(label_types, collapse = '|')
# compatibility with bookdown <= 0.4.7: ex was the prefix for Example; now it's exm
reg_label_types = paste(reg_label_types, 'ex', sep = '|')
reg_ref_links = '(\\(ref:[-/[:alnum:]]+\\))'
parse_ref_links = function(x, regexp) {
  r = sprintf(regexp, reg_ref_links)
  if (length(i <- grep(r, x)) == 0) return()
  tags = gsub(r, '\\1', x[i])
  txts = gsub(r, '\\2', x[i])
  if (any(k <- duplicated(tags))) {
    warning('Possibly duplicated text reference labels: ', paste(tags[k], collapse = ', '))
    k = !k
    tags = tags[k]
    txts = txts[k]
    i = i[k]
  }
  x[i] = ''
  list(content = x, tags = tags, txts = txts, matches = i)
}

restore_ref_links = function(x, regexp, tags, txts, alt = TRUE) {
  r = sprintf(regexp, reg_ref_links)
  m = gregexpr(r, x, perl = TRUE)
  tagm = regmatches(x, m)
  for (i in seq_along(tagm)) {
    tag = tagm[[i]]
    if (length(tag) == 0) next
    k = match(tag, tags)
    tag[!is.na(k)] = txts[na.omit(k)]
    if (alt && is_img_line(x[i])) tag = strip_html(tag)
    tagm[[i]] = tag
  }
  regmatches(x, m) = tagm
  x
}
all_math_env = c(names(theorem_abbr), names(label_names_math2))
