#' Convert R Markdown to a PDF with monography formatting
#'
#' Convert R Markdown files to PDF after resolving the special tokens of
#' \pkg{bookdown} (e.g., the tokens for references and labels) to native LaTeX
#' commands.
#'
#' This function is based on \code{rmarkdown::\link{pdf_document}} (by default)
#' with better default arguments. You can also change the default format to
#' other LaTeX/PDF format functions using the \code{base_format} argument.
#'
#' The global R option \code{bookdown.post.latex} can be set to a function to
#' post-process the LaTeX output. This function takes the character vector of
#' the LaTeX output as its input argument, and should return a character vector
#' to be written to the \file{.tex} output file. This gives you full power to
#' post-process the LaTeX output.
#' @param ... Other arguments to be passed to \code{base_format}.
#' @param base_format An output format function to be used as the base format.
#' @param toc_unnumbered Whether to add unnumberred headers to the table of
#'   contents.
#' @param toc_appendix Whether to add the appendix to the table of contents.
#' @param toc_bib Whether to add the bibliography section to the table of
#'   contents.
#' @param quote_footer If a character vector of length 2 and the quote footer
#'   starts with three dashes (\samp{---}), \code{quote_footer[1]} will be
#'   prepended to the footer, and \code{quote_footer[2]} will be appended; if
#'   \code{NULL}, the quote footer will not be processed.
#' @param highlight_bw Whether to convert colors for syntax highlighting to
#'   black-and-white (grayscale).
#' @note This output format can only be used with \code{\link{render_book}()}.
#' @export
mono_mopp = function(
  pandoc_args = NULL, ...,
  base_format = rmarkdown::pdf_document, toc_unnumbered = TRUE,
  toc_appendix = TRUE, toc_bib = TRUE, quote_footer = NULL, highlight_bw = FALSE,
  template = pkg_file("rmd/mopp.tex")
) {
  config = get_base_format(base_format, list(
    toc_depth = 4, subparagraph = TRUE, template=template,
    toc = TRUE, number_sections = TRUE, fig_caption = TRUE,
    pandoc_args = pandoc_args2(pandoc_args), ...
  ))
  config$pandoc$ext = '.tex'
  post = config$post_processor  # in case a post processor have been defined
  config$post_processor = function(metadata, input, output, clean, verbose) {
    if (is.function(post)) output = post(metadata, input, output, clean, verbose)
    f = with_ext(output, '.tex')
    x = read_utf8(f)
    x = restore_block2(x, !number_sections)
    x = resolve_refs_latex(x)
    x = resolve_ref_links_latex(x)
    x = restore_part_latex(x)
    x = restore_appendix_latex(x, toc_appendix)
    if (!toc_unnumbered) x = remove_toc_items(x)
    if (toc_bib) x = add_toc_bib(x)
    if (!is.null(quote_footer)) {
      if (length(quote_footer) != 2 || !is.character(quote_footer)) warning(
        "The 'quote_footer' argument should be a character vector of length 2"
      ) else x = process_quote_latex(x, quote_footer)
    }
    if (highlight_bw) x = highlight_grayscale_latex(x)
    post = getOption('bookdown.post.latex')
    if (is.function(post)) x = post(x)
    write_utf8(x, f)
    tinytex::latexmk(
      f, config$pandoc$latex_engine,
      if ('--biblatex' %in% config$pandoc$args) 'biber' else 'bibtex'
    )

    output = with_ext(output, '.pdf')
    o = opts$get('output_dir')
    keep_tex = isTRUE(config$pandoc$keep_tex)
    if (!keep_tex) file.remove(f)
    if (is.null(o)) return(output)

    output2 = file.path(o, output)
    file.rename(output, output2)
    if (keep_tex) file.rename(f, file.path(o, f))
    output2
  }
  # always enable tables (use packages booktabs, longtable, ...)
  pre = config$pre_processor
  config$pre_processor = function(...) {
    c(
      if (is.function(pre)) pre(...), '--variable', 'tables=yes', '--standalone',
      if (rmarkdown::pandoc_available('2.7.1')) '-Mhas-frontmatter=false'
    )
  }
  config = common_format_config(config, 'latex')
  config
}
opts = knitr:::new_defaults(list(config = list()))
pandoc2.0 <- function() {
  rmarkdown::pandoc_available("2.0")
}
resolve_refs_latex = function(x) {
  # equation references \eqref{}
  x = gsub(
    '(?<!\\\\textbackslash{})@ref\\((eq:[-/:[:alnum:]]+)\\)', '\\\\eqref{\\1}', x,
    perl = TRUE
  )
  # normal references \ref{}
  x = gsub(
    '(?<!\\\\textbackslash{})@ref\\(([-/:[:alnum:]]+)\\)', '\\\\ref{\\1}', x,
    perl = TRUE
  )
  x = gsub(sprintf('\\(\\\\#((%s):[-/[:alnum:]]+)\\)', reg_label_types), '\\\\label{\\1}', x)
  x
}

resolve_ref_links_latex = function(x) {
  res = parse_ref_links(x, '^%s (.+)$')
  if (is.null(res)) return(x)
  x = res$content; txts = res$txts; i = res$matches
  # text for a tag may be wrapped into multiple lines; collect them until the
  # empty line
  for (j in seq_along(i)) {
    k = 1
    while (x[i[j] + k] != '') {
      txts[j] = paste(txts[j], x[i[j] + k], sep = '\n')
      x[i[j] + k] = ''
      k = k + 1
    }
  }
  restore_ref_links(x, '(?<!\\\\texttt{)%s', res$tags, txts, FALSE)
}

restore_part_latex = function(x) {
  r = '^\\\\(chapter|section)\\*\\{\\(PART(\\*)?\\)( |$)'
  i = grep(r, x)
  if (length(i) == 0) return(x)
  x[i] = gsub(r, '\\\\part\\2{', x[i])
  # remove (PART*) from the TOC lines for unnumbered parts
  r = '^(\\\\addcontentsline\\{toc\\}\\{)(chapter|section)(\\}\\{)\\(PART\\*\\)( |$)'
  x = gsub(r, '\\1part\\3', x)
  # for numbered parts, remove the line \addcontentsline since it is not really
  # a chapter title and should not be added to TOC
  j = grep('^\\\\addcontentsline\\{toc\\}\\{(chapter|section)\\}\\{\\(PART\\)( |$)', x)
  k = j; n = length(x)
  for (i in seq_along(j)) {
    # figure out how many lines \addcontentsline{toc} spans over (search until
    # it finds an empty line)
    l = 1
    while (j[i] + l <= n && x[j[i] + l] != '') {
      k = c(k, j[i] + l)
      l = l + 1
    }
  }
  if (length(k)) x = x[-k]
  x
}

restore_appendix_latex = function(x, toc = FALSE) {
  r = '^\\\\(chapter|section)\\*\\{\\(APPENDIX\\) .*'
  i = find_appendix_line(r, x)
  if (length(i) == 0) return(x)
  level = gsub(r, '\\1', x[i])
  brace = grepl('}}$', x[i])
  x[i] = '\\appendix'
  if (toc) x[i] = paste(
    x[i], sprintf('\\addcontentsline{toc}{%s}{\\appendixname}', level)
  )
  if (brace) x[i] = paste0(x[i], '}')  # pandoc 2.0
  if (grepl('^\\\\addcontentsline', x[i + 1])) x[i + 1] = ''
  x
}

find_appendix_line = function(r, x) {
  i = grep(r, x)
  if (length(i) > 1) stop('You must not have more than one appendix title')
  i
}

remove_toc_items = function(x) {
  r = '^\\\\addcontentsline\\{toc\\}\\{(part|chapter|section|subsection|subsubsection)\\}\\{.+\\}$'
  x[grep(r, x)] = ''
  x
}

add_toc_bib = function(x) {
  # natbib
  r = '^\\s*\\\\bibliography\\{.+\\}$'
  i = grep(r, x)
  if (length(i) != 0) {
    # natbib - add toc manually using \bibname
    # e.g adding \addcontentsline{toc}{chapter}{\bibname}
    i = i[1]
    level = if (length(grep('^\\\\chapter\\*?\\{', x))) 'chapter' else 'section'
    x[i] = sprintf('%s\n\\addcontentsline{toc}{%s}{\\bibname}', x[i], level)
  } else {
    # biblatex - add heading=bibintoc in options
    # e.g \printbibliography[title=References,heading=bibintoc]
    r = '^(\\s*\\\\printbibliography)(\\[.*\\])?$'
    i = grep(r, x)
    if (length(i) == 0) return(x)
    opts = gsub(r, "\\2", x[i])
    bibintoc = "heading=bibintoc"
    if (nzchar(opts)) {
      opts2 = gsub("^\\[(.*)\\]$", "\\1", opts)
      opts = if (!grepl("heading=", opts2)) sprintf("[%s,%s]", opts2, bibintoc)
    } else (
      opts = sprintf("[%s]", bibintoc)
    )
    x[i] = sprintf('%s%s', gsub(r, "\\1", x[i]), opts)
  }
  x
}

restore_block2 = function(x, global = FALSE) {
  i = grep('^\\\\begin\\{document\\}', x)[1]
  if (is.na(i)) return(x)
  # add the necessary definition in the preamble when block2 engine
  # (\BeginKnitrBlock) or pandoc fenced div (\begin) is used if not already
  # define. But don't do it with beamer and it defines already amsthm
  # environments.
  # An options allow external format to skip this part
  # (useful for rticles see rstudio/bookdown#1001)
  if (getOption("bookdown.theorem.preamble", TRUE) &&
      !knitr::pandoc_to("beamer") &&
      length(grep(sprintf('^\\\\(BeginKnitrBlock|begin)\\{(%s)\\}', paste(all_math_env, collapse = '|')), x)) &&
      length(grep('^\\s*\\\\newtheorem\\{theorem\\}', head(x, i))) == 0) {
    theorem_label = vapply(theorem_abbr, function(a) {
      label_prefix(a)()
    }, character(1), USE.NAMES = FALSE)
    theorem_defs = sprintf(
      '%s\\newtheorem{%s}{%s}%s', theorem_style(names(theorem_abbr)),
      names(theorem_abbr), str_trim(theorem_label),
      if (global) '' else {
        if (length(grep('^\\\\chapter[*]?', x))) '[chapter]' else '[section]'
      }
    )
    # the proof environment has already been defined by amsthm
    proof_envs = setdiff(names(label_names_math2), 'proof')
    proof_labels = vapply(proof_envs, function(a) {
      label_prefix(a, dict = label_names_math2)()
    }, character(1), USE.NAMES = FALSE)
    proof_defs = sprintf(
      '%s\\newtheorem*{%s}{%s}', theorem_style(proof_envs), proof_envs,
      gsub('^\\s+|[.]\\s*$', '', proof_labels)
    )
    x = append(x, c('\\usepackage{amsthm}', theorem_defs, proof_defs), i - 1)
  }
  # remove the empty lines around the block2 environments
  i3 = c(
    if (length(i1 <- grep(r1 <- '^(\\\\)BeginKnitrBlock(\\{)', x)))
      (i1 + 1)[x[i1 + 1] == ''],
    if (length(i2 <- grep(r2 <- '(\\\\)EndKnitrBlock(\\{[^}]+})$', x)))
      (i2 - 1)[x[i2 - 1] == '']
  )
  x[i1] = gsub(r1, '\\1begin\\2', x[i1])
  x[i2] = gsub(r2, '\\1end\\2',   x[i2])
  if (length(i3)) x = x[-i3]

  r = '^(.*\\\\begin\\{[^}]+\\})(\\\\iffalse\\{-)([-0-9]+)(-\\}\\\\fi\\{\\})(.*)$'
  if (length(i <- grep(r, x)) == 0) return(x)
  opts = sapply(strsplit(gsub(r, '\\3', x[i]), '-'), function(z) {
    intToUtf8(as.integer(z))
  }, USE.NAMES = FALSE)
  x[i] = paste0(gsub(r, '\\1', x[i]), opts, gsub(r, '\\5', x[i]))
  x
}

style_definition = c('definition', 'example', 'exercise', 'hypothesis')
style_remark = c('remark')
# which styles of theorem environments to use
theorem_style = function(env) {
  styles = character(length(env))
  styles[env %in% style_definition] = '\\theoremstyle{definition}\n'
  styles[env %in% style_remark] = '\\theoremstyle{remark}\n'
  styles
}

process_quote_latex = function(x, commands) {
  for (i in grep('^\\\\end\\{quote\\}$', x)) {
    i1 = NULL; i2 = i - 1
    k = 1
    while (k < i) {
      xk = x[i - k]
      if (grepl('^---.+', xk)) {
        i1 = i - k
        break
      }
      if (xk == '' || grepl('^\\\\begin', xk)) break
      k = k + 1
    }
    if (is.null(i1)) next
    x[i1] = paste0(commands[1], x[i1])
    x[i2] = paste0(x[i2], commands[2])
  }
  x
}

# \newenvironment{Shaded}{\begin{snugshade}}{\end{snugshade}}
# \newcommand{\KeywordTok}[1]{\textcolor[rgb]{x.xx,x.xx,x.xx}{\textbf{{#1}}}}
# \newcommand{\DataTypeTok}[1]{\textcolor[rgb]{x.xx,x.xx,x.xx}{{#1}}}
# ...
highlight_grayscale_latex = function(x) {
  i1 = grep('^\\\\newenvironment\\{Shaded\\}', x)
  if (length(i1) == 0) return(x)
  i1 = i1[1]
  r1 = '^\\\\newcommand\\{\\\\[a-zA-Z]+\\}\\[1]\\{.*\\{#1\\}.*\\}$'
  r2 = '^(.*?)([.0-9]+,[.0-9]+,[.0-9]+)(.*)$'
  i = i1 + 1
  while (grepl('^\\\\newcommand\\{.+\\}$', x[i])) {
    if (grepl(r1, x[i]) && grepl(r2, x[i])) {
      col = as.numeric(strsplit(gsub(r2, '\\2', x[i]), ',')[[1]])
      x[i] = gsub(
        r2, paste0('\\1', paste(round(rgb2gray(col), 2), collapse = ','), '\\3'),
        x[i]
      )
    }
    i = i + 1
  }
  x
}

# https://en.wikipedia.org/wiki/Grayscale
rgb2gray = function(x, maxColorValue = 1) {
  rep(sum(c(.2126, .7152, .0722) * x/maxColorValue), 3)
}

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
