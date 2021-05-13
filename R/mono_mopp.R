mono_mopp <- function(){rmarkdown::pdf_document(toc = TRUE,
                         toc_depth = 4,
                         number_sections = TRUE,
                         fig_width = 6.5,
                         fig_height = 4.5,
                         fig_crop = 'auto',
                         fig_caption = TRUE,
                         dev = 'pdf',
                         df_print = "default",
                         highlight = "default",
                         template = pkg_file("rmd/mopp.tex"),
                         keep_tex = FALSE,
                         keep_md = FALSE,
                         latex_engine = "xelatex",
                         includes = NULL,
                         md_extensions = NULL,
                         output_extensions = NULL,
                         pandoc_args = NULL,
                         extra_dependencies = NULL)}
pkg_file <- function(..., package = "moppNord", mustWork = FALSE) {
  if (is.null(devtools_meta(package))) {
    system.file(..., package = package, mustWork = mustWork)
  } else {
    # used only if package has been loaded with devtools or pkgload
    file.path(getNamespaceInfo(package, "path"), "inst", ...)
  }
}

mono_mopp2 <- function(toc = TRUE,
                         toc_depth = 4,
                         number_sections = TRUE,
                         fig_width = 6.5,
                         fig_height = 4.5,
                         fig_crop = 'auto',
                         fig_caption = TRUE,
                         dev = 'pdf',
                         df_print = "default",
                         highlight = "default",
                         template = paste0(find.package("moppNord")[1], "/rmarkdown/rmd/mopp.tex"),
                         keep_tex = FALSE,
                         keep_md = FALSE,
                         latex_engine = "xelatex",
                         citation_package = c("default", "natbib", "biblatex"),
                         includes = NULL,
                         md_extensions = NULL,
                         output_extensions = NULL,
                         pandoc_args = NULL,
                         extra_dependencies = NULL) {

  # base pandoc options for all PDF output
  args <-c("--latex-engine-opt=-shell-escape")

  args <- c(args, "--self-contained")
  # table of contents
  args <- c(args, pandoc_toc_args(toc, toc_depth))

  append_in_header <- function(text, file = as_tmpfile(text)) {
    includes_to_pandoc_args(includes(in_header = file))
  }

  # template path and assets
  if (!is.null(template) && !identical(template, "default")) {
    args <- c(args, "--template", pandoc_path_arg(template))
  }

  # numbered sections
  #if (number_sections)
    args <- c(args, "--number-sections")

  # highlighting
 # if (!is.null(highlight))
 #   highlight <- match.arg(highlight, highlighters())
 # args <- c(args, pandoc_highlight_args(highlight))

  # latex engine
  latex_engine <- match.arg(latex_engine, c("pdflatex", "lualatex", "xelatex", "tectonic"))
  args <- c(args, pandoc_latex_engine_args(latex_engine))

  # citation package
  args <- c(args, citation_package_arg(citation_package))

  # content includes
  args <- c(args, includes_to_pandoc_args(includes))

  # make sure the graphics package is always loaded
  if (identical(template, "default")) args <- c(args, "--variable", "graphics")

  # args args
  args <- c(args, pandoc_args)

  saved_files_dir <- NULL

  # Use filter to set pdf geometry defaults (while making sure we don't override
  # any geometry settings already specified by the user)
  pdf_pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir,
                                output_dir) {

    # make sure --include-in-header from command line will not completely
    # override header-includes in metadata but give the latter lower precedence:
    # https://github.com/rstudio/rmarkdown/issues/1359
    args <- append_in_header(process_header_includes(metadata))

    # use a geometry filter when we are using the "default" template
    if (identical(template, "default")) {
      # set the margin to 1 inch if no geometry options or document class specified
      if (default_geometry(names(metadata), pandoc_args))
        args <- c(args, "--variable", "geometry:margin=1in")
      # support subtitle for Pandoc < 2.6
      if (("subtitle" %in% names(metadata)) && !pandoc_available("2.6")) args <- c(
        args, append_in_header(file = pkg_file("rmd/latex/subtitle.tex"))
      )
    }

    if (length(extra_dependencies) || has_latex_dependencies(knit_meta)) {
      extra_dependencies <- latex_dependencies(extra_dependencies)
      all_dependencies <- append(extra_dependencies, flatten_latex_dependencies(knit_meta))
      args <- c(args, append_in_header(latex_dependencies_as_string(all_dependencies)))
    }
    args
  }


  pre_processor <- function(metadata, input_file, runtime, knit_meta,
                                files_dir, output_dir) {
    # save files dir (for generating intermediates)
    saved_files_dir <<- files_dir

    pdf_pre_processor(metadata, input_file, runtime, knit_meta, files_dir,
                      output_dir)
  }

  intermediates_generator <- function(...) {
    general_intermediates_generator(saved_files_dir, ...)
  }

  # return format
  output_format(
    knitr = knitr_options_pdf(fig_width, fig_height, fig_crop, dev),
    pandoc = pandoc_options(
      to = paste(c("latex", output_extensions), collapse = ""),
      from = from_rmarkdown(fig_caption, md_extensions),
      args = args,
      latex_engine = latex_engine,
      keep_tex = keep_tex,
      lua_filters = pkg_file_lua(c("pagebreak.lua", "latex-div.lua"))
    ),
    clean_supporting = !keep_tex,
    keep_md = keep_md,
    df_print = df_print,
    pre_processor = pre_processor,
    intermediates_generator = intermediates_generator
  )
}
# return a string as a tempfile
as_tmpfile <- function(str) {
  if (length(str) == 0) return()
  f <- tempfile(tmpfile_pattern, fileext = ".html")
  write_utf8(str, f)
  f
}
general_intermediates_generator <- function(
  saved_files_dir, original_input, intermediates_dir
) {

  # copy all intermediates (pandoc will need to bundle them in the PDF)
  intermediates <- copy_render_intermediates(original_input, intermediates_dir, FALSE)

  # we need figures from the supporting files dir to be available during
  # render as well; if we have a files directory, copy its contents
  if (!is.null(saved_files_dir) && dir_exists(saved_files_dir)) {
    file.copy(saved_files_dir, intermediates_dir, recursive = TRUE)
    intermediates <- c(intermediates, list.files(
      path = file.path(intermediates_dir, basename(saved_files_dir)),
      all.files = TRUE, recursive = TRUE, full.names = TRUE))
  }

  intermediates
}

patch_tex_output <- function(file) {
  x <- read_utf8(file)
  if (length(i <- which(x == '\\begin{document}')) == 0) return()
  if (length(i <- grep('^\\\\date\\{', head(x, i[1]))) == 0) return()

  i <- i[1]
  # add \author{} if missing: https://github.com/jgm/pandoc/pull/5961
  if (length(grep('^\\\\author\\{', head(x, i))) == 0) {
    x <- append(x, '\\author{}', i - 1)
    i <- i + 1
  }
  # reduce the vertical spacing in \date{} if no author is given
  if (any(head(x, i) == '\\author{}')) {
    x[i] <- paste0('\\date{\\vspace{-2.5em}', sub('^\\\\date\\{', '', x[i]))
  }
  write_utf8(x, file)
}

# patch output from Pandoc < 2.8: https://github.com/jgm/pandoc/issues/5801
fix_horiz_rule <- function(file) {
  if (pandoc_available('2.8')) return()
  x <- read_utf8(file)
  i <- x == '\\begin{center}\\rule{0.5\\linewidth}{\\linethickness}\\end{center}'
  if (any(i)) {
    x[i] <- '\\begin{center}\\rule{0.5\\linewidth}{0.5pt}\\end{center}'
    write_utf8(x, file)
  }
}

process_header_includes <- function(x) {
  x <- unlist(x[["header-includes"]])
  gsub('(^|\n)\\s*```\\{=latex\\}\n(.+?\n)```\\s*(\n|$)', '\\1\\2\\3', x)
}

citation_package_arg <- function(value) {
  value <- value[1]
  if (value == "none") {
    warning("citation_package = 'none' was deprecated; please use 'default' instead.")
    value <- "default"
  }
  value <- match.arg(value, c("default", "natbib", "biblatex"))
  if (value != "default") paste0("--", value)
}

default_geometry <- function(meta_names, pandoc_args = NULL) {
  !any(c('geometry', 'documentclass') %in% meta_names) &&
    length(grep('^(--(variable|metadata)=)?documentclass:', pandoc_args)) == 0
}
