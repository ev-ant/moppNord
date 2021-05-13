mono_mopp <- function(toc = TRUE,
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
  args <- c( "--self-contained")
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

   #args <-c(args, "--pdf-engine-opt='-shell-escape'")

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
