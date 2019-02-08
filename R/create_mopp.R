#' create mopp
#' Function to render Rmd and remove all extra files
#'
#' @param file - a name of the Rmd
#'
#' @return file.pdf
create_mopp <- function(file){
  if (is.character(file)) {
    rmarkdown::render(file, output_file = "MOPP.pdf")
    unlink("frontpage.tex")
    unlink("kappe.tex")
    unlink("article.tex")
  } else stop("The file name should be a string with .Rmd extension") 
}
