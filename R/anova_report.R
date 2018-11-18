#' @title Anova Report
#' @description Example of generating an HTML report from R function
#' @details
#' \code{anova_report} generates an HTML report describing a one-way analysis of variance.
#' The grouping variable is coerced to be a factor.
#' @param data frame
#' @param x grouping variable
#' @param y dependent variable
#' @import ggplot2
#' @import ggridges
#' @import formattable
#' @import knitr
#' @export
#' @examples
#' data(mpg, package="ggplot2")
#' anova_report(mpg, class, cty)
#' anova_report(mpg, class, cty, browse=FALSE)
#' anova_report(mtcars, cyl, mpg)

anova_report <- function(data, x, y, browse=TRUE){
  data <- deparse(substitute(data))
  x <- deparse(substitute(x))
  y <- deparse(substitute(y))

  # read template file and modify
  report <- readLines(system.file("template.txt", package = "anovaReport"))
  report <- gsub("xxxIV", x, report, fixed = TRUE)
  report <- gsub("xxxDV", y, report, fixed = TRUE)
  report <- gsub("xxxDATA", data, report, fixed = TRUE)

  # output template and render
  tf <- tempfile(fileext = ".Rmd")
  to <- tempfile(fileext = ".html")
  writeLines(report, tf)
  library(rmarkdown)
  render(input=tf, output_format="html_document", output_file=to)
  if (browse){
    file.show(to)
  } else {
     viewer <- getOption("viewer")
     viewer(to)
  }

}
