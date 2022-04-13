#' Verification of Data Input
#'
#' deside import data from file or just use a variable
#'
#' @param data_phrase string or data.frame
#'
#' @return a data.frame contained data for plot
#'
#' @examples
verify_data_input <- function(data_phrase) {
  if (is.character(data_phrase)) {
    m <- regexpr("(.csv|.tsv|.xlsx|.xls)", data_phrase)
    data <- switch(regmatches(data_phrase, m),
      ".csv" = read.csv(data_phrase),
      ".tsv" = read.csv(data_phrase, sep = "\t"),
      ".xls" = readxl::read_xls(data_phrase),
      ".xlsx" = readxl::read_xlsx(data_phrase),
    )
  } else if (is.data.frame(data_phrase)) {
    data <- data_phrase
  } else {
    message(cli::rule(
      center = crayon::bold("!!Please check your input!!")
    ))
    stop("input data error")
  }
  return(data)
}
