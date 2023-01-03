
#' Load data
#' 
#' - Read csv file 
#' - Assertions:
#'  + outcome
#'  + sen_attr
#'
#' @param file character(1)
#' @param outcome character(1)
#' @param sen_attr character(1)
#'
#' @return
#' @export
#'
#' @examples
load_data <- function(file, outcome, sen_attr) {
  
  ### Read .csv
  dataset <- readr::read_csv(file)
  
  assertthat::assert_that(is.data.frame(dataset), 
                          msg = "readr::read_csv(file) must result in a data frame")
  
  col_names <- names(dataset)
  
  
  ### Data assertions: outcome
  assertthat::assert_that(
    outcome %in% col_names,
    msg = paste0("The outcome_variable (", outcome, ") is not a column in the dataset"))
  
  warning("TO DO: restrictions on outcome variable? Double to integer? Check for NA values?")
  
  
  ### Data assertions: sen_attr
  if (!is.null(sen_attr)) {
    cols_sa <- sen_attr %in% col_names
    assertthat::assert_that(
      all(cols_sa),
      msg = paste0("Some sensitive_attributes are not columns in the dataset: ", 
                   paste(sen_attr[!cols_sa], collapse = ", ")))
    
    dataset <- dataset[c(outcome, sen_attr)]
  } else {
    sen_attr <- col_names[col_names != outcome]
    
    assertthat::assert_that(any(sen_attr != "leaves_"),
                            msg = "sensitive_attributes can't contain 'leaves_'")
  }
  
  cols_chr <- purrr::map_lgl(dataset, is.character)
  cols_chr <- names(cols_chr[cols_chr])
  
  # partykit::cforest doesn't allow characters, they have to be factors
  dataset <- dplyr::mutate(dataset, dplyr::across(cols_chr, ~ as.factor(.)))
  warning("TO DO: restrictions on sensitive attributes?")
  
  
  ### Data assertions: 
  warning("TO DO: restrictions on other variables?")
  
  dataset
}