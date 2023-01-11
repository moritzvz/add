
#' Load data
#' 
#' - Read csv file 
#' - Assertions:
#'  + outcome
#'  + sen_attr
#'
#' @param file character(1)
#' @param outcome character(1)
#' @param prediction character(1)
#' @param ground_truth character(1)
#' @param sen_attr character()
#' @param psi_metric character(1) 
#' @param reserved_colnames character() 
#'
#' @return
#' @export
#'
#' @examples
load_data <- function(file, outcome, prediction, ground_truth, sen_attr, psi_metric, reserved_colnames) {
  
  ### Read .csv
  dataset <- readr::read_csv(file)
  
  assertthat::assert_that(is.data.frame(dataset), 
                          msg = "readr::read_csv(file) must result in a data frame")
  
  col_names <- names(dataset)
  
  
  ### Data assertions: outcome
  if (psi_metric == "statistical parity") {
    assertthat::assert_that(
      outcome %in% col_names,
      msg = paste0("The outcome_variable (", outcome, ") is not a column in the dataset"))
    
    if (outcome != "outcome") {
      dataset$outcome <- dataset[[outcome]]
    }
    
    warning("TO DO: restrictions on outcome variable? Double to integer? Check for NA values?")
  } else if (psi_metric == "equalized odds") {
    assertthat::assert_that(
      prediction %in% col_names,
      msg = paste0("The prediction variable (", prediction, ") is not a column in the dataset"))
    
    if (prediction != "prediction") {
      dataset$prediction <- dataset[[prediction]]
    }
    
    assertthat::assert_that(
      ground_truth %in% col_names,
      msg = paste0("The ground truth variable (", ground_truth, ") is not a column in the dataset"))
    
    if (ground_truth != "ground_truth") {
      dataset$ground_truth <- dataset[[ground_truth]]
    }
    
    warning("TO DO: restrictions on prediction and ground truth variables? Double to integer? Check for NA values?")
    
    dataset <- dplyr::mutate(dataset, error_type = paste0(dplyr::if_else(ground_truth == 1, "t", "f"), 
                                                          dplyr::if_else(prediction == 1, "p", "n")))
    
    dataset <- dplyr::mutate(dataset, outcome = dplyr::if_else(ground_truth == 1, 0, 1))
  }
  
  ### Data assertions: sen_attr
  if (!is.null(sen_attr)) {
    cols_sa <- sen_attr %in% col_names
    assertthat::assert_that(
      all(cols_sa),
      msg = paste0("Some sensitive_attributes are not columns in the dataset: ", 
                   paste(sen_attr[!cols_sa], collapse = ", ")))
    
    # Keep all columns
    # dataset <- dataset[c(outcome, sen_attr)]
  } else {
    sen_attr <- col_names[col_names != outcome]
    
    assertthat::assert_that(any(!sen_attr %in% reserved_colnames),
                            msg = paste("sensitive_attributes can't be any of reserved names:", 
                                        paste(reserved_colnames, collapse = ", ")))
  }
  
  cols_chr <- purrr::map_lgl(dataset, is.character)
  cols_chr <- names(cols_chr[cols_chr])
  
  # partykit::cforest doesn't allow characters, they have to be factors
  dataset <- dplyr::mutate(dataset, dplyr::across(tidyselect::all_of(cols_chr), ~ as.factor(.)))
  warning("TO DO: restrictions on sensitive attributes?")
  
  
  ### Data assertions: 
  warning("TO DO: restrictions on other variables?")
  
  dataset
}

