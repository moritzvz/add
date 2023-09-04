
#' Load data
#' 
#' - Read csv file 
#' - In case of statistical parity 
#'  + check and set the outcome variable
#' - In case of equalized odds
#'  + check the prediction and ground_truth
#'  + set the error_type and outcome variables
#' - check and set the sensitive attribute variables
#' - select the outcome and sensitive attributes as columns, plus the error_type in case of equalized odds
#' - set character variables to factors
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
load_data <- function(file, outcome, prediction, ground_truth, sen_attr, psi_metric, reserved_colnames) {
  
  ### Read .csv
  dataset <- readr::read_csv(file)
  
  assertthat::assert_that(is.data.frame(dataset), 
                          msg = "readr::read_csv(file) must result in a data frame")
  
  col_names <- names(dataset)
  
  
  if (psi_metric == "statistical parity") {
    assertthat::assert_that(
      outcome %in% col_names,
      msg = paste0("The outcome_variable (", outcome, ") is not a column in the dataset"))
    
    if (outcome != "outcome") {
      dataset$outcome <- dataset[[outcome]]
    }
    
    # Optional to do: restrictions on outcome variable? Double to integer? Check for NA values?")
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
    
    # Optional to do: restrictions on prediction and ground truth variables? Double to integer? Check for NA values?")
    
    dataset <- dplyr::mutate(dataset, error_type = paste0(dplyr::if_else(ground_truth == 1, "t", "f"), 
                                                          dplyr::if_else(prediction == 1, "p", "n")))
    
    dataset <- dplyr::mutate(dataset, outcome = dplyr::if_else(ground_truth == 1, 0, 1))
  }
  
  
  if (!is.null(sen_attr)) {
    cols_sa <- sen_attr %in% col_names
    assertthat::assert_that(
      all(cols_sa),
      msg = paste0("Some sensitive_attributes are not columns in the dataset: ", 
                   paste(sen_attr[!cols_sa], collapse = ", ")))
  } else {
    
    not_sensitive_attributes <- c(outcome, "outcome", 
                                  prediction, "prediction", 
                                  ground_truth, "ground_truth",
                                  "error_type")
    
    sen_attr <- col_names[!col_names %in% not_sensitive_attributes]
    
    assertthat::assert_that(any(!sen_attr %in% reserved_colnames),
                            msg = paste("sensitive_attributes can't be any of reserved names:", 
                                        paste(reserved_colnames, collapse = ", ")))
  }
  
  cols_chr <- purrr::map_lgl(dataset, is.character)
  cols_chr <- names(cols_chr[cols_chr])
  
  
  if (psi_metric == "statistical parity") {
    dataset <- dataset[c("outcome", sen_attr)]
  } else if (psi_metric == "equalized odds") {
    dataset <- dataset[c("outcome", sen_attr, "error_type")]
  }
  
  
  # convert to factor since partykit::cforest does not allow characters
  dataset <- dplyr::mutate(dataset, dplyr::across(tidyselect::all_of(cols_chr), ~ as.factor(.)))
  
  # Optional to do: are there restrictions on sensitive attributes?")
  
  list(dataset  = dataset,
       sen_attr = sen_attr)
}


#' Split data
#' 
#' Randomly split dataset into two halves
#'
#' @param dataset prepared data frame
#'
#' @return
split_data <- function(dataset) {
  
  # perform split on data
  dataset_1_index <- sample(seq_len(nrow(dataset)),size = floor(0.5*nrow(dataset)))
  
  # return both splits
  list(dataset_1 = dataset[dataset_1_index,],
       dataset_2 = dataset[-dataset_1_index,])
}
