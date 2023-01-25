

#' Postprocess results
#' 
#' - Clean each group_condition
#' - Remove duplicate conditions
#' - Adjust disparity confidence
#' - Rank results by selected ranking
#'
#' @param results_df data frame, results from analyse_unique_leaves() 
#' @param ranking character(1)
#'
#' @return
postprocess_results <- function(results_df, ranking) {
  
  results_df <- dplyr::mutate(results_df, group_condition = purrr::map_chr(group_condition, clean_condition_str))
  
  results_df <- results_df[!duplicated(results_df$group_condition), ]
  
  results_df <- dplyr::mutate(results_df, disparity_confidence = stats::p.adjust(disparity_confidence, p.adjust.methods[2]))
  
  results_df <- rank_results(results_df = results_df, ranking = ranking)
  
  results_df
}



#' Clean a condition string
#'
#' @param raw_str character(1)
#'
#' @return
clean_condition_str <- function(raw_str) {
  str_splits        <- stringr::str_split(raw_str, pattern = " & ", simplify = TRUE)
  cleaned_condition <- ""
  attr_considered   <- character()
  
  for (i in length(str_splits[,]):1){
    current_attr <- stringr::word(str_splits[,i], 1)
    cont_smaller <- grepl("<=", str_splits[,i])
    cont_larger  <- grepl(">", str_splits[,i])
    if (cont_smaller) {
      current_attr <- paste(current_attr, "<")
    }
    if (cont_larger) {
      current_attr <- paste(current_attr, ">")
    }
    if (current_attr %in% attr_considered) {
      next
    }
    attr_considered   <- c(attr_considered, current_attr)
    cleaned_condition <- paste(cleaned_condition, str_splits[,i], "; ", sep = "")   
  }
  
  pats <- c('"NA", |, "NA"|"|%')
  
  cleaned_condition <- stringr::str_replace_all(cleaned_condition, pats, "")
  cleaned_condition <- stringr::str_replace_all(cleaned_condition, "c\\(", "{")
  cleaned_condition <- stringr::str_replace_all(cleaned_condition, "\\)", "}")
  cleaned_condition <- sapply(strwrap(cleaned_condition, width = 100, simplify = FALSE),
                              paste, collapse = "\n")
  
  cleaned_condition
}


#' Rank results according to either magnitude or confidence
#'
#' @param results_df data frame
#' @param ranking character(1) 'confidence' or 'magnitude'
#'
#' @return
rank_results <- function(results_df, ranking){
  if (ranking == "confidence"){
    results_df <- results_df[order(results_df$disparity_confidence),]
  } else if(ranking == "magnitude"){
    results_df <- results_df[order(-results_df$disparity_magnitude),]
  } else{
    stop(
      paste("Ranking mechanism \"", ranking, 
            "\" is not implemented. Try \"confidence\" or \"magnitude\".", sep=""))
  }
  
  results_df
}
