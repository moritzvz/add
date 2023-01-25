
#' ALD Audit
#' 
#' The ALD audit:
#' \itemize{
#'   \item is performed on a dataset of your choice, the dataset must be provided as a .csv file
#'   \item requires notion of fairness to be set to 'statistical parity' or 'equalized odds'
#'   \item in case of 'statistical parity' you must set the outcome_variable argument to the name of the outcome variable in your dataset
#'   \item for 'equalized odds' you must set the prediction_variable and ground_truth_variable arguments to the names of the prediction and ground truth variables in your dataset
#'   \item by default all other variables (not outcome, prediction, ground truth) in you dataset will be used as sensitive attributes in the audit. You can use the sensitive_attributes argument to specifically set the sensitive attributes to a subset of your dataset varaiables
#'   \item requires a ranking mechanism which must be 'confidence' or 'magnitude'
#'   \item requires a maximum number of groups in the report (n_grp)
#'   \item requires a number of trees to model in partykit::cforest (ntree)
#'   \item requires a alpha argument passed to partykit::cforest (alpha)
#'   \item optionally takes a random seed number that can be used for reproducibility of results
#'   \item writes a report to the directory that you set with the dir argument, with data_name argument used in the name
#' }
#' 
#' @param file .csv file that holds the data to conduct the audit on, character(1) 
#' @param outcome_variable name of the outcome variable in your data file as character(1) or NULL (by default). Only in case notion_of_fairness is 'statistical parity' do you require to give an outcome variable 
#' @param prediction_variable name of the prediction variable in your data file as character(1) or NULL (by default). Only in case notion_of_fairness is 'equalised odds' do you require to give an prediction variable 
#' @param ground_truth_variable name of the ground truth variable in your data file as character(1) or NULL (by default). Only in case notion_of_fairness is 'equalised odds' do you require to give an ground truth variable 
#' @param sensitive_attributes names of the sensitive attribute variables in your data file as character string or NULL (by default) t use all variables that are not the outcome or prediction and ground truth variables
#' @param notion_of_fairness notion of fairness must be 'statistical parity' or 'equalized odds'
#' @param ranking_mechanism ranking mechanism must be 'confidence' or 'magnitude'
#' @param n_grp maximum number of groups in the report, numeric(1)
#' @param ntree number of trees to model in partykit::cforest, numeric(1)
#' @param alpha alpha argument passed to partykit::cforest, numeric(1)
#' @param seed random seed number can be used for reproducibility of results, numeric(1)
#' @param data_name name used in the file name of the resulted report character(1)
#' @param dir directory to which to write the report
#'
#' @return pdf report
#' @export
#'
#' @examples
ald_audit <- function(file,
                      outcome_variable = NULL,
                      prediction_variable = NULL,
                      ground_truth_variable = NULL,
                      sensitive_attributes = NULL,
                      notion_of_fairness,
                      ranking_mechanism,
                      data_name = NULL,
                      dir = here::here(""),
                      n_grp,
                      ntree,
                      alpha,
                      seed = NULL) {
  
  reserved_colnames <- c("leaves_", "leaf_indicator_", "outcome", "prediction", "ground_truth", "error_type")
  
  ### Assert inputs
  assertthat::assert_that(is.character(file), length(file) == 1,
                          msg = "file must be character(1)")
  assertthat::assert_that(tolower(stringr::str_sub(file, -4, -1)) == ".csv",
                          msg = "file must be a .csv")
  assertthat::assert_that(file.exists(file),
                          msg = "file doesn't exist")
  
  
  assertthat::assert_that(is.character(notion_of_fairness), length(notion_of_fairness) == 1,
                          msg = "notion_of_fairness must be character(1)")
  assertthat::assert_that(notion_of_fairness %in% c("statistical parity", "equalized odds"),
                          msg = "notion_of_fairness must be 'statistical parity' or 'equalized odds'")
  
  if (notion_of_fairness == "statistical parity") {
    assertthat::assert_that(is.character(outcome_variable), length(outcome_variable) == 1,
                            msg = "outcome_variable must be character(1)")
    
    # Only the outcome variable can be named 'outcome'
    if (outcome_variable != "outcome") {
      assertthat::assert_that(!outcome_variable %in% reserved_colnames,
                            msg = paste("outcome_variable can't be any of reserved names:", 
                                        paste(reserved_colnames, collapse = ", ")))
    }
  } else if (notion_of_fairness == "equalized odds") {
    assertthat::assert_that(is.character(prediction_variable), length(prediction_variable) == 1,
                            msg = "prediction_variable must be character(1)")
    
    # Only the prediction variable can be named 'prediction'
    if (prediction_variable != "prediction") {
      assertthat::assert_that(!prediction_variable %in% reserved_colnames,
                              msg = paste("prediction_variable can't be any of reserved names:", 
                                          paste(reserved_colnames, collapse = ", ")))
    }
    
    assertthat::assert_that(is.character(ground_truth_variable), length(ground_truth_variable) == 1,
                            msg = "ground_truth_variable must be character(1)")
    
    # Only the ground_truth variable can be named 'ground_truth'
    if (ground_truth_variable != "ground_truth") {
      assertthat::assert_that(!ground_truth_variable %in% reserved_colnames,
                              msg = paste("ground_truth_variable can't be any of reserved names:", 
                                          paste(reserved_colnames, collapse = ", ")))
    }
  }
  
  if (!is.null(sensitive_attributes)) {
    assertthat::assert_that(is.character(sensitive_attributes), length(sensitive_attributes) == 1,
                            msg = "sensitive_attributes must be character(1)")
    
    assertthat::assert_that(any(!sensitive_attributes %in% reserved_colnames),
                            msg = paste("sensitive_attributes can't be any of reserved names:", 
                                        paste(reserved_colnames, collapse = ", ")))
  }
  
  assertthat::assert_that(is.character(ranking_mechanism), length(ranking_mechanism) == 1,
                          msg = "ranking_mechanism must be character(1)")
  assertthat::assert_that(ranking_mechanism %in% c("confidence", "magnitude"),
                          msg = "ranking_mechanism must be 'confidence' or 'magnitude'")
  
  # Optional to do: set limits for n_grp, n_tree and alpha
  assertthat::assert_that(is.numeric(n_grp), length(n_grp) == 1,
                          msg = "n_grp must be numeric(1)")
  
  assertthat::assert_that(is.numeric(ntree), length(ntree) == 1,
                          msg = "ntree must be numeric(1)")
  
  assertthat::assert_that(is.numeric(alpha), length(alpha) == 1,
                          msg = "alpha must be numeric(1)")
  
  if (is.null(data_name)) {
    data_name <- tools::file_path_sans_ext(basename(file))
  } else {
    assertthat::assert_that(is.character(data_name), length(data_name) == 1,
                            msg = "data_name must be character(1)")
  }
  
  assertthat::assert_that(is.character(dir), length(dir) == 1,
                          msg = "dir must be character(1)")
  
  
  ### Rename such that we can easily change function argument names
  # file       <- file
  outcome      <- outcome_variable
  prediction   <- prediction_variable
  ground_truth <- ground_truth_variable
  sen_attr     <- sensitive_attributes
  psi_metric   <- notion_of_fairness
  ranking      <- ranking_mechanism
  # n_grp      <- n_grp
  # ntree      <- ntree
  # alpha      <- alpha
  
  
  ### Load data
  data_load <- load_data(file              = file, 
                         outcome           = outcome, 
                         prediction        = prediction,
                         ground_truth      = ground_truth,
                         sen_attr          = sen_attr, 
                         psi_metric        = psi_metric,
                         reserved_colnames = reserved_colnames)
  
  # In case sen_attr were set to NULL the data load process determined the sensitive attributes
  sen_attr <- data_load$sen_attr
  dataset  <- data_load$dataset
  
  ### Fit random forest of conditional inference trees
  cforest_formula <- stats::as.formula(paste0("outcome ~ ", paste(sen_attr, collapse = " + ")))
  
  # # random seed
  if (!is.null(seed)) {
    assertthat::assert_that(is.numeric(seed), !is.na(seed), msg = "Argument seed must be a valid integer")
    set.seed(seed)
  }
  
  # Train on a fixed data set, because later apply_split needs to know variables and positions!
  partitioning <- partykit::cforest(formula = cforest_formula,
                                    data    = dataset, 
                                    ntree   = ntree, 
                                    alpha   = alpha)
  
  ### Analyse unique leaves
  results_df <- analyse_unique_leaves(partitioning = partitioning,
                                      dataset      = dataset,
                                      psi_metric   = psi_metric,
                                      ntree        = ntree)
  
  
  ### Post process
  results_df <- postprocess_results(results_df = results_df, ranking = ranking)
  
  
  ### Generate report
  export_audit_report(results_df   = results_df,
                      n_grp        = n_grp, 
                      sen_attr     = sen_attr,
                      psi_metric   = psi_metric, 
                      ranking      = ranking, 
                      partitioning = partitioning, 
                      dataset      = dataset, 
                      dir          = dir,
                      data_name    = data_name)
}




