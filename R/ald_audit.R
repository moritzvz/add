
#' ALD Audit
#'
#' @param file 
#' @param outcome_variable 
#' @param sensitive_attributes 
#' @param notion_of_fairness 
#' @param ranking_mechanism 
#' @param n_grp 
#' @param ntree 
#' @param alpha 
#'
#' @return
#' @export
#'
#' @examples
ald_audit <- function(file,
                      outcome_variable,
                      sensitive_attributes = NULL,
                      notion_of_fairness,
                      ranking_mechanism,
                      n_grp,
                      ntree,
                      alpha) {
  
  
  ### Assert inputs
  assertthat::assert_that(is.character(file), length(file) == 1,
                          msg = "file must be character(1)")
  assertthat::assert_that(tolower(stringr::str_sub(file, -4, -1)) == ".csv",
                          msg = "file must be a .csv")
  assertthat::assert_that(file.exists(file),
                          msg = "file doesn't exist")
  
  assertthat::assert_that(is.character(outcome_variable), length(outcome_variable) == 1,
                          msg = "outcome_variable must be character(1)")
  assertthat::assert_that(outcome_variable != "leaves_",
                          msg = "outcome_variable can't be 'leaves_'")
  
  
  if (!is.null(sensitive_attributes)) {
    assertthat::assert_that(is.character(sensitive_attributes), length(sensitive_attributes) == 1,
                            msg = "sensitive_attributes must be character(1)")
    
    assertthat::assert_that(any(sensitive_attributes != "leaves_"),
                            msg = "sensitive_attributes can't contain 'leaves_'")
  }
  
  assertthat::assert_that(is.character(notion_of_fairness), length(notion_of_fairness) == 1,
                          msg = "notion_of_fairness must be character(1)")
  assertthat::assert_that(notion_of_fairness %in% c("statistical parity", "equalized odds"),
                          msg = "notion_of_fairness must be 'statistical parity' or 'equalized odds'")
  
  assertthat::assert_that(is.character(ranking_mechanism), length(ranking_mechanism) == 1,
                          msg = "ranking_mechanism must be character(1)")
  assertthat::assert_that(ranking_mechanism %in% c("confidence", "magnitude"),
                          msg = "ranking_mechanism must be 'confidence' or 'magnitude'")
  
  warning("TO DO: set limits for n_grp, n_tree and alpha")
  assertthat::assert_that(is.numeric(n_grp), length(n_grp) == 1,
                          msg = "n_grp must be numeric(1)")
  
  assertthat::assert_that(is.numeric(ntree), length(ntree) == 1,
                          msg = "ntree must be numeric(1)")
  
  assertthat::assert_that(is.numeric(alpha), length(alpha) == 1,
                          msg = "alpha must be numeric(1)")

  
  ### Rename such that we can easily change function argument names
  # file       <- file
  outcome    <- outcome_variable
  sen_attr   <- sensitive_attributes
  psi_metric <- notion_of_fairness
  ranking    <- ranking_mechanism
  # n_grp      <- n_grp
  # ntree      <- ntree
  # alpha      <- alpha
  
  
  ### Load data
  dataset <- load_data(file, outcome, sen_attr)
  
  
  ### Fit random forest of conditional inference trees
  cforest_formula <- stats::as.formula(paste0(outcome, " ~ . "))
  
  partitioning <- partykit::cforest(formula = cforest_formula,
                                    data    = dataset, 
                                    ntree   = ntree, 
                                    alpha   = alpha)
  
  ### Analyse unique leaves
  results_df <- analyse_unique_leaves(partitioning = partitioning,
                                      dataset      = dataset,
                                      outcome      = outcome,
                                      ntree        = ntree)
  
}




