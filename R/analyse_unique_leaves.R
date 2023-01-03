


#' Title
#'
#' @param partitioning 
#' @param dataset 
#' @param ntree 
#'
#' @return
#' @export
#'
#' @examples
analyse_unique_leaves <- function(partitioning, dataset, outcome, ntree) {
  
  # prepare vectors for subgroup/leaf results
  n_leaves_total       <- get_total_n_leaves(partitioning, ntree)
  group_size           <- numeric(n_leaves_total)
  group_condition      <- character(n_leaves_total)
  disparity_magnitude  <- numeric(n_leaves_total)
  disparity_confidence <- numeric(n_leaves_total)
  tree_id              <- numeric(n_leaves_total)
  
  # iterate through conditional inference trees of random (c)forest...
  # ...and determine the leaf (index) for each sample in data set
  i <- 1
  
  for(t in 1:ntree){
    focal_tree    <- partykit::gettree(partitioning, tree = t)
    
    dataset_tree <- dataset %>% 
      dplyr::mutate(leaves_ = unname(stats::predict(focal_tree, dataset, type = "node")))
    # leaves        <- unname(stats::predict(focal_tree, dataset, type = "node"))
    
    unique_leaves <- unique(dataset_tree$leaves_)
    
    conditions <- partykit:::.list.rules.party(focal_tree, unique_leaves)
    
    for(leaf in unique_leaves){
      # get condition of leaf (subgroup description)
      condition <- conditions[as.character(leaf)]
      
      # get individuals in leaf (subgroup)
      dataset_leave <- dataset_tree %>% 
        dplyr::mutate(leaf_indicator_ = leaves_ == leaf)
      # leaf_indicator <- leaves == leaf
      
      # compute magnitude and (uncorrected) confidence of disparity
      disparity_val <- compute_magnit_and_confid(# leaf_indicator = leaf_indicator, 
                                                 mydata         = dataset_leave, 
                                                 psi_metric     = psi_metric,
                                                 outcome        = outcome)
      
      # record values
      group_size[i] <- sum(leaf_indicator)
      group_condition[i] <- condition
      disparity_magnitude[i] <- disparity_val$disp_metric
      disparity_confidence[i] <- disparity_val$uncor_p_val
      tree_id[i] <- t
      i <- i + 1
    }
  }
  
}


#' Get total number of leaves
#'
#' Get total number of leaves in all trees of causal forest
#' 
#' @param partitioning the result of partykit::cforest
#' @param ntree numeric(1) number of trees
#'
#' @return
#' @export
#'
#' @examples
get_total_n_leaves <- function(partitioning, ntree) {
  n_leaves_per_t <- numeric(ntree)
  
  for(t in 1:ntree){
    n_leaves_per_t[t] <- partykit::width(partykit::gettree(partitioning, tree = t)) 
  }
  
  return(sum(n_leaves_per_t))
}
