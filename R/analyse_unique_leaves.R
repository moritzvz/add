
#' Analyse unique leaves
#'
#' @param partitioning result from partykit::cforest()
#' @param dataset data frame that the cforest was trained on
#' @param ntree numeric(1) number of trees
#'
#' @return
analyse_unique_leaves <- function(partitioning, dataset, psi_metric, ntree) {
  
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
  
  for (t in 1:ntree) {
    focal_tree    <- partykit::gettree(partitioning, tree = t)
    
    dataset_tree <- dplyr::mutate(dataset, leaves_ = unname(stats::predict(focal_tree, dataset, type = "node")))

    unique_leaves <- unique(dataset_tree$leaves_)
    
    conditions <- partykit:::.list.rules.party(focal_tree, unique_leaves)
    
    for (leaf in unique_leaves) {
      # get condition of leaf (subgroup description)
      condition <- conditions[as.character(leaf)]
      
      # get individuals in leaf (subgroup)
      dataset_leaf <- dplyr::mutate(dataset_tree, leaf_indicator_ = leaves_ == leaf)
      # leaf_indicator <- leaves == leaf
      
      # compute magnitude and (uncorrected) confidence of disparity
      disparity_val <- compute_magnit_and_confid(dataset_leaf, psi_metric = psi_metric)
      
      # record values
      group_size[i] <- sum(dataset_leaf$leaf_indicator_)
      group_condition[i] <- condition
      disparity_magnitude[i] <- disparity_val$disp_metric
      disparity_confidence[i] <- disparity_val$uncor_p_val
      tree_id[i] <- t
      i <- i + 1
    }
  }
  
  tibble::tibble(group_size = group_size, 
                 group_condition = group_condition, 
                 disparity_magnitude = disparity_magnitude, 
                 disparity_confidence = disparity_confidence, 
                 tree_id = tree_id)
  
}


#' Get total number of leaves
#'
#' Get total number of leaves in all trees of causal forest
#' 
#' @param partitioning the result of partykit::cforest
#' @param ntree numeric(1) number of trees
#'
#' @return
get_total_n_leaves <- function(partitioning, ntree) {
  n_leaves_per_t <- numeric(ntree)
  
  for(t in 1:ntree){
    n_leaves_per_t[t] <- partykit::width(partykit::gettree(partitioning, tree = t)) 
  }
  
  return(sum(n_leaves_per_t))
}



#' compute magnitude and (uncorrected) confidence for given leaf (subgroup)
#'
#' @param .data data frame with a leaf_indicator_ column and outcome (for statistical parity) or error_type (for equalized odds)
#' @param psi_metric character(1)
#'
#' @return
compute_magnit_and_confid <- function(.data, psi_metric){
  
  if (psi_metric == "equalized odds") {
    
    le_data <- dplyr::filter(.data, leaf_indicator_)
    le <- list()
    
    le$tp <- sum(le_data$error_type == "tp")
    le$tn <- sum(le_data$error_type == "tn")
    le$fp <- sum(le_data$error_type == "fp")
    le$fn <- sum(le_data$error_type == "fn")
    le$n <- sum(le$fn, le$fp, le$tn, le$tp)
    le$tpr <- le$tp / (le$tp + le$fn)
    le$fpr <- le$fp / (le$fp + le$tn)
    
    oe_data <- dplyr::filter(.data, !leaf_indicator_)
    oe <- list()
    
    oe$tp <- sum(oe_data$error_type == "tp")
    oe$tn <- sum(oe_data$error_type == "tn")
    oe$fp <- sum(oe_data$error_type == "fp")
    oe$fn <- sum(oe_data$error_type == "fn")
    oe$n <- sum(oe$fn, oe$fp, oe$tn, oe$tp)
    oe$tpr <- oe$tp / (oe$tp + oe$fn)
    oe$fpr <- oe$fp / (oe$fp + oe$tn)
    
    leaf_errors    <- le 
    outside_errors <- oe 
    
    # calculate magnitude of disparity (absolute odds difference)
    disp_metric <- 0.5*(abs(leaf_errors$fpr - outside_errors$fpr) + 
                          abs(outside_errors$tpr - leaf_errors$tpr))
    
    fpr_diff <- leaf_errors$fpr - outside_errors$fpr
    fnr_diff <- (1 - leaf_errors$tpr) - (1 - outside_errors$tpr)
    
    # calculate confidence (uncorrected) of disparity (absolute odds difference)
    fp_test <- chisq.test(x = matrix(c(leaf_errors$fp, leaf_errors$tn,
                                       outside_errors$fp, outside_errors$tn), 
                                     nrow = 2, ncol = 2), correct=FALSE)
    fn_test <- chisq.test(x = matrix(c(leaf_errors$fn, leaf_errors$tp,
                                       outside_errors$fn, outside_errors$tp), 
                                     nrow = 2, ncol = 2), correct=FALSE)
    uncor_p_val <- pchisq(-2*(log(fp_test$p.value) + log(fn_test$p.value)), 
                          df=4, lower.tail=FALSE)  # fisher's method
    
  } else if (psi_metric == "statistical parity") {
    
    stats <- .data %>%
      dplyr::group_by(leaf_indicator_) %>%
      dplyr::summarise(n = dplyr::n(), x = sum(outcome)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(share_x = x / n)
    
    leaf_val <- stats %>% 
      dplyr::filter(leaf_indicator_) %>% 
      dplyr::select(n, x, share_x) %>% 
      tidyr::pivot_longer(c(n, x, share_x)) %>% 
      tibble::deframe() %>% 
      as.list()
    
    outside_val <- stats %>% 
      dplyr::filter(!leaf_indicator_) %>% 
      dplyr::select(n, x, share_x) %>% 
      tidyr::pivot_longer(c(n, x, share_x)) %>% 
      tibble::deframe() %>% 
      as.list()
    
    # calculate magnitude of disparity (statistical parity difference)
    disp_metric <- leaf_val$share_x - outside_val$share_x
    
    # calculate confidence (uncorrected) of disparity (statistical parity difference)
    uncor_p_val <- chisq.test(matrix(
      c(leaf_val$x, leaf_val$n - leaf_val$x, 
        outside_val$x, outside_val$n - outside_val$x),
      nrow = 2, ncol = 2), correct=FALSE)$p.value
  } else {
    stop(paste(psi_metric,"is not implemented; try 'statistical parity' or 'equalized odds'."))
  }
  
  list("disp_metric" = disp_metric, 
       "uncor_p_val" = uncor_p_val)
}

