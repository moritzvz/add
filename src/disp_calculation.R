# # get number of samples in subset and number of errors
# # (number of true positives, true negatives, false positives, false negatives)
# get_errors <- function(group_indicator, error_type){
#   group_n <- sum(group_indicator)
#   group_tp <- sum(group_indicator & error_type == "tp")
#   group_tn <- sum(group_indicator & error_type == "tn")
#   group_fp <- sum(group_indicator & error_type == "fp")
#   group_fn <- sum(group_indicator & error_type == "fn")
#   group_tpr <- group_tp / (group_tp + group_fn)
#   group_fpr <- group_fp / (group_fp + group_tn)
#   ret_list <- list("n" = group_n, 
#                    "tp" = group_tp, "tn" = group_tn, 
#                    "fp" = group_fp, "fn" = group_fn, 
#                    "tpr" = group_tpr, "fpr" = group_fpr)
#   return(ret_list)
# }


# # get number of samples in subset and share of outcome = x 
# # (where x is the favorable label)
# get_outcome_share <- function(group_indicator, outcome, x){
#   group_n <- sum(group_indicator)
#   group_x <- sum(group_indicator & outcome == x)
#   group_share_x <- group_x / group_n
#   ret_list <- list("n" = group_n, "x" = group_x, "share_x" = group_share_x)
#   return(ret_list)
# }

# 
# # compute statistical parity difference directly for two subsets
# compute_stat_par = function(rest_outcome, subset_outcome, x=1){
#   sub_psi = sum(subset_outcome == x)/length(subset_outcome)
#   rest_psi = sum(rest_outcome == x)/length(rest_outcome)
#   psi = sub_psi - rest_psi
# 
#   return(psi)
# }
# 
# 
# # compute average absolute odds difference for given dataset
# compute_abs_odds = function(rest_error_type, subset_error_type){
# 
#   # compute false positive/negative rates for subgroup and rest
#   fpr_sub <- sum(subset_error_type == "fp")/
#     (sum(subset_error_type == "fp")+sum(subset_error_type == "tn"))
#   fnr_sub  <- sum(subset_error_type == "fn")/
#     (sum(subset_error_type == "fn")+sum(subset_error_type == "tp"))
#   fpr_rest  <- sum(rest_error_type == "fp")/
#     (sum(rest_error_type == "fp")+sum(rest_error_type == "tn"))
#   fnr_rest  <- sum(rest_error_type == "fn")/
#     (sum(rest_error_type == "fn")+sum(rest_error_type == "tp"))
# 
#   # compute avg abs odds difference psi
#   psi = 0.5*(abs(fpr_rest - fpr_sub) + abs(fnr_rest - fnr_sub))
# 
#   return(psi)
# }


# compute magnitude and (uncorrected) confidence for given leaf (subgroup)
compute_magnit_and_confid <- function(mydata, 
                                      psi_metric){

  if (psi_metric == "equalized odds"){
    
    # stats <- mydata %>% 
    #   dplyr::count(leaf_indicator_, error_type)
    # 
    # le <- stats %>% 
    #   dplyr::filter(leaf_indicator_) %>% 
    #   dplyr::select(error_type, n) %>% 
    #   tibble::deframe() %>% 
    #   as.list()
    
    le_data <- dplyr::filter(mydata, leaf_indicator_)
    le <- list()
    
    le$tp <- sum(le_data$error_type == "tp")
    le$tn <- sum(le_data$error_type == "tn")
    le$fp <- sum(le_data$error_type == "fp")
    le$fn <- sum(le_data$error_type == "fn")
    le$n <- sum(le$fn, le$fp, le$tn, le$tp)
    le$tpr <- le$tp / (le$tp + le$fn)
    le$fpr <- le$fp / (le$fp + le$tn)
    
    # oe <- stats %>% 
    #   dplyr::filter(!leaf_indicator_) %>% 
    #   dplyr::select(error_type, n) %>% 
    #   tibble::deframe() %>% 
    #   as.list()
    
    oe_data <- dplyr::filter(mydata, !leaf_indicator_)
    oe <- list()
    
    oe$tp <- sum(oe_data$error_type == "tp")
    oe$tn <- sum(oe_data$error_type == "tn")
    oe$fp <- sum(oe_data$error_type == "fp")
    oe$fn <- sum(oe_data$error_type == "fn")
    oe$n <- sum(oe$fn, oe$fp, oe$tn, oe$tp)
    oe$tpr <- oe$tp / (oe$tp + oe$fn)
    oe$fpr <- oe$fp / (oe$fp + oe$tn)
    
    leaf_errors    <- le # get_errors(leaf_indicator, mydata$error_type)
    outside_errors <- oe # get_errors(outside_indicator, mydata$error_type)
    
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
  }
  else if(psi_metric == "statistical parity"){
    
    stats <- mydata %>%
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
          
    # leaf_val    <- get_outcome_share(leaf_indicator, mydata$outcome, x=1)
    # outside_val <- get_outcome_share(outside_indicator, mydata$outcome, x=1)
    
    # calculate magnitude of disparity (statistical parity difference)
    disp_metric <- leaf_val$share_x - outside_val$share_x
    
    # calculate confidence (uncorrected) of disparity (statistical parity difference)
    uncor_p_val <- chisq.test(matrix(
      c(leaf_val$x, leaf_val$n - leaf_val$x, 
        outside_val$x, outside_val$n - outside_val$x),
      nrow = 2, ncol = 2), correct=FALSE)$p.value
  }
  else{
    stop(paste(psi_metric,"is not implemented; try 'statistical parity' or 'equalized odds'."))
  }
  ret_list <- list("disp_metric" = disp_metric, 
                   "uncor_p_val" = uncor_p_val)
  return(ret_list)
}



