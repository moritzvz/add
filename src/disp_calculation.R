# get number of samples in subset and number of errors
# (number of true positives, true negatives, false positives, false negatives)
get_errors <- function(group_indicator, error_type){
  group_n <- sum(group_indicator)
  group_tp <- sum(group_indicator & error_type == "tp")
  group_tn <- sum(group_indicator & error_type == "tn")
  group_fp <- sum(group_indicator & error_type == "fp")
  group_fn <- sum(group_indicator & error_type == "fn")
  group_tpr <- group_tp / (group_tp + group_fn)
  group_fpr <- group_fp / (group_fp + group_tn)
  ret_list <- list("n" = group_n, 
                   "tp" = group_tp, "tn" = group_tn, 
                   "fp" = group_fp, "fn" = group_fn, 
                   "tpr" = group_tpr, "fpr" = group_fpr)
  return(ret_list)
}


# compute statistical parity difference for given dataset
compute_stat_par = function(rest_outcome, subset_outcome){
  
  sub_psi = sum(subset_outcome == 1)/length(subset_outcome)
  rest_psi = sum(rest_outcome == 1)/length(rest_outcome)
  psi = sub_psi - rest_psi
  
  return(psi)
}


# compute average absolute odds difference for given dataset
compute_abs_odds = function(rest_error_type, subset_error_type){

  # compute false positive/negative rates for subgroup and rest
  fpr_sub <- sum(subset_error_type == "fp")/
    (sum(subset_error_type == "fp")+sum(subset_error_type == "tn"))
  fnr_sub  <- sum(subset_error_type == "fn")/
    (sum(subset_error_type == "fn")+sum(subset_error_type == "tp"))
  fpr_rest  <- sum(rest_error_type == "fp")/
    (sum(rest_error_type == "fp")+sum(rest_error_type == "tn"))
  fnr_rest  <- sum(rest_error_type == "fn")/
    (sum(rest_error_type == "fn")+sum(rest_error_type == "tp"))
  
  # compute avg abs odds difference psi
  psi = 0.5*(abs(fpr_rest - fpr_sub) + abs(fnr_rest - fnr_sub))
  
  return(psi)
}