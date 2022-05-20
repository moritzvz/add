source("src/misc_methods.R")
source("src/disp_calculation.R")
source("src/audit_report_generation.R")
source("src/results_postprocessing.R")

# libraries
library("ggparty")
library("partykit")
library("gridExtra")
library("cowplot")
library("scales")
library("stringr")
library("qpdf")

# random seed
set.seed(2022)

# PARAMETERS
data_name <- "COMPAS" # COMPAS or Adult
n_grp <- 3  # number of top subgroups to be featured in audit report
ntree <- 25  # number of search trees in conditional inference (c)forest
alpha <- 0.1  # maximum threshold for p-value of permutation-based split
psi_metric <- "equalized odds"  # fairness: "statistical parity"/"equalized odds"
ranking <- "confidence"  # ranking mechanism: "confidence" or "magnitude"

# get preprocessed data
mydata = get_dataset(data_name, psi_metric)

# fit random forest of conditional inference trees
partitioning <- cforest(outcome ~ .,
                        data = mydata[, !(names(mydata) %in% "error_type")], 
                        ntree = ntree, alpha = alpha)

# prepare vectors for subgroup/leaf results
n_leaves_total <- get_total_n_leaves(partitioning, ntree)
group_size <- numeric(n_leaves_total)
group_condition <- character(n_leaves_total)
disparity_magnitude <- numeric(n_leaves_total)
disparity_confidence <- numeric(n_leaves_total)
tree_id <- numeric(n_leaves_total)

# iterate through conditional inference trees of random (c)forest...
# ...and determine the leaf (index) for each sample in data set
i <- 1
for(t in 1:ntree){
  focal_tree <- gettree(partitioning, tree = t)
  leaves <- predict(focal_tree, mydata, type = "node")
  
  for(leaf in unique(leaves)){
    # get condition of leaf (subgroup description)
    condition <- partykit:::.list.rules.party(focal_tree, i = leaf)
    
    # get individuals in leaf (subgroup)
    leaf_indicator <- leaves == leaf
    
    # compute magnitude and (uncorrected) confidence of disparity
    focal_columns <- names(mydata)[names(mydata) %in% c("outcome", "error_type")]
    disparity_val <- compute_magnit_and_confid(leaf_indicator, 
                                               mydata[focal_columns], psi_metric)
    
    # record values
    group_size[i] <- sum(leaf_indicator)
    group_condition[i] <- condition
    disparity_magnitude[i] <- disparity_val$disp_metric
    disparity_confidence[i] <- disparity_val$uncor_p_val
    tree_id[i] <- t
    i <- i + 1
  }
}

# post-process and rank results
results_df <- clean_results(
  group_size, group_condition, disparity_magnitude, disparity_confidence, tree_id)
results_df <- correct_pval(results_df)
results_df <- rank_results(results_df, ranking)

# create and export audit report including visualizations to folder "output"
export_audit_report(results_df, n_grp, psi_metric, ranking, partitioning, mydata,
                    data_name)

