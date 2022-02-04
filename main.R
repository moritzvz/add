source("src/disp_calculation.R")
source("src/visualization.R")
source("src/misc_methods.R")

library("ggparty")
library("partykit")
library("gridExtra")
library("cowplot")
library("scales")
library("stringr")

# PARAMETERS
set.seed(1)   # random seed
n_grp = 3     # number of top subgroups to be featured in audit report
ntree <- 25   # number of search trees in conditional inference (c)forest
alpha <- 0.1  # maximum threshold for p-value of permutation-based split
ranking <- "confidence" # ranking mechanism: "confidence" or "magnitude"
psi_metric = "equalized odds" # fairness: "statistical parity" / "equalized odds"

# read in data
# mydata = read.table("C:/Users/morit/PycharmProjects/FairAI/Data/TreeAlg/adult_census.csv", header=TRUE,sep=",")
mydata <- read.table("data/processed/compas_data_preproc.csv", header=TRUE,sep=",")
col_names <- names(mydata)
target_col <- c("outcome", "error_type")
sensitive_attr <- col_names[!col_names %in% target_col]

# order: outcome, sensitive attributes, error_type (equalized odds)
mydata = mydata[, c("outcome", sensitive_attr, "error_type")]

# convert non-numeric columns to R factors
for (c in colnames(mydata))
{
  if(is.character(mydata[[c]]))
  {
    mydata[[c]] <- as.factor(mydata[[c]])
  }
}

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
i = 1
for(t in 1:ntree){
  # print("")
  # print(t)
  focal_tree <- gettree(partitioning, tree = t)
  leaves <- predict(focal_tree, mydata, type = "node")
  
  for(leaf in unique(leaves)){
    # print(paste("We look at leaf with index number", leaf))
      
    condition <- partykit:::.list.rules.party(focal_tree, i = leaf)
    
    # calculate magnitude of disparity (absolute odds difference)
    leaf_indicator <- leaves == leaf
    leaf_errors <- get_errors(leaf_indicator, mydata$error_type)
    
    outside_indicator <- !leaf_indicator
    outside_errors <- get_errors(outside_indicator, mydata$error_type)
    
    abs_odds_diff <- 0.5*(abs(leaf_errors$fpr - outside_errors$fpr) + 
                            abs(outside_errors$tpr - leaf_errors$tpr))
    
    fpr_diff <- leaf_errors$fpr - outside_errors$fpr
    fnr_diff <- (1 - leaf_errors$tpr) - (1 - outside_errors$tpr)
    
    # calculate confidence of disparity (absolute odds difference)
    fp_test = chisq.test(x = matrix(c(leaf_errors$fp, leaf_errors$tn,
                                      outside_errors$fp, outside_errors$tn), 
                                    nrow = 2, ncol = 2), correct=FALSE)
    fn_test = chisq.test(x = matrix(c(leaf_errors$fn, leaf_errors$tp,
                                      outside_errors$fn, outside_errors$tp), 
                                    nrow = 2, ncol = 2), correct=FALSE)
    comb_p_val = pchisq(-2*(log(fp_test$p.value) + log(fn_test$p.value)), 
                        df=4, lower.tail=FALSE)  # fisher's method
    
    group_size[i] <- sum(leaf_indicator)
    group_condition[i] <- condition
    disparity_magnitude[i] <- abs_odds_diff
    disparity_confidence[i] <- comb_p_val
    tree_id[i] <- t
    i <- i + 1
  }
}

results_df <- data.frame(group_size, group_condition, disparity_magnitude, disparity_confidence, tree_id)

results_df$group_condition <- v_clean_condition_str(results_df$group_condition)
results_df <- results_df[!duplicated(results_df$group_condition), ]

results_df$disparity_confidence <- p.adjust(results_df$disparity_confidence, p.adjust.methods[2])

if (ranking == "confidence"){
  results_df <- results_df[order(results_df$disparity_confidence),]
} else if(ranking == "magnitude"){
  results_df <- results_df[order(-results_df$disparity_magnitude),]
} else{
  stop(
    paste("Ranking mechanism \"", ranking, 
          "\" is not implemented. Try \"confidence\" or \"magnitude\".", sep=""))
}

# visualize trees of top n_grp trees 
for(t in unique(head(results_df$tree_id, n_grp))){
  visualize_tree(t, gettree(partitioning, tree = t), mydata, psi_metric=psi_metric)
}

# create and export audit report
export_audit_report(results_df, n_grp, psi_metric, ranking)






