source("src/disp_calculation.R")
source("src/visualization.R")
source("src/misc_methods.R")

# use partykit
# install.packages("ggparty")
# install.packages("partykit")
# install.packages("gridExtra")
# install.packages("cowplot")
library("ggparty")
library("partykit")
library("gridExtra")
library("cowplot")
library("scales")
library("stringr")

# set parameters
set.seed(1)
n_grp = 3
ntree <- 25
alpha <- 0.1
ranking <- "confidence"

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

results_df$disparity_confidence <- round(results_df$disparity_confidence, 3)
results_df$disparity_magnitude <- round(results_df$disparity_magnitude, 3)

print(head(results_df, n_grp))

# visualize trees of top n_grp trees 
for(t in unique(head(results_df$tree_id, n_grp))){
  # visualize_tree(gettree(partitioning, tree = t))
  print(t)
}








ct <- gettree(partitioning, tree = 1)

# predict the leaf, append to dataframe, get numbr of leaves
leaves <- predict(ct, mydata[, !(names(mydata) %in% "error_type")], type = "node")
mydata$leaf <- leaves
unique(leaves)

print(ct)


#____________________________________________________________________
# fairness metric (sp vs eo)
psi_metric = "eo"

# apply split on data
apply_split = function(cnode, data, side){
  data$split = kidids_split(split_node(cnode), data)
  subset = data[data$split == side, ]
  subset = subset[!is.na(subset$split),]
  return(subset)
}

# compute metrics
cnode = node_party(ct)
metrics = list()


metrics = rec_nodes(cnode, mydata, mydata, psi_metric=psi_metric, first=TRUE)


#
# plot tree
#
tree_plot = ggparty(ct) +
  geom_edge() +
  geom_edge_label(size = 6) +
  
  # inner nodes
  geom_node_label(mapping = aes(col = "black", fill = get_disp_color(metrics[metrics$node_id == id,]$disparity)),
                  line_list = list(# aes(label = paste("Subgroup:", id)),
                    aes(label = paste("n:", metrics[metrics$node_id == id,]$n, "/",
                                      format(round(metrics[metrics$node_id == id,]$n_rel*100, 1), nsmall = 1), "%")),
                    aes(label = paste("Disparity:",
                                      format(round(metrics[metrics$node_id == id,]$disparity, 3), nsmall = 3))),
                    
                    aes(label = paste("Split: \"", splitvar, "\""))
                  ),
                  line_gpar = list(# list(size = 8, col = "black", fontface = "plain", alignment = "left"),
                    list(size = 16, col = "black", fontface = "plain", alignment = "left"),
                    list(size = 16, col = "black", fontface = "plain", alignment = "left"),
                    list(size = 16, col = "black", fontface = "bold", alignment = "left")
                  ),
                  #label.fill = rbPal1(0.1), #"gray",
                  label.col = "black",
                  ids = "inner") +
  
  # terminal nodes
  geom_node_label(mapping = aes(col = splitvar, fill = get_disp_color(metrics[metrics$node_id == id,]$disparity)),
                  line_list = list(# aes(label = paste("Subgroup:", id)),
                    aes(label = paste("n:", metrics[metrics$node_id == id,]$n, "/",
                                      format(round(metrics[metrics$node_id == id,]$n_rel*100, 1), nsmall = 1), "%")),
                    aes(label = paste("Disparity:",
                                      format(round(metrics[metrics$node_id == id,]$disparity, 3), nsmall = 3)))
                  ),
                  line_gpar = list(# list(size = 8, col = "black", fontface = "plain", alignment = "left"),
                    list(size = 16, col = "black", fontface = "plain", alignment = "left"),
                    list(size = 16, col = "black", fontface = "plain", alignment = "left")
                  ),
                  # label.fill = "green",
                  label.col = "black",
                  ids = "terminal") +
  
  theme(legend.position = "none")


#
# plot heatmap legend
#
heat_leg_data = expand.grid(X=1, Y=(1:100)/100)
heat_leg_data$Z = (1:100)/100

cst_legend = ggplot(heat_leg_data, aes(X, Y, fill= Z)) + 
  
  geom_tile() +
  
  scale_fill_gradient2(
    low = "white",
    mid = "red",
    high = "darkviolet",
    midpoint = 0.3) +
  
  scale_y_continuous(name = 
                       ifelse(psi_metric == "sp", 
                              expression(paste("Abs. Disparity |", psi[sp], "|")),
                              expression(paste("Abs. Disparity |", psi[eo], "|"))), limits = c(0,1)) +
  
  theme(axis.line=element_blank(),
        axis.text.y = element_text(size = 16),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_text(hjust = 0.9, size=16),
        axis.title.x = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank())


#
# show tree + legend
#
plot_grid(tree_plot, cst_legend, align="v", rel_widths = c(13/14, 1/14))


# TODO: list of conditions (if desired?)
# partykit:::.list.rules.party(ct)
# mynode = node_party(ct)
# thenode = mynode[1][2]
# split_node(thenode)
# mydata








