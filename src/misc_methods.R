# read in dataset: COMPAS or Adult data
get_dataset <- function(name, psi_metric){
  name <- tolower(name)
  if(name == "compas"){
    mydata <- read.table("data/processed/compas_data_preproc.csv", header=TRUE,sep=",")
    # build outcome depending on metric
    if(psi_metric=="equalized odds"){
      mydata$outcome <- as.integer(mydata["error_type"] == "fp" | mydata["error_type"] == "fn")
    }
    else if (psi_metric=="statistical parity"){
      mydata$outcome <- as.integer(mydata["error_type"] == "tn" | mydata["error_type"] == "fn")
      # outcome = 1 is the favorable label (i.e., low predicted risk of recidvism)
    }
    else{
      stop(paste(psi_metric,"is not implemented. Try 'statistical parity' or 'equalized odds'."))
    }
    col_names <- names(mydata)
    target_col <- c("outcome", "error_type")
    sensitive_attr <- col_names[!col_names %in% target_col]
    # order: outcome, sensitive attributes, error_type (equalized odds)
    mydata <- mydata[, c("outcome", sensitive_attr, "error_type")]
  }
  else if(name == "adult"){
    if(psi_metric!="statistical parity"){
      stop(paste(psi_metric,"is not implemented; for adult data, only 'statistical parity' is implemented."))
    }
    mydata <- read.table("data/processed/adult_data_preproc.csv", header=TRUE,sep=",")
  }
  else{
    stop(paste(name, "is not valid; try 'COMPAS' or 'Adult'"))
  }

  # convert non-numeric columns to R factors
  for (c in colnames(mydata))
  {
    if(is.character(mydata[[c]]))
    {
      mydata[[c]] <- as.factor(mydata[[c]])
    }
  }

  return(mydata)
}


# # get total number of leaves in all trees of causal forest
# get_total_n_leaves <- function(partitioning, ntree){
#   n_leaves_per_t <- numeric(ntree)
#   for(t in 1:ntree){
#     n_leaves_per_t[t] <- width(gettree(partitioning, tree = t)) 
#   }
#   return(sum(n_leaves_per_t))
# }


# apply split on data
apply_split = function(cnode, data, side){
  data$split = kidids_split(split_node(cnode), data)
  subset = data[data$split == side, ]
  subset = subset[!is.na(subset$split),]
  return(subset)
}

# get depths for nodes of trees
# https://stackoverflow.com/questions/35265552/depth-of-a-node-in-partykit 
get_node_depths <- function(tree) {
  outTree <- capture.output(tree)
  idCount <- 1
  depthValues <- rep(NA, length(tree))
  names(depthValues) <- 1:length(tree)
  for (index in seq_along(outTree)){
    if (grepl("\\[[0-9]+\\]", outTree[index])) {
      depthValues[idCount] <- str_count(outTree[index], "\\|")
      idCount = idCount + 1
    }
  }
  return(depthValues)
}