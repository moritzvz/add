# get total number of leaves in all trees of causal forest
get_total_n_leaves <- function(partitioning, ntree){
  n_leaves_per_t <- numeric(ntree)
  for(t in 1:ntree){
    n_leaves_per_t[t] <- width(gettree(partitioning, tree = t)) 
  }
  return(sum(n_leaves_per_t))
}


# clean condition string returned by partykit package
clean_condition_str <- function(raw_str){
  str_split <- str_split(raw_str, pattern=" & ", simplify = TRUE)
  cleaned_condition <- ""
  attr_considered <- character()
  for(i in length(str_split[,]):1){
    current_attr <- word(str_split[,i], 1)
    if(current_attr %in% attr_considered){
      next
    }
    cleaned_condition <- paste(cleaned_condition, str_split[,i], "; ", sep="")   
    attr_considered <- c(attr_considered, current_attr)
  }
  pats <- c('"NA", |, "NA"|"|%')
  cleaned_condition <- str_replace_all(cleaned_condition, pats, "")
  cleaned_condition <- str_replace_all(cleaned_condition, "c\\(", "{")
  cleaned_condition <- str_replace_all(cleaned_condition, "\\)", "}")#
  return(cleaned_condition)
}


# vectorize function "clean_condition_str"
v_clean_condition_str <- Vectorize(clean_condition_str)


# apply split on data
apply_split = function(cnode, data, side){
  data$split = kidids_split(split_node(cnode), data)
  subset = data[data$split == side, ]
  subset = subset[!is.na(subset$split),]
  return(subset)
}