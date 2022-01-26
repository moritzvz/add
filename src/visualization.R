library("ggparty")
library("gridExtra")
library("cowplot")
library("scales")

source("src/disp_calculation.R")
source("src/misc_methods.R")


# return color for given disparity
get_disp_color = function(disparity){
  disparity_abs = abs(disparity)
  return_colors = list()
  for(i in 1:length(disparity_abs)){
    if(disparity_abs[i] == 0){
      return_colors = c(return_colors, "#FFFFFF")
    }
    else{
      if(disparity_abs[i] <= 0.3){
        rbPal = seq_gradient_pal(low = "white", high = "red", space = "Lab")
        return_colors = c(return_colors, rbPal(disparity_abs[i]/0.3))
      }
      else{
        rbPal = seq_gradient_pal(low = "red", high = "darkviolet", space = "Lab")
        return_colors = c(return_colors, rbPal((disparity_abs[i]-0.3)/0.7))
      }
    }
  }
  return(return_colors)
}


# visualize given tree
visualize_tree <- function(t, ct, mydata, psi_metric){
  # function to recursively compute metrics for all nodes (not only leaves)
  rec_nodes = function(cnode, data, subset, psi_metric, first=FALSE){
    if(first){
      metrics = data.frame(cnode$id, 0.0, nrow(subset), nrow(subset)/nrow(data))
      names(metrics) = c("node_id", "disparity", "n", "n_rel")
    }
    else{
      rest_ids <- setdiff(rownames(data), rownames(subset))
      rest <- data[rest_ids,]
      
      if(psi_metric=="sp"){
        psi_computed = compute_stat_par(rest$outcome, subset$outcome)
      }
      else if(psi_metric=="eo"){
        psi_computed = compute_abs_odds(rest$error_type, subset$error_type)
      }
      else{
        stop("The fairness metric provided is not implemented.")
      }
      metric_new = data.frame(cnode$id,
                              psi_computed,
                              nrow(subset),
                              nrow(subset)/nrow(data))
      names(metric_new) = c("node_id", "disparity", "n", "n_rel")
      metrics = rbind(metrics, metric_new)
    }
    if(!(is.terminal(cnode))){
      subset1 = apply_split(cnode, subset, side = 1)
      subset2 = apply_split(cnode, subset, side = 2)
      metrics = rbind(metrics, rec_nodes(cnode[1], data, subset1, psi_metric))
      metrics = rbind(metrics, rec_nodes(cnode[2], data, subset2, psi_metric))
      return(metrics)
    }
    else{
      return(metrics)
    }
  }
  
  
  # compute metrics
  cnode = node_party(ct)
  metrics = list()
  metrics = rec_nodes(cnode, mydata, mydata, psi_metric=psi_metric, first=TRUE)
  
  
  # plot tree
  tree_plot <- ggparty(ct) +
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
  
  # plot heatmap legend
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
  
  # save tree + legend
  plotted_grid <- plot_grid(tree_plot, cst_legend, align="v", rel_widths = c(13/14, 1/14))
  ggsave(filename=paste("output/tree", t,".pdf", sep = ""), scale=2, width=12, height=6)
}

