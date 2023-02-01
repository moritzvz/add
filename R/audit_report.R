

#' Create and save an audit report
#' 
#' Audit report contains a title, description, table and 3 tree plots.
#' 
#' It writes the file to <dir>/audit_report_<data_name>.pdf
#' 
#' @param results_df data frame, results from the analysis
#' @param n_grp numeric(1),number of groups
#' @param psi_metric character(1)
#' @param ranking character(1)
#' @param sen_attr character()
#' @param partitioning result from partykit::cforest()
#' @param dataset data frame that the cforest was trained on
#' @param data_name character(1) name of the dataset
#' @param dir character(1) name of directory to write to
#'
#' @return
export_audit_report <- function(results_df, 
                                n_grp, 
                                psi_metric, 
                                ranking, 
                                sen_attr, 
                                partitioning, 
                                dataset, 
                                data_name,
                                dir){
  
  temp_dir   <- tempdir()
  tempReport <- file.path(temp_dir, "report.Rmd")
  
  # system.file will use the report that is installed with the package
  template_file <- system.file("report_pdf.Rmd", package = "ald")
  
  # In case the package isn't installed. For development mode mostly
  if (template_file == "") {
    template_file <- here::here("inst/report_pdf.Rmd")
  }
  
  assertthat::assert_that(file.exists(template_file),
                          msg = "Report template can't be found: report_pdf.Rmd")
  
  file.copy(template_file,
            tempReport,
            overwrite = TRUE)
  
  title <- "Audit report"
  
  # plots and is_pruned
  trees <- audit_report_trees(results_df   = results_df, 
                              partitioning = partitioning, 
                              dataset      = dataset, 
                              psi_metric   = psi_metric, 
                              sen_attr     = sen_attr,
                              n_grp        = n_grp)
  
  description <- paste0(
    "The following table comprises the ", n_grp, " groups most ",
    "affected by disparate outcomes. Disparate outcomes are ", 
    "defined by ", psi_metric, " and groups are ranked based on ",
    ranking, " (of the disparity). Visualizations of ",
    "trees ", trees$is_pruned," that generated the groups are provided ",
    "on the following pages.")
  
  table <- audit_report_table(results_df = results_df, 
                              n_grp      = n_grp,
                              dataset    = dataset)
  
  params <- list(
    title       = title,
    description = description,
    table       = table,
    trees_plots = trees$plots)
  
  # Knit the document, passing in the `params` list, and eval it in a
  # child of the global environment (this isolates the code in the document
  # from the code in this app).
  rmarkdown::render(tempReport, 
                    output_file = paste0("audit_report_", data_name, ".pdf"),
                    output_dir  = dir,
                    params      = params,
                    envir       = new.env(parent = globalenv()))
  
}
  
  
#' Create audit report trees
#' 
#' Get the n_grp first number of trees and prune if necessary. Create the visualisations.
#' 
#' Return the visualisations and pruning message
#'
#' @param results_df data frame, results from the analysis
#' @param partitioning result from partykit::cforest()
#' @param dataset data frame that the cforest was trained on
#' @param psi_metric character(1)
#' @param sen_attr character()
#'
#' @return
#'
#' @examples
audit_report_trees <- function(results_df, partitioning, dataset, psi_metric, sen_attr, n_grp) {
  
  is_pruned <- ""
  
  plots <- list()
  
  for (t in unique(head(results_df$tree_id, n_grp))) {
    focal_ct <- partykit::gettree(partitioning, tree = t)
    # if tree to plot has >3 depth, prune it accordingly
    if (grid::depth(focal_ct) > 3) {
      depth_of_nodes <- get_node_depths(focal_ct)
      nodes_to_display <- depth_of_nodes[depth_of_nodes < 4]
      nodes_as_terminal <- as.integer(names(nodes_to_display[nodes_to_display == 3]))
      focal_ct <- partykit::nodeprune(focal_ct, nodes_as_terminal)
      is_pruned <- "(pruned)"
    }
    
    plots[[as.character(t)]] <- visualize_tree(results_df = results_df, 
                                               t          = t, 
                                               ct         = focal_ct, 
                                               dataset    = dataset,  
                                               psi_metric = psi_metric)
  }
  
  list(plots     = plots,
       is_pruned = is_pruned)
}


#' Create the table for the audit report
#' 
#' Creates a flextable
#'
#' @param results_df data frame, results from the analysis
#' @param n_grp numeric(1), number of groups
#' @param dataset data frame that the cforest was trained on
#'
#' @return
#'
#' @examples
audit_report_table <- function(results_df, n_grp, dataset) {
  
  total_n <- nrow(dataset)
  
  table <- results_df %>% 
    dplyr::slice(seq_len(n_grp)) %>% 
    dplyr::mutate(disparity_confidence = round(disparity_confidence, 3),
                  disparity_magnitude  = round(disparity_magnitude, 3)) %>% 
    dplyr::mutate(group_size = paste0(group_size, " / ",
                                      round(100 * group_size / total_n, 2), "%")) %>% 
    dplyr::select(group_size, group_condition, disparity_magnitude, disparity_confidence)

  flextable::flextable(table) %>% 
    flextable::set_header_labels(
      group_size           = "Group size", 
      group_condition      = "Group condition",
      disparity_magnitude  = "Disparity magnitude" ,
      disparity_confidence = "Disparity confidence") %>% 
    # flextable::theme_alafoli() %>% 
    flextable::width(j = 1, width = 1.3) %>% 
    flextable::width(j = 2, width = 4.5) %>% 
    flextable::width(j = 3, width = 1.5) %>% 
    flextable::width(j = 4, width = 1.6) 
  
}

#' Get depths for nodes of trees
#' 
#' https://stackoverflow.com/questions/35265552/depth-of-a-node-in-partykit 
#' 
#' @param tree result from partykit::gettree()
#'
#' @return
get_node_depths <- function(tree) {
  outTree <- utils::capture.output(tree)
  idCount <- 1
  depthValues <- rep(NA, length(tree))
  names(depthValues) <- 1:length(tree)
  for (index in seq_along(outTree)){
    if (grepl("\\[[0-9]+\\]", outTree[index])) {
      depthValues[idCount] <- stringr::str_count(outTree[index], "\\|")
      idCount = idCount + 1
    }
  }
  return(depthValues)
}

#' Visualize given tree
#' 
#' - Recursively analyse nodes
#' - plot
#' 
#' @param results_df data frame, results from the analysis
#' @param t numeric(1) tree number
#' @param ct partykit tree
#' @param dataset data frame that the cforest was trained on
#' @param psi_metric character(1)
#'
#' @return
visualize_tree <- function(results_df, t, ct, dataset, psi_metric){
  
  # round results
  results_df$disparity_confidence <- round(results_df$disparity_confidence, 3)
  results_df$disparity_magnitude  <- round(results_df$disparity_magnitude, 3)
  
  # compute metrics
  cnode   <- partykit::node_party(ct)
  metrics <- rec_nodes(cnode      = cnode, 
                       data       = dataset, 
                       subset     = dataset, 
                       psi_metric = psi_metric, 
                       first      = TRUE)
  
  # plot tree
  tree_plot <- ggparty::ggparty(ct) +
    ggparty::geom_edge() +
    ggparty::geom_edge_label(size = 3) + 
    ggplot2::theme(legend.position = "none")
  
  for (r in seq_len(nrow(metrics))) {
    if (!metrics$is_terminal[r]) {
      tree_plot <- tree_plot +
        ggparty::geom_node_label(
          mapping = ggplot2::aes(col = "black"), #, fill = get_disp_color(metrics[metrics$node_id == id,]$disparity)),
          line_list = list(# aes(label = paste("Subgroup:", id)),
            ggplot2::aes(label = paste("n:", metrics[metrics$node_id == id,]$n, "/",
                                       format(round(metrics[metrics$node_id == id,]$n_rel*100, 1), nsmall = 1), "%")),
            ggplot2::aes(label = paste("Disparity:",
                                       format(round(metrics[metrics$node_id == id,]$disparity, 3), nsmall = 3))),
            
            ggplot2::aes(label = paste("Split: \"", splitvar, "\""))
          ),
          line_gpar = list(# list(size = 8, col = "black", fontface = "plain", alignment = "left"),
            list(size = 8, col = "black", fontface = "plain", alignment = "left"),
            list(size = 8, col = "black", fontface = "plain", alignment = "left"),
            list(size = 8, col = "black", fontface = "bold", alignment = "left")
          ),
          #label.fill = rbPal1(0.1), #"gray",
          label.col = "black",
          label.fill = get_disp_color(metrics$disparity[r])[[1]],
          ids = r)
    } else {
      tree_plot <- tree_plot +
        ggparty::geom_node_label(
          mapping = ggplot2::aes(col = splitvar), # , fill = get_disp_color(metrics[metrics$node_id == id,]$disparity)),
          line_list = list(# aes(label = paste("Subgroup:", id)),
            ggplot2::aes(label = paste("n:", metrics[metrics$node_id == id,]$n, "/",
                                       format(round(metrics[metrics$node_id == id,]$n_rel*100, 1), nsmall = 1), "%")),
            ggplot2::aes(label = paste("Disparity:",
                                       format(round(metrics[metrics$node_id == id,]$disparity, 3), nsmall = 3)))
          ),
          line_gpar = list(# list(size = 8, col = "black", fontface = "plain", alignment = "left"),
            list(size = 8, col = "black", fontface = "plain", alignment = "left"),
            list(size = 8, col = "black", fontface = "plain", alignment = "left")
          ),
          # label.fill = "green",
          label.col = "black",
          label.fill = get_disp_color(metrics$disparity[r])[[1]],
          ids = r)
    }
    
  }
  
  # tree_plot <- tree_plot
  
  # plot heatmap legend
  heat_leg_data = expand.grid(X=1, Y=(1:100)/100)
  heat_leg_data$Z = (1:100)/100
  
  cst_legend <- ggplot2::ggplot(heat_leg_data, ggplot2::aes(X, Y, fill= Z)) + 
    
    ggplot2::geom_tile() +
    
    ggplot2::scale_fill_gradient2(
      low = "white",
      mid = "red",
      high = "darkviolet",
      midpoint = 0.3) +
    
    ggplot2::scale_y_continuous(name = 
                         ifelse(psi_metric == "statistical parity", 
                                expression(paste("Abs. Disparity |", psi[sp], "|")),
                                expression(paste("Abs. Disparity |", psi[eo], "|"))), limits = c(0,1)) +
    
    ggplot2::theme(axis.line=ggplot2::element_blank(),
          axis.text.y = ggplot2::element_text(size = 8),
          axis.text.x = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(),
          axis.title.y = ggplot2::element_text(hjust = 0.9, size=8),
          axis.title.x = ggplot2::element_blank(),
          legend.position = "none",
          panel.background = ggplot2::element_blank(),
          panel.border = ggplot2::element_blank(),
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          plot.background = ggplot2::element_blank())
  
  # Combine tree + legend
  plotted_grid <- cowplot::plot_grid(tree_plot, cst_legend, align="v", rel_widths = c(13/14, 1/14))
  
  plotted_grid
}

#' Return color for given disparity
#'
#' @param disparity 
#'
#' @return
get_disp_color <- function(disparity) {
  disparity_abs = abs(disparity)
  return_colors = list()
  for(i in 1:length(disparity_abs)){
    if(disparity_abs[i] == 0){
      return_colors = c(return_colors, "#FFFFFF")
    }
    else{
      if(disparity_abs[i] <= 0.3){
        rbPal = scales::seq_gradient_pal(low = "white", high = "red", space = "Lab")
        return_colors = c(return_colors, rbPal(disparity_abs[i]/0.3))
      }
      else{
        rbPal = scales::seq_gradient_pal(low = "red", high = "darkviolet", space = "Lab")
        return_colors = c(return_colors, rbPal((disparity_abs[i]-0.3)/0.7))
      }
    }
  }
  return(return_colors)
}


#' Recursively compute metrics for all nodes (not only leaves)
#'
#' @param cnode node
#' @param data full dataset
#' @param subset subset of node
#' @param psi_metric character(1)
#' @param first logical(1) Initial computation of recursive flow  
#'
#' @return
#'  metrics <- rec_nodes(cnode, dataset, dataset, psi_metric = psi_metric, first = TRUE)
rec_nodes <- function(cnode, data, subset, psi_metric, first = FALSE) {
  if (first) {
    metrics <- data.frame(cnode$id, 0.0, nrow(subset), nrow(subset)/nrow(data), partykit::is.terminal(cnode))
    names(metrics) <- c("node_id", "disparity", "n", "n_rel", "is_terminal")
  } else {
    rest_ids <- setdiff(rownames(data), rownames(subset))
    rest <- data[rest_ids,]
    if (psi_metric == "statistical parity") {
      psi_computed = compute_stat_par(rest$outcome, subset$outcome)
    } else if (psi_metric == "equalized odds") {
      psi_computed = compute_abs_odds(rest$error_type, subset$error_type)
    } else{
      stop("The fairness metric provided is not implemented.")
    }
    metric_new <- data.frame(cnode$id, psi_computed, nrow(subset), nrow(subset)/nrow(data), partykit::is.terminal(cnode))
    names(metric_new) <- c("node_id", "disparity", "n", "n_rel", "is_terminal")
    metrics <- metric_new
    # metrics <- rbind(metrics, metric_new)
  }
  if (!(partykit::is.terminal(cnode))) {
    subset1 <- apply_split(cnode, subset, side = 1)
    subset2 <- apply_split(cnode, subset, side = 2)
    metrics <- rbind(metrics, rec_nodes(cnode = cnode[1], data = data, subset = subset1, psi_metric = psi_metric))
    metrics <- rbind(metrics, rec_nodes(cnode = cnode[2], data = data, subset = subset2, psi_metric = psi_metric))
    return(metrics)
  } else {
    return(metrics)
  }
}

#' Apply split on data
#'
#' @param cnode node
#' @param data data frame to split
#' @param side numeric(1) side 1 or 2
#'
#' @return
apply_split = function(cnode, data, side){
  data$split = partykit::kidids_split(partykit::split_node(cnode), data)
  subset = data[data$split == side, ]
  subset = subset[!is.na(subset$split),]
  return(subset)
}

#' Compute statistical parity difference directly for two subsets
#'
#' @param rest_outcome 
#' @param subset_outcome 
#' @param x 
#'
#' @return
compute_stat_par = function(rest_outcome, subset_outcome, x=1){
  sub_psi = sum(subset_outcome == x)/length(subset_outcome)
  rest_psi = sum(rest_outcome == x)/length(rest_outcome)
  psi = sub_psi - rest_psi
  
  return(psi)
}

#' Compute average absolute odds difference for given dataset
#'
#' @param rest_error_type 
#' @param subset_error_type 
#'
#' @return
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
