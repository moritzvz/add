library("ggparty")
library("gridExtra")
library("cowplot")
library("scales")

source("src/disp_calculation.R")

# visualize given tree
visualize_tree <- function(tree){
  print("lol")
}


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