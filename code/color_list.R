# This is a resource for a high contrast color palette up to 18 colors. In order to use it, 
# simply source the file and call the function get_color_list(n), where n equals the number of 
# colors required. The colors are selected to maximize contrast for the number of samples, and
# are returned in approximate rainbow order.   

## Example:
# source("https://github.com/jenfransson/r_resources/raw/main/code/color_list.R")
# mycolors = get_color_list(4)
# mycolors # returns "#9E1531" "#F9C027" "#97C13A" "#3C87EA"

get_color_list = function(n){
  indeces = c(1,11,5,7,15,14,16,13,6,9,17,2,10,8,3,4,12,18)
  
  if(n>18) stop("n must be 18 or smaller")
  c("#9E1531","#C66A27","#F3953A","#C9971D","#F9C027","#DDE48A",
    "#97C13A","#64A638","#174E3F","#43ADA8","#3C87EA","#7863A9",
    "#C9CDFF","#1C124A","#6D1C8C","#CF2561","#EC8483","#ECAFC6")[sort(indeces[1:n])]
}



showcolors = function(){
  allcolors = do.call("rbind",lapply(1:18, function(i){
    data.frame(n = i, index = 1:i, color = get_color_list(i))
  }))
  allcolors$n = factor(allcolors$n)
  allcolors$index = factor(allcolors$index)
  
  colorscale = get_color_list(18)
  names(colorscale) = colorscale
  
  ggplot2::ggplot(allcolors, ggplot2::aes(x = index, y = n, color = color)) + 
    ggplot2::theme(panel.background = element_blank(),
                   panel.grid = element_blank()) +
    ggplot2::geom_point() + 
    ggplot2::scale_color_manual(values = colorscale)
}
