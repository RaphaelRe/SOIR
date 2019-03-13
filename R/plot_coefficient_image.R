#'Plotting a coefficient image
#'
#'Plotting a coefficient image using ggplot
#'
#'@param im image given as matrix
#'@param title String, title of the image, default is NULL (no title)
#'@param col color palette of the image, default is color interplotaion from black to white
#'@param barh numeric, only nedded to adjust the height of the legend bar
#'@param breaks number, of breaks for the legend
#'@param round integer, indicates, how many decimal digits are use in the legend
#'
#'@return A ggplot object
#'@export

plot_coefficient_image <- function(im, title = NULL,
                                   col = colorRampPalette(c("black", "white"))(100),
                                   barh = 12, breaks = 5, round = 3){
  require(ggplot2)
  require(checkmate)
  assert_matrix(im)
  assert_string(title, null.ok = T)
  assert_character(col)
  assertNumber(barh)
  assert_numeric(breaks)
  assert_integerish(round)

  # generate data.frame for ggplot
  g1 <- seq(0,1, len = nrow(im))
  g2 <- seq(0,1, len = ncol(im))
  surface <- data.frame(expand.grid(g1,g2),z = as.vector(im))

  # set limits to get a cleaner legend
  lim_lower <- ceiling(range(im)[1] * 10^round) / 10^round
  lim_upper <- floor(range(im)[2] * 10^round) / 10^round

  # plot
  g <-  ggplot(surface, aes(Var1, Var2)) +
    geom_raster(aes(fill = z), interpolate = F)+
    scale_fill_gradientn(colours = col, limits = range(im),
                         breaks = round(seq(lim_lower, lim_upper, len = breaks),round)) +
    guides(fill = guide_colourbar(barwidth = 1, barheight = barh,
                                  title = expression(" "~beta)))+
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
          axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(),
          panel.grid = element_blank(),
          plot.title = element_text(size=14, face="bold", hjust = 0.5))+
    labs(title = title)
  return(g)
}
