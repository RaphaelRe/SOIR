# some short utility functions

#'Setting dimensions within a pipe
#'
#'Setting dimensions in pipe-like environment from e.g. magrittr
#'
#'@param x Numeric matrix
#'@param dim Vector containing the dimension to set
#'
#'@return same matrix with new dimension
#'
#'@examples
#'require(magrittr)
#'rnorm(100) %>% set_dim(c(20,5)) %>% head
#'@export
set_dim <- function(x, dim){
  checkmate::assert_integerish(dim)
  if(!is.array(x)){
    x <- as.array(x)
  }
  dim(x) <- dim
  return(x)
}

#'Extract beta coeffiscients from sarim output
#'
#'Extract beta coeffiscients from sarim output
#'
#'@param model list, output list from sarim function
#'@param intercept logical indicating if sarim was calculated with intercept
#'@param burnin integer, controlls how many iterations at start should be deleted
#'@param reduce logical, controlls wether the output should be reduced to 1 number per beta
#'@param reduce_with function, defines the function to reduce output, e.g. mean
#'
#'@return either a vector, the mean of each beta or a matrix
#'
#'@examples
#'beta <- rnorm(32*32)
#' y <- ims  %*% beta + rnorm(100, sd = 5)
#' Sarim::sarim(y ~ SOIR(ims, neighbours = "2dallfirst", add_diag = 0.1), nIter = 10) %>%
#' get_beta(burnin = 2) %>%
#' plot_coefficient_image
#'@export
get_beta <- function(model, intercept = FALSE, burnin = 50, reduce = TRUE, reduce_with = mean){
  require(magrittr)
  checkmate::assert_list(model)
  checkmate::assert_logical(T)
  checkmate::assert_integerish(burnin)
  checkmate::assert_logical(reduce)
  checkmate::assert_function(reduce_with)

  # indicating if model has intercept or not
  i <- ifelse(intercept, 2, 1)

  if(reduce){
    model$coef_results[[i]] %>%
      .[,-(1:burnin)] %>%
      apply(.,1,reduce_with) %>%
      return()
  }
  else{
    model$coef_results[[i]] %>%
      .[,-(1:burnin)]
  }
}

#'Decrease resolution of an image
#'
#'Decreasing resolution of an image from dimension (d x d) to (d/2 x d/2)
#'
#'@param image image represented as matrix wirh dimension (d x d), Note that d must be even
#'
#'@return matrix of dimension (d/2 x d/2)
#'
#'@examples
#' See example from bumpsBeta()
#'@export
decrease_reolution <- function(image){
  checkmate::assert_matrix(image, mode = "numeric")
  if(dim(image)[1] != dim(image)[2]){stop("Image must be quadratic")}
  if(!is.integer(dim(image)[1])){"Sidelengths of the image even"}
  s <- seq(1,nrow(image), by = 2)
  im <- vector("numeric", len = length(s)^2)
  k <- 1
  for (j in s) {
    for (i in s) {
      im[k] <- mean(image[c(i,i+1),c(j,j+1)])
      k <- k + 1
    }
  }
  dim(im) <- (c((dim(image)/2)[1], (dim(image)/2)[2]))
  im
}

