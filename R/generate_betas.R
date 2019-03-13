#'SmoothBeta
#'
#'Returns a smooth image
#'
#'@param x1 sequence between 0 and 1
#'@param x2 see x1
#'
#'@return smooth image as matrix
#'
#'@examples
#'require(magrittr)
#'grid <- seq(0,1, len = 64)
#'smoothBeta(grid,grid) %>% plot_coefficient_image()
#'@export
smoothBeta <- function(x1,x2, random = FALSE)
{
  grid <- expand.grid(x1, x2)
  sideLengths <- c(length(x1), length(x2))

  means <- {if(random) {matrix(runif(6), nrow = 3)} else {rbind(c(.15, .5), c(.7, .2), c(0.8, 0.7))}}

  beta <- array(-0.05+ 0.09*mvtnorm::dmvnorm(x = grid, mean = means[1,], sigma=diag(c(0.1, 0.1))) -
                  0.01*mvtnorm::dmvnorm(x = grid, mean = means[2,], sigma=diag(c(0.02, 0.03))) +
                  0.05*mvtnorm::dmvnorm(x = grid, mean = means[3,], sigma = matrix(c(0.05,0.02, 0.02, 0.1), nrow = 2)),
                dim = sideLengths)
  return(beta)
}

#'SparseBeta
#'
#'Returns a sparse image
#'
#'@param x1 sequence between 0 and 1
#'@param x2 see x1
#'@param random logical: Random generated?
#'
#'@return sparse image as matrix
#'@examples
#'require(magrittr)
#'grid <- seq(0,1, len = 64)
#'sparseBeta(grid,grid) %>% plot_coefficient_image()
#'@export
sparseBeta <- function(x1,x2, random = FALSE)
{
  grid <- expand.grid(x1, x2)
  sideLengths <- c(length(x1), length(x2))

  means <- {if(random) {matrix(runif(4), nrow = 2)} else {rbind(c(.2, .3), c(0.4, 0.8))}}

  beta <- array(.006 * mvtnorm::dmvnorm(x = grid, mean = means[1,], sigma=diag(c(.0025, 0.0015))) -
                  0.003 * mvtnorm::dmvnorm(x = grid, mean = means[2,], sigma = matrix(c(0.002,-0.001, -0.001, 0.001), nrow = 2, ncol = 2)),
                dim =sideLengths)
  beta[abs(beta) < 0.25] <- 0
  return(beta/2)
}

#'BumpsBeta
#'
#'Returns a bumpy image
#'
#'@param seed intger, set a seed
#'@param n side length of the quadratic image
#'
#'@return bumpy image as matrix
#'@examples
#'require(magrittr)
#' ### divide by 50 and decrease resolution to obtain a bumpy 32x32 image.
#'bumpsBeta(123) %>%
#'divide_by(50) %>%
#'  decrease_reolution() %>%
#'  plot_coefficient_image()
#'
#'@export
bumpsBeta <- function(seed, n = 64)
{
  # bummpy wave
  xcol <- matrix(rep(1:n, n), ncol=n)
  xrow <- matrix(rep(1:n, n), ncol=n, byrow=TRUE)

  set.seed(seed)
  t <- floor(matrix(runif(22), ncol=2)*n)
  h2 <- c(4, 5, 3, 4, 5, 4.2, 2.1, 4.3, 3.1, 5.1, 4.2)
  w <- seq(1,50,length=length(h2))
  bumps <- array(0, dim = c(n,n))
  for (i in 1 : length(h2)){
    bumps <- bumps + h2[i] * pmax(0, 1-abs(sqrt((xcol - t[i,1])^2 + (xrow - t[i,2])^2)/w[i]))^4
  }

  return(bumps)
}

#'Circle
#'
#'Returns a circle image
#'
#'@param n side length og the quadratic image
#'
#'@return circle image as matrix
#'@export
circle <- function(n = 64){
  foo <- function(x1,x2){
    if((x1^2+x2^2)<1) return(.1)
    0
  }
  x1 <- x2 <- seq(-2,2,len = n)
  outer(x1,x2,Vectorize(foo))
}
