#'Scalar-on-Image regression in STAR framework using sarim
#'
#'Returns a sx-object for Scalar-on-Image regression within sarim function
#'
#'@param images The vectorized images (columnwise)
#'@param dimension Vector which defines the dimension of the images
#'@param neighbours String defining the type of neighbourhood of underlying GMRF
#'@param solver String defining the solve which should be used
#'@param demean Logical, definign wether the mena of images should be substraced
#'@param add_diag numeric value, default is NULL. For numeric stability one can
#'add a value to main diagonal to structure matrix
#'@param ... Further arguments for the underlying function \code{Sarim::sx()},
#'most relevant are ka_a, ka_b and ka_start
#'
#'@return sx-object with images, structure matrix and so on.
#'@export
#'@examples
#'
#' ### simple call:
#' ims <- matrix(1:(100*32*32), nrow = 100, ncol = 32*32)
#' SOIR(ims)
#'
#' ### visualization of the structure matrix
#' plot(SOIR(ims))
#'
#' ### Call within Sarim:
#' beta <- rnorm(32*32)
#' y <- ims  %*% beta + rnorm(100, sd = 5)
#' mod <- Sarim::sarim(y ~ SOIR(ims, add_diag = 0.1, ), nIter = 10)
#'
SOIR <- function(images, dimension = rep(sqrt(ncol(images)),2),
                 neighbours = c("2dfirst", "2dsecond", "2dallfirst",
                                "3dfirst", "3dsecond", "3dallfirst"), solver =
                   c("rue", "lanczos"), demean = FALSE, add_diag = NULL, ...){
  require(Matrix)
  require(Sarim)
  neighbours <- match.arg(neighbours)
  solver <- match.arg(solver)
  ## argument checking
  dimension <- checkmate::assert_integerish(dimension)
  checkmate::assert_logical(demean)
  checkmate::assert_number(add_diag, null.ok = T)
  if(Reduce("*", dimension) != ncol(images)){
    stop("Dimensions must fit the length of the vectorized image (ncol(images))")
  }

  ## assuming img is a (N x L)-matrix (N images with L pixels each)
  ## attention! L is here a vector defining the dimensions cd of one image
  ## calculating structure matrix
  struc_mat <- calc_struc_mat(L = dimension, neighbours = neighbours)

  ## if one wants to add something to main diagonal (e.g. for stability)
  if(!is.null(add_diag)){
    add_diag <- checkmate::assert_number(add_diag)
    dim <- Reduce("*", dimension)
    struc_mat <- struc_mat + bandSparse(dim, dim, k = 0, diag = list(rep(add_diag, dim)))
  }

  # substract mean of all images if needed:
  if(demean){
    images <- t(apply(images, 1, function(x) x-mean(x)))# + 0.01
  }

  structure(
    sx(Z = images, penalty = "gmrf", K = struc_mat, solver = solver,...),
    class = "sx",
    neighbours = neighbours,
    dimension = dimension
  )
}

## print function because full object is not realy printable
## option for full print is possible

#'Print function for sx objects
#'
#'Easy print function to get nice readable output for sx-object
#'
#'@param x A sx object
#'@param full Logical, indicating if output should be full
#'
#'@return Null
#'
#'@description Since the output of Sarim::sx() is a very long list, a short version is
#'implemented. The option full = TRUE can be used, to get the full printet list object.
#'@export
print.sx <- function(x, full = F){
  if(full == T){
    class(x) <- "matrix"
    print(x)
    return(NULL)
  }
  cat("sx-object for Scala-on-Image Regression \n")
  cat(sprintf("Structure: %s neighbours \n", attr(x, "neighbours")))
  cat(sprintf("Solver: %s \n", attr(x,"solver")))
  cat(sprintf("Dimension of images: %s \n", paste(attr(x,"dimension",T),
                                                         collapse = "x")))
  cat(sprintf("Dimension structure matrix: %s \n", paste(dim(attr(x,"K")),
                                                      collapse = "x")))
}

# Plot function of sx object as image with neighbourhood structue

#'Plot function of sx object
#'
#'plots the neighbourhood structue of sx objects
#'
#'@param x A sx object
#'@param full colors for plot
#'
#'@return Null
#'
#'@export
plot.sx <- function(x, col = grey.colors(100), ...){
  image(attr(x, "K"), main = "Structure Matrix", ...)
}

