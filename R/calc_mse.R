#'Calculates the MSE over all non-zero pixels
#'
#'Calulation is done by automatically indication non-zero pixels. See Details.
#'
#'@param res matrix, contains in each row the simulated Markov chain from sarim. (e.g. by using get_beta(reduce = FALSE))
#'Therefore each row corresponds to a specific beta.
#'@param alpha numeric between 0 and 1, defining the width of the credibility interval (see details)
#'@param trueBeta the true coefficient image (e.g. smoothBeta()).
#'@param info logical, if true the number of non-zero pixels will be printed.
#'@param tol numeric, threshold at which value a pixel is assumed to be zero.
#'
#'@details The MSE is calculated over the union of all non-zero pixels of the estimated
#'and the real coefficient image. Firstly the credible inverval for each markov chain
#'is calculated (i.e. alpha/2 for lower and upper bound). Afterwards it will be examined,
#'if the interval contains the zero. Then all coefficients, which are non-zero
#'(indicated through the credible interval or in the real coefficient image) will be
#'used for the calculation of the MSE between the true and the estimated
#'image where one has to specify the point estimatior for the posterior (default is mean).
#'
#'@return Returns MSE
#'@export
#'
calc_mse <- function(res, alpha = 0.05, trueBeta, info = TRUE, tol = 1e-4, reduce = mean){
  require(magrittr)
  checkmate::assert_number(alpha, lower = 0, upper = 1)
  checkmate::assert_logical(info)
  checkmate::assert_numeric(tol)
  checkmate::assert_matrix(res, mode = "numeric")
  checkmate::assert_matrix(trueBeta, mode = "numeric")
  if(nrow(res) != length(trueBeta)) {stop("dimension between true and estimation differ")}

  # calculate quantiles
  q <- c(alpha/2, 1-alpha/2)

  # calculate upper and lower quantile for each coefs
  quants <- apply(res, 1, quantile, probs = q)

  # identfy all coefs where the 0 is not in the interval
  ident <- apply(quants, 2, function(x) ifelse(x[1]< 0 & x[2]>0, F, T))
  if (info)print(paste("Sum of all non-zero values: ", sum(ident)))

  # calc union of ident-zeros and true non-zeros
  union_set <- c(which(ident), which(abs(trueBeta) > tol)) %>% unique()
  (reduce((res[union_set]-trueBeta[union_set])^2))
}
