#' This function takes in a vector mu and a positive definite matrix sigma and returns realizations from the multivariate normal distribution using cholesky decomposition
#' @param mu First input, n dimensional vector
#' @param sigma Second input, nxn positive definite matrix
#' @param N Third input, number of realizations
#' @return Multivariate normal distribution 
#' @export
#' @examples mvn(rnorm(4),Posdef(4))
mvn=function(mu,sigma,N){
  n=length(mu)# dimension of space
  L=t(chol(sigma)) # cholesky factorization
  z=matrix(rnorm(n*N),nrow=4)
  x=mu+L%*%z
  return(x)
}
