#' Generating a random positive-definite matrix with user-specified positive eigenvalues
#' @param n First input, dimension of matrix
#' @return Random pos def matrix 
#' @export
#' @examples Posdef(4)
Posdef <- function (n, ev = runif(n, 0, 10)) 
{
  Z <- matrix(ncol=n, rnorm(n^2))
  decomp <- qr(Z)
  Q <- qr.Q(decomp) 
  R <- qr.R(decomp)
  d <- diag(R)
  ph <- d / abs(d)
  O <- Q %*% diag(ph)
  Z <- t(O) %*% diag(ev) %*% O
  return(Z)
}

# sourced to Ravi Varadhan
# https://stat.ethz.ch/pipermail/r-help/2008-February/153708
