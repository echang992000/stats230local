#' This function uses the SVD decomposition to estimate OLS regresion coefficients
#' @param data CSV file where first column is the response vector
#' @return vector of coefficients
#' @export
#' @examples ols_svd(data)
ols_svd=function(data){
  mdata=as.matrix(data)
  y=mdata[,1]
  x=mdata[,2:6]
  s=svd(x)
  b=s$v%*%matrix(diag(1/s$d),nrow=5)%*%t(s$u)%*%y
  return(b)
}