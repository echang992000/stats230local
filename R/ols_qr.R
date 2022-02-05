#' This function uses the QR method to estimate OLS regresion coefficients
#' @param data CSV file where first column is the response vector
#' @return vector of coefficients
#' @export
#' @examples ols_qr(data)
ols_qr=function(data){
  mdata=as.matrix(data)
  y=mdata[,1]
  x=mdata[,2:6]
  QR=qr(x)
  q=qr.Q(QR)
  r=qr.R(QR)
  b=solve(r)%*%t(q)%*%y
  return(b)
}