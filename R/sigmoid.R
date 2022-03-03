#' Calculate sigmoid 
#' @param theta vector of length equal to number of parameters
#' @param x matrix of data points by parameter number
#' @return  sigmoid of each data point
#' @export
sigmoid=function(theta,x){
  return((1+exp(-x%*%theta))**-1)
}
