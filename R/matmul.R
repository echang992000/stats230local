#' This function takes in two matrices and a vector and multiplies them either (AB)x or A(Bx)
#' @param A First input matrix, square
#' @param B Second input matrix, square
#' @param x Vector input, assume dimension matches that of the matrices
#' @param arg Input of 1 or 2 corresponding to order of multiplication above
#' @return Product of the matrices and vector
#' @export
#' @examples matmul(matrix(1:9,nrow=3),matrix(10:18,nrow=3),matrix(1:3,nrow=3),1)
matmul=function(A,B,x,arg){if (arg==1){return((A %*% B) %*%x)} else if (arg==2){return(A %*% (B %*% x))} else print("error")}
