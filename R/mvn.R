# input mu (vector) and sigma (square pos def matrix)
# output: N realizations from multivariate normal dist using cholesky decomp

mvn=function(mu,sigma){
  n=length(mu)# dimension of space
  L=t(chol(sigma)) # cholesky factorization
  z=rnorm(n)
  x=mu+L%*%z
  return(x)
}

# n=4
# mu=matrix(rnorm(n),nrow=4)
# sigma=Posdef(n)