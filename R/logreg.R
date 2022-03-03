#' Calculate log regression coefficients 
#' @param x matrix of data points by parameter number
#' @param y reponse variable
#' @param method 1 if grad desc, 2 if newton
#' @return  coefficients, confidence interval (width/2), likelihood over iterations
#' @export
logreg=function(x,y,method){
if (method==1){
n=100
eta=0.001 # step size
theta=matrix(0,ncol(x),n+1) # initialize parameters as 0, no. of parameters rows and no. of timestep columns
for (j in 1:n){
  theta[,j+1]=theta[,j]+eta*t((y-sigmoid(theta[,j],x)))%*%x
}

lik=numeric(n) #likelihood calculation
for (j1 in 1:n){
  for (j2 in 1:dim(x)[1]){
    lik[j1]=lik[j1]+y[j2]*log((1+exp(-theta[,j1]%*%x[j2,]))**-1)+(1-y[j2])*log(1-(1+exp(-theta[,j1]%*%x[j2,]))**-1)
  }
}
} else{ # newton method
  n=100
  beta=matrix(0,ncol(x),n+1)
  for (k in 1:n){
    s=sigmoid(beta[,k],x)
    w=diag(c(s*(1-s)))
    beta[,k+1]=beta[,k]+solve(t(x)%*%w%*%x)%*%(t(x)%*%(y-s))
  }
  likb=numeric(n)
  for (j1 in 1:n){
    for (j2 in 1:dim(x)[1]){
      likb[j1]=likb[j1]+y[j2]*log((1+exp(-beta[,j1]%*%x[j2,]))**-1)+(1-y[j2])*log(1-(1+exp(-beta[,j1]%*%x[j2,]))**-1)
    }
  }

theta=beta
lik=likb
}
  
  
theta_sub=theta[,92:101]
error=numeric(9)
for (i in 1:9){
  error[i]=qnorm(0.975)*sd(theta_sub[i,])/sqrt(10)
}

return(list(theta,error,lik))
}


# define sigmoid function
sigmoid=function(theta,x){
  return((1+exp(-x%*%theta))**-1)
}
