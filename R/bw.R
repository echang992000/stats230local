#' Baum Welch Alg 
#' @param p0 prob matrix from uniform dist
#' @param e0 emissions matrix, top row is true, rest is from uniform dist
#' @param v0 starting point, uniform dist
#' @param tol cutoff for convergence
#' @return  p,e, and v after
#' @export

p0=matrix(c(0.5,.5,.5,.5),ncol=2)
e0=matrix(rep(c(1/6,1/6),6),ncol=6)
v0=matrix(c(1/2,1/2),ncol=1)
g2=10
bw=function(p0,e0,v0,tol){
for (k in 1:1000){
  a=matrix(0,2,101)
  a[,1]=v0*e0[,y[1]]
  for (t in 1:100){
    for (i in 1:2){
      for (j in 1:2){
        a[i,t+1]=a[i,t+1]+e0[i,y[t+1]]*a[j,t]*p0[i,j]
      }
    }
  }
  
  b=matrix(0,2,101)
  b[,dim(b)[2]]=1
  for (t in 100:1){
    for (i in 1:2){
      for (j in 1:2){
        b[i,t]=b[i,t]+p0[i,j]*e0[1,y[t+1]]*b[j,t+1]
      }
    }
  }
  b[,1]=sum(v0*e0[,y[1]]*b[,2])
  
  gam=matrix(0,2,101)
  for (t in 1:101){
    for (i in 1:2){
      gam[i,t]=a[i,t]*b[i,t]/(t(a[,t])%*%b[,t])
    }
  }
  g1=sum(gam)
  
  g=array(0,dim=c(2,2,101))
  for (t in 2:101){
    denom=0
    for (i in 1:2){
      for (j in 1:2){
        g[i,j,t]=b[j,t]*e0[j,y[t]]*p0[i,j]*a[i,t-1]
        denom=denom+b[j,t]*e0[j,y[t]]*p0[i,j]*a[i,t-1]
      }
    }
    g[,,t]=g[,,t]/denom
  }
  
  v0=gam[,1]
  p0=matrix(0,2,2)
  for (i in 1:2){
    for (j in 1:2){
      p0[i,j]=sum(g[i,j,2:101])/(sum(g[i,1,2:101])+sum(g[i,2,2:101]))
    }
  }
  
  for (l in 1:6){
    e0[2,l]=sum((y==l)*gam[i,])/sum(gam[i,])
  }
  if (abs(log(g2)-log(g1)<tol)){
    break
  }
}
  return(list(p0,e0,v0))
}