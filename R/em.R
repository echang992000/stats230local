#' EM alg 
#' @param n vector of na, nab, nb, no
#' @param pa probability of type A
#' @param pb probability of type B
#' @param po probability of type O
#' @return  3x10 matrix, each row corresponds to pa, pb, and po
#' @export

em=function(n,pa,pb,po){
  k=10  
  na=n[1]
  nab=n[2]
  nb=n[3]
  no=n[4]
  p=matrix(0,3,k+1)
  p[,1]=c(pa,pb,po)
  for (k in 1:10){
    pa=p[1,k]
    pb=p[2,k]
    po=p[3,k]
    maa=na*pa**2/(pa**2+2*pa*po)
    mao=na*pa*2*po/(pa**2+2*pa*po)
    mbb=nb*pb**2/(pb**2+2*pb*po)
    mbo=nb*pb*2*po/(pb**2+2*pb*po)
    mab=nab
    moo=no
    pa=(2*maa+mao+nab)/(2*sum(n))
    pb=(2*mbb+mbo+nab)/(2*sum(n))
    po=(2*no+mao+mbo)/(2*sum(n))
    p[,k+1]=c(pa,pb,po)
  }
  return(p)
}
