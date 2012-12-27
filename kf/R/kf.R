# initialize translation and noizes as steady state
kf_steady <- function(a,b,d,u,v,w){
  return(list(a=a,bu=b%*%u,dwd=d%*%w%*%t(d),v=v))
}
# unit procedure
kf_unit <- function(x,y,ct,p,ksys){
  # predict t-1|t-1 |-> t|t-1
  x <- ksys$a %*% x + ksys$bu
  e <- y - ct %*% x
  m <- ksys$a %*% p %*% t(ksys$a) + ksys$dwd
  mc <- m %*% ct
  k <- mc %*% solve(ct %*% mc + ksys$v)
  # update t|t-1 |-> t|t
  x <- x + k %*% e
  p <- (diag(nrow(p)) - k %*% ct) %*% m
  p <- (p + t(p)) * 0.5                   # p should be symmetric matrix
  return(list(x=x,e=e,k=k,p=p))
}
# routine for time-series
kf_rtn <- function(x,y,ct,p,ksys){
  if( is.list(x) )
    return(Recall(x$xs[x$n+1,],x$y,x$ct,x$ps[[x$n+1]],x$ksys))

  n <- nrow(y)
  xs <- ks <- matrix(nrow=n+1,ncol=2); xs[1,] <- x
  fs <- matrix(nrow=n,ncol=ncol(y))
  ps <- list(); ps[[1]] <- p
  for(k in 1:n){
    fs[k,] <- ct[k,,] %*% xs[k,]
    wk <- kf_unit(xs[k,],y[k,],ct[k,,],p,ksys)
    ps[[k+1]] <- p <- wk$p
    xs[k+1,] <- wk$x
    ks[k,] <- wk$k
  }
  return(list(xs=xs,y=y,ct=ct,ps=ps,ks=ks,fs=fs,n=n,ksys=ksys))
}
kf_demo <- function(v=0.5,w=v*v*0.5){
  ct <- cbind(cos(seq(0,4*pi,0.01)),1)
  y <- matrix(ct %*% c(2,1) + v * rnorm(nrow(ct)),nrow=nrow(ct),ncol=1)
  dim(ct) <- c(nrow(ct),1,2)
  ksys <- kf_steady(diag(2),diag(2),diag(2),c(0,0),v*v*diag(1),w*diag(2))
  return( kf_rtn(c(0,0),y,ct,diag(2),ksys) )
}
kf_plot <- function(rs,xr=1:rs$n,ad=1){
  y <- rs$y[xr,ad]; f <- rs$fs[xr,ad]
  yl <- range(pretty(y),pretty(f))
  plot(y,ylim=yl,xlab='t',ylab='value')
  lines(f,col=2,lty=2)
  legend("topleft",c('obs','fcst(t,t-1)'),col=c(1,2),pch=c(1,-1),lty=c(0,2))
}

