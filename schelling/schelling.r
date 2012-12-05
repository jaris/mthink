# schelling's segregation model simulation
#
#= references
# Schelling, T. (1978). Micromotives and Macrobehavior. New York: Norton.
#

# initialize environment for simulation
# @param x width
# @param y height
# @param th threshold ifelse move[%]
# @param agent number of agent.
# @param dx     search ray to calculate satisification
# @param pratio percent ratio between type1 and type2
# @return list()
#         p :: 1: type1, -1: type2, 0: empty
schell_init <- function(x,y,th=40,agent=as.integer(x*y*0.4)*2,dx=1,pratio=50){
  res <- list(x=x,y=y,n=x*y)
  rr <- max(0,min(1,pratio / 100))
  wk <- c(rep(1,agent * rr), rep(-1,agent * (1-rr)), rep(0,res$n - agent))
  res$p <- matrix(sample(wk),nrow=x,ncol=y)
  res$th <- min(100,max(0,th))
  res$agent <- agent
  res$empty <- res$n - agent
  res$ead <- order(res$p==0)[1:res$empty + res$agent]
  res$dx <- 1
  return(res)
}

# run simulation
# @param d return value from "schell_init" or this
# @param img if TRUE, draw image, else, not draw
# @param col colors to shade each types, c(type2,empty,type1)
# @param ... par for image()
# @return updated result
schell_run <- function(d,img=TRUE,...,col=c('black','gray','white')){
  cad <- order(d$p == 0)[sample.int(d$agent)]
  mad <- sample.int(d$empty,size=d$agent,replace=TRUE)
  
  #cat("***", d$agent,d$empty,sum(d$p!=0),sum(d$p==0),"\n")
  sat <- sim <- 0
  dx2 <- (2*d$dx+1)**2 
  iis <- t(matrix((cad-1)%%d$x + 1,nrow=d$agent,ncol=2*d$dx+1)) + (-d$dx:d$dx)
  jjs <- t(matrix((cad-1)%/%d$x + 1,nrow=d$agent,ncol=2*d$dx+1)) + (-d$dx:d$dx)
  iis <- ifelse(iis > 0, (iis-1) %% d$x + 1, d$x + iis)
  jjs <- ifelse(jjs > 0, (jjs-1) %% d$y + 1, d$y + jjs)
  for( cc in 1:d$agent ){
    num <- cad[cc]
    nb <- d$p[iis[,cc],jjs[,cc]]
    ne <- dx2 - sum(nb == 0) - 1
    n <- ifelse(ne == 0, 100, (sum(nb == d$p[num]) - 1) * 100 / ne)
    sim <- sim + n
    if( n < d$th ){
      d$p[d$ead[mad[cc]]] <- d$p[num]
      d$p[num] <- 0
      d$ead[mad[cc]] <- num
    }else{
      sat <- sat + 1
    }
  }
  if(img) image(d$p,...,col=col)
  cat(sprintf("satisfy: %8d (%7.4g)[%%], similarly: %7.4g[%%]\n",sat,sat / d$agent * 100,sim / d$agent))
  return(d)
}

if( FALSE ){
  # sample code to run
  wk <- schell_init(100,100)
  for(i in 1:100) wk <- schell_run(wk)
}
