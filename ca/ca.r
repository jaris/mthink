# cellular automata simulation

# run 
# @param x the length of pattern at a generation
# @param y the number of generations
# @param v0 initial pattern
# @param rule the rule as integer
# @return result of the simulation
ca_run <- function(x,y,v0=ifelse(runif(x) > 0.5, TRUE, FALSE),
    rule=30, cycle=FALSE){
  res <- matrix(v0,nrow=x,ncol=y)
  tt <- matrix( c(FALSE,FALSE,FALSE, FALSE,FALSE, TRUE,
                  FALSE, TRUE,FALSE, FALSE, TRUE, TRUE,
                   TRUE,FALSE,FALSE,  TRUE,FALSE, TRUE,
                   TRUE, TRUE,FALSE,  TRUE, TRUE, TRUE), nrow=3,ncol=8)
  bpat <- rule
  for(i in 2:8) bpat[(i-1):i] <- c(bpat[i-1] %% 2,bpat[i-1] %/% 2)
  ipat <- integer()
  for(i in 1:8) if( bpat[i] == 1 ) ipat <- c(ipat,sum(c(4,2,1) * tt[,i]))

  ad <- c(1,2:x-1,1:x,2:x,x)
  if( cycle ) ad[c(1,length(ad))] <- c(x,1)

  for(j in 2:y){
    vl <- matrix(res[ad,j-1],nrow=x,ncol=3) %*% c(4,2,1)
    res[,j] <- is.element(vl,ipat)
  }
  return(res)
}

# demo
if( FALSE ) image(wk <- ca_run(100,100))

