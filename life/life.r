# life game simulation
# ( cyclic boundary )

# initialize environment to run life game
# @param x width
# @param y height
# @param live alive condition
# @param birth birth condition
# @return environment to run life game
life_init <- function(x,y,live=c(2,3),birth=3){
  res <- list(x=x,y=y,n=x*y)
  res$p <- matrix(ifelse(runif(res$n) > 0.5,TRUE,FALSE),nrow=x,ncol=y)
  res$live <- live
  res$birth <- birth
  return(res)
}

# run life game
# @param d return value of life_init() or this
# @param img if TRUE, draw image, else, not
# @param col color for image()
# @param ... par for image()
# @return updated environment
life_run <- function(d,img=TRUE,...,col=c('white','black')){
  res <- d$p
  for(j in 1:d$y){
    j2 <- (j-1):(j+1)
    j2 <- ifelse(j2 > 0, (j2-1) %% d$y + 1, d$y - j2)
    for(i in 1:d$x){
      i2 <- (i-1):(i+1)
      i2 <- ifelse(i2 > 0, (i2-1) %% d$x + 1, d$x - i2)
      n <- sum(d$p[i2,j2]) - d$p[i,j]
      if( d$p[i,j] ){
        res[i,j] <- is.element(n,d$live)
      }else{
        res[i,j] <- is.element(n,d$birth)
      }
    }
  }
  d$p <- res
  if(img) image(res,...,col=col)
  return(d)
}

if( FALSE ){
  # below, sample code to run
  wk <- life_init(100,100)
  # life_run() may be too fast, ask each drawing
  par(ask=TRUE)
  for(i in 1:100) wk <- life_run(wk)
}
