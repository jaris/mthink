# geme of life simulation
# ( cyclic boundary )

# initialize environment to run game of life
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

# return famouse pattern
life_famous <- function(num=0){
  if( num == 0 ){
     res <- life_init(38,38)
     res$p[,] <- FALSE
     res$p[2:3,6:7] <- TRUE
     res$p[c(12,18),6:8] <- TRUE
     res$p[c(13,17),c(5,9)] <- TRUE
     res$p[14:15,c(4,10)] <- TRUE
     res$p[c(16,19),7] <- TRUE
     res$p[22:23,4:6] <- TRUE
     res$p[24,c(3,7)] <- TRUE
     res$p[26,c(2:3,7:8)] <- TRUE
     res$p[36:37,4:5] <- TRUE
  }
  return(res)
}

# run game of life
# @param d return value of life_init() or this
# @param img if TRUE, draw image, else, not
# @param col color for image()
# @param ... par for image()
# @return updated environment
life_run <- function(d,img=TRUE,cycle=TRUE,...,col=c('white','black')){
  res <- d$p
  for(j in 1:d$y){
    j2 <- (j-1):(j+1)
    if( cycle ){
      j2 <- ifelse(j2 > 0, (j2-1) %% d$y + 1, d$y + j2)
    }else{
      j2 <- ifelse(j2 > 0, ifelse(j2 > d$y,d$y,j2), 1)
    }
    for(i in 1:d$x){
      i2 <- (i-1):(i+1)
      if( cycle ){
        i2 <- ifelse(i2 > 0, (i2-1) %% d$x + 1, d$x + i2)
      }else{
        i2 <- ifelse(i2 > 0, ifelse(i2 > d$x,d$x,i2), 1)
      }
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
