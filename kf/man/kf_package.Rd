\name{kf-package}
\alias{kf-package}
\alias{kf}
\docType{package}
\title{
  A implementation of Kalman Filter
}
\description{
  This contains basic procedures to solve Kalman Filter.
}
\details{
\tabular{ll}{
Package: \tab kf\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2012-12-22\cr
License: \tab MIT\cr
}
}
\author{
  jaris

Maintainer: jaris <yourfault@somewhere.net>
}
\references{
  R. E. Kalman,1960: A new approach to linear filtering and prediction problems, J. Basic Eng., Trans. ASME, 82D, 1, pp. 35-45
}
\keyword{ package }
\keyword{ kalman filter }
\seealso{
~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
  # v,w: amplitude of steady noizes
  # x: true state
  v <- 0.5
  w <- v * v * 0.25
  x <- c(2,1)
  # generate sample data
  ct <- cbind(cos(seq(0,4*pi,0.01)),1)
  y <- matrix(ct \%*\% x + v * rnorm(nrow(ct)),nrow=nrow(ct),ncol=1)
  dim(ct) <- c(nrow(ct),1,2)
  # get steady matrices
  ksys <- kf_steady(diag(2),diag(2),diag(2),c(0,0),v*v*diag(1),w*diag(2))
  # run kalman filter
  kf.res <- kf_rtn(c(0,0),y,ct,diag(2),ksys)
  # plot y(t), y~(t|t-1,t-1)
  kf_plot(kf.res)
  # estimating error at the last state
  mean((x-kf.res$x[kf.res$n,])**2)
}
