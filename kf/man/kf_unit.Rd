\name{kf_unit}
\alias{kf_unit}
\title{
  Unit procedures of Kalman Filter
}
\description{
  t-1|t-1 => t|t-1 => t|t
}
\usage{
kf_unit(x,y,ct,p,ksys)
}
\arguments{
  \item{x}{
    a state vector, or result of this function.
    if is.list(x), other arguments would be ignored.
  }
  \item{y}{ observation matrix or vector }
  \item{ct}{ translation matrix or vector }
  \item{p}{ cov(x,x) }
  \item{ksys}{ other system information }
}
\details{
}
\value{
  \item{x}{ updated state vector }
  \item{k}{ kalman gain vector }
  \item{p}{ updated cov(x,x) }
  \item{e}{ estimation error at t|t-1 }
}
\references{
}
\author{
}
\note{
}

\seealso{
}
\examples{
}
\keyword{ kalman filter }
