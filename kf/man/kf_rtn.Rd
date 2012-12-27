\name{kf_rtn}
\alias{kf_rtn}
\alias{kf_rtn}
\title{
  run Kalman Filter with time series datas
}
\description{
}
\usage{
kf_rtn(x,y,ct,p,ksys)
}
\arguments{
  \item{x}{
    initial state vector, or result of this function.
    if is.list(x), other arguments would be ignored.
  }
  \item{y}{ observation matrix, [number of time,number of observation] }
  \item{ct}{ translation array, [number of time,ncol(y),length(x)] }
  \item{p}{ initial cov(x,x) }
  \item{ksys}{ other system information }
}
\details{
}
\value{
  \item{xs}{ matrix of updated state, row: n+1(1 means initial state), col: state }
  \item{y}{ same to input }
  \item{ct}{ same to input }
  \item{ks}{ matrix of kalman gain, row: n, col: state }
  \item{fs}{ matrix of estimate values with x(t-1|t-1) }
  \item{ps}{ list of cov(x(t|t),x(t|t)), length: n+1(1 means initial state) }
  \item{n}{ number of time }
  \item{ksys}{ same to input }
}
\references{
}
\author{
}
\note{
}

\section{Warning }{....}

\seealso{
}
\examples{
}
\keyword{ kalman filter }
