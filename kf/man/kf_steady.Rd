\name{kf_steady}
\alias{kf_steady}
\title{
  get steady noizes in Kalman Filter probrems
}
\description{
  In general, Kalman Filter concern many parameter to solve its probrems.
  However, these noizes maynot be well-known except for a ideal experiment.
  This setup these as steady state.
\usage{
kf_steady(a,b,d,u,v,w)
}
\arguments{
  \item{a}{ time operator of state vector  }
  \item{b}{ correration of elements in u }
  \item{d}{ correration of elements in w }
  \item{u}{ control signals }
  \item{v}{ dispersion of observation noizes }
  \item{w}{ dispersion of time prediction noizes }
}
\details{
}
\value{
  \item{a}{ same to input }
  \item{bu}{ b \%*\% u }
  \item{dwd}{ d \%*\% w \%*\% t(d) }
  \item{v}{ same to input }
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
