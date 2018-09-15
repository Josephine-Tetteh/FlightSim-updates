\name{trajectory}
\alias{trajectory}
\docType{data}
\title{ Sample trajectory}
\description{A 3D flight trajectory containing x,y,z- coordinates and time.}
\usage{data(trajectory)}
\format{
  A data frame with 40 observations on the following 4 variables.
  \describe{
    \item{\code{x}}{a numeric vector}
    \item{\code{y}}{a numeric vector}
    \item{\code{z}}{a numeric vector}
    \item{\code{t}}{a numeric vector}
  }
}
\details{3D flight trajectories of the \emph{Peregrine falcon} obtained from Akos et al (2008).}
\examples{utils::data(trajectory)
FSC <- FlightSpeedComponents(trajectory[,4], trajectory[,1], trajectory[,2], trajectory[,3])}

\references{Akos, Z., Nagy, M., & Vicsek, T. (2008). Comparing bird and human soaring strategies. Proceedings of the National Academy of Sciences, 105(11), 4139-4143.}
\keyword{datasets}




