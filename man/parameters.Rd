\name{parameters}
\alias{parameters}
\docType{data}
\title{ Morphological parameters of bird species}
\description{Morphological parameters of species extracted from \emph{Pennycuick, C. J., Åkesson, S., & Hedenström, A. (2013)} }
\usage{data(parameters)}
\format{
  A data frame with 25 observations on the following 5 variables.
  \describe{
    \item{\code{Scientific.Name}}{a character vector}
    \item{\code{Common.Name}}{a character vector}
    \item{\code{Mass}}{a numeric vector}
    \item{\code{Span}}{a numeric vector}
    \item{\code{Wing.area}}{a numeric vector}
  }
}
\details{Morphological parameters of species}
\source{
Pennycuick, C. J., Åkesson, S., & Hedenström, A. (2013). Air speeds of migrating birds observed by ornithodolite and compared with predictions from flight theory. Journal of the Royal Society Interface, 10(86), 20130419.
}
\examples{utils::data(parameters)
morphParam <- BirdMorphParam(parameters[,3], parameters[,4], parameters[,5])}
\keyword{datasets}
