% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Powercurve.R
\name{PowerCurve}
\alias{PowerCurve}
\title{Plot mechanical Powercurve}
\usage{
PowerCurve(FlightPerformance, TrueAirSpeed1, TrueAirSpeed2)
}
\arguments{
\item{FlightPerformance}{Evaluated mechanical power}

\item{TrueAirSpeed1}{True airspeed}

\item{TrueAirSpeed2}{True airspeed}
}
\value{
Mechanical power curve
}
\description{
Plot mechanical Powercurve
}
\details{
This function plots mechanical power against flight speed.
}
\examples{
utils::data(trajectory)
trajectory = trajectory
bird = BirdMorphParam(0.7710, 0.98, 0.119, C_db=0.1)
FSComponents = FlightSpeedComponents(t=trajectory[,4],
                   x=trajectory[,1], y=trajectory[,2], z=trajectory[,3])
Tairspeed = TrueAirSpeed1(FSComponents)
FPerformance = FlightPerformance(bird,FSComponents, Tairspeed,
                   C_l = 0.5, C_t = 0.1)
PC = PowerCurve(FPerformance,Tairspeed)
}
