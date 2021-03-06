% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_trajectory.R
\name{PlotTrajectory}
\alias{PlotTrajectory}
\title{Plot input trajectory}
\usage{
PlotTrajectory(x, y, z)
}
\arguments{
\item{x}{x-position from data}

\item{y}{y-position from data}

\item{z}{z-position from data}
}
\value{
3D scatterplot of input trajectory
}
\description{
Plot input trajectory
}
\examples{
utils::data(trajectory)
trajectory = trajectory
PlotTrajectory(x=trajectory[,1], y=trajectory[,2], z=trajectory[,3])
}
