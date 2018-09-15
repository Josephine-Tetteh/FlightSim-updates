# FUNCTION 1
#' Morphological data from bird
#'
#' This function creates a list of user-defined morphological description parameters of an
#' input bird. At least three parameters need to be specified: body mass, wing span and
#' wing area. This definition of the bird is then used by the other functions in the
#' package to estimate flight performance.

#' @param BMass Body mass of bird in kilograms (kg)
#' @param WSpan Wingspan (maximum distance between wingtips) of bird in meters (m)
#' @param WArea Wing area (area of fully stretched wings) of bird in square meters (\eqn{m^{2}}{m2})
#' @param C_db Body drag coefficient
#' @param C_dpro Profile drag coefficient
#' @param ADensity Air density
#' @param grav Acceleration due to gravity
#' @param k induced drag factor
#' @details This function defines the morphological parameters of an input bird and some
#' other variables for power calculation. At least three bird parameters, body mass (\code{BMass}), wing span (\code{WSpan}) and
#' wing area (\code{WArea}),  need to be specified.
#'
#' The default value for both body drag and profile drag coeficient: \code{C_db} and \code{C_dpro} is 0.2. A constant air density
#' of 1.23 \eqn{kg/m^{3}}{kg/m3} is used with acceleration due to gravity, \code{grav} of 9.8 \eqn{m/s^{2}}{m/s2}.
#' The induced drag factor, \code{k}, is also given as 1.2 as used in the \emph{Flight Program (Pennycuick, 2008)}.
#' All default arguments can be varied as suitable. If no arguments are defined, the default values will be used.
#'
#' @return Morphological parameters of bird and other variables for power calculation.
#' @references Pennycuick, C. J. (2008). \emph{Modelling the flying bird} (Vol. 5). Elsevier.
#' @seealso \code{FlightSpeedComponents}
#'
#' @export
#
BirdMorphParam = function(BMass, WSpan, WArea, C_db=0.2, C_dpro=0.2, ADensity = 1.23,
                          grav=9.8,k=1.2){
  WLoading = BMass / WArea # calculate wing loading
  AR = WSpan^2/WArea  # calculate aspect ratio of wing
  BWeight = BMass*grav # calculate weight of bird

  ## flight performance parameters
  ## the following formulations are adapted from C.J. Pennycuick's FLIGHT 1.25 program
  Sb=0.00813*BMass^(0.666) # calculate body frontal area
  flap_freq = BMass^(3/8)*grav^(1/2)*WSpan^(-23/24)*WArea^(-1/3)*ADensity^(-3/8)  #### calculate flapping frequency
  Pbmr = 10^(log10(3.79*BMass^(0.723))) # calculate basal metabolic rate
  Pmet = 0.23*Pbmr # calculate metabolic Power
  Vmp = (0.807*k^0.25*BMass^0.5*grav^0.5)/(1.23^0.5*WSpan^0.5*Sb^0.25*C_db^0.25)  # calculate speed at minimum Power
  Pam = (1.05*k^0.75*BMass^(3/2)*grav^(3/2)*Sb^(1/4)*C_db^0.25)/(ADensity^0.5*WSpan^(3/2))   # calculate absolute minimum Power
  Mmusc = 0.17*BMass # calculate muscle mass

  #print results
  BirdParam = data.frame(BMass, BWeight, WSpan, WArea, AR, ADensity, WLoading, C_db, C_dpro, flap_freq, Sb, Pmet, Pbmr, Vmp, Pam, grav,k)
  return(BirdParam)
}
# END OF FUNCTION 1

