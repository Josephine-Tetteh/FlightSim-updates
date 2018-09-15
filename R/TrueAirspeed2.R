# FUNCTION 4 Alternative 2(user inputs wind speed(in m/s) and wind directions(in degrees) and then function calculates true airspeed)

#' True airspeed (alternative 2)
#'
#' Calculate true airspeed from wind direction and speed
#'
#' @param FlightSpeedComponents Components of speed of flight
#' @param V_WIND Windspeed
#' @param theta Wind directions in degrees
#' @param  phi Wind directions in degrees
#'
#' @return True airspeed
#' @examples
#' x <- c(1:5)
#' y <- c(1:5)
#' z <- c(1:5)
#' t <- c(1:5)
#' FS.Components <- FlightSpeedComponents(t,x,y,z)
#' T.airspeed <- TrueAirSpeed1(FS.Components, 30, 15, 10)
#'
#'
#' @export


TrueAirSpeed2 = function(FlightSpeedComponents, V_WIND, theta, phi){  #V_WIND is wind speed, theta and phi are wind directions
  FlightSpeed.x = FlightSpeedComponents$FlightSpeed.x
  FlightSpeed.y = FlightSpeedComponents$FlightSpeed.y
  FlightSpeed.z = FlightSpeedComponents$FlightSpeed.z
  if(missing(phi)) WindSpeed.z=0;
  WindSpeed.x = V_WIND*cos(theta)
  WindSpeed.y = V_WIND*sin(theta)
  WindSpeed.z = V_WIND*sin(phi)
  WindSpeed.total = sqrt((WindSpeed.x^2 + WindSpeed.y^2 + WindSpeed.z^2))
  TAS = sqrt((FlightSpeed.x + WindSpeed.x)^2 + (FlightSpeed.y + WindSpeed.y)^2 + (FlightSpeed.x + WindSpeed.z)^2)
  vuframe = data.frame(TAS)
  return(vuframe)
}
# END OF FUNCTION 4
