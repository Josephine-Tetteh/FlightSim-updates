# FUNCTION 4 Alternative 2(user inputs wind speed(in m/s) and wind directions(in degrees) and then function calculates true airspeed)

#' True airspeed (alternative 2)
#'
#' Calculate true airspeed from wind direction and speed
#'
#' @param FlightSpeedComponents Components of speed of flight
#' @param V_WIND Windspeed
#' @param theta Wind directions
#' @param  phi Wind directions
#'
#' @details bbb
#'
#' @return TrueAirSpeed
#'
#' @references bbb
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
