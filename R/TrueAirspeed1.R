# FUNCTION 3. To calculate effect of the wind on flight speed of the bird. Alternative 1 (user inputs x,y,z components of wind speed and the function, TAS, calculates true airspeed)

#' True airspeed (alternative 1)
#'
#' This function is the first function for evaluating the effect of wind on flight speed.
#' It calculates true airspeed from user-defined x,y,z- windspeed components.
#'

#' @param FlightSpeedComponents Components of speed of flight
#' @param WindSpeed.x x-compenent of speed of wind
#' @param WindSpeed.y y-compenent of speed of wind
#' @param WindSpeed.z z-compenent of speed of wind
#'
#' @details bbbb

#' @return True air speed
#'
#' @references bbb
#' @export


TrueAirSpeed1 = function(FlightSpeedComponents, WindSpeed.x, WindSpeed.y, WindSpeed.z){
  FlightSpeed.x = FlightSpeedComponents$FlightSpeed.x
  FlightSpeed.y = FlightSpeedComponents$FlightSpeed.y
  FlightSpeed.z = FlightSpeedComponents$FlightSpeed.z
  if(missing(WindSpeed.x))   WindSpeed.x = 0;
  if(missing(WindSpeed.y))   WindSpeed.y = 0;
  if(missing(WindSpeed.z))   WindSpeed.z = 0;
  # if(is.nan(FlightSpeed.y))  WindSpeed.y = 0;
  # if(is.infinite(FlightSpeed.x))  WindSpeed.x = 0;
  # if(is.infinite(FlightSpeed.z))  WindSpeed.z = 0;
  # if(FlightSpeed.z == "-Inf")  WindSpeed.z=0;
  WindSpeed.total = sqrt((WindSpeed.x^2 + WindSpeed.y^2 + WindSpeed.z^2))
  TAS = sqrt((FlightSpeed.x + WindSpeed.x)^2 + (FlightSpeed.y + WindSpeed.y)^2 + (FlightSpeed.z + WindSpeed.z)^2)
  vuframe = data.frame(TAS)
  return(vuframe)
}
# END OF FUNCTION 3
