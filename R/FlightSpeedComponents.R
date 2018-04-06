# FUNCTION 2
#' Flight speed components
#'
#' The function first evaluates the x,y,z-components of the speed and then further evaluates the overall speed of
#' flight. This flight speed is the actual speed at which the bird moves in air relative to incident airflow.
#'
#' @details \code{FlightSpeedComponents} sets up the x,y,z- components of the speed at which the bird flies
#' from pre-defined 3D position and timestep after which it estimates the actual speed at which the bird is flying.
#'

# To calculate flight speed components from the position of the bird
#' @param t Timestep from data
#' @param x x-position from data
#' @param y y-position from data
#' @param z z-position from data
#' @return Numeric value of flight Speed
#'
#' @references ...

#' @export

FlightSpeedComponents = function(t,x,y,z){
  dataday11=data.frame(t, x=x, y=y, z=z)
  change_x=c(dataday11[1,2],diff(dataday11$x))
  change_y=c(dataday11[1,1],diff(dataday11$y))
  change_z=c(dataday11[1,3],diff(dataday11$z))
  FlightSpeed.x = change_x/t # speed of flight in the x-direction
  FlightSpeed.y = change_y/t # speed of flight in the y-direction
  FlightSpeed.z = change_z/t # speed of flight in the z-direction
  FlightSpeed = sqrt(FlightSpeed.x^2 + FlightSpeed.y^2 + FlightSpeed.z^2) #speed of flight due to position vectors of the bird
  Vframe = data.frame(t,x,y,z,FlightSpeed.x, FlightSpeed.y, FlightSpeed.z, FlightSpeed)
  return(Vframe)
}
# END OF FUNCTION 2
