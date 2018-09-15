 # FUNCTION 6
#' Plot mechanical Powercurve
# Plots mechanical power and airspeed
#' @param FlightPerformance Evaluated mechanical power
#' @param TrueAirSpeed1 True airspeed
#' @param TrueAirSpeed2 True airspeed
#' @return Mechanical power curve
#' @import graphics stats
#' @details  This function plots mechanical power against flight speed.
#' @examples
#' utils::data(trajectory)
#' trajectory = trajectory
#' bird = BirdMorphParam(0.7710, 0.98, 0.119, C_db=0.1)
#' FSComponents = FlightSpeedComponents(t=trajectory[,4],
#'                    x=trajectory[,1], y=trajectory[,2], z=trajectory[,3])
#' Tairspeed = TrueAirSpeed1(FSComponents)
#' FPerformance = FlightPerformance(bird,FSComponents, Tairspeed,
#'                    C_l = 0.5, C_t = 0.1)
#' PC = PowerCurve(FPerformance,Tairspeed)

#' @export

PowerCurve = function(FlightPerformance, TrueAirSpeed1, TrueAirSpeed2){
  if(missing(TrueAirSpeed1))  TAS = TrueAirSpeed2$TAS;
  if(missing(TrueAirSpeed2)) TAS = TrueAirSpeed1$TAS;
  AA = FlightPerformance$TAS
  BB = as.numeric(FlightPerformance$Pmech_data)
  Vmp = FlightPerformance$Vmp_data
  Powercurve = plot(x= AA, y= BB,type = "p", log="y", xlab="Velocity (m/s)", ylab="Mechanical Power (W)", main="Mechanical Powercurve")
  abline(v = FlightPerformance$Vmp_data, col = "blue", lty = 2)
  return(Powercurve)
}
# END OF FUNCTION 6


