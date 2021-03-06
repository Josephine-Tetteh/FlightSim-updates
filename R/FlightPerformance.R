# FUNCTION 5
#' Compute flight performance parameters
#'
#' This function evaluates the power associated with the user-input flight trajectory
#' by using some other functions in this package. It also considers the 3D position of
#' the bird.
#'
#' @param BirdMorphParam Basic morphological data of bird
#' @param FlightSpeedComponents Calculated flight speed components
#' @param TrueAirSpeed1 True airspeed from function \code{TrueAirSpeed1}
#' @param TrueAirSpeed2 Trueairspeed from function \code{TrueAirSpeed2}
#' @param C_l lift coefficent
#' @param C_t thrust coefficient
#'
#' @details This function estimates all flight performance parameters by evaluating the various
#' types of aerodynamic forces acting during flight. It also checks for the feasibility of the
#' input trajectory and evaluates the amount of power required at each timestep. It also calculates the
#' mechanical and chemical power required for flight. The function makes use of the inputs for
#' \code{BirdMorphParam} and flight speed from \code{FlightSpeedComponents} as well as the 3D position
#' of the bird in flight. There is no default value for lift coefficient and thrust coefficient.
#' To accounT for wind effect, the true airspeed is used either from \code{TrueAirSpeed1} or
#' \code{TrueAirSpeed2}.
#'
#' @return Flight performance parameters made up of a dataset of all associated powers
#' which do not include infinite values. It also investigates the flight types as either straight steady, climbing or descending flight.
#'
#' @references Norberg, U. M. (2012). Vertebrate flight: mechanics, physiology, morphology, ecology and
#' evolution (Vol. 27). Springer.
#' @examples
#' utils::data(trajectory)
#' trajectory = trajectory
#' birdpa = BirdMorphParam(0.043, 0.4, 0.0171)
#' FS.Components = FlightSpeedComponents(trajectory[,4],
#'   trajectory[,1], trajectory[,2], trajectory[,3])
#' T.airspeed = TrueAirSpeed1(FS.Components)
#' FPerformance = FlightPerformance(birdpa,FS.Components,
#' T.airspeed, C_l = 0.5, C_t = 0.1)

#'
#' @export

FlightPerformance = function(BirdMorphParam,FlightSpeedComponents, TrueAirSpeed1, TrueAirSpeed2,
                             C_l, C_t) {
  if(missing(TrueAirSpeed1)) TrueAirSpeed2;
  if(missing(TrueAirSpeed2)) TrueAirSpeed1;

  BMass = BirdMorphParam$BMass
  WSpan = BirdMorphParam$WSpan
  WArea = BirdMorphParam$WArea
  BWeight = BirdMorphParam$BWeight
  AR = BirdMorphParam$AR
  C_db = BirdMorphParam$C_db
  C_dpro = BirdMorphParam$C_dpro
  Sb = BirdMorphParam$Sb
  Pam = BirdMorphParam$Pam
  Pmet = BirdMorphParam$Pmet
  TAS = TrueAirSpeed1$TAS
  ADensity = BirdMorphParam$ADensity
  grav = BirdMorphParam$grav
  k = BirdMorphParam$k

  # Prepare dataframe
  dataday11=data.frame(t=FlightSpeedComponents$t, x=FlightSpeedComponents$x, y=FlightSpeedComponents$y, z=FlightSpeedComponents$z)
  change_x=c(dataday11[1,2],diff(dataday11$x))
  change_y=c(dataday11[1,1],diff(dataday11$y))
  change_z=c(dataday11[1,3],diff(dataday11$z))
  step_length = sqrt(change_x^2+change_y^2+change_z^2)
  beta = atan(change_z/sqrt(change_x^2+change_y^2))  #angle of climb
  beta= ifelse(is.nan(beta), 0, beta)
  dataday11=data.frame(dataday11,change_x,change_y, change_z, TAS,beta)

  # Drag components and total drag
  d_ind = (2*k*(BMass*grav)^2)/(TAS^2*pi*WSpan^2*ADensity)  # induced drag
  d_pro = 0.5*ADensity*WArea*C_dpro*TAS^2  # profile drag
  d_par = 0.5*ADensity*Sb*C_db*TAS^2 # parasite drag

  drag_total = d_ind + d_par + d_pro #total aerodynamic drag
  #################################################################
  transform(dataday11)
  ### flight types
  for(i in 1:length(dataday11$x)){
    if(dataday11$change_z[i]<0){
      dataday11$flighttype[i] = "1"  ##### descent
    } else if(dataday11$change_z[i]>0){
      dataday11$flighttype[i] = "2"   #### climb
    }  else if(dataday11$change_z[i]==0){
      dataday11$flighttype[i] = "3"  #### steady
    }}

  ### Descending
  drag_desc = drag_total + BWeight*sin(beta)

  ## Straight
  StrSpeed = sqrt((2*BWeight)/(C_l*ADensity*WArea))  # velocity in straight flight
  d_ind_str = (2*k*(BMass*grav)^2)/(StrSpeed^2*pi*WSpan^2*ADensity)
  d_pro_str = 0.5*ADensity*WArea*C_dpro*StrSpeed^2
  d_par_str = 0.5*ADensity*Sb*C_db*StrSpeed^2
  StrDrag = d_ind_str + d_par_str + d_pro_str # total drag in a straight flight

  # Climbing
  thrust_climb = drag_total + BWeight*sin(beta)  ## thrust in a climb
  #Calculate climbing velocity
  #attach(dataday11)
  ClimbSpeed = c()
  for(j in 1:length(dataday11$x)){
    if((dataday11$flighttype[j]==2)){
      ClimbSpeed[j] = sqrt((2*BWeight*cos(beta[j]))/(C_l*ADensity*WArea))}  ### climbing velocity
    else{
      ClimbSpeed[j]= NA
    }}


  ##Checks for climbing and descending
  #Climb is feasible if TAS is greater or equals climbing velocity otherwise it is not feasible.
  Power=c()
  count = 0
  for(i in 1:length(dataday11$x)){
    if((dataday11$flighttype[i]==2)&(TAS[i]<as.numeric(ClimbSpeed[i]))){
      dataday11$feasib[i] = "not feas climb"
      Power[i] = NA
      count = count+1
    }
    else{
      dataday11$feasib[i] = "feas climb"
      Power[i] = TAS[i]*thrust_climb[i]
    }
    if(dataday11$flighttype[i]==1){
      #drag_desc = drag_total+BWeight*sin(beta)
      Power[i] = TAS[i]*drag_desc[i]
      dataday11$feasib[i] = "desc"
    }
    if((dataday11$flighttype[i]==3)&(TAS[i]>StrSpeed)){
      dataday11$feasib[i] = "feas str"
      Power[i] = TAS[i]*StrDrag
    }
    else if((dataday11$flighttype[i]==3)&(TAS[i]<StrSpeed)){
      dataday11$feasib[i] = "notfeas str"
      Power[i] = NA
      count = count+1
    }
  }

  power2 = c()
  for(i in 1:length(dataday11$x)){
    if(dataday11$flighttype[i]==2){
      power2[i] = TAS[i]*thrust_climb[i]
    }
    if(dataday11$flighttype[i]==1){
      #drag_desc = drag_total+weight*sin(beta)
      power2[i] = abs(TAS[i]*drag_desc[i])

    }
    if(dataday11$flighttype[i]==3){
      power2[i] = TAS[i]*StrDrag
    }
  }
  # end of checks

  # evaluate mechanical and chemical Power
  Pmech_data = (2*k*(BMass*grav)^2)/(TAS*pi*WSpan^2*ADensity) + (ADensity*TAS^3*Sb*C_db)/(2) + (ADensity*TAS^3*WArea*C_dpro)/(2) # + (8.4/Ra)*Pam  ##mechanical Power
  Pchem_data = 1.1*(Pmech_data+Pmet)/0.23  ##chemical Power

  # add to dataframe
  mydata = data.frame(dataday11,d_ind,d_pro, d_par, drag_total,  ClimbSpeed, Power, power2, Pmech_data,Pchem_data)

  #filter Inf values from dataset in order to plot
  Newdata = mydata[!is.infinite(Pmech_data), ]

  #calculate minimum Power velocity from data
  Vmp_data= TAS[which(Newdata$Pmech_data==min(Newdata$Pmech_data))]
  return(mydata)
}


