###########################################################################################################################################################
# Title: Ditermins plot expansion factor from sample design infromation
# Writer: Soren Donisvitch 
# Date 4/23/22
# Cited material: 
# Contact: soren.donisvitch@gmail.com
# Requirements: Basic knowledge of sampling methodology 
# Reliances : STANDARDIZE_UNIT
###########################################################################################################################################################
#
# This function will return the plot expansion factor (EXPF) which controls how information collected for a single sample tree will be expanded to a 
# whole-acre or land unit area basis.
#
# Inputs:
# MEASURMENT_SYST = "imperial" : Imperial system of measurement 
#                 = "metric" : Metric system
#                   NOTE: this is not a redundant feature, it may prove useful in plotting and further calculations to know what unit system was used in 
#                         original sampling system - and may be used as vector for future naming of automated plotting 
# UNIT = "in": inches to meters
#      = "ft": feet to meters
#      = "yard": yards to meters
#      = "chain": chain to meters
#        NOTE: Chain refers to the old Gunter's System of measurement. An Archaic system of measurement common in land systems that stems from old English 
#              surveying. An an actual physical metal chain was a standard unit of measurement, and was once used to parse land. As a result, much of
#              traditional forestry still has reference to this unit of measurement and is therefor included. 
#      = "cm": centimeters to meters
#      = "m": meters to meters
#      = "km": kilometers to meters 
# DESIRED_AREA_UNIT = "acre": measurements in a per acre basis
#                   = "hectare": measurements in a per hectare basis
# SHAPE = "circle": plot is sampled a circle
#       = "square": plot is sampled a square
# X = length of primary plot measurement unit
#   !NOTE!: for circles this is the radius and squares this is a side
#
# Return:
# This function returns plot expansion factor (EXPF) 
#
# Example:
# I sampled the forest with a circular plot with a 25 ft radius and want to expand to a per acre basis
# DETERMINE_EXPF(MEASURMENT_SYST="imperial",UNIT="ft",DESIRED_AREA_UNIT="acre",SHAPE="circle",X = 25)
#
# I sampled the forest with a square plot with a 25 ft side and want to expand to a per hectare basis
# with function argument order understanding
# DETERMINE_EXPF("imperial","ft","hectare","square",X = 25)

DETERMINE_EXPF = function(MEASURMENT_SYST,UNIT,DESIRED_AREA_UNIT,SHAPE,X){
  if(MEASURMENT_SYST != "metric" & UNIT != "m"){
    x = STANDARDIZE_UNIT(MEASURMENT_SYST=MEASURMENT_SYST,UNIT=UNIT,X=X)
  } else if(MEASURMENT_SYST == "metric" & UNIT == "m" ){ 
    x = X
  } else {print("Unit conversion issue. Check unit system input and function compatability")}
  if(SHAPE == "circle"){
    area = pi*x^2
  } else if(SHAPE == "square"){
    area = x^2
  } 
  if(DESIRED_AREA_UNIT == "acre"){
    EXPF = area/4047
  } else if(DESIRED_AREA_UNIT == "hectare"){
    EXPF = area/10000
  }
  return(EXPF)
  print(EXPF)
}



  





