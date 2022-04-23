###########################################################################################################################################################
# Title: Standardizing sampling unit function
# Writer: Soren Donisvitch 
# Date 4/23/22
# Cited material: 
# Contact: soren.donisvitch@gmail.com
# Requirements: Basic knowledge of sampling units and system of measurement
###########################################################################################################################################################
#
# Standardizes sampling unit to singular unit metric and system (meters in metric). This function is built to be used in determining the relative 
# expansion factor associated with tree sampling to per acre plot summary statistics. However this function proves useful for other calculations that may 
# standardize a rage of input metrics so that calculations may be simplified to single iterative processes instead of variable calculations based in 
# different sampling systems and units.
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
# X = numeric unit desired to be standardized to meters
#
# Return:
# this function returns a single numeric value in standardized meters 
#
# Example:
#
# 2_chainz_in_meters = STANDARDIZE_UNIT(MEASURMENT_SYST = "imperial",UNIT = "chain",2)

STANDARDIZE_UNIT=function(MEASURMENT_SYST,UNIT,X){
  if (MEASURMENT_SYST == "imperial"){
    if (UNIT == "in"){x = X/39.37} 
    else if (UNIT == "ft"){x = X/3.281}
    else if (UNIT == "yard"){x = X/1.094}
    else if (UNIT == "chain"){x = X/20.117}} 
  else if(MEASURMENT_SYST == "metric"){
    if (UNIT == "cm"){x = X/10} 
    else if (UNIT == "m"){x = X}
    else if (UNIT == "km"){x = X*1000}
    return(as.numeric(x))}
}


