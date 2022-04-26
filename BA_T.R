###########################################################################################################################################################
# Title: Individual Tree Basal Area
# Writer: Soren Donisvitch 
# Date 4/23/22
# Cited material: 
# Contact: soren.donisvitch@gmail.com
# Requirements: Basal area factor and diameter of tree 
# Reliance: If Basal area factor is unknown use BAF
###########################################################################################################################################################
#
# Description: Basal area is the cross-sectional area of trees at breast height (1.3m or 4.5 ft above ground). 
#
# Inputs: DBH = Diameter at breast height (1.3m or 4.5 ft above ground) in either inches or centimeters 
#
# Return: Basal area of tree either in meters^2 or in ft^2 dependent on BAF 
#         NOTE: if measured in inches will return ft^2, and if measured in cm will return meters^2
#
# Example: The basal area of a single tree with a diameter of 5.5 inches 
#          ba_t = BA_T(DBH= 5.5,BAF=0.005454)
#
#          With unkown BAF:
#          ba_t = BA_T(DBH= 5.5,BAF=BAF(UNIT = "in"))
#          With unkown BAF in metric:
#          ba_t = BA_T(DBH= 5.5,BAF=BAF(UNIT = "cm"))

BA_T = function(DBH,BAF){
  ba_t = BAF * (DBH^2)
}
