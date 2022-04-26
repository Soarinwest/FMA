###########################################################################################################################################################
# Title: Individual Tree Tree Per Acre Value
# Writer: Soren Donisvitch 
# Date 4/23/22
# Cited material: 
# Contact: soren.donisvitch@gmail.com
# Requirements: EXPF (the unique expansion value which brings sampling to per area unit representation)
# Reliance: If EXPF is unknown use DITERMINE_EXPF
###########################################################################################################################################################
#
# Description: Returns the individual tree value that represents the average number of trees, of a given size, present on any one acre of a forested tract.
#
# Inputs: EXPF = The ratio of plot area to a given land area
#             NOTE: see DITERMINE_EXPF function for further detail 
#
# Return: Numeric value of a given trees count over a given area of space 
#
# Example: A single tree within a 24 ft radius fixed area plot
#          tpa_t = TPA_T(0.04154172)
#
#          If you don't know EXPF:
#          tpa_t = TPA_T(DITERMINE_EXPF(MEASURMENT_SYST="imperial",UNIT="ft",DESIRED_AREA_UNIT="acre",SHAPE="circle",X = 24))

TPA_T = function(EXPF){
  tpa_t = 1/EXPF
}

