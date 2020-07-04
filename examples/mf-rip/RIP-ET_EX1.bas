# RIP-ET_EX1
# MODFLOW-RIP Example problem documented in: 
#   Maddock, T., III, Baird, K.J., Hanson, R.T., Schmid, Wolfgang, and Ajami, H., 2012, RIP-ET: A riparian evapotranspiration package for MODFLOW-2005: U.S. Geological Survey Techniques and Methods 6-A39, 76 p.
# 
# Input modified for MODFLOW-OWHM by adding a BAS OPTIONS BLOCK with output options.
#
#-----------------------------------------------------------------------------------------------------------------------------------------------------
# Riparian ET Test Model, 2 Season Model
#
BEGIN OPTIONS
   BUDGETDB                ./output/RIP-ET_EX1_VolumetricBudget.txt  
   PRINT_HEAD   NPER       ./output/RIP-ET_EX1_Head.txt
   CUMULATIVE_HEAD_CHANGE  ./output/RIP-ET_EX1_CumHCHG.txt  
END OPTIONS
#
INTERNAL  1 (FREE) -1   # IBOUND Lay 1
-1 1 1 1 1 0 0 0 0 0 1 1 1 1 1 1 1 1 0 0
-1 1 1 1 1 0 0 0 0 0 1 1 1 1 1 1 1 1 0 0
-1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0
0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0
0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
0 0 0 0 0 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0
0 0 0 0 0 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0
-999.0000
EXTERNAL 40 1.0 (FREE) 3
