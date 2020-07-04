# SWRSample04
# MODFLOW-SWR Example problem documented in: 
#    Hughes, J.D., Langevin, C.D., Chartier, K.L., and White, J.T., 2012, Documentation of the Surface-Water Routing (SWR1) Process for modeling surface-water flow with the U.S. Geological Survey Modular Ground-Water Model: U.S. Geological Survey Techniques and Methods 6-A40, 113 p.
# 
# Input modified for MODFLOW-OWHM by adding a BAS OPTIONS BLOCK with output options.
#
#-----------------------------------------------------------------------------------------------------------------------------------------------------
#
# MODFLOW2005 Basic Package
#   
BEGIN OPTIONS
   NOFREE                  # Fixed Formatted Read
   NOCBC
   BUDGETDB                ./output/SWRSample04_VolumetricBudget.txt  
   PRINT_HEAD   NPER       ./output/SWRSample04_Head.txt
   CUMULATIVE_HEAD_CHANGE  ./output/SWRSample04_CumHCHG.txt  
END OPTIONS
#
CONSTANT         1 (25I3)                      -1     # IBOUND Layer   1
9.9900e+02
CONSTANT     1.000 (10e12.4)                    0     # STARTING HEADS Layer   1
