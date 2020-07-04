# mnw1
# MODFLOW-2005 Example problem documented in: 
#   Harbaugh, A.W., 2005, MODFLOW-2005, the U.S. Geological Survey modular ground-water model -- the Ground-Water Flow Process: U.S. Geological Survey Techniques and Methods 6-A16.
#   Harbaugh, A.W., Banta, E.R., Hill, M.C. and McDonald, M.G., 2000, MODFLOW-2000, The U.S. Geological Survey Modular Ground-Water Model - User Guide to Modularization Concepts and the Ground-Water Flow Process: U.S Geological Survey Open-File Report 2000-92, 121 p., https://doi.org/10.3133/ofr200092
#   Halford, K.J. and Hanson R.T., 2002, User Guide for the Drawdown-Limited, Multi-Node Well (MNW) Package for the U.S. Geological Survey’s Modular Three-Dimensional Finite-Difference Ground-Water Flow Model, Versions MODFLOW-96 and MODFLOW-2000t: U.S. Geological Survey Open-File Report 02-293, 33 p.
#
# Input modified for MODFLOW-OWHM by including the BAS options:
#    NOCBC  BUDGETDB ./output/mnw1_VolumetricBudget.txt  CUMULATIVE_HEAD_CHANGE ./output/mnw1_CumHCHG.txt  PRINT_HEAD NPER ./output/mnw1_Head.txt
#
#-----------------------------------------------------------------------------------------------------------------------------------------------------
#
#  3D, Transient aquifer to demonstrate MNW1 package
FREE  NOCBC  BUDGETDB ./output/mnw1_VolumetricBudget.txt  CUMULATIVE_HEAD_CHANGE ./output/mnw1_CumHCHG.txt  PRINT_HEAD NPER ./output/mnw1_Head.txt
CONSTANT  1      # Item 2: IBOUND, layer 1
CONSTANT  2      #         IBOUND, layer 2 
    999.         # Item 3: HNOFLO
CONSTANT  100.   # Item 4: STRT, layer 1
CONSTANT  100.   #         STRT, layer 2
