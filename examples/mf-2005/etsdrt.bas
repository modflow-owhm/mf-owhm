# etsdrt
# MODFLOW-2005 Example problem documented in: 
#   Harbaugh, A.W., 2005, MODFLOW-2005, the U.S. Geological Survey modular ground-water model -- the Ground-Water Flow Process: U.S. Geological Survey Techniques and Methods 6-A16.
#   Harbaugh, A.W., Banta, E.R., Hill, M.C. and McDonald, M.G., 2000, MODFLOW-2000, The U.S. Geological Survey Modular Ground-Water Model - User Guide to Modularization Concepts and the Ground-Water Flow Process: U.S Geological Survey Open-File Report 2000-92, 121 p., https://doi.org/10.3133/ofr200092
#   Banta, E.R., 2000, MODFLOW-2000, the U.S. Geological Survey Modular Ground-Water Model - Documentation of Packages for Simulating Evapotranspiration with a Segmented Function (ETS1) and Drains with Return Flow (DRT1): U.S. Geological Survey Open-File Report 00-466, 127 p.
# 
# Input modified for MODFLOW-OWHM by including the BAS options:
#    BUDGETDB ./output/etsdrt_VolumetricBudget.txt  CUMULATIVE_HEAD_CHANGE ./output/etsdrt_CumHCHG.txt  PRINT_HEAD NPER ./output/etsdrt_Head.txt
#
#-----------------------------------------------------------------------------------------------------------------------------------------------------
#
# BAS file for ets1, drt1, and etsdrt test cases
free  BUDGETDB ./output/etsdrt_VolumetricBudget.txt  CUMULATIVE_HEAD_CHANGE ./output/etsdrt_CumHCHG.txt  PRINT_HEAD NPER ./output/etsdrt_Head.txt
internal 1 (free) -1              # Item 2: Ibound
 -1  1  1  1  1  1  1  1  1  1 -1
 -1  1  1  1  1  1  1  1  1  1 -1
 -1  1  1  1  1  1  1  1  1  1 -1
 -1  1  1  1  1  1  1  1  1  1 -1
 -1  1  1  1  1  1  1  1  1  1 -1
 -1  1  1  1  1  1  1  1  1  1 -1
 -1  1  1  1  1  1  1  1  1  1 -1
 -1  1  1  1  1  1  1  1  1  1 -1
 -1  1  1  1  1  1  1  1  1  1 -1
 -1  1  1  1  1  1  1  1  1  1 -1
 -1  1  1  1  1  1  1  1  1  1 -1
-999.                             # Item 3: HNOFLO
INTERNAL  1.0  (FREE)  -1         # Item 4: STRT
50.0 55.0 60.0 65.0 70.0 75.0 80.0 85.0 90.0 95.0 100.0
50.0 55.0 60.0 65.0 70.0 75.0 80.0 85.0 90.0 95.0 100.0
50.0 55.0 60.0 65.0 70.0 75.0 80.0 85.0 90.0 95.0 100.0
50.0 55.0 60.0 65.0 70.0 75.0 80.0 85.0 90.0 95.0 100.0
50.0 55.0 60.0 65.0 70.0 75.0 80.0 85.0 90.0 95.0 100.0
50.0 55.0 60.0 65.0 70.0 75.0 80.0 85.0 90.0 95.0 100.0
50.0 55.0 60.0 65.0 70.0 75.0 80.0 85.0 90.0 95.0 100.0
50.0 55.0 60.0 65.0 70.0 75.0 80.0 85.0 90.0 95.0 100.0
50.0 55.0 60.0 65.0 70.0 75.0 80.0 85.0 90.0 95.0 100.0
50.0 55.0 60.0 65.0 70.0 75.0 80.0 85.0 90.0 95.0 100.0
50.0 55.0 60.0 65.0 70.0 75.0 80.0 85.0 90.0 95.0 100.0
