# etsdrt
# MODFLOW-2005 Example problem documented in: 
#   Harbaugh, A.W., 2005, MODFLOW-2005, the U.S. Geological Survey modular ground-water model -- the Ground-Water Flow Process: U.S. Geological Survey Techniques and Methods 6-A16.
#   Harbaugh, A.W., Banta, E.R., Hill, M.C. and McDonald, M.G., 2000, MODFLOW-2000, The U.S. Geological Survey Modular Ground-Water Model - User Guide to Modularization Concepts and the Ground-Water Flow Process: U.S Geological Survey Open-File Report 2000-92, 121 p., https://doi.org/10.3133/ofr200092
#   Banta, E.R., 2000, MODFLOW-2000, the U.S. Geological Survey Modular Ground-Water Model - Documentation of Packages for Simulating Evapotranspiration with a Segmented Function (ETS1) and Drains with Return Flow (DRT1): U.S. Geological Survey Open-File Report 00-466, 127 p.
# 
#      -Comments are preceded by a # sign
#    
#-----------------------------------------------------------------------------------------------------------------------------------------------------
# ETS input file for ets1 and etsdrt test cases
  1  0  0  2           # Item 1: NETSOP IETSCB NPETS NETSEG
  1  1  1  0  1        # Item 4: INETSS INETSR INETSX INIETS INSGDF, period 1
CONSTANT  100.0        # Item 5: ETSS
CONSTANT  0.01         # Item 6: ETSR (ETS-Max)
constant  36.0         # Item 8: ETSX
constant   0.5         # Item 10: PXDP
constant   0.1         # Item 11: PETM
