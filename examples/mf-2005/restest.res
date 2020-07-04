# restest
# MODFLOW-2005 Example problem documented in: 
#   Harbaugh, A.W., 2005, MODFLOW-2005, the U.S. Geological Survey modular ground-water model -- the Ground-Water Flow Process: U.S. Geological Survey Techniques and Methods 6-A16.
#   Harbaugh, A.W., Banta, E.R., Hill, M.C. and McDonald, M.G., 2000, MODFLOW-2000, The U.S. Geological Survey Modular Ground-Water Model - User Guide to Modularization Concepts and the Ground-Water Flow Process: U.S Geological Survey Open-File Report 2000-92, 121 p., https://doi.org/10.3133/ofr200092
#   Fenske, J.P., Leake, S.A., and Prudic, D.E., 1996, Documentation of a computer program (RES1) to simulate leakage from reservoirs using the modular finite-difference ground-water flow model (MODFLOW): U.S. Geological Survey Open-File Report 96-364, 51 p.
# 
#      -Comments are preceded by a # sign
#
#-----------------------------------------------------------------------------------------------------------------------------------------------------

         1         0         1         1        15 
        27         1(12I2)                       3                 # IRES
 0 0 0 0 0 0 0 0 0 0 0 0
 0 0 0 0 0 0 0 0 0 0 0 0
 0 0 0 0 1 1 1 1 1 1 0 0
 0 0 0 1 1 1 1 1 1 1 0 0
 0 0 0 1 1 1 1 1 1 1 0 0
 0 0 0 1 1 1 1 1 1 1 1 0
 0 0 0 1 1 1 1 1 1 1 0 0
 0 0 0 1 1 1 1 1 1 1 0 0
 0 0 0 0 1 1 1 1 1 1 0 0
 0 0 0 0 0 0 0 0 0 0 0 0
 0 0 0 0 0 0 0 0 0 0 0 0
 0 0 0 0 0 0 0 0 0 0 0 0
        27         1(12F4.0)                     8                  # BRES
   0   0   0   0   0   0   0   0   0   0   0   0
   0   0   0   0   0   0   0   0   0   0   0   0
   0   0   0   0   6   7   8   9  10  10   0   0
   0   0   0   6   6   7   8   9  10  10   0   0
   0   0   0   6   6   7   8   9  10  10   0   0
   0   0   0   6   6   7   8   9  10  10  21   0
   0   0   0   6   6   7   8   9  10  10   0   0
   0   0   0   6   6   7   8   9  10  10   0   0
   0   0   0   0   6   7   8   9  10  10   0   0
   0   0   0   0   0   0   0   0   0   0   0   0
   0   0   0   0   0   0   0   0   0   0   0   0
   0   0   0   0   0   0   0   0   0   0   0   0
         0         1                     # HCres
         0         2                     # Rbthck
         4        12                     # Ststage, Endstage  Res 1 SP 1
        12        14                     # Ststage, Endstage  Res 1 SP 2
        14         4                     # Ststage, Endstage  Res 1 SP 3
