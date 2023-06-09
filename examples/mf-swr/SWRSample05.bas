# SWRSample05
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
   NOCBC
   BUDGETDB                ./output/SWRSample05_VolumetricBudget.txt  
   PRINT_HEAD   NPER       ./output/SWRSample05_Head.txt
   CUMULATIVE_HEAD_CHANGE  ./output/SWRSample05_CumHCHG.txt  
END OPTIONS
#
  CONSTANT         1 (FREE)                     -1     # IBOUND Layer   1
  CONSTANT         1 (FREE)                     -1     # IBOUND Layer   2
9.9900e+02
  INTERNAL     1.00 (FREE)                       0     # STARTING HEADS Layer   1
       1.534       1.362       1.413       1.158       1.043      0.8706
       1.534       1.184       1.188      0.9792      0.8792      0.8554
       1.534       1.257       1.153       1.006      0.9445      0.7678
       1.534       1.180       1.144      0.9598      0.9259      0.6595
       1.534       1.269       1.151       1.026      0.5374      0.5005
       1.534       1.357       1.243       1.186      0.7546      0.9789
  INTERNAL     1.00 (FREE)                       0     # STARTING HEADS Layer   2
       1.530       1.363       1.402       1.157       1.039      0.8739
       1.527       1.196       1.187      0.9858      0.8854      0.8547
       1.528       1.258       1.153       1.006      0.9405      0.7711
       1.526       1.190       1.141      0.9648      0.9142      0.6645
       1.528       1.272       1.153       1.021      0.5595      0.5311
       1.530       1.356       1.242       1.175      0.7637      0.9646
