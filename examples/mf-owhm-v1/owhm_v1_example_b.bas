# OWHM_EX1
# MODFLOW-OWHM Example problem documented in: 
#   Hanson, R.T., Boyce, S.E., Schmid, Wolfgang, Hughes, J.D., Mehl, S.M., Leake, S.A., Maddock, Thomas, III, and Niswonger, R.G., 2014, One-Water Hydrologic Flow Model (MODFLOW-OWHM): U.S. Geological Survey Techniques and Methods 6–A51, 120 p., https://dx.doi.org/10.3133/tm6A51.
# 
# Input modified for MODFLOW-OWHM by adding a BAS OPTIONS BLOCK with output options.
#
#-----------------------------------------------------------------------------------------------------------------------------------------------------
# generic model 1
BEGIN OPTIONS
   START_DATE  12/31/1999                  # DEC 31, 1999 AT START OF First Stress Period  (WATCH OUT ITS Y2K!)
   #
   SHOW_PROGRESS  5                        # Show iteration counter and mass error every 5 iterations
   #
   NOCBC
   NO_FAILED_CONVERGENCE_STOP              # Continue simulation, even if solver convergence is not met
   PERCENTERROR 5                          # Raise warning if mass error exceeds 5%
   MAXPARAM 10 20 1                        # Specify MXPAR, MXCLST, MXINST
   MAXBUDGET          32                   # Maximum number of budget entries to store (that is each row in the LIST file volumetric budget and columns in the BUDGETDB output file). 
   #                                        This should be at least the number of packages in use plus 10. If not specified the default is 100
   #
   # MF-OWHM special output files
   CUMULATIVE_RESIDUAL_ERROR_ARRAY  ./output/owhm_v1_example_b_Cumulative_Residual_Error.txt    # Print cumulative residual mass error for the entire model grid at the end of the simulation
   PRINT_ITERATION_INFO             ./output/owhm_v1_example_b_Iteration_Info.txt               # Print iteration count and mass error for every time step 
   PRINT_PROPERTY                   ./output/example_b_aquifer_property                         # Print aquifer properties to specified directory
   #
   BUDGETDB                ./output/owhm_v1_example_b_VolumetricBudget.txt 
   PRINT_HEAD   NPER       ./output/owhm_v1_example_b_Head.txt
   CUMULATIVE_HEAD_CHANGE  ./output/owhm_v1_example_b_CumHCHG.txt  
END OPTIONS
constant 1
constant 1
constant 1
constant 1
constant 1
constant 1
constant 1
-999
OPEN/CLOSE  data_fmp/gse.in  1.0  (FREE)  -1  # TOP of layer 1
OPEN/CLOSE  data_fmp/gse.in  1.0  (FREE)  -1  # TOP of layer 1
OPEN/CLOSE  data_fmp/gse.in  1.0  (FREE)  -1  # TOP of layer 1
OPEN/CLOSE  data_fmp/gse.in  1.0  (FREE)  -1  # TOP of layer 1
OPEN/CLOSE  data_fmp/gse.in  1.0  (FREE)  -1  # TOP of layer 1
OPEN/CLOSE  data_fmp/gse.in  1.0  (FREE)  -1  # TOP of layer 1
OPEN/CLOSE  data_fmp/gse.in  1.0  (FREE)  -1  # TOP of layer 1