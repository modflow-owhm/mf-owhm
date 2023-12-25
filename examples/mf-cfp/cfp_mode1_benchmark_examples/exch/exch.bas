#1 layer, 4 rows, 5 columns; transient
BEGIN OPTIONS
    #
    # Has the same effect as including the "COMPACT BUDGET" in the Output Control (OC) package
    COMPACT BUDGET 
    #
    # Default is to print every the iteration count every 10 solver iterations to command prompt
    # Due to the speed of the model, the progress will be impossible to see, it is disabled.
    NO_SHOWPROGRESS # Disables "Solver Iter" printing; useful if redirecting cmd output to a file.
    #
    # Output Files ----------------------------------------------------------------------------------------------------------------------------------
    #   Generic_Output is a place holder for the output file location.
    #   The file name that is commented to the right is the recommended name.
    #   Example use:
    #               BUDGETDB   ./output/package_flow_budget.txt
    #
    # Print the budget information in a column based format for all packages in a single file.
    BUDGETDB                ./output/VolumetricBudget.txt 
    PRINT_HEAD   NPER       ./output/Head_NPER.txt 
    CUMULATIVE_HEAD_CHANGE  ./output/cfp_bench_exch_CumHCHG.txt
    #
END OPTIONS
INTERNAL        1 (5I3)       3   # ibound layer 1
  1  1 -1  1  1
  1  1  1  1  1
  1  1  1  1  1
 -1 -1 -1 -1 -1
   -999
         1         1(5G11.4)                    1
       10.0       10.0       10.0       10.0       10.0     
       10.0       10.0       10.0       10.0       10.0     
       10.0       10.0       10.0       10.0       10.0     
       11.0       11.0       11.0       11.0       11.0     

