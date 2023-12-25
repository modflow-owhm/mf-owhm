#3 layer, 4 rows, 5 columns; transient
#const heads col 1  example for CFPM1
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
    CUMULATIVE_HEAD_CHANGE  ./output/cfp_mode1_CumHCHG.txt
    #
END OPTIONS
INTERNAL        1 (4I3)       3   ibound layer 1
 -1  1  1 -1
 -1  1  1 -1
 -1  1  1 -1
 -1  1  1 -1
 INTERNAL        1 (4I3)       3   ibound layer 2
 -1  1  1 -1
 -1  1  1 -1
 -1  1  1 -1
 -1  1  1 -1
 INTERNAL        1 (4I3)       3   ibound layer 3
 -1  1  1 -1
 -1  1  1 -1
 -1  1  1 -1
 -1  1  1 -1
   -999
         1         1(4f8.4)         1
    20.0   20.01   20.02   20.3
    20.0   20.01   20.02   20.3
    20.0   20.01   20.02   20.3
    20.0   20.01   20.02   20.3
         1         1(4G8.4)        1
    20.0   20.01   20.02   20.3
    20.0   20.01   20.02   20.3
    20.0   20.01   20.02   20.3
    20.0   20.01   20.02   20.3
         1         1(4G8.4)        1
    20.0   20.01   20.02   20.3
    20.0   20.01   20.02   20.3
    20.0   20.01   20.02   20.3
    20.0   20.01   20.02   20.3
