BEGIN OPTIONS
    #
    # If a time step fails to meet the solver's convergence criteria, do not stop the simulation.
    NO_FAILED_CONVERGENCE_STOP
    #
    # Has the same effect as including the "COMPACT BUDGET" in the Output Control (OC) package
    COMPACT BUDGET 
    #
    # Overrides all the package Cell-By-Cell (CBC) unit numbers and sets them to 9. Note that NOCBC is identical to " CBC_UNIT 0 "
    CBC_UNIT  9
    #
    #  Override Output Control (OC) for when the all package flows are written to the cell-by-cell file (CBC)
 ###NOCBC                # Disable CBC writing
    CBC_EVERY_TIMESTEP     # Writes CBC every Time Step
    #
    # Specify the starting calendar date for the simulation. (Jan 1st, 2010 at midnight)
    #   Date format could also be specified as 1/1/2010
    START_DATE  2010-01-01
    #
    # Default is to print every the iteration count every 10 solver iterations to command prompt
    # Due to the speed of the model, the progress will be impossible to see, it is disabled.
    NO_SHOWPROGRESS # Disables "Solver Iter" printing; useful if redirecting cmd output to a file.
 ###SHOWPROGRESS [NPRT] # Prints every 10 or NPRT iterations HCLOSE and RCLOSE; NPRT < 0 only prints the solver iter # every |NPRT| iterations; OneWater default is "SHOWPROGRESS -10"
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
    CUMULATIVE_HEAD_CHANGE  ./output/owhm_2_fmp_CumHCHG.txt  
    #
END OPTIONS
CONSTANT        1 # IBOUND Lay1
CONSTANT        1 # IBOUND Lay2
-9999             # HNOFLO
CONSTANT     90   # STRT Upper Aquifer
CONSTANT     90   # STRT Middle Aquifer
