#
# Recommended List of BAS Options (this is a trimmed list from BAS_Options_Cheatsheet.bas)
#
# Options are grouped based on common usage or when only one should be selected
# Options are grouped based on common usage or when only one should be selected
#  Some options are commented with a ### because they are useful but should not be left on by default.
#
# The Option block is placed at the start of the BAS package input.
#    If there are multiple block style inputs (BEGIN/END), the order of the blocks does not matter.
#
BEGIN OPTIONS
    #
    # Run model but don't solve gw equation to cycle through all input files 
 ###INPUT_CHECK
    #
    # Has the same effect as including the "COMPACT BUDGET" in the Output Control (OC) package
    COMPACT BUDGET 
    #  Override Output Control (OC) for when the all package flows are written to the cell-by-cell file (CBC)
 ###NOCBC                  # Disable CBC writing
    CBC_LAST_TIMESTEP        # Writes CBC at the end of every Time Step
 ###CBC_EVERY_TIMESTEP     # Writes CBC every Time Step
    #
    # Overrides all the package Cell-By-Cell (CBC) unit numbers and sets them to IUCBC. Note that NOCBC is identical to " CBC_UNIT 0 "
    CBC_UNIT  IUCBC
    #
    # If CONSTANT_HEAD_BUDGET_OPTIONAL is present and there are no constant heads in the model (IBOUND<0, CHD, or FHB), 
    #   then the '   CONSTANT HEAD' array is not written to the CBC file. This reduces the CBC size by about 5%.
    # If you use this option, you must use ZoneBudget version 3.2 or newer.
    CONSTANT_HEAD_BUDGET_OPTIONAL
    #
    # Specify the starting calendar date for the simulation.
    #          DATE must be in one of the accepted formats: mm/dd/yyyy or yyyy-mm-dd or mm/dd/yyyyThh:MM:ss or yyyy-mm-ddThh:MM:ss or DecimalYear
    START_DATE  DATE
    #
    # If a time step fails to meet the solver's convergence criteria, do not stop the simulation.
    NO_FAILED_CONVERGENCE_STOP
    #
    # Prints to terminal window, LIST, and WARN files the volumetric budget
    #   If the percent mass error exceeds MinP. That is, "Print Budget" is invoked when %Error > MinP.
    #   If not specified then MinP = 5%
    PERCENTERROR  MinP
    #
    # Define the maximum allowed relative volume error (MxRE), where the Relative volume error is: Cell_Volume_Error / Cell_Volume
    #          This is applied in addition to the standard solver criteria (HCLOSE and RCLOSE)
    #          MxRE = 0.025 is the default. Specify MxRE = 2.0 to disable (or any large number)
    MAX_RELATIVE_VOLUME_ERROR  MxRE
    #
    # Default is to print every the iteration count every 10 solver iterations to command prompt
    #
    NO_SHOWPROGRESS # Disables "Solver Iter" printing; useful if redirecting cmd output to a file.
    #
 ###SHOWPROGRESS [NPRT] # Prints every 10 or NPRT iterations HCLOSE and RCLOSE; NPRT < 0 only prints the solver iter # every |NPRT| iterations; OneWater default is "SHOWPROGRESS -10"
                        # Note that "SHOWPROGRESS 0" is identical to "NO_SHOWPROGRESS"
    #
    # Applies additional dampening, DAMP, to the simulation's first time step for the first set of DmpIT iterations.
    #   This is useful if there are issues with the initial condition that cause floating point errors.
 ###DAMPEN_START  DmpIT  DAMP
    #
    #
    # Output Files ----------------------------------------------------------------------------------------------------------------------------------
    #   Generic_Output is a place holder for the output file location.
    #   The file name that is commented to the right is the recommended name.
    #   Example use:
    #               BUDGETDB   ./output/package_flow_budget.txt
    #
    # Print the budget information in a column based format for all packages in a single file.
    BUDGETDB               Generic_Output    # package_flow_budget.txt
    #
    # Write at the end of the simulation the cumulative volume errors (L^3) for the entire model grid.
    CUMULATIVE_RESIDUAL_ERROR_ARRAY Generic_Output   # Cumulative_Model_Error.txt
    #
    # Prints for each time step, the number of iterations required to solve and the mass error.
    PRINT_ITERATION_INFO   Generic_Output            # iter_info.txt
    #
    # Print convergence information by iteration for every time step to a file.
    #  Useful for diagnosing model construction problems for time steps that fail to converge.
    #  NTERM       is the number of worst model cells to print (for example, NTERM = 2 will print every solver iteration the 2 worst model cells).
    #  OUTER_START is the solver iteration number to start printing the convergence information.
    #                 If set to 0, then output is only for the last solver iteration.
    #                 If set   <0, then output is only for MXITER + OUTER_START solver iterations (that is, the last OUTER_START iterations).
    #
    PRINT_CONVERGENCE           NTERM  OUTER_START  Generic_Output    # 1 0 ./conv_hclose.txt    -> Head change     convergence (HCLOSE)
    PRINT_FLOW_RESIDUAL         NTERM  OUTER_START  Generic_Output    # 1 0 ./conv_rclose.txt    -> Solver residual convergence (RCLOSE)
    #
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Print entire model grid head value at the specified stress period and time step or date.
    #   SPTS must either be a single date, 4/23/1979, which is located within a time step
    #                                                 or a stress period and time step, such as:  55  2
    #        can be set to NPER to get the last stress period, and optionally specify the time step afterwards
    #        if set to the keyword "LAST_TIMESTEP",  then will write the head for the last time step of every stress period -> For example, "PRINT_HEAD  LAST_TIMESTEP  ./output/head.txt" will write to head.txt the head arrays at the end of the last time step for each stress period.
    #        if set to the keyword "EVERY_TIMESTEP", then will write the head for    every time step
    #   
    #   Generic_Output is the location to write the output head.
    #        The post-keyword SIGFIG specifies the number of significant figure digits to write out (NDIG).
    #        That is, "SIGFIG 11" will produce output that contains 11 significant digits.
    #        If not specified, then the default is 5 digits.
    #   
    #   PRINT_HEAD can be repeated, one per line, to output head for different "SPTS" time steps 
    #                                             --Note only one PRINT_HEAD can be specified if SPTS is "LAST_TIMESTEP" or "EVERY_TIMESTEP".
    PRINT_HEAD  SPTS  Generic_Output  [SIGFIG  NDIG]
    #
    # PRINT_WATER_TABLE and PRINT_WATER_DEPTH use the same input options as PRINT_HEAD, but 
    #   the output for:
    #        PRINT_WATER_TABLE is the water table elevation, which is defined as the head for the upper most layer with the HEAD > that layer's bottom elevation (upper most saturated cell)
    #        PRINT_WATER_DEPTH is the depth to the water table from the ground surface elevation (GSE). That is, GSE minus the water table elevation. 
    #           GSE is ether the top elevation of the upper most active cell
    #                  or specified in the DIS package with the SURFACE_ELEVATION option.
    #
    PRINT_WATER_TABLE  SPTS  Generic_Output  [SIGFIG  NDIG]
    # 
    PRINT_WATER_DEPTH  SPTS  Generic_Output  [SIGFIG  NDIG]
    #
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Save entire model grid head value use the MODFLOW-2005 standard write utility.
    #   This produces an equivalent output to OUTPUT CONTROL options "SAVE HEAD" or "PRINT HEAD"
    #   Only one of the following can be selected:
    #      "LAST_TIMESTEP",  then will write the head for the last time step of every stress period.
    #      "EVERY_TIMESTEP", then will write the head for    every time step
    #   
    #   Generic_Output is the location to write the output head.
    #      The post-keyword SIGFIG specifies the number of significant figure digits to write out (NDIG).
    #         That is, "SIGFIG 11" will produce output that contains 11 significant digits.
    #         If not specified, then the default is 5 digits.
    #      The post-keyword BINARY indicates that the file should be a binary formatted file rather than a text file.
    #         It is equivalent to DATA(BINARY) in the name file
    #                --Note you can specify SAVE_HEAD only once.
    #  
    SAVE_HEAD  LAST_TIMESTEP   Generic_Output  [BINARY]  [SIGFIG  NDIG]
    SAVE_HEAD  EVERY_TIMESTEP  Generic_Output  [BINARY]  [SIGFIG  NDIG]
    #
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #
END OPTIONS
#