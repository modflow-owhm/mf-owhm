# CBC_UNIT
# Listing of all available Options for the BAS package
#
# Options are grouped based on common usage or when only one should be selected
#
# The Option block is placed at the start of the BAS package input.
#    If there are multiple block style inputs (BEGIN/END), the order of the blocks does not matter.
#
BEGIN OPTIONS
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
    # Simulation continues unless %Error > StpP. --Note "NO_FAILED_CONVERGENCE_STOP" is equivalent to "STOP_ERROR  1E30"
    STOP_ERROR    StpP
    #
    # Define the maximum allowed relative volume error (MxRE), where the Relative volume error is: Cell_Volume_Error / Cell_Volume
    #          This is applied in addition to the standard solver criteria (HCLOSE and RCLOSE)
    #          MxRE = 0.025 is the default. Specify MxRE = 2.0 to disable (or any large number)
    MAX_RELATIVE_VOLUME_ERROR  MxRE
    #
    # Require the user to press Enter after the simulation completes.
    #   Useful to hold terminal window open after simulation is complete, BUT bad for automatic calibration.
    PAUSE
    #
    # Run model but don't solve gw equation to cycle through all input files 
    INPUT_CHECK
    #
    #  Run model from Stress Period (or Date) STR to STP.
    #          STP is optional (otherwise its set to NPER)
    #          Note that the initial head (STRT) defined in the BAS
    #          is carried forward and used as the starting head for STR.         --Note that INPUT_CHECK is the same as  FASTFORWARD NPER+1
    FASTFORWARD STR STP
    #
    #
    # Has the same effect as including the "COMPACT BUDGET" in the Output Control (OC) package
    COMPACT BUDGET 
    #
    #  Override Output Control (OC) for when the all package flows are written to the cell-by-cell file (CBC)
    NOCBC                  # Disable CBC writing
    CBC_LAST_TIMESTEP      # Writes CBC at the end of every Time Step
    CBC_EVERY_TIMESTEP     # Writes CBC every Time Step
    #
    # Overrides all the package Cell-By-Cell (CBC) unit numbers and sets them to IUCBC. Note that NOCBC is identical to " CBC_UNIT 0 "
    CBC_UNIT  IUCBC
    #
    # Indicates that the output written to the cell-by-cell (CBC) binary file should be double precision instead of single for all floating point numbers. This doubles its size and is not recommended.
    DOUBLE_PRECISION_CBC
    #
    # Default is to print every the iteration count every 10 solver iterations to command prompt
    #
    NO_SHOWPROGRESS # Disables "Solver Iter" printing; useful if redirecting cmd output to a file.
    #
    SHOWPROGRESS [NPRT] # Prints every 10 or NPRT iterations HCLOSE and RCLOSE; NPRT < 0 only prints the solver iter # every |NPRT| iterations; OneWater default is "SHOWPROGRESS -10"
                        # Note that "SHOWPROGRESS 0" is identical to "NO_SHOWPROGRESS"
    #
    # Time and Memory saving options ----------------------------------------------------------------------------------------------------------------
    #
    NO_DIM_CHECK         # By pass minimum model cell size check (saves a few seconds if you know the model grid is good)
    DEALLOCATE_MULT      # Reduce Memory Footprint by deallocating the MULT package arrays when they are no longer used.
    #
    # Set the size of the maximum number of parameters, clusters, and instances, which is MaxPar, MaxCluster, MaxInstance, respectively.
    #   Default when not specified is:       MAXPARAM  2000  2000000  50000
    #   If not using parameters you can set: MAXPARAM  63    64       1
    MAXPARAM MaxPar MaxCluster MaxInstance
    #
    # Set the size the number of volumetric budgets (MxBud) that are kept track of.
    #   MxBud should be greater than the number of MODFLOW Packages used plus 10
    #   If not specified, then the default is MxBud = 100
    MAXBUDGET  MxBud
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
    # Write to the directory, OUT_DIR, a set of files that are major aquifer properties (eg HK, VK, SY, SS). Can also use the keyword PROPPRINT
    PRINT_PROPERTY              OUT_DIR              # ./aquiferProperty/
    #
    # File that prints for all time steps the Time step Date, Length, etc information.
    #   This is useful for post-processing tools.
    PRINT_TIME_INFO        Generic_Output            # model_time_info.txt
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
    PRINT_RELATIVE_VOLUME_ERROR NTERM  OUTER_START  Generic_Output    # 1 0 ./conv_volerr.txt    -> Relative volume error       (MxRE)   => See MAX_RELATIVE_VOLUME_ERROR option
    #
    # Write at the end of every time step the residual error (L^3/T) for the entire model grid.
    RESIDUAL_ERROR_ARRAY   Generic_Output
    #
    # Imposes a limit when to print output from the RESIDUAL_ERROR_ARRAY option.
    #   The residual error is only written for RESIDUAL_ERROR_ARRAY
    #     when the mass percent error exceeds MinP. (That is when PercentError > MinP)
    RESIDUAL_ERROR_ARRAY_THRESHOLD  MinP
    #
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Print entire model grid head value at the specified stress period and time step or date.
    #   SPTS must either be a single date, 4/23/1979, which is located within a time step
    #                                                 or a stress period and time step, such as:  55  2
    #        can be set to NPER to get the last stress period, and optionally specify the time step afterwards
    #        if set to the keyword "LAST_TIMESTEP",  then will write the head for the last time step of every stress period -> For example, "PRINT_HEAD  LAST_TIMESTEP  ./output/head.txt" will write to head.txt the head arrays at the end of the last time step for each stress period.
    #        if set to the keyword "EVERY_TIMESTEP", then will write the head for    every time step
    #   Generic_Output is the location to write the output head.
    #   
    #   PRINT_HEAD can be repeated, one per line, to output head for different "SPTS" time steps 
    #                                             --Note only one PRINT_HEAD can be specified if SPTS is "LAST_TIMESTEP" or "EVERY_TIMESTEP".
    PRINT_HEAD  SPTS  Generic_Output
    #
    # PRINT_WATER_TABLE and PRINT_WATER_DEPTH use the same input options as PRINT_HEAD, but 
    #   the output for:
    #        PRINT_WATER_TABLE is the water table elevation, which is defined as the head for the upper most layer with the HEAD > that layer's bottom elevation (upper most saturated cell)
    #        PRINT_WATER_DEPTH is the depth to the water table from the ground surface elevation (GSE). That is, GSE minus the water table elevation. 
    #           GSE is ether the top elevation of the upper most active cell
    #                  or specified in the DIS package with the SURFACE_ELEVATION option.
    #
    PRINT_WATER_TABLE  SPTS  Generic_Output
    #
    PRINT_WATER_DEPTH  SPTS  Generic_Output
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #
    # Print the layer, row, column of any cell whose head value exceeds the ground surface elevation plus DstPrt.
    PRINT_HEAD_DISTANCE_ABOVE_GSE  DstPrt  Generic_Output             # 10.0  ./head_above_gse.txt
    #
    # Special Options - NOT RECOMMENDED - USE WITH CAUTION ------------------------------------------------------------------------------------------
    #
    # Applies additional dampening, DAMP, to the simulation's first time step for the first set of DmpIT iterations.
    #   This is useful if there are issues with the initial condition that cause floating point errors.
    DAMPEN_START  DmpIT  DAMP
    #
    # Requires at least MnIT outer iterations for all time steps.
    #          The secondary keyword BY_STRESS_PERIOD indicates that Generic_Input specifies the minimum iterations.
    #            Each line in the file specifies a starting stress period and the MnIT that are required until the next starting stress period is reached (see documentation for more info).
    MIN_SOLVER_ITERATION  MnIT   # Requires
    MIN_SOLVER_ITERATION BY_STRESS_PERIOD  Generic_Input
    #
    # Read with ULOAD NLAY values with List-Style Input that are added to the NLAY starting heads (STRT).
    #   Note that this is redundant to the U2DREL post-keyword SHIFT that can shift the STRT arrays when initially read. For example STRT input reads: "OPEN/CLOSE ini_head.txt SHIFT -10" would add to the head values in ini_head.txt the value -10)
    SHIFT_STRT  ULOAD
    #
    # Solver cannot allow any head to go above DstLim + GSE.
    #   For example, HdLim = 25 indicates that solver does not allow
    #   the head to be 25 or more above the ground/land surface elevation above it.
    #   If the head exceeds this threshold, then the head is set to it.
    HEAD_DISTANCE_ABOVE_GSE_LIMIT DstLim
    #
    # If solver takes more than DmpIT outer iterations, apply aggressive dampening
    #   in addition to the solver to reduce oscillations in solution.
    DAMPEN_OSCILLATIONS    DmpIT
    #
    # Legacy options ---------------------------------------------------------------------------------------------------------------------------------
    #
    # Default assumes input is free formatted that is space delimited, such as:  1 1 -1 1 2
      FREE   # Free formatted input is used. Specifying keyword is optional.
    NOFREE   # Indicates Fix Formatted Input should be used. This is a legacy feature and is not recommended.
    #
    # Input is a cross section of col by lay
    #       Indicates that the model is a 1-row cross section for which STRT and IBOUND
    #         should each be read as single two-dimensional variables with dimensions of NCOL and NLAY.
    #       Likewise, head and drawdown should be printed and saved in disk files as single 2D variables.
    XSECTION
    #
    # Indicates that flow between adjacent constant-head cells should be calculated.
    CHTOCH
END OPTIONS
#