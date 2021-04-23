# Changelog - Features

**USGS Modflow One-Water Hydrologic-Flow Model (MF-OWHM)**

&nbsp; 

------

[[_TOC_]]

------

## 2.0.2

2021-04-23

- `BAS` — Options Block
    - `PRINT_HEAD         SPTS   Generic_Output `  
      `PRINT_WATER_TABLE  SPTS   Generic_Output `  
      `PRINT_WATER_DEPTH  SPTS   Generic_Output `
        - Prints the head, water table, or depth to water to a file. If head is requested then `NLAY` 2D (`NROW,NCOL`) head arrays are written with `NaN` as the placeholder for inactive or dry cells. Water table writes a single 2D array that specifies for each (`row, col`) the upper most active (saturated) cell's head. Water depth is the distance from either the upper most active cells top elevation to the water table or if the distance from the `SURFACE_ELEVATION` specified in the`DIS` or `FMP`.
        - `SPTS` indicates what time step to use for the output. It must either be a single date (eg `4/23/1979`), or a stress period and time step (eg `5 2`), can be set to `NPER` for the last stress period and time step, the keyword `LAST_TIMESTEP`  to indicate it should write the last time step of every stress period, or `EVERY_TIMESTEP` to write for every time step.
        - The keywords may be repeated multiple times to specify different times.  
          For example:  
          `PRINT_HEAD  5 2  ./output/Head_SP5_TS2.txt `  
          `PRINT_HEAD  8 1  ./output/Head_SP8_TS1.txt `  
          `PRINT_HEAD  3 2  ./output/Head_SP3_TS2.txt `  
          however you can only specify it once if using either `LAST_TIMESTEP`  or `EVERY_TIMESTEP`:  
          `PRINT_HEAD  LAST_TIMESTEP  ./output/Head_End_of_Each_SP.txt `

&nbsp; 

------

## 2.0.1

2021-03-15

- Output files will now automatically make missing directories if they do not exist rather than raising an error.
    - For example, in the if the following entry is in the Name file  
      `DATA 44  ./output/heads.out`  
      If the folder `output` does not exist, then it is created along with the file `heads.out`

* `U1DREL` and `U2DREL` support a post keyword `SHIFT` **FLOAT** that adds the specified float to the array that is read.
    * For example, `OPEN/CLOSE ./IC.txt 1 (FREE) -1 SHIFT 15` would add 15 to the input read in `IC.txt`

- **EXTERNAL** and **DATAUNIT** can reference a file's based name
    - Normal input is to follow these keywords with a unit number specified in the Name file.
    - Now *GENERIC_Input* and *GENERIC_OUTPUT* check for a unit number and the `basename` of a file.  
      For example, if the Name file declare the following Data File  
      `DATA   23    ./Dir1/Dir2/MyFile.txt`  
      Then the file can be accessed by one of the two following commands (the first is the normal method):  
      `EXTERNAL 23`  
      `EXTERNAL MyFile.txt`
    - This allows for file output/input locations to be defined at different locations in the name file, but enables packages to refer to he same root file name.

* `BAS` — Options Block
    * `SHOWPROGRESS  [NPRT]`
        * Indicates that the command prompt should show during each time step a live iteration count (a number that auto-updates).  
          `NPRNT` is optional, and if present specifies the print interval and indicates the printing should include the iteration's mass error.  
          If not present, then the iteration count is printed every 10 iterations (that is 10, 20, 30, ...) and no mass printing occurs.
        * Note that `SHOWPROGRESS` is disabled if any stress period is solved in less than 12 seconds because the iteration counter appears as a blur from being updated too quickly.
    * `CBC_UNIT` **INT**
        * Defines a global cell-by-cell unit and overwrites each packages `IxxxCB` input variable (for example `IWELCB`).

- `BAS` — Options Block — Making the `OC` package optional.
    - `COMPACT BUDGET`
    - Indicates the cell-by-cell uses a compact/smaller structure. Same effect as specifying keyword in the `OC` package
    - `HEAD_SAVE` **PER STP** *Generic_Output*  
      `HEAD_SAVE` **DATE** *Generic_Output*
        - Write the head arrays for the specified stress period and time step.
        - Write the head arrays for the specified date, heads will be interpolated using the closest two time steps.
    - `HEAD_SAVE_LAST_TIMESTEP`   *Generic_Output*  
      `HEAD_SAVE_EVERY_TIMESTEP`  *Generic_Output*
        - Write heads at the end of the last time step or every time step.

* `DRT` Option
    * `AUTOMATIC_NEGATIVE_ITMP`
        * Indicates that the first stress period input is read and then reused for the remainder of the simulation.

- `FMP` Updates
    - Land Use Block has the keyword:
        - `PRINT ROW_COLUMN` **INT INT** *GENERIC_OUTPUT*
            - Prints detailed/verbose land use information at every time step for the specified Row and Column.
            - For example, `PRINT ROW_COLUMN  3  4  ./Row3_Col4.txt`
    - Global Dimension has the keyword:
        - `HEAD_PREDICTOR_FACTOR` **FLOAT**
            - Keyword to indicate how the water table elevation is calculated. The water table elevation determines the amount of evaporation and transpiration from groundwater.
            - The default is to use the head solution (`HNEW`) at the end of the time step.
            - It may be more accurate to use a combination of the head at the start of the time step (`HOLD`) with `HNEW`.
            - `HEAD_PREDICTOR_FACTOR` specifies a weight (`FACT` as a float) for calculating the water table (`WT`):  
              `WT = FACT*HNEW + (1-FACT)*HOLD`
                - `FACT` should be set to `0.0` or `0.5` or `1.0` to indicate using only `HOLD`, the average, or only `HNEW`
                - If keyword is not specified, then, by default, `FACT = 1` which makes:  
                  `WT = HNEW`

* `MNW2` Print Options
    * The following can be added to the MNW2 `OPTIONS` block for additional output:
        * `PRINT_WELL_PUMPING`   &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;                      *GENERIC_OUTPUT*
        * `PRINT_NODE_INFO`      &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; *GENERIC_OUTPUT*
        * `PRINT_WELL_NODE_FLOW` &nbsp; &nbsp; &nbsp; &nbsp;                                    *GENERIC_OUTPUT*
        * `PRINT_WELL_INOUT`     &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;        *GENERIC_OUTPUT*

- `NWT` outer iteration count, `MAXITEROUT`, now represents the actual outer iteration count rather than solving for the Jacobian.
    - Note that previously, NWT did not count backtracking solver iterations (`BACKFLAG=1` or `MODERATE` or `COMPLEX`), even though they required solving the finite difference equations.
    - **Previous models that use the NWT solver should increase the  `MAXITEROUT` to ensure sufficient iterations.**

* `RCH` can set `NRCHOP` to `-1` to indicate that the initial upper most non-zero `IBOUND` is used. This is a hybrid between `NRCHOP` as `1` and `3`.
    * This option was created to mimic the original behavior of `RCH` with the `NWT` solver and  `NRCHOP=3`.  
      NWT does not zero the `IBOUND` value for dry cells resulting in recharge not being passed to the upper most non-dry cell.  
      See `Version 2.0.1` bug fixes for more details.

- `SFR` Option
    - `HEAD_PREDICTOR_FACTOR` **FLOAT**
        - Same effect as described for FMP with the same keyword, except it affects how the water table elevation is calculated with respect to the SFR stream bottom and stream aquifer interaction.

* `UPW` Option — Data Set 1 "Options"
    * `LPF_STORAGE`
        * Keyword indicates that UPW follows the storage formulation used in the LPF package.
        * Specifically, it assumes the derivative is zero, Specific Storage is zero for unconfined conditions, and Specific Yield does not vary with saturated thickness.

- `UPW` and `LPF` package — New Keywords as alternative to integer flags
    - `LAYTYP` flag can specify:
        - `CONFINED`
            - All layers are set to `LAYTYPE = 0` (confined) and use fixed transmissivity and specific storage
        - `CONVERTIBLE`  
          `CONVERTIBLE` **INT**
            - All layers are set to `LAYTYP = 1` (convertible) and have variable transmissivity and specific yield.
            - If **INT** is specified, then it represents the deepest layer that is convertible and all layers above it are convertible.
            - For example, `CONVERTIBLE 3` indicates that Layers 1, 2, and 3 are convertible and 4 to `NLAY` are confined.
    - `LAYAVE` flag can specify:
        - `HARMONIC` to automatically assign 0 for all layers (harmonic averaging)
        - `LOGARITHMIC` to automatically assign 1 for all layers (logarithmic averaging)
        - `ARITHMETIC` to automatically assign 2 for all layers (arithmetic averaging)
    - `CHANI` flag can specify:
        - `NO HORIZONTAL ANISOTROPY` to automatically set for all layers `CHANI = 1.0`
      - `LAYVKA` flag can specify:
        - `VKA` to set all layers to `LAYVKA = 0` (`VKA` is specified as vertical hydraulic conductivity)
        - `RATIO` to set all layers to `LAYVKA = 1` (`VKA` is specified as the ratio of horizontal to vertical hydraulic conductivity)
    - `LAYWET` flag can specify:
        - `NO WETTING`
            - Sets all layers to `LAYWET = 0`
        - `WETTING`  
          `WETTING` **INT**
            - All layers are set to `LAYWET = 1`
            - If **INT** is specified, then it represents the deepest layer that has `WETTING` and all layers above it that have it.
            - For example, `WETTING 3` indicates that Layers 1, 2, and 3 have enabled `WETTING` and 4 to `NLAY` do not.

&nbsp; 

------

## 2.0.0

2020-04-07

Initial Release of MF-OWHMv2.

For a full listing of features in this release see:
- Boyce, S.E., Hanson, R.T., Ferguson, I., Schmid, W., Henson, W., Reimann, T., Mehl, S.M., and Earll, M.M., 2020, One-Water Hydrologic Flow Model: A MODFLOW based conjunctive-use simulation software: U.S. Geological Survey Techniques and Methods 6–A60, 435 p., https://doi.org/10.3133/tm6A60