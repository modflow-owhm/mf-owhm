C  Code in this file defines values for OPEN-statement specifiers.  Some
C  of the values are extensions to ANSI Fortran 90 and 95.  One of the
C  specifiers is not included in ANSI FORTRAN 77. The included
C  specifiers are ACCESS, FORM and ACTION.
C
      CHARACTER*20 ACCESS,FORM,ACTION(2)
C
C
C  Specifiers for OPEN statements for unformatted files, which are
C  sometimes compiler specific.
C  The included specifiers are ACCESS and FORM.
C
C  ACCESS specifier --
C
C    Standard Fortran 2003 Supports Unformatted Stream Binary files as FORM='UNFORMATTED', ACCESS='STREAM'
C          which is identical to nonstandard BINARY as FORM='BINARY', ACCESS='SEQUENTIAL' (See FORM specifier)
C          Set access to 'SEQUENTIAL' if your compiler does not support the F03 Standard yet [ie Comment STREAM and uncomment SEQUENTIAL by removing the C]
C
C
       DATA ACCESS/'STREAM'/
C      DATA ACCESS/'SEQUENTIAL'/
C
C
C  FORM specifier --
C
C    Standard Fortran 90/95, UNFORMMATED without STREAM option results in vender dependent
C         files. [ie non-portable: they only work on same OS and maybe compiler dependent]
C    Standard Fortran 2003 binary representations use the STREAM option with UNFORMATTED,
C    The UNFORMATTED STREAM option creates an unstructured binary file. UNFORMMATED files may be smaller in size
C    and faster to read and write. Some post-processing software requires binary input.
C
C
      DATA FORM/'UNFORMATTED'/
C
C
C    Intel Fortran has a non-standard extension for FORM='BINARY' that produces identical binaries 
C        as UNFORMATTED STREAM files. Note that when using the BINARY option you must use: DATA ACCESS/'SEQUENTIAL'/ 
C
C
C      DATA FORM/'BINARY'/
C
C
C  OPEN-statement specifiers related to file-sharing.
C
C  ACTION specifier --
C
C    Standard FORTRAN 77 -- Eliminate the ACTION= specifier from all
C    OPEN statements in the source-code files.
C
C    Standard Fortran 90 and 95 -- Use unless there is a reason to do
C    otherwise.
      DATA (ACTION(I),I=1,2)/'READ','READWRITE'/
C
C    Non-standard Fortran that causes code compiled by the Lahey LF90
C    compiler to create files that can be shared.  For use when parallel
C    processing is used or to enable an editor to view output files
C    while the program is running.
C      DATA (ACTION(I),I=1,2)/'READ,DENYWRITE','READWRITE,DENYNONE'/
