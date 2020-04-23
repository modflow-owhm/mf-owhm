C  Code in this file defines values for unformatted OPEN-statement specifiers.
C        The included specifiers are ACCESS and FORM.
C
      CHARACTER*20 ACCESS,FORM
C
C  ACCESS specifier --
C
C    Standard Fortran -- Use unless there is a reason to do otherwise.
      DATA ACCESS/'STREAM'/      
C     DATA ACCESS/'SEQUENTIAL'/
C
C    Non-standard Fortran that causes code compiled by some compilers
C    to use unstructured non-formatted files.
C      DATA ACCESS/'TRANSPARENT'/
C
C  FORM specifier --
C
C    Non-standard Fortran, but this has become widely adopted by most compiler
C    makers.  This results in unformatted files that can be read by other
C    programs compiled using other compilers.  Use this specifier if your
C    compiler supports this specifier.
      DATA FORM/'UNFORMATTED'/
C     DATA FORM/'BINARY'/
C
C    Standard Fortran, which results in vender dependent (non-portable) files.
C    This results in unformatted files that can only be read by other programs
C    compiled using the same compiler.  Avoid using this if the compiler
C    supports the non-standard 'BINARY' specifier described above.
C      DATA FORM/'UNFORMATTED'/
C
