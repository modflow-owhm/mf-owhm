C ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C LINK-MT3DMS (LMT) PACKAGE V8 FOR MODFLOW-OWHM
C Modified from LMT V7 for MODFLOW-2000 and MF2005 as documented in:
C     Zheng, C., M.C. Hill, and P.A. Hsieh, 2001,
C         MODFLOW-2000, the U.S. Geological Survey modular ground-water
C         model--User guide to the LMT6 Package, the linkage with
C         MT3DMS for multispecies mass transport modeling:
C         U.S. Geological Survey Open-File Report 01-82
C
C Revision History: 
C     Version 7.0: 06-23-2016 cz
C     Version 7.0: 08-15-2009 swm: added LMTMODULE to support LGR
C     Version 7.0: 02-12-2010 swm: rolled in include file
C     Version 8.0: 07-05-2016: added support for pending release of 
C     MT3D-USGS
C ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
C
      MODULE LMTMODULE
        INTEGER, SAVE, POINTER :: ISSMT3D,IUMT3D,ILMTFMT,ISFRLAKCONNECT,
     +                            ISFRUZFCONNECT,ILAKUZFCONNECT,
     +                            ISNKUZFCONNECT,NPCKGTXT,IUZFFLOWS,
     +                            ISFRFLOWS,ILAKFLOWS,
     +                            NLAKCON
        INTEGER, SAVE, DIMENSION(:,:), POINTER, CONTIGUOUS :: IGWET
        INTEGER, SAVE, DIMENSION(:,:), POINTER, CONTIGUOUS :: IUZFRCH
        TYPE LMTTYPE
         INTEGER, POINTER :: ISSMT3D,IUMT3D,ILMTFMT,ISFRLAKCONNECT,
     +                       ISFRUZFCONNECT,ILAKUZFCONNECT,NPCKGTXT,
     +                       ISNKUZFCONNECT,IUZFFLOWS,ISFRFLOWS,
     +                       ILAKFLOWS,NLAKCON
         INTEGER, DIMENSION(:,:), POINTER, CONTIGUOUS :: IGWET
        INTEGER,  DIMENSION(:,:), POINTER, CONTIGUOUS :: IUZFRCH
        END TYPE
        TYPE(LMTTYPE), SAVE  ::LMTDAT(10)
      END MODULE LMTMODULE