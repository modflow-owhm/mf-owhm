﻿! 
! CODE DEVELOPED BY SCOTT E BOYCE 
!                   CONTACT <seboyce@usgs.gov> or <Boyce@engineer.com>
! 
! xoshiro256** is a Public Domain algorithm that yields a sequence of pseudo-randomized numbers
!   Blackman, D., & Vigna, S. (2018). Scrambled linear pseudorandom number generators. arXiv preprint arXiv:1805.01407
!   Homepage: http://prng.di.unimi.it/ 
!   Wiki:     https://en.wikipedia.org/wiki/Xorshift#xoshiro256**
! 
!                     => Note random numbers are generated for 2^256-1 times before the algorithm cycles again.
! 
! The algorithm uses four 64bt integers that represent a 256bit seed
!    that generates successive 63bit integer, 31bit integer or 53bit float pseudo-random numbers.
!       That is the positive range for INT64 and INT32, and REAL64 range from 0.0 to 1.0
!   
!   This is not intended, and must not be used, for cryptographic applications. 
!     They are best for random number simulations, such as Monte Carlo.
!
!   -> Note this is the method used by the GCC GFortran intrinsic CALL RANDOM_NUMBER(RND), 
!        but it is not a PURE routine and only generates a REAL32 number from 0 to 1.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
! The Normal and Exponential Distribution Random Variables are generated using the Ziggurat Method as originally described by 
!    Marsaglia G, Tsang WW. The ziggurat method for generating random variables. Journal of Statistical Software. 2000; 5(8):1–7.
!    Doornik, JA. An improved ziggurat method to generate normal random samples. University of Oxford; 2005.
! With the final algorithm based on
!    McFarland, C. D. (2016). A modified ziggurat algorithm for generating exponentially and 
!        normally distributed pseudorandom numbers. Journal of statistical computation and simulation, 86(7), 1281-1294.
! With modifications that involve Fortran specific improvements
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!  It is best to use 
!                   TYPE(RANDOM_GENERATOR):: RG 
!                   CALL RG%INIT() ! Run one time to set a SEED based on the date and time
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!  Provided standalone routines use an internal RANDOM_GENERATOR type that is associated with that routine.
!    On the first call the routine sets the internal generator to a seed based on the 
!    current date and time for the first random generation.
!
!  For example, I = randINT(1,10), will set I to a random value from 1 to 10, inclusive.
!
!  The first call to randINT initializes its internal random_generator (LCG_BETWEEN_INT) 
!    with a SEED based on the date and time during the first call.
!
!   Any routines that accept SEED as an optional argument can override and set the SEED value, 
!     if 1 <= SEED <= 1, then it is set to the Default_seed
!     *Note that SEED is passed to the splitmix64 algorithm to generate the four seed values for xoshiro256**
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
! Example use:
!
!  TYPE(RANDOM_GENERATOR):: RG 
!  CALL RG%INIT([SEED], [ID], [NJUMP])  ! Run one time to set a SEED based on the date and time
!                                             SEED overrides the random seed with the user specified
!                                             ID is a value added to the SEED--useful for different process IDs
!                                             NJUMP runs the xoshiro256** jump routine NJUMP times, which moves the SEED forward 2^64 random numbers.
! 
!  CALL RG%EXPONENTIAL(RAND, [lambda])               ! Set RAND to next Exponential  Distribution pseudo-random number; lambda is the Exponential rate parameter (λ > 0)
!  CALL RG%NORMAL     (RAND, [mean], [stdev], [var]) ! Set RAND to next Normal/Gauss Distribution pseudo-random number; mean is the normal mean; stdev is the normal population standard deviation, var is the normal population variance. If stdev and var are specified, then only stdev is used.
! 
!  CALL RG%GET(RAND, [LOWER, UPPER]) ! Set RAND to next pseudo-random number between 0.0 and 1.0, or 0 and 2147483647, or between LOWER and UPPER. If RAND is a CHARACTER(*) then range is all non-blank ASCII characters, ie ACHAR(33) to ACHAR(126), and lower and upper are set to a letter, eg. ('A','F')
!  CALL RG%GET(RAND, [SIGN])         ! SIGN set to true indicates that float random is from -1 to 1, instead of 0 to 1.
!  CALL RG%GET(RAND, [CHAR])         ! CHAR is a string of character that can be accepted as options.
!                                    ! This routine is PURE and rand can be a scalar, or 1D, 2D, or 3D array.
! 
!  CALL RG%SHUFFLE(A)                ! Pseudo-random shuffle of the values in A. This routine is PURE
!
!  I = RG%GET_INT   ([LOWER, UPPER]) ! Set I to next pseudo-random number between 0   and 2147483647, or between LOWER and UPPER
!  I = RG%GET_INT64 ([LOWER, UPPER]) ! 
!  X = RG%GET_DOUBLE([LOWER, UPPER]) ! Set X to next pseudo-random number between 0.0 and 1.0,        or between LOWER and UPPER
!  X = RG%GET_SINGLE([LOWER, UPPER]) ! Set X to next pseudo-random number between 0.0 and 1.0,        or between LOWER and UPPER
!  X = RG%GET_LETTER([lower_case])   ! Set X to next pseudo-random ASCII letter  between 'A' and 'Z',  or if lower_case is present and true, between 'a' and 'z'
!  X = RG%GET_LETTER(siz, [lower_case]) Set X to siz pseudo-random ASCII letters between 'A' and 'Z',  or if lower_case is present and true, between 'a' and 'z'
!  I = RG%GET_BINARY()               ! Set I to 0 or 1
!  L = RG%FLIP_COIN ()               ! Set L to .true. or .false.
!
!  X = RG%GET_DOUBLE(SIGN)           ! If SIGN is TRUE set X to next pseudo-random number between -1.0 and 1.0, otherwise set X to 0.0 and 1.0
!
MODULE RANDOM_ROUTINES!, ONLY: RANDOM_GENERATOR, SHUFFLE, SET_FORTRAN_SEED, COIN_TOSS, BINARY_TOSS
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: INT8, INT16, INT32, INT64,  &
                                          REL32 => REAL32, REL64 => REAL64, &
                                          qp => REAL128
  IMPLICIT NONE
  !
  PRIVATE
  !
  PUBLIC:: RANDOM_GENERATOR
  !
  PUBLIC:: rand            !([Lower, Upper], [SIGN], [SEED])  0-1 oe -1 to 1 random REL64
  PUBLIC:: randINT         !( Lower, Upper,  [SEED])
  !
  PUBLIC:: SHUFFLE         !CALL SHUFFLE(VEC, [SEED])
  PUBLIC:: COIN_TOSS       !()  RESULT(T or F)
  PUBLIC:: BINARY_TOSS     !()  RESULT(1 or 0)
  !
  PUBLIC:: SET_FORTRAN_SEED     ! Helper subroutine that sets the Fortran SEED. It has nothing to do with the RANDOM_GENERATOR type
  !
  ! Constants used internally to module ----------------------------------------------------
  !
  INTEGER(INT64), dimension(4), parameter:: Default_seed = [-3300711175878204139_int64,   8258803693257250632_int64, &  ! Based on an arbitrary number run in splitmix64 to create a starting point when SEED is not requested.
                                                             1051100212301966062_int64,  -7302830465610773213_int64   ]
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  INTEGER(INT32),  PARAMETER:: NEG   = -1_int32
  INTEGER(INT32),  PARAMETER:: Z     =  0_int32
  INTEGER(INT32),  PARAMETER:: ONE   =  1_int32
  INTEGER(INT32),  PARAMETER:: TWO   =  2_int32
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  REAL(REL64),     PARAMETER:: DNEG  = -1.0_rel64
  REAL(REL64),     PARAMETER:: nHALF = -0.5_rel64
  REAL(REL64),     PARAMETER:: DZ    =  0.0_rel64
  REAL(REL64),     PARAMETER:: UNO   =  1.0_rel64
  REAL(REL64),     PARAMETER:: pow53 =  2.0_rel64**(-53)
  REAL(REL64),     PARAMETER:: p2to63=  2.0_rel64**63
  !REAL(REL64),     PARAMETER:: HALF = 0.5_rel64
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !INTEGER(INT64),  PARAMETER:: LONG_NEG    = -1_int64
  INTEGER(INT64),  PARAMETER:: LONG_ZER    = 0_int64
  INTEGER(INT64),  PARAMETER:: LONG_ONE    = 1_int64
  !INTEGER(INT64),  PARAMETER:: BYTE_MASK   = 255_int64
  INTEGER(INT64),  PARAMETER::  b63_MASK   = 9223372036854775807_int64 ! = b'0111111111111111111111111111111111111111111111111111111111111111'
  !INTEGER(INT64),  PARAMETER:: LONG_inf_I  =  INT( HUGE(Z), int64)
  !
  ! ----------------------------------------------------------------------------------------
  INTERFACE rand               
    MODULE PROCEDURE randVal  ! x = rand([seed])
    MODULE PROCEDURE randLU   ! x = rand(Lower, Upper, [seed])  -> Lower and Upper are rel64
    MODULE PROCEDURE randLUI  ! x = rand(Lower, Upper, [seed])  -> Lower and Upper are int32
    MODULE PROCEDURE randSN   ! x = rand(SIGN, [seed])
  END INTERFACE
  !
  INTERFACE SHUFFLE              ! SHUFFLE(VEC,SEED)
    MODULE PROCEDURE SHUFFLE_INT32 
    MODULE PROCEDURE SHUFFLE_INT64  
    MODULE PROCEDURE SHUFFLE_REL32
    MODULE PROCEDURE SHUFFLE_REL64
    MODULE PROCEDURE SHUFFLE_CHAR
  END INTERFACE
  !
  ! ----------------------------------------------------------------------------------------
  !
  TYPE RANDOM_GENERATOR                      ! Uses a xoshiro256** method described in Blackman, D., & Vigna, S. (2018). Scrambled linear pseudorandom number generators. arXiv preprint arXiv:1805.01407.
      !
      INTEGER(INT64), dimension(4):: seed = Default_seed
      !
      CONTAINS
      !
      PROCEDURE, pass(rg):: INIT       => INITIALIZE_RG                  ! INIT([SEED], [ID], [NJUMP])
      PROCEDURE, pass(rg):: RESET      => RESET_RG                       ! RESET()
      ! 
      GENERIC             :: GET =>  GENERATE_RANDOM_INT32_0D_RG, GENERATE_RANDOM_INT32_0D_LU, &  ! GET(RAND, [LOWER, UPPER])
                                     GENERATE_RANDOM_INT64_0D_RG, GENERATE_RANDOM_INT64_0D_LU, &  ! GET(RAND, [SIGN])
                                     GENERATE_RANDOM_REL32_0D_RG, GENERATE_RANDOM_REL32_0D_LU, GENERATE_RANDOM_REL32_0D_SN, &
                                     GENERATE_RANDOM_REL64_0D_RG, GENERATE_RANDOM_REL64_0D_LU, GENERATE_RANDOM_REL64_0D_SN, &
                                     GENERATE_RANDOM_INT32_1D_RG, GENERATE_RANDOM_INT32_1D_LU, &
                                     GENERATE_RANDOM_INT64_1D_RG, GENERATE_RANDOM_INT64_1D_LU, &
                                     GENERATE_RANDOM_REL32_1D_RG, GENERATE_RANDOM_REL32_1D_LU, GENERATE_RANDOM_REL32_1D_SN, &
                                     GENERATE_RANDOM_REL64_1D_RG, GENERATE_RANDOM_REL64_1D_LU, GENERATE_RANDOM_REL64_1D_SN, &
                                     GENERATE_RANDOM_INT64_0D_LU32, GENERATE_RANDOM_REL64_0D_LU32,            &
                                              GENERATE_RANDOM_REL64_0D_LUI32, GENERATE_RANDOM_REL32_0D_LUI32, &
                                     GENERATE_RANDOM_INT64_1D_LU32, GENERATE_RANDOM_REL64_1D_LU32,            &
                                              GENERATE_RANDOM_REL64_1D_LUI32, GENERATE_RANDOM_REL32_1D_LUI32, &
                                     GENERATE_RANDOM_CHAR_0D_RG, GENERATE_RANDOM_CHAR_0D_LU, GENERATE_RANDOM_CHAR_0D_CH, &
                                     GENERATE_RANDOM_CHAR_1D_RG, GENERATE_RANDOM_CHAR_1D_LU, GENERATE_RANDOM_CHAR_1D_CH
                                           
      !
      GENERIC::             EXPONENTIAL => GENERATE_EXPONENTIAL_0D, GENERATE_EXPONENTIAL_1D
      GENERIC::             EXP         => GENERATE_EXPONENTIAL_0D, GENERATE_EXPONENTIAL_1D
      GENERIC::             NORMAL      => GENERATE_NORMAL_0D, GENERATE_NORMAL_1D
      GENERIC::             GAUSS       => GENERATE_NORMAL_0D, GENERATE_NORMAL_1D
      !
      PROCEDURE, pass(rg):: GET_BINARY  => GENERATE_RANDOM_BINARY_FN          ! GET_BINARY() 
      PROCEDURE, pass(rg):: FLIP_COIN   => GENERATE_RANDOM_COIN_FN            ! FLIP_COIN () 
      !
      GENERIC:: GET_INT    => GENERATE_RANDOM_INT32_0D_RG_FN, GENERATE_RANDOM_INT32_0D_LU_FN       ! GET_INT  ([LOWER, UPPER])
      GENERIC:: GET_INT64  => GENERATE_RANDOM_INT64_0D_RG_FN, GENERATE_RANDOM_INT64_0D_LU_FN,   &  ! GET_INT64([LOWER, UPPER])
                                                              GENERATE_RANDOM_INT64_0D_LU32_FN  
      GENERIC:: GET_DOUBLE => GENERATE_RANDOM_REL64_0D_RG_FN, GENERATE_RANDOM_REL64_0D_LU_FN,   &  ! GET_DOUBLE([LOWER, UPPER])  or GET_DOUBLE([SIGN])
                              GENERATE_RANDOM_REL64_0D_SN_FN, GENERATE_RANDOM_REL64_0D_LU32_FN, &
                                                              GENERATE_RANDOM_REL64_0D_LUI32_FN   
      GENERIC:: GET_SINGLE => GENERATE_RANDOM_REL32_0D_RG_FN, GENERATE_RANDOM_REL32_0D_LU_FN,   &  ! GET_SINGLE([LOWER, UPPER])     GET_SINGLE([SIGN])
                                                              GENERATE_RANDOM_REL32_0D_SN_FN  
      GENERIC:: GET_LETTER => GENERATE_RANDOM_CHAR_FN, GENERATE_RANDOM_CHAR_SIZ_FN                 ! GET_LETTER([lower_case] ) or GET_LETTER(siz, [lower_case] )
      !
      GENERIC:: SHUFFLE    => SHUFFLE_INT32_1D_RG, SHUFFLE_INT64_1D_RG, SHUFFLE_REL32_1D_RG, & ! SHUFFLE(A)
                                      SHUFFLE_REL64_1D_RG, SHUFFLE_CHAR_1D_RG,               &   
                              SHUFFLE_INT32_2D_RG, SHUFFLE_INT64_2D_RG, SHUFFLE_REL32_2D_RG, &
                                      SHUFFLE_REL64_2D_RG, SHUFFLE_CHAR_2D_RG,               &
                              SHUFFLE_INT32_3D_RG, SHUFFLE_INT64_3D_RG, SHUFFLE_REL32_3D_RG, &
                                      SHUFFLE_REL64_3D_RG, SHUFFLE_CHAR_3D_RG
      !
      PROCEDURE, nopass::    splitmix64     ! %splitmix64(seed, rnd) - supplemental random generator used internally for generating random seeds. Provided in case user wants to use it.
      !
      PROCEDURE, pass(rg), private:: GENERATE_RANDOM_INT32_0D_RG, GENERATE_RANDOM_INT32_0D_LU
      PROCEDURE, pass(rg), private:: GENERATE_RANDOM_INT64_0D_RG, GENERATE_RANDOM_INT64_0D_LU, GENERATE_RANDOM_INT64_0D_LU32
      PROCEDURE, pass(rg), private:: GENERATE_RANDOM_REL32_0D_RG, GENERATE_RANDOM_REL32_0D_LU, GENERATE_RANDOM_REL32_0D_SN, &
                                     GENERATE_RANDOM_REL32_0D_LUI32
      PROCEDURE, pass(rg), private:: GENERATE_RANDOM_REL64_0D_RG, GENERATE_RANDOM_REL64_0D_LU, GENERATE_RANDOM_REL64_0D_SN, &
                                     GENERATE_RANDOM_REL64_0D_LU32, GENERATE_RANDOM_REL64_0D_LUI32
      PROCEDURE, pass(rg), private:: GENERATE_RANDOM_CHAR_0D_RG, GENERATE_RANDOM_CHAR_0D_LU, GENERATE_RANDOM_CHAR_0D_CH
      PROCEDURE, pass(rg), private:: GENERATE_RANDOM_CHAR_1D_RG, GENERATE_RANDOM_CHAR_1D_LU, GENERATE_RANDOM_CHAR_1D_CH
      !
      PROCEDURE, pass(rg), private:: GENERATE_RANDOM_INT32_0D_RG_FN, GENERATE_RANDOM_INT32_0D_LU_FN
      PROCEDURE, pass(rg), private:: GENERATE_RANDOM_INT64_0D_RG_FN, GENERATE_RANDOM_INT64_0D_LU_FN, &
                                     GENERATE_RANDOM_INT64_0D_LU32_FN
      PROCEDURE, pass(rg), private:: GENERATE_RANDOM_REL32_0D_RG_FN, GENERATE_RANDOM_REL32_0D_LU_FN, GENERATE_RANDOM_REL32_0D_SN_FN
      PROCEDURE, pass(rg), private:: GENERATE_RANDOM_REL64_0D_RG_FN, GENERATE_RANDOM_REL64_0D_LU_FN,   &
                                     GENERATE_RANDOM_REL64_0D_SN_FN, GENERATE_RANDOM_REL64_0D_LU32_FN, &
                                     GENERATE_RANDOM_REL64_0D_LUI32_FN
      PROCEDURE, pass(rg), private:: GENERATE_RANDOM_CHAR_FN, GENERATE_RANDOM_CHAR_SIZ_FN
      !
      PROCEDURE, pass(rg), private:: GENERATE_RANDOM_INT32_1D_RG, GENERATE_RANDOM_INT32_1D_LU
      PROCEDURE, pass(rg), private:: GENERATE_RANDOM_INT64_1D_RG, GENERATE_RANDOM_INT64_1D_LU, &
                                     GENERATE_RANDOM_INT64_1D_LU32
      PROCEDURE, pass(rg), private:: GENERATE_RANDOM_REL32_1D_RG, GENERATE_RANDOM_REL32_1D_LU, GENERATE_RANDOM_REL32_1D_SN, &
                                     GENERATE_RANDOM_REL32_1D_LUI32
      PROCEDURE, pass(rg), private:: GENERATE_RANDOM_REL64_1D_RG, GENERATE_RANDOM_REL64_1D_LU, GENERATE_RANDOM_REL64_1D_SN, &
                                     GENERATE_RANDOM_REL64_1D_LU32, GENERATE_RANDOM_REL64_1D_LUI32
      !
      PROCEDURE, pass(rg), private:: SHUFFLE_INT32_1D_RG, SHUFFLE_INT64_1D_RG, SHUFFLE_REL32_1D_RG, &
                                     SHUFFLE_REL64_1D_RG, SHUFFLE_CHAR_1D_RG
      PROCEDURE, pass(rg), private:: SHUFFLE_INT32_2D_RG, SHUFFLE_INT64_2D_RG, SHUFFLE_REL32_2D_RG, &
                                     SHUFFLE_REL64_2D_RG, SHUFFLE_CHAR_2D_RG
      PROCEDURE, pass(rg), private:: SHUFFLE_INT32_3D_RG, SHUFFLE_INT64_3D_RG, SHUFFLE_REL32_3D_RG, &
                                     SHUFFLE_REL64_3D_RG, SHUFFLE_CHAR_3D_RG
      !
      PROCEDURE, pass(rg), private:: GENERATE_EXPONENTIAL_0D, GENERATE_EXPONENTIAL_1D
      PROCEDURE, pass(rg), private:: GENERATE_NORMAL_0D, GENERATE_NORMAL_1D
  END TYPE
  !
  INTRINSIC:: RANDOM_SEED, RANDOM_NUMBER, DATE_AND_TIME
  INTRINSIC:: INT
  INTRINSIC:: REAL
  !	
  !	The following values were calculated from: call calc_ziggurat_exponential_layers(256)  
  !
  INTEGER(INT64), parameter:: exp_iE_max = 513303011048449572_int64
  INTEGER(INT16), parameter:: exp_X_dim  = 253_int16  ! Set equal to the dim of the next two arrays
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  REAL(REL64), dimension(exp_X_dim), parameter:: exp_X = [ &
           8.20662406753488137750E-19_rel64, 7.39737323516072797130E-19_rel64, 6.91333133779152971301E-19_rel64, &
           6.56473588209645340352E-19_rel64, 6.29125399598185126414E-19_rel64, 6.06572241296049681564E-19_rel64, &
           5.87352761037372679921E-19_rel64, 5.70588505285369442184E-19_rel64, 5.55709456916223856047E-19_rel64, &
           5.42324389037439502443E-19_rel64, 5.30152976965087723972E-19_rel64, 5.18987392577080620494E-19_rel64, &
           5.08669226179983307929E-19_rel64, 4.99074929387964695329E-19_rel64, 4.90106258944495372685E-19_rel64, &
           4.81683790106491905571E-19_rel64, 4.73742386536447110851E-19_rel64, 4.66227958071968213053E-19_rel64, &
           4.59095090177840503442E-19_rel64, 4.52305277906581531841E-19_rel64, 4.45825588163539585541E-19_rel64, &
           4.39627631263683813317E-19_rel64, 4.33686759671064672323E-19_rel64, 4.27981436184697133388E-19_rel64, &
           4.22492730270648880606E-19_rel64, 4.17203912534641103730E-19_rel64, 4.12100125224656151171E-19_rel64, &
           4.07168112258692313725E-19_rel64, 4.02395996310069003840E-19_rel64, 3.97773093428773550200E-19_rel64, &
           3.93289757853344978094E-19_rel64, 3.88937251293103249491E-19_rel64, 3.84707632187203836730E-19_rel64, &
           3.80593661381801418877E-19_rel64, 3.76588721385447289313E-19_rel64, 3.72686746920301772757E-19_rel64, &
           3.68882164922481640706E-19_rel64, 3.65169842488000669859E-19_rel64, 3.61545041532874753136E-19_rel64, &
           3.58003379153180328375E-19_rel64, 3.54540792845334342105E-19_rel64, 3.51153509887842447380E-19_rel64, &
           3.47838020300309616125E-19_rel64, 3.44591052889073382266E-19_rel64, 3.41409553965633160849E-19_rel64, &
           3.38290668387411614461E-19_rel64, 3.35231722622890021462E-19_rel64, 3.32230209586858756379E-19_rel64, &
           3.29283775028044735545E-19_rel64, 3.26390205282020487355E-19_rel64, 3.23547416228108147907E-19_rel64, &
           3.20753443310807902082E-19_rel64, 3.18006432504786111102E-19_rel64, 3.15304632118208461639E-19_rel64, &
           3.12646385342651327876E-19_rel64, 3.10030123469342118701E-19_rel64, 3.07454359701373030582E-19_rel64, &
           3.04917683500055582366E-19_rel64, 3.02418755410945664709E-19_rel64, 2.99956302321445493524E-19_rel64, &
           2.97529113107425900013E-19_rel64, 2.95136034631132230229E-19_rel64, 2.92775968056842655137E-19_rel64, &
           2.90447865454425648606E-19_rel64, 2.88150726664167113395E-19_rel64, 2.85883596399069302516E-19_rel64, &
           2.83645561563316154950E-19_rel64, 2.81435748767797995667E-19_rel64, 2.79253322025531239753E-19_rel64, &
           2.77097480611528768177E-19_rel64, 2.74967457073202325028E-19_rel64, 2.72862515378733992745E-19_rel64, &
           2.70781949192060552745E-19_rel64, 2.68725080264190503593E-19_rel64, 2.66691256931534426508E-19_rel64, &
           2.64679852712788920120E-19_rel64, 2.62690264996684354593E-19_rel64, 2.60721913813597573510E-19_rel64, &
           2.58774240684651439543E-19_rel64, 2.56846707542481683097E-19_rel64, 2.54938795718354799310E-19_rel64, &
           2.53050004990774828256E-19_rel64, 2.51179852691127091766E-19_rel64, 2.49327872862278055727E-19_rel64, &
           2.47493615466386591580E-19_rel64, 2.45676645638486693451E-19_rel64, 2.43876542982678413817E-19_rel64, &
           2.42092900908015286871E-19_rel64, 2.40325326001405367110E-19_rel64, 2.38573437435051489027E-19_rel64, &
           2.36836866406146470514E-19_rel64, 2.35115255606712535909E-19_rel64, 2.33408258721632829269E-19_rel64, &
           2.31715539953067957864E-19_rel64, 2.30036773569583332446E-19_rel64, 2.28371643478434802971E-19_rel64, &
           2.26719842819571755967E-19_rel64, 2.25081073580019367159E-19_rel64, 2.23455046227395921210E-19_rel64, &
           2.21841479361407767088E-19_rel64, 2.20240099382244246201E-19_rel64, 2.18650640174868418125E-19_rel64, &
           2.17072842808267163829E-19_rel64, 2.15506455248786763942E-19_rel64, 2.13951232086737777832E-19_rel64, &
           2.12406934275506393699E-19_rel64, 2.10873328882458748043E-19_rel64, 2.09350188850970358121E-19_rel64, &
           2.07837292772955076074E-19_rel64, 2.06334424671307133245E-19_rel64, 2.04841373791706147958E-19_rel64, &
           2.03357934403268647283E-19_rel64, 2.01883905607560910453E-19_rel64, 2.00419091155516967571E-19_rel64, &
           1.98963299271832554490E-19_rel64, 1.97516342486430890612E-19_rel64, 1.96078037472619454600E-19_rel64, &
           1.94648204891578614986E-19_rel64, 1.93226669242843148557E-19_rel64, 1.91813258720456459076E-19_rel64, &
           1.90407805074494793029E-19_rel64, 1.89010143477675029757E-19_rel64, 1.87620112396774784933E-19_rel64, &
           1.86237553468607686209E-19_rel64, 1.84862311380309828787E-19_rel64, 1.83494233753705661603E-19_rel64, &
           1.82133171033532951214E-19_rel64, 1.80778976379317074762E-19_rel64, 1.79431505560694755467E-19_rel64, &
           1.78090616855996519605E-19_rel64, 1.76756170953905664423E-19_rel64, 1.75428030858019420159E-19_rel64, &
           1.74106061794145300803E-19_rel64, 1.72790131120172399078E-19_rel64, 1.71480108238363619910E-19_rel64, &
           1.70175864509920589243E-19_rel64, 1.68877273171678244757E-19_rel64, 1.67584209254790932573E-19_rel64, &
           1.66296549505276218083E-19_rel64, 1.65014172306286585686E-19_rel64, 1.63736957601982765849E-19_rel64, &
           1.62464786822885600515E-19_rel64, 1.61197542812586149685E-19_rel64, 1.59935109755696161215E-19_rel64, &
           1.58677373106923078727E-19_rel64, 1.57424219521155453523E-19_rel64, 1.56175536784445957959E-19_rel64, &
           1.54931213745780170458E-19_rel64, 1.53691140249519914956E-19_rel64, 1.52455207068410186574E-19_rel64, &
           1.51223305837038575463E-19_rel64, 1.49995328985635604373E-19_rel64, 1.48771169674103512865E-19_rel64, &
           1.47550721726159739942E-19_rel64, 1.46333879563479662686E-19_rel64, 1.45120538139721023666E-19_rel64, &
           1.43910592874309904354E-19_rel64, 1.42703939585865052032E-19_rel64, 1.41500474425133816909E-19_rel64, &
           1.40300093807308873960E-19_rel64, 1.39102694343590255730E-19_rel64, 1.37908172771851969109E-19_rel64, &
           1.36716425886266566878E-19_rel64, 1.35527350465734444515E-19_rel64, 1.34340843200957278848E-19_rel64, &
           1.33156800619986855829E-19_rel64, 1.31975119012071480521E-19_rel64, 1.30795694349612145639E-19_rel64, &
           1.29618422208029568811E-19_rel64, 1.28443197683330995649E-19_rel64, 1.27269915307152196888E-19_rel64, &
           1.26098468959035241020E-19_rel64, 1.24928751775686262106E-19_rel64, 1.23760656056939412245E-19_rel64, &
           1.22594073168133316825E-19_rel64, 1.21428893438584444388E-19_rel64, 1.20265006055817643270E-19_rel64, &
           1.19102298955187438395E-19_rel64, 1.17940658704494246133E-19_rel64, 1.16779970383167139766E-19_rel64, &
           1.15620117455548828284E-19_rel64, 1.14460981637778695576E-19_rel64, 1.13302442757725629978E-19_rel64, &
           1.12144378607373438066E-19_rel64, 1.10986664787007292400E-19_rel64, 1.09829174540489239012E-19_rel64, &
           1.08671778580843519598E-19_rel64, 1.07514344905297467965E-19_rel64, 1.06356738598840013926E-19_rel64, &
           1.05198821625266214785E-19_rel64, 1.04040452604571404410E-19_rel64, 1.02881486575440968383E-19_rel64, &
           1.01721774741449647241E-19_rel64, 1.00561164199435583977E-19_rel64, 9.93994976483466811867E-20_rel64, &
           9.82366130766674411792E-20_rel64, 9.70723434263200895345E-20_rel64, 9.59065162306906399967E-20_rel64, &
           9.47389532241542022115E-20_rel64, 9.35694699201590377351E-20_rel64, 9.23978751545694708201E-20_rel64, &
           9.12239705905564691797E-20_rel64, 9.00475501808528782633E-20_rel64, 8.88683995826476286341E-20_rel64, &
           8.76862955197674515214E-20_rel64, 8.65010050860710107700E-20_rel64, 8.53122849831411909369E-20_rel64, &
           8.41198806843852140645E-20_rel64, 8.29235255165134223828E-20_rel64, 8.17229396480345013895E-20_rel64, &
           8.05178289728392110605E-20_rel64, 7.93078838750992275341E-20_rel64, 7.80927778595244292591E-20_rel64, &
           7.68721660284290430445E-20_rel64, 7.56456833839651236406E-20_rel64, 7.44129429301791245459E-20_rel64, &
           7.31735335450933351355E-20_rel64, 7.19270175876310734225E-20_rel64, 7.06729281976667831582E-20_rel64, &
           6.94107662395003600205E-20_rel64, 6.81399968292564252262E-20_rel64, 6.68600453746102282434E-20_rel64, &
           6.55702930402100838137E-20_rel64, 6.42700715333685271360E-20_rel64, 6.29586570809235574886E-20_rel64, &
           6.16352634381431402799E-20_rel64, 6.02990337321516959349E-20_rel64, 5.89490308928501869402E-20_rel64, &
           5.75842263598859351795E-20_rel64, 5.62034866695973986575E-20_rel64, 5.48055574134993124499E-20_rel64, &
           5.33890439090032945417E-20_rel64, 5.19523877179899193399E-20_rel64, 5.04938378663383534761E-20_rel64, &
           4.90114152226294886634E-20_rel64, 4.75028679333661158555E-20_rel64, 4.59656150012654554425E-20_rel64, &
           4.43966738979975669137E-20_rel64, 4.27925663021485864884E-20_rel64, 4.11491932734300153646E-20_rel64, &
           3.94616667626062845336E-20_rel64, 3.77240771314016825146E-20_rel64, 3.59291640862043605396E-20_rel64, &
           3.40678366911005647555E-20_rel64, 3.21284476415640470486E-20_rel64, 3.00956469163999967980E-20_rel64, &
           2.79484694555983276950E-20_rel64, 2.56569130487186439382E-20_rel64, 2.31752097568039087576E-20_rel64, &
           2.04266952282512914823E-20_rel64, 1.72617703302134865455E-20_rel64, 1.32818892594425785591E-20_rel64, &
           0.00000000000000000000E+00_rel64                                                                      &
           ]
  
  REAL(REL64), dimension(exp_X_dim), parameter:: exp_Y = [ &
           5.59520549511273654000E-23_rel64, 1.18025099827033127822E-22_rel64, 1.84444233867358286303E-22_rel64, &
           2.54390304666983111378E-22_rel64, 3.27376943115093333428E-22_rel64, 4.03077321327067153256E-22_rel64, &
           4.81254783194951187141E-22_rel64, 5.61729148965833096022E-22_rel64, 6.44358205404435236847E-22_rel64, &
           7.29026623434636829618E-22_rel64, 8.15638884563219414108E-22_rel64, 9.04114536834822230769E-22_rel64, &
           9.94384884863992074717E-22_rel64, 1.08639060459691144716E-21_rel64, 1.18007997754612693457E-21_rel64, &
           1.27540755348312076734E-21_rel64, 1.37233311763772897867E-21_rel64, 1.47082087943752138315E-21_rel64, &
           1.57083882574404442583E-21_rel64, 1.67235819843745657318E-21_rel64, 1.77535306750305149725E-21_rel64, &
           1.87979997851045959623E-21_rel64, 1.98567765878325029716E-21_rel64, 2.09296677040532451822E-21_rel64, &
           2.20164970099582412820E-21_rel64, 2.31171038523061778492E-21_rel64, 2.42313415161254637881E-21_rel64, &
           2.53590759014208914431E-21_rel64, 2.65001843741705400019E-21_rel64, 2.76545547636603906081E-21_rel64, &
           2.88220844834686042625E-21_rel64, 3.00026797575477120689E-21_rel64, 3.11962549361303771641E-21_rel64, &
           3.24027318888017498109E-21_rel64, 3.36220394641870924275E-21_rel64, 3.48541130074090362309E-21_rel64, &
           3.60988939278594717583E-21_rel64, 3.73563293109717681692E-21_rel64, 3.86263715686200554911E-21_rel64, &
           3.99089781235528399793E-21_rel64, 4.12041111239189463852E-21_rel64, 4.25117371844889124852E-21_rel64, &
           4.38318271516337356607E-21_rel64, 4.51643558895106559991E-21_rel64, 4.65093020852348026277E-21_rel64, &
           4.78666480710960042678E-21_rel64, 4.92363796621199704239E-21_rel64, 5.06184860074789947951E-21_rel64, &
           5.20129594544347350798E-21_rel64, 5.34197954236489444714E-21_rel64, 5.48389922948309590670E-21_rel64, &
           5.62705513018063454738E-21_rel64, 5.77144764361919377268E-21_rel64, 5.91707743589506789329E-21_rel64, &
           6.06394543191770251269E-21_rel64, 6.21205280795316786689E-21_rel64, 6.36140098478043744675E-21_rel64, &
           6.51199162141364286597E-21_rel64, 6.66382660934816986231E-21_rel64, 6.81690806729262825542E-21_rel64, &
           6.97123833635243800163E-21_rel64, 7.12681997563408200043E-21_rel64, 7.28365575824203376882E-21_rel64, &
           7.44174866764301742864E-21_rel64, 7.60110189437463577334E-21_rel64, 7.76171883307754167946E-21_rel64, &
           7.92360307983225678096E-21_rel64, 8.08675842978348349973E-21_rel64, 8.25118887503633350046E-21_rel64, &
           8.41689860281032603222E-21_rel64, 8.58389199383830975940E-21_rel64, 8.75217362099864592075E-21_rel64, &
           8.92174824817007162448E-21_rel64, 9.09262082929965094224E-21_rel64, 9.26479650767512806690E-21_rel64, &
           9.43828061539382988932E-21_rel64, 9.61307867302103268714E-21_rel64, 9.78919638943141610998E-21_rel64, &
           9.96663966182788344733E-21_rel64, 1.01454145759326357792E-20_rel64, 1.03255274063459539627E-20_rel64, &
           1.05069846170686709202E-20_rel64, 1.06897928621848113461E-20_rel64, 1.08739589867013403205E-20_rel64, &
           1.10594900275423996586E-20_rel64, 1.12463932146958240626E-20_rel64, 1.14346759725101209538E-20_rel64, &
           1.16243459211404706790E-20_rel64, 1.18154108781426598256E-20_rel64, 1.20078788602142016947E-20_rel64, &
           1.22017580850822264342E-20_rel64, 1.23970569735380406769E-20_rel64, 1.25937841516185643561E-20_rel64, &
           1.27919484529351521910E-20_rel64, 1.29915589211506005537E-20_rel64, 1.31926248126054283566E-20_rel64, &
           1.33951555990948044981E-20_rel64, 1.35991609707977754502E-20_rel64, 1.38046508393607259053E-20_rel64, &
           1.40116353411372841078E-20_rel64, 1.42201248405871626435E-20_rel64, 1.44301299338367060582E-20_rel64, &
           1.46416614524041997533E-20_rel64, 1.48547304670932811320E-20_rel64, 1.50693482920580849457E-20_rel64, &
           1.52855264890440511870E-20_rel64, 1.55032768718086266843E-20_rel64, 1.57226115107264017644E-20_rel64, &
           1.59435427375835419764E-20_rel64, 1.61660831505667029400E-20_rel64, 1.63902456194519549493E-20_rel64, &
           1.66160432909995941195E-20_rel64, 1.68434895945610797293E-20_rel64, 1.70725982479047141577E-20_rel64, &
           1.73033832632670736526E-20_rel64, 1.75358589536376063788E-20_rel64, 1.77700399392842401019E-20_rel64, &
           1.80059411545282868799E-20_rel64, 1.82435778547773977378E-20_rel64, 1.84829656238258080482E-20_rel64, &
           1.87241203814316258981E-20_rel64, 1.89670583911814528413E-20_rel64, 1.92117962686531909836E-20_rel64, &
           1.94583509898884843207E-20_rel64, 1.97067399001868677584E-20_rel64, 1.99569807232343565625E-20_rel64, &
           2.02090915705799045355E-20_rel64, 2.04630909514738935778E-20_rel64, 2.07189977830835932647E-20_rel64, &
           2.09768314011013496081E-20_rel64, 2.12366115707621305045E-20_rel64, 2.14983584982879749137E-20_rel64, &
           2.17620928427778672872E-20_rel64, 2.20278357285625921432E-20_rel64, 2.22956087580452202808E-20_rel64, &
           2.25654340250490425540E-20_rel64, 2.28373341286960044116E-20_rel64, 2.31113321878400099473E-20_rel64, &
           2.33874518560808638454E-20_rel64, 2.36657173373861096858E-20_rel64, 2.39461534023496104290E-20_rel64, &
           2.42287854051174089873E-20_rel64, 2.45136393010132116374E-20_rel64, 2.48007416648977634122E-20_rel64, &
           2.50901197102984420223E-20_rel64, 2.53818013093475956328E-20_rel64, 2.56758150135705012224E-20_rel64, &
           2.59721900755663365066E-20_rel64, 2.62709564716282528933E-20_rel64, 2.65721449253515241980E-20_rel64, &
           2.68757869322818418026E-20_rel64, 2.71819147856591489545E-20_rel64, 2.74905616033159739928E-20_rel64, &
           2.78017613557930552431E-20_rel64, 2.81155488957391719494E-20_rel64, 2.84319599886665308873E-20_rel64, &
           2.87510313451378346718E-20_rel64, 2.90728006544663053641E-20_rel64, 2.93973066200154889382E-20_rel64, &
           2.97245889961916589655E-20_rel64, 3.00546886272281117113E-20_rel64, 3.03876474878676440341E-20_rel64, &
           3.07235087260570789851E-20_rel64, 3.10623167077759059128E-20_rel64, 3.14041170641299920359E-20_rel64, &
           3.17489567408509671484E-20_rel64, 3.20968840503523558764E-20_rel64, 3.24479487265049143169E-20_rel64, &
           3.28022019823060106591E-20_rel64, 3.31596965706313734481E-20_rel64, 3.35204868482722289433E-20_rel64, &
           3.38846288434768859868E-20_rel64, 3.42521803272333430511E-20_rel64, 3.46232008885486443621E-20_rel64, &
           3.49977520140016757069E-20_rel64, 3.53758971718690625225E-20_rel64, 3.57577019011490341679E-20_rel64, &
           3.61432339058357974562E-20_rel64, 3.65325631548273995721E-20_rel64, 3.69257619878835712218E-20_rel64, &
           3.73229052280869820872E-20_rel64, 3.77240703013021161483E-20_rel64, 3.81293373631710418510E-20_rel64, &
           3.85387894342352310310E-20_rel64, 3.89525125438278619213E-20_rel64, 3.93705958834423987630E-20_rel64, &
           3.97931319703514420467E-20_rel64, 4.02202168223257681021E-20_rel64, 4.06519501443881316281E-20_rel64, &
           4.10884355286309459252E-20_rel64, 4.15297806682327130812E-20_rel64, 4.19760975869265834817E-20_rel64, &
           4.24275028853074523264E-20_rel64, 4.28841180055136017166E-20_rel64, 4.33460695159874512684E-20_rel64, &
           4.38134894182102581350E-20_rel64, 4.42865154775208394592E-20_rel64, 4.47652915803723541593E-20_rel64, &
           4.52499681206583052039E-20_rel64, 4.57407024180544142586E-20_rel64, 4.62376591716830151692E-20_rel64, &
           4.67410109528183675899E-20_rel64, 4.72509387408234126758E-20_rel64, 4.77676325070512166465E-20_rel64, &
           4.82912918520698939959E-20_rel64, 4.88221267022928031249E-20_rel64, 4.93603580729338490856E-20_rel64, &
           4.99062189051820230400E-20_rel64, 5.04599549866255396689E-20_rel64, 5.10218259652853236286E-20_rel64, &
           5.15921064691782548971E-20_rel64, 5.21710873451692315880E-20_rel64, 5.27590770330452832350E-20_rel64, &
           5.33564030933258560175E-20_rel64, 5.39634139103995100733E-20_rel64, 5.45804805962592501189E-20_rel64, &
           5.52079991245355797360E-20_rel64, 5.58463927298738309556E-20_rel64, 5.64961146141937670274E-20_rel64, &
           5.71576510092907092078E-20_rel64, 5.78315246549566343324E-20_rel64, 5.85182987637943265614E-20_rel64, &
           5.92185815587917070964E-20_rel64, 5.99330314883387041606E-20_rel64, 6.06623632467968854605E-20_rel64, &
           6.14073547584349989444E-20_rel64, 6.21688553204997617046E-20_rel64, 6.29477951501037269151E-20_rel64, &
           6.37451966432143927376E-20_rel64, 6.45621877375379813901E-20_rel64, 6.54000178818890992366E-20_rel64, &
           6.62600772633093429855E-20_rel64, 6.71439201451466169177E-20_rel64, 6.80532934473017024285E-20_rel64, &
           6.89901720881330025051E-20_rel64, 6.99568031585644970529E-20_rel64, 7.09557617948784345070E-20_rel64, &
           7.19900227889450788603E-20_rel64, 7.30630537391054600102E-20_rel64, 7.41789382662668865389E-20_rel64, &
           7.53425421341731220084E-20_rel64, 7.65597421711429721382E-20_rel64, 7.78377498634128443210E-20_rel64, &
           7.91855826740295089420E-20_rel64, 8.06147755373533057126E-20_rel64, 8.21405027698180673876E-20_rel64, &
           8.37834459782805211813E-20_rel64, 8.55731292496781621886E-20_rel64, 8.75544596695901034125E-20_rel64, &
           8.98023880577068813857E-20_rel64, 9.24624714211510822901E-20_rel64, 9.59196413449517198532E-20_rel64, &
           1.08420217248550443413E-19_rel64                                                                      &
           ]
            
  INTEGER(INT16), dimension(256), parameter:: exp_map = [ &
             1_int16,   1_int16,   2_int16, 236_int16,   4_int16,   5_int16,   6_int16,   1_int16,   1_int16,   1_int16, &
             1_int16,   1_int16,   1_int16,   1_int16,   1_int16,   1_int16,   1_int16,   1_int16,   2_int16,   2_int16, &
             2_int16,   2_int16,   3_int16,   3_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, &
           253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, &
           253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, &
           253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, &
           253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, &
           253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, &
           253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, &
           253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, &
           253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, &
           253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, &
           253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, &
           253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, &
           253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, &
           253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, &
           253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, &
           253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 252_int16, 252_int16, 252_int16, &
           252_int16, 252_int16, 252_int16, 252_int16, 252_int16, 252_int16, 252_int16, 252_int16, 252_int16, 252_int16, &
           251_int16, 251_int16, 251_int16, 251_int16, 251_int16, 251_int16, 251_int16, 250_int16, 250_int16, 250_int16, &
           250_int16, 250_int16, 250_int16, 249_int16, 249_int16, 249_int16, 249_int16, 248_int16, 248_int16, 248_int16, &
           248_int16, 247_int16, 247_int16, 247_int16, 246_int16, 246_int16, 245_int16, 245_int16, 244_int16, 244_int16, &
           243_int16, 242_int16, 242_int16, 241_int16, 240_int16, 238_int16,   4_int16,   4_int16,   5_int16,   5_int16, &
             7_int16,   1_int16,   1_int16,   1_int16,   1_int16, 237_int16, 238_int16, 239_int16, 240_int16, 241_int16, &
           242_int16, 243_int16, 244_int16, 245_int16, 246_int16, 247_int16, 248_int16, 249_int16, 250_int16, 251_int16, &
           252_int16, 253_int16,   3_int16,   1_int16,   1_int16,   1_int16                                              &
           ] 
      
  INTEGER(INT64), dimension(256), parameter:: exp_ipmf= [ &
            9223372036854775806_int64,  1623796909450836942_int64,  2664290944894287536_int64, &
            7387971354164062286_int64,  6515064486552725727_int64,  8840508362680705564_int64, &
            6099647593382931854_int64,  7673130333659518959_int64,  6220332867583442119_int64, &
            5045979640552799519_int64,  4075305837223961434_int64,  3258413672162528204_int64, &
            2560664887087755460_int64,  1957224924672901793_int64,  1429800935350586317_int64, &
             964606309710805398_int64,   551043923599587507_int64,   180827629096887271_int64, &
            -152619738120024135_int64,  -454588624410297994_int64,  -729385126147771550_int64, &
            -980551509819436091_int64, -1211029700667469343_int64, -1423284293868552853_int64, &
           -1619396356369054015_int64, -1801135830956208679_int64, -1970018048575620636_int64, &
           -2127348289059702419_int64, -2274257249303686369_int64, -2411729520096647511_int64, &
           -2540626634159186189_int64, -2661705860113411427_int64, -2775635634532452931_int64, &
           -2883008316030452685_int64, -2984350790383660344_int64, -3080133339198120492_int64, &
           -3170777096303094023_int64, -3256660348483807146_int64, -3338123885075143810_int64, &
           -3415475560473292784_int64, -3488994201966436213_int64, -3558932970354465681_int64, &
           -3625522261068040523_int64, -3688972217741992772_int64, -3749474917563780918_int64, &
           -3807206277531066033_int64, -3862327722496832777_int64, -3914987649156774371_int64, &
           -3965322714631868789_int64, -4013458973776904711_int64, -4059512885612775571_int64, &
           -4103592206186240140_int64, -4145796782586126309_int64, -4186219260694351160_int64, &
           -4224945717447272663_int64, -4262056226866286506_int64, -4297625367836515404_int64, &
           -4331722680528539029_int64, -4364413077437474043_int64, -4395757214229410182_int64, &
           -4425811824915126951_int64, -4454630025296931623_int64, -4482261588141301290_int64, &
           -4508753193105274668_int64, -4534148654077814519_int64, -4558489126279958535_int64, &
           -4581813295192218010_int64, -4604157549138257917_int64, -4625556137145252094_int64, &
           -4646041313519107008_int64, -4665643470413307024_int64, -4684391259530330202_int64, &
           -4702311703971758561_int64, -4719430301145093973_int64, -4735771117539952483_int64, &
           -4751356876102085678_int64, -4766209036859141945_int64, -4780347871386006289_int64, &
           -4793792531638886797_int64, -4806561113635134843_int64, -4818670716409303206_int64, &
           -4830137496634475108_int64, -4840976719260841080_int64, -4851202804490340302_int64, &
           -4860829371376465578_int64, -4869869278311660680_int64, -4878334660640769131_int64, &
           -4886236965617420889_int64, -4893586984900801361_int64, -4900394884772701206_int64, &
           -4906670234238888961_int64, -4912422031164499511_int64, -4917658726580128817_int64, &
           -4922388247283526639_int64, -4926618016851058129_int64, -4930354975163349944_int64, &
           -4933605596540647482_int64, -4936375906575298263_int64, -4938671497741363402_int64, &
           -4940497543854573923_int64, -4941858813449628344_int64, -4942759682136115973_int64, &
           -4943204143989096034_int64, -4943195822025520534_int64, -4942737977813217760_int64, &
           -4941833520255016417_int64, -4940485013586754412_int64, -4938694684624350782_int64, &
           -4936464429291796994_int64, -4933795818458819764_int64, -4930690103114058905_int64, &
           -4927148218896869823_int64, -4923170790008281939_int64, -4918758132519204034_int64, &
           -4913910257091649047_int64, -4908626871126539190_int64, -4902907380349533220_int64, &
           -4896750889844278395_int64, -4890156204540517421_int64, -4883121829162564021_int64, &
           -4875645967641788341_int64, -4867726521994914537_int64, -4859361090668117144_int64, &
           -4850546966345100146_int64, -4841281133215543008_int64, -4831560263698491528_int64, &
           -4821380714613448338_int64, -4810738522790068329_int64, -4799629400105478223_int64, &
           -4788048727936306618_int64, -4775991551010520594_int64, -4763452570642106428_int64, &
           -4750426137329493684_int64, -4736906242696391928_int64, -4722886510751374910_int64, &
           -4708360188440094804_int64, -4693320135461424682_int64, -4677758813316095437_int64, &
           -4661668273553497421_int64, -4645040145179239295_int64, -4627865621182777734_int64, &
           -4610135444140930865_int64, -4591839890849342850_int64, -4572968755929952125_int64, &
           -4553511334358204380_int64, -4533456402849113793_int64, -4512792200036279803_int64, &
           -4491506405372576101_int64, -4469586116675404269_int64, -4447017826233103268_int64, &
           -4423787395382280037_int64, -4399880027458422930_int64, -4375280239014120273_int64, &
           -4349971829190466088_int64, -4323937847117722365_int64, -4297160557210942232_int64, &
           -4269621402214952430_int64, -4241300963840747800_int64, -4212178920821854968_int64, &
           -4182234004204454263_int64, -4151443949668870748_int64, -4119785446662303522_int64, &
           -4087234084103185161_int64, -4053764292396157155_int64, -4019349281473095944_int64, &
           -3983960974549683762_int64, -3947569937258414679_int64, -3910145301787349605_int64, &
           -3871654685619035220_int64, -3832064104425386085_int64, -3791337878631549422_int64, &
           -3749438533114321402_int64, -3706326689447986810_int64, -3661960950051856150_int64, &
           -3616297773528530358_int64, -3569291340409183073_int64, -3520893408440942549_int64, &
           -3471053156460668043_int64, -3419717015797774549_int64, -3366828488034801678_int64, &
           -3312327947826463661_int64, -3256152429334017728_int64, -3198235394669707121_int64, &
           -3138506482563180856_int64, -3076891235255160882_int64, -3013310801389728773_int64, &
           -2947681612411377003_int64, -2879915029671676459_int64, -2809916959107513838_int64, &
           -2737587429961860778_int64, -2662820133571330167_int64, -2585501917733382572_int64, &
           -2505512231579379230_int64, -2422722515205210513_int64, -2336995527534099024_int64, &
           -2248184604988707062_int64, -2156132842510781916_int64, -2060672187261021857_int64, &
           -1961622433929369701_int64, -1858790108950103127_int64, -1751967229002893790_int64, &
           -1640929916937145353_int64, -1525436855617589068_int64, -1405227557075248277_int64, &
           -1280020420662655717_int64, -1149510549536592414_int64, -1013367289578707083_int64, &
            -871231448632098451_int64,  -722712146453669790_int64,  -567383236774430108_int64, &
            -404779231966951925_int64,  -234390647591536485_int64,   -55658667960112217_int64, &
             132030985907824249_int64,   329355128892814265_int64,   537061298001092428_int64, &
             755977262693563850_int64,   987022116608035296_int64,  1231219266829423286_int64, &
            1489711711346527014_int64,  1763780090187555756_int64,  2054864117341786919_int64, &
            2364588157623778796_int64,  2694791916990489737_int64,  3047567482883486812_int64, &
            3425304305830815533_int64,  3830744187097289433_int64,  4267048975685834645_int64, &
            4737884547990024084_int64,  5247525842198996915_int64,  5800989391535350163_int64, &
            6404202162993299938_int64,  7064218894258535062_int64,  7789505049452342962_int64, &
            8590309807749433783_int64,  7643763810684500598_int64,  8891950541491446588_int64, &
            5457384281016217099_int64,  9083704440929284705_int64,  7976211653914441081_int64, &
            8178631350487105377_int64,  2821287825726750080_int64,  6322989683301725109_int64, &
            4309503753387613243_int64,  4685170734960179824_int64,  8404845967535220334_int64, &
            7330522972447578232_int64,  1960945799077012817_int64,  4742910674644906342_int64, &
            -751799822533487403_int64,  7023456603741968565_int64,  3843116882594667183_int64, &
            3927231442413912436_int64, -9223372036854775807_int64, -9223372036854775807_int64, &
           -9223372036854775807_int64                                                          &
           ]
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  INTEGER(INT64), parameter:: nrm_iE_min =  760463704284035181_int64
  INTEGER(INT64), parameter:: nrm_iE_max = -2269182951627975918_int64   ! Note sign was flipped to change if(Ud < -nrm_iE_max) cycle to if(Ud < nrm_iE_max) cycle
  INTEGER(int16), parameter:: nrm_inflection = 206_int16                ! Transition point from concave to convex curve
  INTEGER(INT16), parameter:: nrm_X_dim  = 254_int16                    ! Set equal to the dim of the next two arrays
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  REAL(REL64), dimension(nrm_X_dim), parameter:: nrm_X   = [ &
           3.94216628253981313165E-19_rel64, 3.72049450041190115432E-19_rel64, 3.58270244806286766940E-19_rel64, &
           3.48074762365402488083E-19_rel64, 3.39901771718821375510E-19_rel64, 3.33037783603401379846E-19_rel64, &
           3.27094388176175514127E-19_rel64, 3.21835771324950997879E-19_rel64, 3.17107585418404326164E-19_rel64, &
           3.12803074070340668951E-19_rel64, 3.08845206558040194831E-19_rel64, 3.05176506241073501875E-19_rel64, &
           3.01752902925845993970E-19_rel64, 2.98539834407053226435E-19_rel64, 2.95509674628017987235E-19_rel64, &
           2.92639979884916621193E-19_rel64, 2.89912258699774746086E-19_rel64, 2.87311087802262931891E-19_rel64, &
           2.84823463271013365045E-19_rel64, 2.82438315351943892332E-19_rel64, 2.80146139647270319964E-19_rel64, &
           2.77938712618077979302E-19_rel64, 2.75808869214112097290E-19_rel64, 2.73750326983087576657E-19_rel64, &
           2.71757545433910483436E-19_rel64, 2.69825612475384830170E-19_rel64, 2.67950151887715027873E-19_rel64, &
           2.66127247304400311700E-19_rel64, 2.64353379279766342647E-19_rel64, 2.62625372820284379794E-19_rel64, &
           2.60940353352241413697E-19_rel64, 2.59295709543310011518E-19_rel64, 2.57689061732147248287E-19_rel64, &
           2.56118234977196076787E-19_rel64, 2.54581235933933594865E-19_rel64, 2.53076232923724610608E-19_rel64, &
           2.51601538677984006442E-19_rel64, 2.50155595336461910938E-19_rel64, 2.48736961354031570570E-19_rel64, &
           2.47344300030792048871E-19_rel64, 2.45976369428927284997E-19_rel64, 2.44632013479124514193E-19_rel64, &
           2.43310154111392059433E-19_rel64, 2.42009784271329539811E-19_rel64, 2.40729961704458803802E-19_rel64, &
           2.39469803409033492142E-19_rel64, 2.38228480672526733474E-19_rel64, 2.37005214619317984760E-19_rel64, &
           2.35799272207413299671E-19_rel64, 2.34609962620699725390E-19_rel64, 2.33436634010544557043E-19_rel64, &
           2.32278670546738394192E-19_rel64, 2.31135489743037646152E-19_rel64, 2.30006540027042378476E-19_rel64, &
           2.28891298527976049485E-19_rel64, 2.27789269059218973481E-19_rel64, 2.26699980275273212286E-19_rel64, &
           2.25622983985274177845E-19_rel64, 2.24557853607272618425E-19_rel64, 2.23504182749339114597E-19_rel64, &
           2.22461583905132919569E-19_rel64, 2.21429687252962498804E-19_rel64, 2.20408139548575537122E-19_rel64, &
           2.19396603102976033022E-19_rel64, 2.18394754837496166369E-19_rel64, 2.17402285409168542190E-19_rel64, &
           2.16418898400165189421E-19_rel64, 2.15444309565706136328E-19_rel64, 2.14478246135403449958E-19_rel64, &
           2.13520446163505710558E-19_rel64, 2.12570657923951068852E-19_rel64, 2.11628639346531261865E-19_rel64, &
           2.10694157490820248686E-19_rel64, 2.09766988054834669317E-19_rel64, 2.08846914915673634713E-19_rel64, &
           2.07933729699636340595E-19_rel64, 2.07027231379541071019E-19_rel64, 2.06127225897171293938E-19_rel64, &
           2.05233525808956348996E-19_rel64, 2.04345949953157962812E-19_rel64, 2.03464323136981494504E-19_rel64, &
           2.02588475842164167335E-19_rel64, 2.01718243947713123764E-19_rel64, 2.00853468468575310810E-19_rel64, &
           1.99993995309120160946E-19_rel64, 1.99139675030405842282E-19_rel64, 1.98290362630281450575E-19_rel64, &
           1.97445917335451738361E-19_rel64, 1.96606202404698563536E-19_rel64, 1.95771084942514849577E-19_rel64, &
           1.94940435722463068685E-19_rel64, 1.94114129019621610458E-19_rel64, 1.93292042451529349108E-19_rel64, &
           1.92474056827081689313E-19_rel64, 1.91660056002870728195E-19_rel64, 1.90849926746498253498E-19_rel64, &
           1.90043558606423406420E-19_rel64, 1.89240843787937241529E-19_rel64, 1.88441677034884358618E-19_rel64, &
           1.87645955516777480658E-19_rel64, 1.86853578720974505141E-19_rel64, 1.86064448349609340687E-19_rel64, &
           1.85278468220987917214E-19_rel64, 1.84495544175179271098E-19_rel64, 1.83715583983548687855E-19_rel64, &
           1.82938497261995652426E-19_rel64, 1.82164195387673918890E-19_rel64, 1.81392591418984464741E-19_rel64, &
           1.80623600018644528244E-19_rel64, 1.79857137379647421215E-19_rel64, 1.79093121153938436715E-19_rel64, &
           1.78331470383641998024E-19_rel64, 1.77572105434684282170E-19_rel64, 1.76814947932663952942E-19_rel64, &
           1.76059920700831404863E-19_rel64, 1.75306947700044096191E-19_rel64, 1.74555953970572177101E-19_rel64, &
           1.73806865575634736366E-19_rel64, 1.73059609546552630107E-19_rel64, 1.72314113829409050777E-19_rel64, &
           1.71570307233113771460E-19_rel64, 1.70828119378771385449E-19_rel64, 1.70087480650257877045E-19_rel64, &
           1.69348322145913527395E-19_rel64, 1.68610575631263497834E-19_rel64, 1.67874173492680459463E-19_rel64, &
           1.67139048691906366640E-19_rel64, 1.66405134721352917108E-19_rel64, 1.65672365560102414381E-19_rel64, &
           1.64940675630532659125E-19_rel64, 1.64209999755491154381E-19_rel64, 1.63480273115945322228E-19_rel64, &
           1.62751431209036602902E-19_rel64, 1.62023409806467246579E-19_rel64, 1.61296144913149316510E-19_rel64, &
           1.60569572726045902627E-19_rel64, 1.59843629593134798148E-19_rel64, 1.59118251972424918429E-19_rel64, &
           1.58393376390955539959E-19_rel64, 1.57668939403708005762E-19_rel64, 1.56944877552358877858E-19_rel64, &
           1.56221127323802612940E-19_rel64, 1.55497625108370687877E-19_rel64, 1.54774307157672699268E-19_rel64, &
           1.54051109541983297154E-19_rel64, 1.53327968107096876310E-19_rel64, 1.52604818430569727118E-19_rel64, &
           1.51881595777266827816E-19_rel64, 1.51158235054127624946E-19_rel64, 1.50434670764061981199E-19_rel64, &
           1.49710836958883949409E-19_rel64, 1.48986667191187135650E-19_rel64, 1.48262094465061118119E-19_rel64, &
           1.47537051185543663771E-19_rel64, 1.46811469106698300491E-19_rel64, 1.46085279278201124369E-19_rel64, &
           1.45358411990314511232E-19_rel64, 1.44630796717118616593E-19_rel64, 1.43902362057864141545E-19_rel64, &
           1.43173035676301761984E-19_rel64, 1.42442744237834807077E-19_rel64, 1.41711413344332166300E-19_rel64, &
           1.40978967466427931749E-19_rel64, 1.40245329873122865196E-19_rel64, 1.39510422558490330186E-19_rel64, &
           1.38774166165275751584E-19_rel64, 1.38036479905163850390E-19_rel64, 1.37297281475471730687E-19_rel64, &
           1.36556486972008234584E-19_rel64, 1.35814010797820681168E-19_rel64, 1.35069765567529000725E-19_rel64, &
           1.34323662006924180186E-19_rel64, 1.33575608847482642426E-19_rel64, 1.32825512715420458331E-19_rel64, &
           1.32073278014880875764E-19_rel64, 1.31318806804815251489E-19_rel64, 1.30561998669080761682E-19_rel64, &
           1.29802750579237874114E-19_rel64, 1.29040956749486073022E-19_rel64, 1.28276508483127265488E-19_rel64, &
           1.27509294009892133170E-19_rel64, 1.26739198313404822962E-19_rel64, 1.25966102947995110776E-19_rel64, &
           1.25189885843993747615E-19_rel64, 1.24410421100565222805E-19_rel64, 1.23627578765041648284E-19_rel64, &
           1.22841224597620730116E-19_rel64, 1.22051219820178532839E-19_rel64, 1.21257420847822450787E-19_rel64, &
           1.20459679001669745809E-19_rel64, 1.19657840201180199760E-19_rel64, 1.18851744634195564110E-19_rel64, &
           1.18041226402640912015E-19_rel64, 1.17226113141620633983E-19_rel64, 1.16406225609391096101E-19_rel64, &
           1.15581377245408745206E-19_rel64, 1.14751373693331852807E-19_rel64, 1.13916012285490473566E-19_rel64, &
           1.13075081484925913325E-19_rel64, 1.12228360280630249862E-19_rel64, 1.11375617531079026178E-19_rel64, &
           1.10516611250535266069E-19_rel64, 1.09651087831897552453E-19_rel64, 1.08778781199053729446E-19_rel64, &
           1.07899411880766550519E-19_rel64, 1.07012685997036407918E-19_rel64, 1.06118294147632854085E-19_rel64, &
           1.05215910191029279523E-19_rel64, 1.04305189900275513555E-19_rel64, 1.03385769480354717395E-19_rel64, &
           1.02457263929236988812E-19_rel64, 1.01519265222093105782E-19_rel64, 1.00571340294882350318E-19_rel64, &
           9.96130287996728092151E-20_rel64, 9.86438405994599149248E-20_rel64, 9.76632529647558129534E-20_rel64, &
           9.66707074276234541246E-20_rel64, 9.56656062408666746961E-20_rel64, 9.46473083804332139508E-20_rel64, &
           9.36151250173235032081E-20_rel64, 9.25683143708872878592E-20_rel64, 9.15060758376387785829E-20_rel64, &
           9.04275432677257169595E-20_rel64, 8.93317772337636842574E-20_rel64, 8.82177561023278796235E-20_rel64, &
           8.70843656748923183778E-20_rel64, 8.59303871096121592530E-20_rel64, 8.47544827642443525483E-20_rel64, &
           8.35551795084623473868E-20_rel64, 8.23308489335853699301E-20_rel64, 8.10796837291298517310E-20_rel64, &
           7.97996692841338626200E-20_rel64, 7.84885492860727394229E-20_rel64, 7.71437837009346921356E-20_rel64, &
           7.57624969794675663410E-20_rel64, 7.43414135784853342192E-20_rel64, 7.28767768073784255770E-20_rel64, &
           7.13642454435253785310E-20_rel64, 6.97987602407610693858E-20_rel64, 6.81743689447990548682E-20_rel64, &
           6.64839929861985403694E-20_rel64, 6.47191103451627720678E-20_rel64, 6.28693148131036933805E-20_rel64, &
           6.09216875482812683851E-20_rel64, 5.88598735755768238068E-20_rel64, 5.66626751160909834057E-20_rel64, &
           5.43018136308945724369E-20_rel64, 5.17381717444942191928E-20_rel64, 4.89150317223985469934E-20_rel64, &
           4.57447418907553031556E-20_rel64, 4.20788025685834174808E-20_rel64, 3.76259867224047619527E-20_rel64, &
           3.16285898058818791173E-20_rel64, 0.00000000000000000000E+00_rel64                                    &
           ]
  REAL(REL64), dimension(nrm_X_dim), parameter:: nrm_Y   = [ &
           1.45984107966190637471E-22_rel64, 3.00666134279427966627E-22_rel64, 4.61297288151034654752E-22_rel64, &
           6.26633500492343626735E-22_rel64, 7.95945247618815479074E-22_rel64, 9.68746550217050413050E-22_rel64, &
           1.14468770023794384394E-21_rel64, 1.32350363043791664807E-21_rel64, 1.50498576920531300212E-21_rel64, &
           1.68896530007192975179E-21_rel64, 1.87530253827116272885E-21_rel64, 2.06387984236951902721E-21_rel64, &
           2.25459669136447058505E-21_rel64, 2.44736615188017990808E-21_rel64, 2.64211227277635327561E-21_rel64, &
           2.83876811878799073462E-21_rel64, 3.03727425674572850045E-21_rel64, 3.23757756999865884488E-21_rel64, &
           3.43963031579487806997E-21_rel64, 3.64338936579977947081E-21_rel64, 3.84881558689123113882E-21_rel64, &
           4.05587333094927734040E-21_rel64, 4.26453001042835865774E-21_rel64, 4.47475574223050650732E-21_rel64, &
           4.68652304653555825175E-21_rel64, 4.89980659027752550922E-21_rel64, 5.11458296721054925331E-21_rel64, &
           5.33083050820461750694E-21_rel64, 5.54852911670317618454E-21_rel64, 5.76766012526904735671E-21_rel64, &
           5.98820616991784644835E-21_rel64, 6.21015107954422192820E-21_rel64, 6.43347977822572069134E-21_rel64, &
           6.65817819857138976124E-21_rel64, 6.88423320458931752462E-21_rel64, 7.11163252279570879051E-21_rel64, &
           7.34036468049030961555E-21_rel64, 7.57041895028864191875E-21_rel64, 7.80178530013797474202E-21_rel64, &
           8.03445434815700121586E-21_rel64, 8.26841732173331148128E-21_rel64, 8.50366602039150151993E-21_rel64, &
           8.74019278201095141541E-21_rel64, 8.97799045202819052908E-21_rel64, 9.21705235530614430291E-21_rel64, &
           9.45737227039288261894E-21_rel64, 9.69894440592694272520E-21_rel64, 9.94176337897584235616E-21_rel64, &
           1.01858241951198183535E-20_rel64, 1.04311222301147706292E-20_rel64, 1.06776532129873958682E-20_rel64, &
           1.09254132104320038617E-20_rel64, 1.11743986123928914844E-20_rel64, 1.14246061187287141113E-20_rel64, &
           1.16760327268663009725E-20_rel64, 1.19286757203610278370E-20_rel64, 1.21825326582893718615E-20_rel64, &
           1.24376013654067856751E-20_rel64, 1.26938799230106735953E-20_rel64, 1.29513666604541455152E-20_rel64, &
           1.32100601472614602666E-20_rel64, 1.34699591858007331705E-20_rel64, 1.37310628044736433787E-20_rel64, &
           1.39933702513855970074E-20_rel64, 1.42568809884631351174E-20_rel64, 1.45215946859883673330E-20_rel64, &
           1.47875112175229023039E-20_rel64, 1.50546306551961700695E-20_rel64, 1.53229532653352190406E-20_rel64, &
           1.55924795044150482730E-20_rel64, 1.58632100153103272091E-20_rel64, 1.61351456238309804889E-20_rel64, &
           1.64082873355255927115E-20_rel64, 1.66826363327379329583E-20_rel64, 1.69581939719031254185E-20_rel64, &
           1.72349617810711129236E-20_rel64, 1.75129414576460855289E-20_rel64, 1.77921348663314861829E-20_rel64, &
           1.80725440372710686288E-20_rel64, 1.83541711643772766790E-20_rel64, 1.86370186038389457480E-20_rel64, &
           1.89210888728010031653E-20_rel64, 1.92063846482094688044E-20_rel64, 1.94929087658156368945E-20_rel64, &
           1.97806642193338579513E-20_rel64, 2.00696541597478405693E-20_rel64, 2.03598818947608599551E-20_rel64, &
           2.06513508883856968058E-20_rel64, 2.09440647606705393914E-20_rel64, 2.12380272875574661642E-20_rel64, &
           2.15332424008704883182E-20_rel64, 2.18297141884304736741E-20_rel64, 2.21274468942945971007E-20_rel64, &
           2.24264449191182702459E-20_rel64, 2.27267128206377963543E-20_rel64, 2.30282553142722759490E-20_rel64, &
           2.33310772738435576143E-20_rel64, 2.36351837324132863617E-20_rel64, 2.39405798832363513555E-20_rel64, &
           2.42472710808302762665E-20_rel64, 2.45552628421603303200E-20_rel64, 2.48645608479403672171E-20_rel64, &
           2.51751709440496235196E-20_rel64, 2.54870991430659287156E-20_rel64, 2.58003516259159969000E-20_rel64, &
           2.61149347436436856619E-20_rel64, 2.64308550192973221804E-20_rel64, 2.67481191499374104771E-20_rel64, &
           2.70667340087662480311E-20_rel64, 2.73867066473811952902E-20_rel64, 2.77080442981535587410E-20_rel64, &
           2.80307543767352678875E-20_rel64, 2.83548444846957494655E-20_rel64, 2.86803224122916292188E-20_rel64, &
           2.90071961413721233585E-20_rel64, 2.93354738484232191477E-20_rel64, 2.96651639077539877059E-20_rel64, &
           2.99962748948286228810E-20_rel64, 3.03288155897480587335E-20_rel64, 3.06627949808852856463E-20_rel64, &
           3.09982222686787622089E-20_rel64, 3.13351068695886077334E-20_rel64, 3.16734584202205595122E-20_rel64, &
           3.20132867816229907193E-20_rel64, 3.23546020437626102556E-20_rel64, 3.26974145301848059631E-20_rel64, &
           3.30417348028649486531E-20_rel64, 3.33875736672573475852E-20_rel64, 3.37349421775489396999E-20_rel64, &
           3.40838516421252064944E-20_rel64, 3.44343136292562454229E-20_rel64, 3.47863399730113787330E-20_rel64, &
           3.51399427794111634276E-20_rel64, 3.54951344328261734266E-20_rel64, 3.58519276026324609682E-20_rel64, &
           3.62103352501341709597E-20_rel64, 3.65703706357643816625E-20_rel64, 3.69320473265758802127E-20_rel64, &
           3.72953792040342547107E-20_rel64, 3.76603804721263987982E-20_rel64, 3.80270656657982828728E-20_rel64, &
           3.83954496597366517001E-20_rel64, 3.87655476775101647669E-20_rel64, 3.91373753010864071821E-20_rel64, &
           3.95109484807421694974E-20_rel64, 3.98862835453854290595E-20_rel64, 4.02633972133085684027E-20_rel64, &
           4.06423066033935431286E-20_rel64, 4.10230292467909685755E-20_rel64, 4.14055830990964377352E-20_rel64, &
           4.17899865530488192747E-20_rel64, 4.21762584517768217519E-20_rel64, 4.25644181026217564344E-20_rel64, &
           4.29544852915661955584E-20_rel64, 4.33464802983001152737E-20_rel64, 4.37404239119581436798E-20_rel64, &
           4.41363374475637160814E-20_rel64, 4.45342427632182848155E-20_rel64, 4.49341622780762539094E-20_rel64, &
           4.53361189911490250025E-20_rel64, 4.57401365009844675158E-20_rel64, 4.61462390262712818127E-20_rel64, &
           4.65544514274211297937E-20_rel64, 4.69647992291850858549E-20_rel64, 4.73773086443649376302E-20_rel64, &
           4.77920065986841682748E-20_rel64, 4.82089207568881110299E-20_rel64, 4.86280795501478165130E-20_rel64, &
           4.90495122048476513920E-20_rel64, 4.94732487728425957224E-20_rel64, 4.98993201632776718188E-20_rel64, &
           5.03277581760689718695E-20_rel64, 5.07585955371534122362E-20_rel64, 5.11918659356226937964E-20_rel64, &
           5.16276040628660615396E-20_rel64, 5.20658456538564131446E-20_rel64, 5.25066275307251952281E-20_rel64, &
           5.29499876487834480332E-20_rel64, 5.33959651451594274868E-20_rel64, 5.38446003902375748230E-20_rel64, &
           5.42959350420993613377E-20_rel64, 5.47500121041838705461E-20_rel64, 5.52068759864050741515E-20_rel64, &
           5.56665725699838177873E-20_rel64, 5.61291492762757908449E-20_rel64, 5.65946551399024767399E-20_rel64, &
           5.70631408865205668739E-20_rel64, 5.75346590155969163632E-20_rel64, 5.80092638885912137841E-20_rel64, &
           5.84870118229875783049E-20_rel64, 5.89679611926597983307E-20_rel64, 5.94521725351034747237E-20_rel64, &
           5.99397086661226057357E-20_rel64, 6.04306348026189303717E-20_rel64, 6.09250186942005335460E-20_rel64, &
           6.14229307644028540304E-20_rel64, 6.19244442624015364843E-20_rel64, 6.24296354261939415586E-20_rel64, &
           6.29385836583362175411E-20_rel64, 6.34513717154475662765E-20_rel64, 6.39680859128349600881E-20_rel64, &
           6.44888163457527366655E-20_rel64, 6.50136571289953422180E-20_rel64, 6.55427066567317080538E-20_rel64, &
           6.60760678847307201855E-20_rel64, 6.66138486374041986302E-20_rel64, 6.71561619424129804857E-20_rel64, &
           6.77031263959505740024E-20_rel64, 6.82548665622464107799E-20_rel64, 6.88115134113278245543E-20_rel64, &
           6.93732047996596837824E-20_rel64, 6.99400859989591117323E-20_rel64, 7.05123102792795092318E-20_rel64, &
           7.10900395533971723848E-20_rel64, 7.16734450906447958839E-20_rel64, 7.22627083096557881725E-20_rel64, &
           7.28580216610573341955E-20_rel64, 7.34595896130358035981E-20_rel64, 7.40676297549675549074E-20_rel64, &
           7.46823740370528196764E-20_rel64, 7.53040701672266678784E-20_rel64, 7.59329831906985511358E-20_rel64, &
           7.65693972824837545394E-20_rel64, 7.72136177894876786566E-20_rel64, 7.78659735664170154574E-20_rel64, &
           7.85268196594567579500E-20_rel64, 7.91965404038505578948E-20_rel64, 7.98755530170379677549E-20_rel64, &
           8.05643117889016357703E-20_rel64, 8.12633129964261780372E-20_rel64, 8.19731007037063044381E-20_rel64, &
           8.26942736526340398520E-20_rel64, 8.34274935088367915896E-20_rel64, 8.41734948074534105850E-20_rel64, &
           8.49330970528320618359E-20_rel64, 8.57072195782309040494E-20_rel64, 8.64968999859306892632E-20_rel64, &
           8.73033172956553322410E-20_rel64, 8.81278213788595089594E-20_rel64, 8.89719709281966608663E-20_rel64, &
           8.98375832393140686296E-20_rel64, 9.07268006978695414040E-20_rel64, 9.16421814840635397741E-20_rel64, &
           9.25868264067027590425E-20_rel64, 9.35645614802788640806E-20_rel64, 9.45802100126361735376E-20_rel64, &
           9.56400155508503632674E-20_rel64, 9.67523347705031340935E-20_rel64, 9.79288516978088272607E-20_rel64, &
           9.91869058575313280859E-20_rel64, 1.00554562713433970882E-19_rel64, 1.02084073773055658234E-19_rel64, &
           1.03903609932407111672E-19_rel64, 1.08420217248550443413E-19_rel64                                    &
           ]
  INTEGER(INT16), dimension(256), parameter:: nrm_map = [ &
             1_int16,   1_int16, 240_int16,   3_int16,   1_int16,   1_int16,   1_int16,   1_int16,   1_int16,   1_int16, &
             1_int16,   1_int16,   2_int16,   2_int16,   2_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, &
           254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, &
           254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, &
           254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, &
           254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, &
           254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, &
           254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, &
           254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, &
           254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, &
           254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, &
           254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, &
           254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, &
           254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, &
           254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, &
           254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, &
           254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, &
           254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, &
           254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, 254_int16, &
           254_int16, 254_int16, 254_int16, 254_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, 253_int16, &
           253_int16, 253_int16, 253_int16, 253_int16, 252_int16, 252_int16, 252_int16, 252_int16, 252_int16, 252_int16, &
           252_int16, 251_int16, 251_int16, 251_int16, 251_int16, 251_int16, 250_int16, 250_int16, 250_int16, 249_int16, &
           249_int16, 249_int16, 248_int16, 248_int16, 248_int16, 247_int16, 247_int16, 246_int16, 245_int16, 245_int16, &
           244_int16, 243_int16, 241_int16,   3_int16,   3_int16,   4_int16,   4_int16,   1_int16,   1_int16, 241_int16, &
           242_int16, 243_int16, 244_int16, 245_int16, 246_int16, 247_int16, 248_int16, 249_int16, 250_int16, 251_int16, &
           252_int16, 253_int16, 254_int16,   2_int16,   1_int16,    1_int16                                             &
           ]
  INTEGER(INT64), dimension(256), parameter:: nrm_ipmf= [ &
            9223372036854775807_int64,  1100243796470411621_int64,  7866600928967250930_int64, &
            6788754710669696542_int64,  9022865200207144863_int64,  6522434035182588998_int64, &
            4723064097388362089_int64,  3360495653202216738_int64,  2289663232347314637_int64, &
            1423968905585863458_int64,   708364817795267048_int64,   106102487338930753_int64, &
            -408333464668576829_int64,  -853239722790486501_int64, -1242095211827097900_int64, &
           -1585059631108673940_int64, -1889943050267338444_int64, -2162852901996530861_int64, &
           -2408637386596948951_int64, -2631196530256962321_int64, -2833704942542521341_int64, &
           -3018774289008790490_int64, -3188573753501855188_int64, -3344920681670403338_int64, &
           -3489349705095956332_int64, -3623166100045372309_int64, -3747487436861272675_int64, &
           -3863276422709148967_int64, -3971367044055518755_int64, -4072485557008390973_int64, &
           -4167267476835659290_int64, -4256271432259152236_int64, -4339990541931722486_int64, &
           -4418861817116128144_int64, -4493273980399813712_int64, -4563574004455586789_int64, &
           -4630072609765607697_int64, -4693048910437213976_int64, -4752754358851368939_int64, &
           -4809416110064308998_int64, -4863239903553551854_int64, -4914412541525450390_int64, &
           -4963104028438371752_int64, -5009469424783408506_int64, -5053650458852382942_int64, &
           -5095776932714620736_int64, -5135967952538793751_int64, -5174333008440011819_int64, &
           -5210972924976803212_int64, -5245980700089108030_int64, -5279442247516602580_int64, &
           -5311437055455727165_int64, -5342038772315689808_int64, -5371315728848241176_int64, &
           -5399331404596872149_int64, -5426144845492953749_int64, -5451811038482572561_int64, &
           -5476381248268657966_int64, -5499903320574195562_int64, -5522421955754026689_int64, &
           -5543978956088647146_int64, -5564613449670104702_int64, -5584362093426444811_int64, &
           -5603259257517948598_int64, -5621337193067973646_int64, -5638626184957139723_int64, &
           -5655154691206533444_int64, -5670949470299032213_int64, -5686035697633974059_int64, &
           -5700437072176021853_int64, -5714175914241474892_int64, -5727273255262165113_int64, &
           -5739748920276479896_int64, -5751621603817307174_int64, -5762908939796372639_int64, &
           -5773627565922309014_int64, -5783793183134812265_int64, -5793420610488494761_int64, &
           -5802523835876760449_int64, -5811116062947528159_int64, -5819209754528332525_int64, &
           -5826816672847731481_int64, -5833947916812604931_int64, -5840613956576458023_int64, &
           -5846824665611910289_int64, -5852589350480867031_int64, -5857916778478176126_int64, &
           -5862815203308648100_int64, -5867292388942963102_int64, -5871355631785032475_int64, &
           -5875011781271672546_int64, -5878267259014849673_int64, -5881128076587149736_int64, &
           -5883599852042407008_int64, -5885687825255503611_int64, -5887396872158138792_int64, &
           -5888731517940793642_int64, -5889695949285116095_int64, -5890294025685464884_int64, &
           -5890529289913327324_int64, -5890404977673717937_int64, -5889924026498431163_int64, &
           -5889089083917124306_int64, -5887902514943612818_int64, -5886366408911438678_int64, &
           -5884482585689696666_int64, -5882252601307248040_int64, -5879677753010794116_int64, &
           -5876759083779803205_int64, -5873497386318964789_int64, -5869893206546667776_int64, &
           -5865946846595949824_int64, -5861658367342427720_int64, -5857027590471881677_int64, &
           -5852054100098417010_int64, -5846737243942453675_int64, -5841076134076187196_int64, &
           -5835069647242613555_int64, -5828716424752706462_int64, -5822014871963869175_int64, &
           -5814963157341346144_int64, -5807559211102864230_int64, -5799800723445370945_int64, &
           -5791685142351340553_int64, -5783209670970720163_int64, -5774371264573179642_int64, &
           -5765166627063903647_int64, -5755592207054713659_int64, -5745644193480824773_int64, &
           -5735318510752018208_int64, -5724610813425437739_int64, -5713516480385587930_int64, &
           -5702030608515415262_int64, -5690148005840580450_int64, -5677863184127171500_int64, &
           -5665170350911151771_int64, -5652063400935774062_int64, -5638535906971008479_int64, &
           -5624581109986715427_int64, -5610191908648831413_int64, -5595360848105209228_int64, &
           -5580080108024948839_int64, -5564341489852052995_int64, -5548136403231022530_int64, &
           -5531455851558548986_int64, -5514290416611743216_int64, -5496630242199332042_int64, &
           -5478465016777932771_int64, -5459783954970846041_int64, -5440575777921757048_int64, &
           -5420828692410265636_int64, -5400530368650235419_int64, -5379667916685514805_int64, &
           -5358227861290587323_int64, -5336196115276098412_int64, -5313557951090917843_int64, &
           -5290297970603361334_int64, -5266400072934333841_int64, -5241847420204383969_int64, &
           -5216622401044877175_int64, -5190706591710597222_int64, -5164080714616950035_int64, &
           -5136724594109436823_int64, -5108617109256032884_int64, -5079736143434385243_int64, &
           -5050058530465135684_int64, -5019559997019971489_int64, -4988215101007962902_int64, &
           -4955997165616091829_int64, -4922878208649303098_int64, -4888828866781570171_int64, &
           -4853818314291967860_int64, -4817814175818138875_int64, -4780782432613321964_int64, &
           -4742687321741700218_int64, -4703491227589571839_int64, -4663154565005992677_int64, &
           -4621635653315233404_int64, -4578890580363653611_int64, -4534873055674295895_int64, &
           -4489534251682353324_int64, -4442822631912177240_int64, -4394683764829955584_int64, &
           -4345060121963625698_int64, -4293890858720699712_int64, -4241111576152819607_int64, &
           -4186654061709937657_int64, -4130446006793444037_int64, -4072410698652145832_int64, &
           -4012466683862839461_int64, -3950527400292567752_int64, -3886500774045765687_int64, &
           -3820288777448429155_int64, -3751786943603808360_int64, -3680883832458829622_int64, &
           -3607460442634311640_int64, -3531389562479388257_int64, -3452535052892673425_int64, &
           -3370751053387219477_int64, -3285881101636356540_int64, -3197757155290705819_int64, &
           -3106198503163952069_int64, -3011010550898977333_int64, -2911983463889098687_int64, &
           -2808890647471117000_int64, -2701487041141552641_int64, -2589507199668914250_int64, &
           -2472663129352363447_int64, -2350641842148592339_int64, -2223102583752268533_int64, &
           -2089673683718549192_int64, -1949948966041645259_int64, -1803483646850564850_int64, &
           -1649789631543420087_int64, -1488330106106077262_int64, -1318513295716691268_int64, &
           -1139685236949881816_int64,  -951121376567001875_int64,  -752016768187464716_int64, &
            -541474585679288331_int64,  -318492605702539737_int64,   -81947227237770942_int64, &
             169425512586606639_int64,   437052607251345214_int64,   722551297576803378_int64, &
            1027761939321774997_int64,  1354787941562563859_int64,  1706044619231651456_int64, &
            2084319374410752592_int64,  2492846399585976196_int64,  2935400169364871386_int64, &
            3416413484632152027_int64,  3941127949845234205_int64,  4515787798750205840_int64, &
            5147892401460636766_int64,  5846529325404289247_int64,  6622819682189712845_int64, &
            7490522659877241412_int64,  8466869998300390792_int64,  8216968526327325546_int64, &
            4550693915429777408_int64,  7628019504075672067_int64,  6605080500885724440_int64, &
            7121156327618499689_int64,  2484871780310663815_int64,  7179104797025802029_int64, &
            7066086283790282497_int64,  1516500120772212280_int64,   216305945406514865_int64, &
            6295963418490464265_int64,  2889316805640788711_int64, -2712587580563250868_int64, &
            6562498853480437177_int64,  7975754821117416020_int64, -9223372036854775807_int64, &
            -9223372036854775807_int64                                                         &
           ]
  !REAL(REL64),    dimension(254), parameter:: nrm_X   = [ REAL(REL64):: 3.94216628254e-19, 3.72049450041e-19, 3.58270244806e-19, 3.48074762365e-19, 3.39901771719e-19, 3.33037783603e-19, 3.27094388176e-19, 3.21835771325e-19, 3.17107585418e-19, 3.1280307407e-19, 3.08845206558e-19, 3.05176506241e-19, 3.01752902926e-19, 2.98539834407e-19, 2.95509674628e-19, 2.92639979885e-19, 2.899122587e-19, 2.87311087802e-19, 2.84823463271e-19, 2.82438315352e-19, 2.80146139647e-19, 2.77938712618e-19, 2.75808869214e-19, 2.73750326983e-19, 2.71757545434e-19, 2.69825612475e-19, 2.67950151888e-19, 2.66127247304e-19, 2.6435337928e-19, 2.6262537282e-19, 2.60940353352e-19, 2.59295709543e-19, 2.57689061732e-19, 2.56118234977e-19, 2.54581235934e-19, 2.53076232924e-19, 2.51601538678e-19, 2.50155595336e-19, 2.48736961354e-19, 2.47344300031e-19, 2.45976369429e-19, 2.44632013479e-19, 2.43310154111e-19, 2.42009784271e-19, 2.40729961704e-19, 2.39469803409e-19, 2.38228480673e-19, 2.37005214619e-19, 2.35799272207e-19, 2.34609962621e-19, 2.33436634011e-19, 2.32278670547e-19, 2.31135489743e-19, 2.30006540027e-19, 2.28891298528e-19, 2.27789269059e-19, 2.26699980275e-19, 2.25622983985e-19, 2.24557853607e-19, 2.23504182749e-19, 2.22461583905e-19, 2.21429687253e-19, 2.20408139549e-19, 2.19396603103e-19, 2.18394754837e-19, 2.17402285409e-19, 2.164188984e-19, 2.15444309566e-19, 2.14478246135e-19, 2.13520446164e-19, 2.12570657924e-19, 2.11628639347e-19, 2.10694157491e-19, 2.09766988055e-19, 2.08846914916e-19, 2.079337297e-19, 2.0702723138e-19, 2.06127225897e-19, 2.05233525809e-19, 2.04345949953e-19, 2.03464323137e-19, 2.02588475842e-19, 2.01718243948e-19, 2.00853468469e-19, 1.99993995309e-19, 1.9913967503e-19, 1.9829036263e-19, 1.97445917335e-19, 1.96606202405e-19, 1.95771084943e-19, 1.94940435722e-19, 1.9411412902e-19, 1.93292042452e-19, 1.92474056827e-19, 1.91660056003e-19, 1.90849926746e-19, 1.90043558606e-19, 1.89240843788e-19, 1.88441677035e-19, 1.87645955517e-19, 1.86853578721e-19, 1.8606444835e-19, 1.85278468221e-19, 1.84495544175e-19, 1.83715583984e-19, 1.82938497262e-19, 1.82164195388e-19, 1.81392591419e-19, 1.80623600019e-19, 1.7985713738e-19, 1.79093121154e-19, 1.78331470384e-19, 1.77572105435e-19, 1.76814947933e-19, 1.76059920701e-19, 1.753069477e-19, 1.74555953971e-19, 1.73806865576e-19, 1.73059609547e-19, 1.72314113829e-19, 1.71570307233e-19, 1.70828119379e-19, 1.7008748065e-19, 1.69348322146e-19, 1.68610575631e-19, 1.67874173493e-19, 1.67139048692e-19, 1.66405134721e-19, 1.6567236556e-19, 1.64940675631e-19, 1.64209999755e-19, 1.63480273116e-19, 1.62751431209e-19, 1.62023409806e-19, 1.61296144913e-19, 1.60569572726e-19, 1.59843629593e-19, 1.59118251972e-19, 1.58393376391e-19, 1.57668939404e-19, 1.56944877552e-19, 1.56221127324e-19, 1.55497625108e-19, 1.54774307158e-19, 1.54051109542e-19, 1.53327968107e-19, 1.52604818431e-19, 1.51881595777e-19, 1.51158235054e-19, 1.50434670764e-19, 1.49710836959e-19, 1.48986667191e-19, 1.48262094465e-19, 1.47537051186e-19, 1.46811469107e-19, 1.46085279278e-19, 1.4535841199e-19, 1.44630796717e-19, 1.43902362058e-19, 1.43173035676e-19, 1.42442744238e-19, 1.41711413344e-19, 1.40978967466e-19, 1.40245329873e-19, 1.39510422558e-19, 1.38774166165e-19, 1.38036479905e-19, 1.37297281475e-19, 1.36556486972e-19, 1.35814010798e-19, 1.35069765568e-19, 1.34323662007e-19, 1.33575608847e-19, 1.32825512715e-19, 1.32073278015e-19, 1.31318806805e-19, 1.30561998669e-19, 1.29802750579e-19, 1.29040956749e-19, 1.28276508483e-19, 1.2750929401e-19, 1.26739198313e-19, 1.25966102948e-19, 1.25189885844e-19, 1.24410421101e-19, 1.23627578765e-19, 1.22841224598e-19, 1.2205121982e-19, 1.21257420848e-19, 1.20459679002e-19, 1.19657840201e-19, 1.18851744634e-19, 1.18041226403e-19, 1.17226113142e-19, 1.16406225609e-19, 1.15581377245e-19, 1.14751373693e-19, 1.13916012285e-19, 1.13075081485e-19, 1.12228360281e-19, 1.11375617531e-19, 1.10516611251e-19, 1.09651087832e-19, 1.08778781199e-19, 1.07899411881e-19, 1.07012685997e-19, 1.06118294148e-19, 1.05215910191e-19, 1.043051899e-19, 1.0338576948e-19, 1.02457263929e-19, 1.01519265222e-19, 1.00571340295e-19, 9.96130287997e-20, 9.86438405995e-20, 9.76632529648e-20, 9.66707074276e-20, 9.56656062409e-20, 9.46473083804e-20, 9.36151250173e-20, 9.25683143709e-20, 9.15060758376e-20, 9.04275432677e-20, 8.93317772338e-20, 8.82177561023e-20, 8.70843656749e-20, 8.59303871096e-20, 8.47544827642e-20, 8.35551795085e-20, 8.23308489336e-20, 8.10796837291e-20, 7.97996692841e-20, 7.84885492861e-20, 7.71437837009e-20, 7.57624969795e-20, 7.43414135785e-20, 7.28767768074e-20, 7.13642454435e-20, 6.97987602408e-20, 6.81743689448e-20, 6.64839929862e-20, 6.47191103452e-20, 6.28693148131e-20, 6.09216875483e-20, 5.88598735756e-20, 5.66626751161e-20, 5.43018136309e-20, 5.17381717445e-20, 4.89150317224e-20, 4.57447418908e-20, 4.20788025686e-20, 3.76259867224e-20, 3.16285898059e-20, 0.0 ]
  !REAL(REL64),    dimension(254), parameter:: nrm_Y   = [ REAL(REL64):: 1.45984107966e-22, 3.00666134279e-22, 4.61297288151e-22, 6.26633500492e-22, 7.95945247619e-22, 9.68746550217e-22, 1.14468770024e-21, 1.32350363044e-21, 1.50498576921e-21, 1.68896530007e-21, 1.87530253827e-21, 2.06387984237e-21, 2.25459669136e-21, 2.44736615188e-21, 2.64211227278e-21, 2.83876811879e-21, 3.03727425675e-21, 3.23757757e-21, 3.43963031579e-21, 3.6433893658e-21, 3.84881558689e-21, 4.05587333095e-21, 4.26453001043e-21, 4.47475574223e-21, 4.68652304654e-21, 4.89980659028e-21, 5.11458296721e-21, 5.3308305082e-21, 5.5485291167e-21, 5.76766012527e-21, 5.98820616992e-21, 6.21015107954e-21, 6.43347977823e-21, 6.65817819857e-21, 6.88423320459e-21, 7.1116325228e-21, 7.34036468049e-21, 7.57041895029e-21, 7.80178530014e-21, 8.03445434816e-21, 8.26841732173e-21, 8.50366602039e-21, 8.74019278201e-21, 8.97799045203e-21, 9.21705235531e-21, 9.45737227039e-21, 9.69894440593e-21, 9.94176337898e-21, 1.01858241951e-20, 1.04311222301e-20, 1.0677653213e-20, 1.09254132104e-20, 1.11743986124e-20, 1.14246061187e-20, 1.16760327269e-20, 1.19286757204e-20, 1.21825326583e-20, 1.24376013654e-20, 1.2693879923e-20, 1.29513666605e-20, 1.32100601473e-20, 1.34699591858e-20, 1.37310628045e-20, 1.39933702514e-20, 1.42568809885e-20, 1.4521594686e-20, 1.47875112175e-20, 1.50546306552e-20, 1.53229532653e-20, 1.55924795044e-20, 1.58632100153e-20, 1.61351456238e-20, 1.64082873355e-20, 1.66826363327e-20, 1.69581939719e-20, 1.72349617811e-20, 1.75129414576e-20, 1.77921348663e-20, 1.80725440373e-20, 1.83541711644e-20, 1.86370186038e-20, 1.89210888728e-20, 1.92063846482e-20, 1.94929087658e-20, 1.97806642193e-20, 2.00696541597e-20, 2.03598818948e-20, 2.06513508884e-20, 2.09440647607e-20, 2.12380272876e-20, 2.15332424009e-20, 2.18297141884e-20, 2.21274468943e-20, 2.24264449191e-20, 2.27267128206e-20, 2.30282553143e-20, 2.33310772738e-20, 2.36351837324e-20, 2.39405798832e-20, 2.42472710808e-20, 2.45552628422e-20, 2.48645608479e-20, 2.5175170944e-20, 2.54870991431e-20, 2.58003516259e-20, 2.61149347436e-20, 2.64308550193e-20, 2.67481191499e-20, 2.70667340088e-20, 2.73867066474e-20, 2.77080442982e-20, 2.80307543767e-20, 2.83548444847e-20, 2.86803224123e-20, 2.90071961414e-20, 2.93354738484e-20, 2.96651639078e-20, 2.99962748948e-20, 3.03288155897e-20, 3.06627949809e-20, 3.09982222687e-20, 3.13351068696e-20, 3.16734584202e-20, 3.20132867816e-20, 3.23546020438e-20, 3.26974145302e-20, 3.30417348029e-20, 3.33875736673e-20, 3.37349421775e-20, 3.40838516421e-20, 3.44343136293e-20, 3.4786339973e-20, 3.51399427794e-20, 3.54951344328e-20, 3.58519276026e-20, 3.62103352501e-20, 3.65703706358e-20, 3.69320473266e-20, 3.7295379204e-20, 3.76603804721e-20, 3.80270656658e-20, 3.83954496597e-20, 3.87655476775e-20, 3.91373753011e-20, 3.95109484807e-20, 3.98862835454e-20, 4.02633972133e-20, 4.06423066034e-20, 4.10230292468e-20, 4.14055830991e-20, 4.1789986553e-20, 4.21762584518e-20, 4.25644181026e-20, 4.29544852916e-20, 4.33464802983e-20, 4.3740423912e-20, 4.41363374476e-20, 4.45342427632e-20, 4.49341622781e-20, 4.53361189911e-20, 4.5740136501e-20, 4.61462390263e-20, 4.65544514274e-20, 4.69647992292e-20, 4.73773086444e-20, 4.77920065987e-20, 4.82089207569e-20, 4.86280795501e-20, 4.90495122048e-20, 4.94732487728e-20, 4.98993201633e-20, 5.03277581761e-20, 5.07585955372e-20, 5.11918659356e-20, 5.16276040629e-20, 5.20658456539e-20, 5.25066275307e-20, 5.29499876488e-20, 5.33959651452e-20, 5.38446003902e-20, 5.42959350421e-20, 5.47500121042e-20, 5.52068759864e-20, 5.566657257e-20, 5.61291492763e-20, 5.65946551399e-20, 5.70631408865e-20, 5.75346590156e-20, 5.80092638886e-20, 5.8487011823e-20, 5.89679611927e-20, 5.94521725351e-20, 5.99397086661e-20, 6.04306348026e-20, 6.09250186942e-20, 6.14229307644e-20, 6.19244442624e-20, 6.24296354262e-20, 6.29385836583e-20, 6.34513717154e-20, 6.39680859128e-20, 6.44888163458e-20, 6.5013657129e-20, 6.55427066567e-20, 6.60760678847e-20, 6.66138486374e-20, 6.71561619424e-20, 6.7703126396e-20, 6.82548665622e-20, 6.88115134113e-20, 6.93732047997e-20, 6.9940085999e-20, 7.05123102793e-20, 7.10900395534e-20, 7.16734450906e-20, 7.22627083097e-20, 7.28580216611e-20, 7.3459589613e-20, 7.4067629755e-20, 7.46823740371e-20, 7.53040701672e-20, 7.59329831907e-20, 7.65693972825e-20, 7.72136177895e-20, 7.78659735664e-20, 7.85268196595e-20, 7.91965404039e-20, 7.9875553017e-20, 8.05643117889e-20, 8.12633129964e-20, 8.19731007037e-20, 8.26942736526e-20, 8.34274935088e-20, 8.41734948075e-20, 8.49330970528e-20, 8.57072195782e-20, 8.64968999859e-20, 8.73033172957e-20, 8.81278213789e-20, 8.89719709282e-20, 8.98375832393e-20, 9.07268006979e-20, 9.16421814841e-20, 9.25868264067e-20, 9.35645614803e-20, 9.45802100126e-20, 9.56400155509e-20, 9.67523347705e-20, 9.79288516978e-20, 9.91869058575e-20, 1.00554562713e-19, 1.02084073773e-19, 1.03903609932e-19, 1.08420217249e-19 ]
  !INTEGER(INT64), dimension(256), parameter:: nrm_ipmf= [ INTEGER(INT64):: 9223372036854775807, 1100243796532604147, 7866600928978262881, 6788754710654027089, 9022865200181691852, 6522434035205505475, 4723064097359993915, 3360495653216419673, 2289663232373874393, 1423968905551925104, 708364817827802907, 106102487305606492, -408333464665790208, -853239722779020926, -1242095211825517037, -1585059631105792592, -1889943050287164616, -2162852901990665326, -2408637386594506662, -2631196530262949902, -2833704942520918738, -3018774289025815052, -3188573753472182441, -3344920681707440829, -3489349705062145773, -3623166100042174483, -3747487436868330185, -3863276422712168700, -3971367044063122321, -4072485557029853479, -4167267476830907467, -4256271432240150335, -4339990541927301065, -4418861817133796561, -4493273980372371289, -4563574004462236379, -4630072609770443287, -4693048910430993529, -4752754358862853623, -4809416110052798111, -4863239903586974371, -4914412541515869230, -4963104028439154501, -5009469424769141876, -5053650458856546947, -5095776932695070785, -5135967952544950843, -5174333008451188631, -5210972924952682065, -5245980700100453012, -5279442247516290042, -5311437055462362013, -5342038772315636138, -5371315728843324427, -5399331404632497705, -5426144845493682879, -5451811038474681565, -5476381248265612057, -5499903320558330825, -5522421955752302985, -5543978956085246988, -5564613449659052023, -5584362093436129148, -5603259257517445975, -5621337193070977432, -5638626184974114133, -5655154691220924683, -5670949470294749069, -5686035697601828417, -5700437072199142976, -5714175914219797723, -5727273255295211389, -5739748920272022600, -5751621603810396917, -5762908939773930926, -5773627565914992314, -5783793183152402325, -5793420610475612574, -5802523835894645221, -5811116062947594592, -5819209754516104281, -5826816672854561136, -5833947916825296227, -5840613956570562553, -5846824665591787384, -5852589350491064419, -5857916778480708968, -5862815203334817577, -5867292388935689664, -5871355631762300668, -5875011781262872391, -5878267259039068099, -5881128076579864607, -5883599852028866964, -5885687825288587920, -5887396872144944193, -5888731517955022366, -5889695949247708370, -5890294025706661862, -5890529289910843690, -5890404977676009159, -5889924026487152552, -5889089083913561497, -5887902514965187544, -5886366408898350160, -5884482585690660648, -5882252601321067956, -5879677752995005083, -5876759083794187244, -5873497386318817608, -5869893206505495615, -5865946846617000703, -5861658367354170141, -5857027590486142268, -5852054100063403979, -5846737243971479915, -5841076134082348607, -5835069647234555080, -5828716424754558627, -5822014871949050545, -5814963157357505615, -5807559211080035948, -5799800723447248431, -5791685142338046612, -5783209670985131963, -5774371264582507258, -5765166627072198898, -5755592207057629351, -5745644193442020886, -5735318510777140130, -5724610813433637581, -5713516480385064197, -5702030608511931905, -5690148005851000163, -5677863184109376595, -5665170350903283020, -5652063400924584736, -5638535907000098559, -5624581109999495711, -5610191908627545348, -5595360848093635657, -5580080108034221525, -5564341489875517452, -5548136403221361788, -5531455851545388194, -5514290416638313566, -5496630242181647032, -5478465016761708394, -5459783954986630496, -5440575777891763554, -5420828692432362328, -5400530368638759086, -5379667916699386572, -5358227861294079939, -5336196115274289941, -5313557951078348351, -5290297970633413513, -5266400072915204637, -5241847420213976978, -5216622401043707762, -5190706591719514516, -5164080714589163015, -5136724594099061292, -5108617109269271995, -5079736143458208314, -5050058530461699570, -5019559997031867155, -4988215101008300666, -4955997165600740840, -4922878208651982466, -4888828866780310778, -4853818314258448763, -4817814175855136221, -4780782432601640934, -4742687321746673837, -4703491227581398702, -4663154564978669839, -4621635653358718847, -4578890580370737438, -4534873055659651863, -4489534251700544707, -4442822631898778778, -4394683764809052552, -4345060121983309848, -4293890858708851568, -4241111576153757717, -4186654061692562932, -4130446006804691432, -4072410698657642967, -4012466683838341666, -3950527400304957273, -3886500774061817392, -3820288777467775968, -3751786943594814089, -3680883832433444937, -3607460442623855428, -3531389562483238811, -3452535052936037985, -3370751053395794721, -3285881101589156030, -3197757155301271700, -3106198503156390075, -3011010550911843739, -2911983463883482375, -2808890647470171482, -2701487041141041038, -2589507199690499622, -2472663129329060997, -2350641842139723534, -2223102583769914387, -2089673683729348624, -1949948966045216354, -1803483646855866339, -1649789631524907106, -1488330106094837958, -1318513295725471712, -1139685236971903433, -951121376551959675, -752016768184573709, -541474585687415681, -318492605680814263, -81947227248966622, 169425512568350963, 437052607277165029, 722551297569085274, 1027761939300002045, 1354787941578333500, 1706044619204253453, 2084319374409947591, 2492846399638817506, 2935400169348911565, 3416413484613541924, 3941127949861028145, 4515787798749165735, 5147892401484995974, 5846529325380992513, 6622819682194933019, 7490522659874903812, 8466869998278641829, 8216968526368126501, 4550693915471153825, 7628019504122306461, 6605080500893076940, 7121156327637209657, 2484871780365768829, 7179104797069433749, 7066086283825115773, 1516500120794063563, 216305945442773460, 6295963418513296140, 2889316805672339623, -2712587580543026574, 6562498853519217374, 7975754821145999232, -9223372036854775807, -9223372036854775807]
  !
  CONTAINS
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  ! 
  ! Uses the splitmix64 algorithm to generate a set of seeds for 
  ! splitmix64 was modified from the original implementation 
  !   to ensure that x and s do not have an INT64 overflow, 
  !   which otherwise would work for an unsigned INT64.
  PURE SUBROUTINE splitmix64(s, r)      ! s is a seed for generating the random integer r
    INTEGER(INT64), intent(inout) :: s
    INTEGER(INT64), intent(inout) :: r
    !
    if( s > 9217757075524191209_int64) then
        s = SHIFTR(s, 17)
    else
        s = s + 5614961330584597_int64
    end if
    !
    r = IEOR( s, SHIFTR(s, 30) ) * 48483319308153_int64  ! (r ^ (r >> 30)) * 4848331912308153_int64
    r = IEOR( r, SHIFTR(r, 27) ) * 68582391223147_int64  ! (r ^ (r >> 27)) * 6858239137223147_int64
    r = IEOR( r, SHIFTR(r, 31) )
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE Seed_RANDOMIZER(s)      ! Runs the splitmix64 seed forward 8191 (2^13-1) times and then randomizes it with splitmix64
    INTEGER(INT64), intent(inout) :: s
    INTEGER:: I
    !
    DO I=1, 8191
       if( s > 9217757075524191209_int64) then
           s = SHIFTR(s, 17)
       else
           s = s + 5614961330584597_int64
       end if
    END DO
    !
    s = ISHFTC( s*5_int64, 7_int64) * 9_int64
    s = IEOR( s, SHIFTR(s, 30) ) * 48483319308153_int64  ! (s ^ (s >> 30)) * 4848331912308153_int64
    s = IEOR( s, SHIFTR(s, 27) ) * 68582391223147_int64  ! (s ^ (s >> 27)) * 6858239137223147_int64
    s = IEOR( s, SHIFTR(s, 31) )
    s = ISHFTC( s, 45_int64)
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !	
  PURE SUBROUTINE GET_RAND_INT64(s, r)               ! xoshiro256** algorithm
    INTEGER(INT64), dimension(4), intent(inout) :: s ! Seed
    INTEGER(INT64),               intent(  out) :: r ! RandomValue
    INTEGER(INT64) :: t
    !
    t = SHIFTL(s(2), 17)
    !
    !r = s(2)*5_int64
    !r =  IOR( SHIFTL(r, 7), SHIFTR(r, 57) ) * 9_int64   ! rol64(s[1] * 5, 7) * 9; rol64(x,k) = (x << k) | (x >> (64 - k));
    r = ISHFTC( s(2)*5_int64, 7_int64) * 9_int64
    !
	s(3) = IEOR( s(3), s(1) ) 
	s(4) = IEOR( s(4), s(2) ) 
	s(2) = IEOR( s(2), s(3) ) 
	s(1) = IEOR( s(1), s(4) ) 
    !
	s(3) = IEOR( s(3), t )
    s(4) = ISHFTC( s(4), 45_int64)  ! rol64(s[3], 45); rol64(x,k) = (x << k) | (x >> (64 - k));
    !
  END SUBROUTINE
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  PURE SUBROUTINE SEED_JUMP(s) ! Moves xoshiro256** seed forward 2^64 values
    INTEGER(INT64), dimension(4), intent(inout) :: s ! Seed
    INTEGER(INT64), dimension(4), parameter :: JUMP = [ 1733541517147835066_int64, -3051731464161248980_int64, &
                                                       -6244198995065845334_int64,  4155657270789760540_int64]
    INTEGER(INT64):: s1, s2, s3, s4, r
    INTEGER:: I, J
    !
    s1 = LONG_ZER
    s2 = LONG_ZER
    s3 = LONG_ZER
    s4 = LONG_ZER
    !
    DO I=1, 4
    DO J=0, 63
       if(IAND( JUMP(I), SHIFTL(LONG_ONE,J) ) /= LONG_ZER ) then
           s1 = IEOR( s1, s(1)) 
           s2 = IEOR( s2, s(2)) 
           s3 = IEOR( s3, s(3)) 
           s4 = IEOR( s4, s(4)) 
       end if
       CALL GET_RAND_INT64(s, r)
    END DO
    END DO
    !
    s(1) = s1
    s(2) = s2
    s(3) = s3
    s(4) = s4
    !
  END SUBROUTINE
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !	Is NEG Bit Set
  !PURE FUNCTION NEGBIT(X) result(ans)
  !  INTEGER(INT64), intent(in)::
  !  logical:: ans
  !  ans = BTEST(x,63)
  !END FUNCTION
  !	
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !	Random INT64 to XYZ routines
  !	
  PURE FUNCTION FIRST_BYTE_INT16(x) RESULT(y)  ! Since this is used as an index, add 1, result is 1 to 256
    INTEGER(INT64), intent(in) :: x
    INTEGER(INT16):: y
    y = INT( IAND(x, 255_int64), INT16) + 1_INT16
  END FUNCTION
  !	
  PURE FUNCTION RG_TO_256_INT16(x) RESULT(y)  ! Since this is used as an index, add 1, result is 1 to 256
    INTEGER(INT64), intent(in) :: x
    INTEGER(INT16):: y
    y = INT( SHIFTR(x, 56), INT16) + 1_INT16
  END FUNCTION
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  PURE FUNCTION RG_TO_INT32(x) RESULT(y)
    INTEGER(INT64), intent(in) :: x
    INTEGER(INT32):: y
    y = INT( SHIFTR(x, 32), int32)
  END FUNCTION
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  PURE FUNCTION RG_TO_INT31(x) RESULT(y)
    INTEGER(INT64), intent(in) :: x
    INTEGER(INT32):: y
    y = INT( SHIFTR(x, 33), int32)
  END FUNCTION
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  PURE FUNCTION RG_TO_01_REL64(x) RESULT(y) !
    INTEGER(INT64),           intent(in) :: x
    REAL   (REL64) :: y
    !
    y = REAL(SHIFTR(x, 11), rel64) * pow53
    !
    IF(y < DZ ) y = DZ 
    IF(y > UNO) y = UNO
    !
    !y = TRANSFER(                                                 & 
    !              IOR(4602678819172646912_int64, SHIFTR(x, 11)),  & ! Mask = 4602678819172646912 = b'0011111111100000000000000000000000000000000000000000000000000000'
    !            y) - 1._rel64
  END FUNCTION
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  PURE FUNCTION RG_TO_11_REL64(x, sgn) RESULT(y) !
    INTEGER(INT64), intent(in) :: x
    LOGICAL,        intent(in) :: sgn
    REAL   (REL64) :: y
    y = REAL(SHIFTR(x, 11), rel64) * pow53
    !
    if(sgn .AND. BTEST(x, 10)) y = -1.0_rel64 * y
    !
    IF(y < DNEG) y = DNEG 
    IF(y > UNO ) y = UNO
    !   
  END FUNCTION
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  PURE FUNCTION RG_TO_REL64_SIGN(x) RESULT(y) !
    INTEGER(INT64), intent(in) :: x
    REAL   (REL64) :: y
    IF( BTEST(x, 10)) THEN
        y = -1.0_rel64
    ELSE
        y =  1.0_rel64
    END IF
  END FUNCTION
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  !  Note that shft, must be set to zero on first call
  RECURSIVE PURE SUBROUTINE GET_CHAR_LU(rg, shft, r, L, U, CH)
    CLASS(RANDOM_GENERATOR), intent(inout):: rg
    INTEGER,                intent(inout):: shft
    INTEGER(INT64),         intent(inout):: r
    INTEGER(INT8),          intent(inout):: L, U
    CHARACTER,              intent(inout):: CH
    INTEGER(INT8):: I
    !
    if(shft < 9 .or. 57 < shft) then
                                CALL GET_RAND_INT64(rg%seed, r) 
                                shft = 57
    END IF
    !
    I = INT(IAND(SHIFTR(r,shft), 127_int64), int8)
    shft = shft - 7
    !
    if( L <= i .and. i <= U ) then
        CH = TRANSFER(i,CH)
        return
    end if
    !
    CALL GET_CHAR_LU(rg, shft, r, L, U, CH)   ! Tail recursive call
    !
  END SUBROUTINE
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  !  Note that shft, must be set to zero on first call
  PURE SUBROUTINE GET_CHAR_STR(rg, shft, r, lstr, str, CH)
    CLASS(RANDOM_GENERATOR), intent(inout):: rg
    INTEGER,                intent(inout):: shft
    INTEGER(INT64),         intent(inout):: r
    INTEGER,                intent(in   ):: lstr
    CHARACTER(lstr),        intent(in   ):: str
    CHARACTER,              intent(inout):: CH
    INTEGER:: I
    !
    if(shft < 9 .or. 57 < shft) then
                                CALL GET_RAND_INT64(rg%seed, r) 
                                shft = 57
    END IF
    !
    I = MODULO(INT(IAND(SHIFTR(r,shft), 127_int64)), lstr) + 1
    shft = shft - 7
    !
    CH = str(I:I)
    !
  END SUBROUTINE
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  ! Initialization Routines
  !
  IMPURE ELEMENTAL SUBROUTINE INITIALIZE_RG(rg, SEED, ID, NJUMP)
    CLASS(RANDOM_GENERATOR), intent(inout):: rg
    CLASS(*), optional,      intent(in   ):: SEED  ! If specified then defines the seed, if < 1 or not specified, then randomly generates SEED based in wall tine and PID. Set > 0 for repeatability, disables use of ID)
    CLASS(*), optional,      intent(in   ):: ID    ! If returned then makes SEED unique to ID (Disables odd-even random shift)
    CLASS(*), optional,      intent(in   ):: NJUMP ! If defined then specifies the number of calls to SEED_JUMP, which moves forward the random calls by 2^64
    !
    CALL SET_RANDOM_PROPERTIES_RG(rg, SEED, ID, NJUMP)
    !  
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !	
  PURE ELEMENTAL SUBROUTINE RESET_RG(rg)
    CLASS(RANDOM_GENERATOR), intent(inout):: rg
    !
    rg%seed = Default_seed
    !  
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !	
  SUBROUTINE SET_RANDOM_PROPERTIES_RG(rg, SEED, ID, NJUMP)
    CLASS(RANDOM_GENERATOR), intent(inout):: rg
    CLASS(*), optional,      intent(in   ):: SEED  ! If specified then defines the seed, if < 1 or not specified, then randomly generates SEED based in wall tine and PID. Set > 0 for repeatability, disables use of ID)
    CLASS(*), optional,      intent(in   ):: ID    ! If returned then makes SEED unique to ID (Disables odd-even random shift)
    CLASS(*), optional,      intent(in   ):: NJUMP ! If defined then specifies the number of calls to SEED_JUMP, which moves forward the random calls by 2^64
    !
    INTEGER(INT64):: S, TMP
    !
    IF(PRESENT(SEED)) THEN
         
         SELECT TYPE(SEED)
         TYPE IS (INTEGER(INT32)); S = INT(SEED,INT64) 
         TYPE IS (INTEGER(INT64)); S = SEED  
         CLASS DEFAULT
                                   S = Default_seed(2)
         END SELECT
         !
         IF(-2_int64 < S .AND. S < 2_int64) S = Default_seed(2)
    ELSE
        BLOCK
          INTEGER, dimension(8) :: T
          CALL DATE_AND_TIME(VALUES = T)  !GET CURRENT TIME VALUES
          !
          S =     INT(T(1), int64)                                                                                   ! yr 
          S = S + INT(T(2), int64) * INT(b'0000000000000000000000000000000000000000000000000000000000000101', int64) ! mon | bit-> 5
          S = S + INT(T(3), int64) * INT(b'0000000000000000000000000000000000000000000000000000000101110000', int64) ! day | bit-> 368
          S = S + INT(T(5), int64) * INT(b'0000000000000000000000000000000000000000000001101101110000000000', int64) ! hr  | bit-> 449536
          S = S + INT(T(6), int64) * INT(b'0000000000000000000000000000000000011110110100000000000000000000', int64) ! min | bit-> 516947968
          S = S + INT(T(7), int64) * INT(b'0000000000000000000000101101111010000000000000000000000000000000', int64) ! sec | bit-> 3154653478912
          S = S + INT(T(8), int64) * INT(b'0000000000010111011100000000000000000000000000000000000000000000', int64) ! ms  | bit-> 6597069766656000
        END BLOCK
    END IF
    !
    IF(PRESENT(ID)) THEN       ! Add process id shift
         SELECT TYPE(ID)
         TYPE IS (INTEGER(INT32)); S = S + INT(ID,INT64)
         TYPE IS (INTEGER(INT64)); S = S + ID
         END SELECT
    END IF
    !
    CALL Seed_RANDOMIZER(S)
    !
    CALL splitmix64(S, TMP)
    CALL splitmix64(S, TMP)
    CALL splitmix64(S, rg%seed(1))
    !
    CALL splitmix64(S, TMP)
    CALL splitmix64(S, TMP)
    CALL splitmix64(S, rg%seed(2))
    !
    CALL splitmix64(S, TMP)
    CALL splitmix64(S, TMP)
    CALL splitmix64(S, rg%seed(3))
    !
    CALL splitmix64(S, TMP)
    CALL splitmix64(S, TMP)
    CALL splitmix64(S, rg%seed(4))
    !
    IF(PRESENT(NJUMP)) THEN
         SELECT TYPE(NJUMP)
         TYPE IS (INTEGER(INT32)); TMP = INT(NJUMP,INT64)
         TYPE IS (INTEGER(INT64)); TMP = NJUMP
         END SELECT
         DO S=LONG_ONE, TMP
                        CALL SEED_JUMP(rg%seed)
         END DO
    END IF
    !  
    END SUBROUTINE
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  ! Generate Normal and Exponential Distribution Random Variable
  !  Uses the Ziggurat Method as originally described by 
  !    Marsaglia G, Tsang WW. The ziggurat method for generating random variables. Journal of Statistical Software. 2000; 5(8):1–7.
  !    Doornik, JA. An improved ziggurat method to generate normal random samples. University of Oxford; 2005.
  !  With the final algorithm based on
  !    McFarland, C. D. (2016). A modified ziggurat algorithm for generating exponentially and normally distributed pseudorandom numbers. Journal of statistical computation and simulation, 86(7), 1281-1294.
  !  With modifications that involve Fortran specific improvements
  !
  PURE SUBROUTINE GENERATE_NORMAL_0D(rg, rnd, mean, stdev, var)
    CLASS(RANDOM_GENERATOR), intent(inout):: rg
    REAL(REL64),             intent(  out):: rnd
    REAL(REL64),   optional, intent(in   ):: mean, stdev, var
    !
    call generate_normal(rg%seed, rnd)
    !
    IF(present(mean) .or. present(stdev) .or. present(var)) CALL NORMAL_SHIFT(rnd, mean, stdev, var)
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE GENERATE_EXPONENTIAL_0D(rg, rnd, lambda)
    CLASS(RANDOM_GENERATOR), intent(inout):: rg
    REAL(REL64),             intent(  out):: rnd
    REAL(REL64),   optional, intent(in   ):: lambda
    !
    call generate_exponential(rg%seed, rnd)
    !
    IF(present(lambda)) rnd = lambda*(rnd**lambda)
  END SUBROUTINE
  !
  PURE SUBROUTINE GENERATE_NORMAL_1D(rg, rnd, mean, stdev, var)
    CLASS(RANDOM_GENERATOR),   intent(inout):: rg
    REAL(REL64), dimension(:), intent(  out):: rnd
    REAL(REL64),     optional, intent(in   ):: mean, stdev, var
    INTEGER:: i
    !
    DO i=1, SIZE(rnd)
       call generate_normal(rg%seed, rnd(i))
    END DO
    !
    IF(present(mean) .or. present(stdev) .or. present(var)) THEN
       BLOCK 
          REAL(REL64):: mu, sig
          mu  = DZ;    sig = UNO
          !
          if(present(mean)) mu = mean
          if(present(var)) then
                  if(var   > pow53 .and.   var /= uno) sig = sqrt(var)
          end if
          if(present(stdev)) then
                  if(stdev > pow53 .and. stdev /= uno) sig = stdev
          end if 
          rnd = rnd * sig + mu
       END BLOCK
    END IF
    !
  END SUBROUTINE
  !
  PURE SUBROUTINE GENERATE_EXPONENTIAL_1D(rg, rnd, lambda)
    CLASS(RANDOM_GENERATOR),   intent(inout):: rg
    REAL(REL64), dimension(:), intent(  out):: rnd
    REAL(REL64),     optional, intent(in   ):: lambda
    INTEGER:: i
    !
    DO i=1, SIZE(rnd)
       call generate_exponential(rg%seed, rnd(i))
    END DO
    !
    IF(present(lambda)) rnd = lambda*(rnd**lambda)
  END SUBROUTINE
  !
  PURE SUBROUTINE EXP_OVERHANG_SAMPLE(s4, i, rnd) 
    INTEGER(INT64), dimension(4), intent(inout):: s4
    INTEGER(INT16),               intent(in   ):: i
    REAL(REL64),                  intent(inout):: rnd
    INTEGER(INT64):: Ux, Ud
    REAL(REL64):: tmp!, exp_X_diff, exp_X_pow, exp_Y_diff, exp_Y_pow
    ! 
    !exp_X_diff = exp_X(i-1) - exp_X(i)
    !exp_X_pow  = exp_X(i)*p2to63
    !exp_Y_diff = exp_Y(i) - exp_Y(i-1)
    !exp_Y_pow  = exp_Y(i-1)*p2to63
    DO 
       CALL GET_RAND_INT64(s4, Ux) 
       IF( BTEST(Ux, 63) ) Ux = IAND(Ux, b63_MASK)
       !
       CALL GET_RAND_INT64(s4, Ud) 
       IF( BTEST(Ud, 63) ) Ud = IAND(Ud, b63_MASK)
       Ud = Ud - Ux
       !
       IF( Ud < LONG_ZER ) THEN
                           Ux = Ux + Ud
                           Ud = -Ud
       END IF
       !
       rnd = exp_X(i)*p2to63 + (exp_X(i-1) - exp_X(i))*REAL(Ux, rel64) 
       !
       if( Ud >= exp_iE_max ) RETURN ! x < y - epsilon
       !
       tmp = p2to63 - REAL(Ux + Ud, rel64)
       if( exp_Y(i-1)*p2to63 + (exp_Y(i) - exp_Y(i-1))*tmp <= exp(-rnd) ) return
    END DO
    !
  END SUBROUTINE
  !
  RECURSIVE PURE SUBROUTINE GENERATE_EXPONENTIAL(s4, rnd)
    INTEGER(INT64), dimension(4), intent(inout):: s4
    REAL(REL64),                  intent(inout):: rnd
    INTEGER(INT64):: r
    INTEGER(INT16):: ilay   ! Ziggurat layer (1 to 256)
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !
    !     yyyyyyyyyyyyyyyy255   -> x sets y to zero and is the zig ilay,                     => Example uses max possible value
    ! r = 9223372036854776xxx   -> y sets x to zero and is the rel64 uses to scale exp_X_dim => Example uses max possible value
    !
    CALL GET_RAND_INT64(s4, r) 
    ilay = FIRST_BYTE_INT16(r)    ! get last 3 digits of r for Ziggurat layer
    !ilay = RG_TO_256_INT16(r)    ! get first 3 digits of r for Ziggurat layer
    !CALL GET_RAND_INT64(s4, r)   ! Refresh random value
    !
    if (ilay < exp_X_dim) then ! Accepted layer, return corresponding rng
                              rnd = exp_X(ilay) * real( IAND(r, b63_MASK) , rel64 ) ! only use the first 63 bits of r. -> Equivalent to IAND(r, z'7fffffffffffffff')  or IAND(r, b'0111111111111111111111111111111111111111111111111111111111111111') 
                              return 
    end if
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !                        -> Failed to sample from rectangular part of Ziggurat layer,
    ! Alias Sampling                check if sample falls in triangle overhang
    !
    CALL GET_RAND_INT64(s4, r)   ! Refresh random value
    ilay = FIRST_BYTE_INT16(r)
    !ilay = RG_TO_256_INT16(r)    ! get first 3 digits of r for Ziggurat layer
    !CALL GET_RAND_INT64(s4, r)   ! Refresh random value
    !
    IF( r >= exp_ipmf(ilay) ) ilay = exp_map(ilay)  !Note r now needs to be refreshed
    !
    if ( 1_int16 < ilay ) then ! Sample is in triangle overhang of modified Ziggurat
                              call exp_overhang_sample(s4, ilay, rnd) 
                              return 
    end if
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ! Failed to sample from main part of exponential, instead sample for x > 7.569 tail
    !    -> 7.56927469414806_rel64 is the max value 
    !       that can be generated by: exp_X(ilay) * 9223372036854776000   -> Note max INT64  is 9223372036854775807 but when converted to REL64
    !                                                                        it is truncated to 9223372036854776000
    ! Start new exponential generation
    CALL GENERATE_EXPONENTIAL(s4, rnd)
    !
    rnd = rnd + 7.56927469414806_rel64
    !
  END SUBROUTINE
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !
  PURE SUBROUTINE GENERATE_NORMAL(s4, rnd)
    INTEGER(INT64), dimension(4), intent(inout):: s4
    REAL(REL64),                  intent(inout):: rnd
    INTEGER(INT64):: r
    INTEGER(INT16):: ilay   ! Ziggurat layer
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !
    !     yyyyyyyyyyyyyyyy255   -> x sets y to zero and is the zig ilay,                     => Example uses max possible value
    ! r = 9223372036854776xxx   -> y sets x to zero and is the rel64 uses to scale nrm_X_dim => Example uses max possible value
    !
    CALL GET_RAND_INT64(s4, r) 
    ilay = FIRST_BYTE_INT16(r)  ! get last  3 digits of r for Ziggurat layer
    !ilay = RG_TO_256_INT16(r)  ! get first 3 digits of r for Ziggurat layer
    !CALL GET_RAND_INT64(s4, r) ! Refresh random value
    !
    if (ilay < nrm_X_dim) then ! Accepted layer, return corresponding rng
                              rnd = nrm_X(ilay) * real( r, rel64 )        ! r includes the sign bit cause normal has both pos and neg values
                              return 
    end if
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !                        -> Failed to sample from rectangular part of Ziggurat layer,
    ! Alias Sampling                check if sample falls in triangle overhang
    !
    BLOCK
      REAL   (REL64):: tmp
      INTEGER(INT64):: Ux, Ud
      LOGICAL       :: is_neg
      !
      is_neg = BTEST(r, 63)                     ! Get the sign bit, save it for later to multiply by -1
      IF( BTEST(r, 63) ) r = IAND(r, b63_MASK)  ! Only use first 63 bits for Ux
      Ux = r
      !
      CALL GET_RAND_INT64(s4, r)  ! Refresh to r to get new ilay for the Alias Sample
      !
      ilay = FIRST_BYTE_INT16(r)
      !
      IF( r >= nrm_ipmf(ilay) ) ilay = nrm_map(ilay) ! Alias sample must update ilay
      !
      ! Four kinds of overhangs: 
      !  j = 0                   -> Sample from tail
      !  0 < j < nrm_inflection  -> Overhang is concave; only sample from Lower-Left triangle
      !  j = nrm_inflection      -> Must sample from entire overhang rectangle
      !  j > nrm_inflection      -> Overhangs are convex; implicitly accept point in Lower-Left triangle
      !
      if ( ilay > nrm_inflection ) then     ! Convex overhang
           !
           rnd = nrm_X(ilay)*p2to63 + (nrm_X(ilay-1) - nrm_X(ilay))*REAL(Ux, rel64) 
           DO
             CALL GET_RAND_INT64(s4, Ud) 
             IF( BTEST(Ud, 63) ) Ud = IAND(Ud, b63_MASK)  !Only use first 63 bits
             Ud = Ud - Ux
             !
             if( Ud > nrm_iE_min ) EXIT
             if( Ud < nrm_iE_max ) CYCLE
             !
             tmp = p2to63 - REAL(Ux + Ud, rel64)
             tmp = nrm_Y(ilay-1)*p2to63 + (nrm_Y(ilay) - nrm_Y(ilay-1))*tmp
             if ( tmp < exp(nHALF*rnd*rnd) ) EXIT
             !
             CALL GET_RAND_INT64(s4, Ux) 
             IF( BTEST(Ux, 63) ) Ux = IAND(Ux, b63_MASK)  !Only use first 63 bits
             !
             rnd = nrm_X(ilay)*p2to63 + (nrm_X(ilay-1) - nrm_X(ilay))*REAL(Ux, rel64) 
           END DO
           !
      elseif( ilay == 1_int16 ) then      ! Sampling from Tail
         ! Sample for x > 3.636 tail
         !    -> X0    = 3.63600662550095 is the max value; 
         !       X0inv = 0.27502700159745 = 1/X0
         !       that can be generated by: nrm_X(1) * 9223372036854776000   -> Note max INT64  is 9223372036854775807 but when converted to REL64
         !
         !!!do
         !!!  CALL GET_RAND_INT64(s4, r) 
         !!!  rnd = RG_TO_01_REL64(r)
         !!!  rnd = -0.275027001597453_rel64 * log(rnd)
         !!!  !
         !!!  CALL GET_RAND_INT64(s4, r) 
         !!!  tmp = RG_TO_01_REL64(r)
         !!!  tmp = -2.0_rel64 * log(tmp)
         !!!  if(tmp > rnd*rnd) EXIT
         !!!end do
         do
           CALL GENERATE_EXPONENTIAL(s4, rnd) 
           rnd = rnd * 0.275027001597453_rel64
           CALL GENERATE_EXPONENTIAL(s4, tmp) 
           tmp = tmp * 2.0_rel64
           if(tmp > rnd*rnd) EXIT
         end do
         rnd = rnd + 3.63600662550095_rel64
         !
      elseif(ilay < nrm_inflection) then   ! Concave overhang
           !
           DO 
              CALL GET_RAND_INT64(s4, Ud) 
              IF( BTEST(Ud, 63) ) Ud = IAND(Ud, b63_MASK)
              Ud = Ud - Ux
              !
              IF( BTEST(Ud, 63) ) THEN           ! Ud < LONG_ZER
                                  Ux = Ux + Ud
                                  Ud = -Ud
              END IF
              !
              rnd = nrm_X(ilay)*p2to63 + (nrm_X(ilay-1) - nrm_X(ilay))*REAL(Ux, rel64) 
              !
              if( Ud > nrm_iE_min ) EXIT ! x < y - epsilon
              !
              tmp = p2to63 - REAL(Ux + Ud, rel64)
              tmp = nrm_Y(ilay-1)*p2to63 + (nrm_Y(ilay) - nrm_Y(ilay-1))*tmp
              if ( tmp < exp(nHALF*rnd*rnd) ) EXIT
              !
              CALL GET_RAND_INT64(s4, Ux) 
              IF( BTEST(Ux, 63) ) Ux = IAND(Ux, b63_MASK)  !Only use first 63 bits
           END DO
      else     !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~> At Inflection point
           DO
              rnd = nrm_X(ilay)*p2to63 + (nrm_X(ilay-1) - nrm_X(ilay))*REAL(Ux, rel64) 
              !
              CALL GET_RAND_INT64(s4, Ud) 
              IF( BTEST(Ud, 63) ) Ud = IAND(Ud, b63_MASK)
              !
              tmp = nrm_Y(ilay-1)*p2to63 + (nrm_Y(ilay) - nrm_Y(ilay-1))*REAL(Ud, rel64)
              if ( tmp < exp(nHALF*rnd*rnd) ) EXIT
              !
              CALL GET_RAND_INT64(s4, Ux) 
              IF( BTEST(Ux, 63) ) Ux = IAND(Ux, b63_MASK)  !Only use first 63 bits
           END DO
      end if
      !
      IF(is_neg) rnd = -rnd
      !
    END BLOCK
    !  
  END SUBROUTINE
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !
  !PURE SUBROUTINE GENERATE_NORMAL(s4, rnd)
  !  INTEGER(INT64), dimension(4), intent(inout):: s4
  !  REAL(REL64),                  intent(inout):: rnd
  !  INTEGER(INT64):: r
  !  INTEGER(INT16):: ilay   ! Ziggurat layer
  !  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !  !
  !  !     yyyyyyyyyyyyyyyy255   -> x sets y to zero and is the zig ilay,                     => Example uses max possible value
  !  ! r = 9223372036854776xxx   -> y sets x to zero and is the rel64 uses to scale nrm_X_dim => Example uses max possible value
  !  !
  !  CALL GET_RAND_INT64(s4, r) 
  !  ilay = FIRST_BYTE_INT16(r)  ! get last 3 digits of r for Ziggurat layer
  !  !
  !  if (ilay < nrm_X_dim) then ! Accepted layer, return corresponding rng
  !                            rnd = nrm_X(ilay) * real( r , rel64 )        ! r includes the sign bit cause normal has both pos and neg values
  !                            return 
  !  end if
  !  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !  !                        -> Failed to sample from rectangular part of Ziggurat layer,
  !  ! Alias Sampling                check if sample falls in triangle overhang
  !  !
  !  BLOCK
  !    REAL   (REL64):: tmp
  !    INTEGER(INT64):: Ux, Ud
  !    LOGICAL       :: is_neg
  !    !
  !    is_neg = .NOT. BTEST(r, 8)                ! Get the sign of the RN from the 9th bit (note that the first 8 bits are never used to generate rnd due to INT64 to DOUBLE truncation)
  !    IF( BTEST(r, 63) ) r = IAND(r, b63_MASK)  ! Only use first 63 bits for Ux
  !    Ux = r
  !    !
  !    CALL GET_RAND_INT64(s4, r)  ! Refresh to r to get new ilay for the Alias Sample
  !    !
  !    ilay = FIRST_BYTE_INT16(r)
  !    !
  !    IF( r >= nrm_ipmf(ilay) ) ilay = nrm_map(ilay) ! Alias sample must update ilay
  !    !
  !    ! Four kinds of overhangs: 
  !    !  j = 0                   -> Sample from tail
  !    !  0 < j < nrm_inflection  -> Overhang is concave; only sample from Lower-Left triangle
  !    !  j = nrm_inflection      -> Must sample from entire overhang rectangle
  !    !  j > nrm_inflection      -> Overhangs are convex; implicitly accept point in Lower-Left triangle
  !    !
  !    if ( ilay > nrm_inflection ) then     ! Convex overhang
  !         !
  !         rnd = nrm_X(ilay)*p2to63 + (nrm_X(ilay-1) - nrm_X(ilay))*REAL(Ux, rel64) 
  !         DO
  !           CALL GET_RAND_INT64(s4, Ud) 
  !           IF( BTEST(Ud, 63) ) Ud = IAND(Ud, b63_MASK)  !Only use first 63 bits
  !           Ud = Ud - Ux
  !           !
  !           if( Ud > nrm_iE_min ) EXIT
  !           if( Ud < nrm_iE_max ) CYCLE
  !           !
  !           tmp = p2to63 - REAL(Ux + Ud, rel64)
  !           tmp = nrm_Y(ilay-1)*p2to63 + (nrm_Y(ilay) - nrm_Y(ilay-1))*tmp
  !           if ( tmp < exp(nHALF*rnd*rnd) ) EXIT
  !           !
  !           CALL GET_RAND_INT64(s4, Ux) 
  !           IF( BTEST(Ux, 63) ) Ux = IAND(Ux, b63_MASK)  !Only use first 63 bits
  !           !
  !           rnd = nrm_X(ilay)*p2to63 + (nrm_X(ilay-1) - nrm_X(ilay))*REAL(Ux, rel64) 
  !         END DO
  !         !
  !    elseif( ilay == 1_int16 ) then      ! Sampling from Tail
  !       ! Sample for x > 3.636 tail
  !       !    -> X0    = 3.63600662550095 is the max value; 
  !       !       X0inv = 0.27502700159745 = 1/X0
  !       !       that can be generated by: nrm_X(1) * 9223372036854776000   -> Note max INT64  is 9223372036854775807 but when converted to REL64
  !       !
  !       do
  !         CALL GENERATE_EXPONENTIAL(s4, rnd) 
  !         rnd = rnd * 0.275027001597453_rel64
  !         CALL GENERATE_EXPONENTIAL(s4, tmp) 
  !         if(tmp < 0.5_rel64*rnd*rnd) EXIT
  !       end do
  !       rnd = rnd + 3.63600662550095_rel64
  !       !
  !    elseif(ilay < nrm_inflection) then   ! Concave overhang
  !         !
  !         DO 
  !            CALL GET_RAND_INT64(s4, Ud) 
  !            IF( BTEST(Ud, 63) ) Ud = IAND(Ud, b63_MASK)
  !            Ud = Ud - Ux
  !            !
  !            IF( BTEST(Ud, 63) ) THEN           ! Ud < LONG_ZER
  !                                Ux = Ux + Ud
  !                                Ud = -Ud
  !            END IF
  !            !
  !            rnd = nrm_X(ilay)*p2to63 + (nrm_X(ilay-1) - nrm_X(ilay))*REAL(Ux, rel64) 
  !            !
  !            if( Ud > nrm_iE_min ) EXIT ! x < y - epsilon
  !            !
  !            tmp = p2to63 - REAL(Ux + Ud, rel64)
  !            tmp = nrm_Y(ilay-1)*p2to63 + (nrm_Y(ilay) - nrm_Y(ilay-1))*tmp
  !            if ( tmp < exp(nHALF*rnd*rnd) ) EXIT
  !            !
  !            CALL GET_RAND_INT64(s4, Ux) 
  !            IF( BTEST(Ux, 63) ) Ux = IAND(Ux, b63_MASK)  !Only use first 63 bits
  !         END DO
  !    else     !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~> At Inflection point
  !         DO
  !            rnd = nrm_X(ilay)*p2to63 + (nrm_X(ilay-1) - nrm_X(ilay))*REAL(Ux, rel64) 
  !            !
  !            CALL GET_RAND_INT64(s4, Ud) 
  !            IF( BTEST(Ud, 63) ) Ud = IAND(Ud, b63_MASK)
  !            !
  !            tmp = nrm_Y(ilay-1)*p2to63 + (nrm_Y(ilay) - nrm_Y(ilay-1))*REAL(Ud, rel64)
  !            if ( tmp < exp(nHALF*rnd*rnd) ) EXIT
  !            !
  !            CALL GET_RAND_INT64(s4, Ux) 
  !            IF( BTEST(Ux, 63) ) Ux = IAND(Ux, b63_MASK)  !Only use first 63 bits
  !         END DO
  !    end if
  !    !
  !    IF(is_neg) rnd = -rnd
  !    !
  !  END BLOCK
  !  !  
  !END SUBROUTINE
  !
  PURE SUBROUTINE NORMAL_SHIFT(rnd, mean, stdev, var)
    REAL(REL64),             intent(inout):: rnd
    REAL(REL64),   optional, intent(in   ):: mean, stdev, var
    REAL(REL64):: mu, sig
    !
    mu  = DZ;    sig = UNO
    !
    if(present(mean)) mu = mean
    if(present(var)) then
            if(var   > pow53 .and.   var /= uno) sig = sqrt(var)
    end if
    if(present(stdev)) then
            if(stdev > pow53 .and. stdev /= uno) sig = stdev
    end if
    !
    rnd = rnd * sig + mu
  END SUBROUTINE
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  ! Generate Random Variable
  !
  PURE SUBROUTINE GENERATE_RANDOM_INT32_0D_LU(rg, rnd, LOWER, UPPER)
    CLASS(RANDOM_GENERATOR), intent(inout):: RG
    INTEGER(INT32),          intent(  out):: rnd
    INTEGER(INT32),          intent(in   ):: LOWER, UPPER
    INTEGER(INT64):: r, tmp
    !
    IF( UPPER <= LOWER) THEN
                        rnd = LOWER
                        RETURN
    END IF
    !
    CALL GET_RAND_INT64(rg%seed, r) 
    r = SHIFTR(r, 31)
    !
    tmp = int(UPPER - LOWER + ONE, int64)  ! Range of integers
    tmp = MODULO( r, tmp )
    !
    rnd = int(tmp, int32) + LOWER
    !
    !!!CALL GET_RAND_INT64(rg%seed, r) 
    !!!!
    !!!rnd = LOWER + INT(                                                      &
    !!!                   RG_TO_01_REL64(r) * REAL(UPPER - LOWER + ONE, REL64), &
    !!!             int32)
    !!!!
    !!!IF(rnd > UPPER) rnd = UPPER  ! Only true if RG_TO_01_REL64(r) returns exactly 1.0, which has a near zero change
    !  
  END SUBROUTINE
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  PURE SUBROUTINE GENERATE_RANDOM_INT32_0D_RG(rg, rnd)
    CLASS(RANDOM_GENERATOR), intent(inout):: RG
    INTEGER(INT32),            intent(  out):: rnd
    INTEGER(INT64):: r
    !
    CALL GET_RAND_INT64(rg%seed, r) 
    !
    rnd = RG_TO_INT31(r)
    !  
  END SUBROUTINE
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  FUNCTION GENERATE_RANDOM_INT32_0D_LU_FN(rg, LOWER, UPPER) RESULT(rnd)
    CLASS(RANDOM_GENERATOR),  intent(inout):: RG
    INTEGER(INT32),           intent(in   ):: LOWER, UPPER
    INTEGER(INT32):: rnd
    !
    CALL GENERATE_RANDOM_INT32_0D_LU(rg, rnd, LOWER, UPPER)
    !  
  END FUNCTION
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  FUNCTION GENERATE_RANDOM_INT32_0D_RG_FN(rg) RESULT(rnd)
    CLASS(RANDOM_GENERATOR),  intent(inout):: RG
    INTEGER(INT32):: rnd
    !
    CALL GENERATE_RANDOM_INT32_0D_RG(rg, rnd)
    !  
  END FUNCTION
  !
  !#############################################################################################################################################################
  !
  PURE SUBROUTINE GENERATE_RANDOM_INT64_0D_LU(rg, rnd, LOWER, UPPER)
    CLASS(RANDOM_GENERATOR), intent(inout):: RG
    INTEGER(INT64),          intent(  out):: rnd
    INTEGER(INT64),          intent(in   ):: LOWER, UPPER
    INTEGER(INT64):: r
    !
    IF( UPPER <= LOWER) THEN
                        rnd = LOWER
                        RETURN
    END IF
    !
    CALL GET_RAND_INT64(rg%seed, r) 
    !
    rnd = UPPER - LOWER + LONG_ONE  ! Range of integers
    !
    rnd = MODULO( r, rnd ) + LOWER
    !  
  END SUBROUTINE
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  PURE SUBROUTINE GENERATE_RANDOM_INT64_0D_LU32(rg, rnd, LOWER, UPPER)
    CLASS(RANDOM_GENERATOR), intent(inout):: RG
    INTEGER(INT64),          intent(  out):: rnd
    INTEGER(INT32),          intent(in   ):: LOWER, UPPER
    INTEGER(INT64):: L, U
    !
    L = int(LOWER, int64)
    U = int(UPPER, int64)
    CALL GENERATE_RANDOM_INT64_0D_LU(rg, rnd, L, U)
    !  
  END SUBROUTINE
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  PURE SUBROUTINE GENERATE_RANDOM_INT64_0D_RG(rg, rnd)
    CLASS(RANDOM_GENERATOR), intent(inout):: RG
    INTEGER(INT64),          intent(  out):: rnd
    !
    CALL GET_RAND_INT64(rg%seed, rnd) 
    !  
  END SUBROUTINE
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  FUNCTION GENERATE_RANDOM_INT64_0D_LU_FN(rg, LOWER, UPPER) RESULT(rnd)
    CLASS(RANDOM_GENERATOR), intent(inout):: RG
    INTEGER(INT64),          intent(in   ):: LOWER, UPPER
    INTEGER(INT64):: rnd
    !
    CALL GENERATE_RANDOM_INT64_0D_LU(rg, rnd, LOWER, UPPER)
    !  
  END FUNCTION
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  FUNCTION GENERATE_RANDOM_INT64_0D_LU32_FN(rg, LOWER, UPPER) RESULT(rnd)
    CLASS(RANDOM_GENERATOR), intent(inout):: RG
    INTEGER(INT32),          intent(in   ):: LOWER, UPPER
    INTEGER(INT64):: rnd
    INTEGER(INT64):: L, U
    !
    L = int(LOWER, int64)
    U = int(UPPER, int64)
    CALL GENERATE_RANDOM_INT64_0D_LU(rg, rnd, L, U)
    !  
  END FUNCTION
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  FUNCTION GENERATE_RANDOM_INT64_0D_RG_FN(rg) RESULT(rnd)
    CLASS(RANDOM_GENERATOR),  intent(inout):: RG
    INTEGER(INT64):: rnd
    !
    CALL GENERATE_RANDOM_INT64_0D_RG(rg, rnd)
    !  
  END FUNCTION
  !
  !#############################################################################################################################################################
  !	
  PURE SUBROUTINE GENERATE_RANDOM_REL64_0D_LU(rg, rnd, LOWER, UPPER)
    CLASS(RANDOM_GENERATOR), intent(inout):: RG
    REAL(REL64),             intent(  out):: rnd
    REAL(REL64),             intent(in   ):: LOWER, UPPER
    INTEGER(INT64):: r
    IF( UPPER <= LOWER) THEN
                        rnd = LOWER
                        RETURN
    END IF
    !
    CALL GET_RAND_INT64(rg%seed, r) 
    !
    rnd = RG_TO_01_REL64(r) * (UPPER - LOWER) + LOWER
    !
  END SUBROUTINE
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  ! 
  PURE SUBROUTINE GENERATE_RANDOM_REL64_0D_LU32(rg, rnd, LOWER, UPPER)
    CLASS(RANDOM_GENERATOR), intent(inout):: RG
    REAL(REL64),             intent(  out):: rnd
    REAL(REL32),             intent(in   ):: LOWER, UPPER
    REAL(REL64):: L, U
    !
    L = real(LOWER, rel64)
    U = real(UPPER, rel64)
    CALL GENERATE_RANDOM_REL64_0D_LU(rg, rnd, L, U)
    !
  END SUBROUTINE
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  ! 
  PURE SUBROUTINE GENERATE_RANDOM_REL64_0D_LUI32(rg, rnd, LOWER, UPPER)
    CLASS(RANDOM_GENERATOR), intent(inout):: RG
    REAL(REL64),             intent(  out):: rnd
    INTEGER(INT32),          intent(in   ):: LOWER, UPPER
    REAL(REL64):: L, U
    !
    L = real(LOWER, rel64)
    U = real(UPPER, rel64)
    CALL GENERATE_RANDOM_REL64_0D_LU(rg, rnd, L, U)
    !
  END SUBROUTINE
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  ! 
  PURE SUBROUTINE GENERATE_RANDOM_REL64_0D_RG(rg, rnd)
    CLASS(RANDOM_GENERATOR), intent(inout):: RG
    REAL(REL64),             intent(  out):: rnd
    INTEGER(INT64):: r
    !
    CALL GET_RAND_INT64(rg%seed, r) 
    !
    rnd = RG_TO_01_REL64(r)
    !  
  END SUBROUTINE
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  PURE SUBROUTINE GENERATE_RANDOM_REL64_0D_SN(rg, rnd, sign)
    CLASS(RANDOM_GENERATOR), intent(inout):: RG
    REAL(REL64),             intent(  out):: rnd
    LOGICAL,                 intent(in   ):: sign
    INTEGER(INT64):: r
    !
    CALL GET_RAND_INT64(rg%seed, r) 
    !
    IF(sign) THEN
             rnd = RG_TO_11_REL64(r, sign)
    ELSE
             rnd = RG_TO_01_REL64(r)
    END IF
    !  
  END SUBROUTINE
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  FUNCTION GENERATE_RANDOM_REL64_0D_LU_FN(rg, LOWER, UPPER) RESULT(rnd)
    CLASS(RANDOM_GENERATOR), intent(inout):: RG
    REAL(REL64),             intent(in   ):: LOWER, UPPER
    REAL(REL64):: rnd
    !
    CALL GENERATE_RANDOM_REL64_0D_LU(rg, rnd, LOWER, UPPER)
    !
  END FUNCTION
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  FUNCTION GENERATE_RANDOM_REL64_0D_LU32_FN(rg, LOWER, UPPER) RESULT(rnd)
    CLASS(RANDOM_GENERATOR), intent(inout):: RG
    REAL(REL32),             intent(in   ):: LOWER, UPPER
    REAL(REL64):: rnd
    REAL(REL64):: L, U
    !
    L = real(LOWER, rel64)
    U = real(UPPER, rel64)
    !
    CALL GENERATE_RANDOM_REL64_0D_LU(rg, rnd, L, U)
    !
  END FUNCTION
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  FUNCTION GENERATE_RANDOM_REL64_0D_LUI32_FN(rg, LOWER, UPPER) RESULT(rnd)
    CLASS(RANDOM_GENERATOR), intent(inout):: RG
    INTEGER(INT32),          intent(in   ):: LOWER, UPPER
    REAL(REL64):: rnd
    REAL(REL64):: L, U
    !
    L = real(LOWER, rel64)
    U = real(UPPER, rel64)
    !
    CALL GENERATE_RANDOM_REL64_0D_LU(rg, rnd, L, U)
    !
  END FUNCTION
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  FUNCTION GENERATE_RANDOM_REL64_0D_RG_FN(rg) RESULT(rnd)
    CLASS(RANDOM_GENERATOR), intent(inout):: RG
    REAL(REL64):: rnd
    !
    CALL GENERATE_RANDOM_REL64_0D_RG(rg, rnd)
    !
  END FUNCTION
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  FUNCTION GENERATE_RANDOM_REL64_0D_SN_FN(rg, sign) RESULT(rnd)
    CLASS(RANDOM_GENERATOR), intent(inout):: RG
    LOGICAL,                 intent(in   ):: sign
    REAL(REL64):: rnd
    !
    CALL GENERATE_RANDOM_REL64_0D_SN(rg, rnd, sign)
    !  
  END FUNCTION
  !
  !#############################################################################################################################################################
  !	
  PURE SUBROUTINE GENERATE_RANDOM_REL32_0D_LU(rg, rnd, LOWER, UPPER)
    CLASS(RANDOM_GENERATOR), intent(inout):: RG
    REAL(REL32),             intent(  out):: rnd
    REAL(REL32),             intent(in   ):: LOWER, UPPER
    INTEGER(INT64):: r
    IF( UPPER <= LOWER) THEN
                        rnd = LOWER
                        RETURN
    END IF
    !
    CALL GET_RAND_INT64(rg%seed, r) 
    !
    rnd = REAL(RG_TO_01_REL64(r), rel32) * (UPPER - LOWER) + LOWER
    !
  END SUBROUTINE
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  ! 
  PURE SUBROUTINE GENERATE_RANDOM_REL32_0D_RG(rg, rnd)
    CLASS(RANDOM_GENERATOR), intent(inout):: RG
    REAL(REL32),             intent(  out):: rnd
    INTEGER(INT64):: r
    !
    CALL GET_RAND_INT64(rg%seed, r) 
    !
    rnd = REAL(RG_TO_01_REL64(r), rel32)
    !  
  END SUBROUTINE
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  PURE SUBROUTINE GENERATE_RANDOM_REL32_0D_SN(rg, rnd, sign)
    CLASS(RANDOM_GENERATOR), intent(inout):: RG
    REAL(REL32),             intent(  out):: rnd
    LOGICAL,                 intent(in   ):: sign
    INTEGER(INT64):: r
    !
    CALL GET_RAND_INT64(rg%seed, r) 
    !
    IF(sign) THEN
             rnd = REAL(RG_TO_11_REL64(r, sign), rel32)
    ELSE
             rnd = REAL(RG_TO_01_REL64(r), rel32)
    END IF
    !  
  END SUBROUTINE
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  FUNCTION GENERATE_RANDOM_REL32_0D_LU_FN(rg, LOWER, UPPER) RESULT(rnd)
    CLASS(RANDOM_GENERATOR), intent(inout):: RG
    REAL(REL32),             intent(in   ):: LOWER, UPPER
    REAL(REL32):: rnd
    !
    CALL GENERATE_RANDOM_REL32_0D_LU(rg, rnd, LOWER, UPPER)
    !
  END FUNCTION
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  FUNCTION GENERATE_RANDOM_REL32_0D_RG_FN(rg) RESULT(rnd)
    CLASS(RANDOM_GENERATOR), intent(inout):: RG
    REAL(REL32):: rnd
    !
    CALL GENERATE_RANDOM_REL32_0D_RG(rg, rnd)
    !
  END FUNCTION
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  FUNCTION GENERATE_RANDOM_REL32_0D_SN_FN(rg, sign) RESULT(rnd)
    CLASS(RANDOM_GENERATOR), intent(inout):: RG
    LOGICAL,                 intent(in   ):: sign
    REAL(REL32):: rnd
    !
    CALL GENERATE_RANDOM_REL32_0D_SN(rg, rnd, sign)
    !  
  END FUNCTION
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  ! 
  PURE SUBROUTINE GENERATE_RANDOM_REL32_0D_LUI32(rg, rnd, LOWER, UPPER)
    CLASS(RANDOM_GENERATOR), intent(inout):: RG
    REAL(REL32),             intent(  out):: rnd
    INTEGER(INT32),          intent(in   ):: LOWER, UPPER
    REAL(REL32):: L, U
    !
    L = real(LOWER, rel32)
    U = real(UPPER, rel32)
    CALL GENERATE_RANDOM_REL32_0D_LU(rg, rnd, L, U)
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  PURE SUBROUTINE GENERATE_RANDOM_CHAR_0D_RG(rg, rnd)
    CLASS(RANDOM_GENERATOR), intent(inout) :: rg
    CHARACTER(*),            intent(  out) :: rnd
    INTEGER:: ldim, shft, i
    INTEGER(INT64):: r
    INTEGER(INT8):: L, U
    !
    L = 33_int8 
    U = 126_int8
    !
    ldim = len(rnd)
    shft = 0
    do i=1, ldim
       CALL GET_CHAR_LU(rg, shft, r, L, U, rnd(i:i))
    end do
    !
  END SUBROUTINE
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  PURE SUBROUTINE GENERATE_RANDOM_CHAR_0D_CH(rg, rnd, str)
    CLASS(RANDOM_GENERATOR), intent(inout) :: rg
    CHARACTER(*),            intent(  out) :: rnd
    CHARACTER(*),            intent(in   ) :: str ! Max size of 127
    INTEGER:: ldim, shft, i, lstr
    INTEGER(INT64):: r
    !
    lstr = len(str)
    !
    ldim = len(rnd)
    shft = 0
    do i=1, ldim
       CALL GET_CHAR_STR(rg, shft, r, lstr, str, rnd(i:i))
    end do
    !
  END SUBROUTINE
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  PURE SUBROUTINE GENERATE_RANDOM_CHAR_0D_LU(rg, rnd, LOWER, UPPER)
    CLASS(RANDOM_GENERATOR), intent(inout) :: rg
    CHARACTER(*),            intent(  out) :: rnd
    CHARACTER,               intent(in   ) :: LOWER, UPPER
    INTEGER:: ldim, shft, i
    INTEGER(INT64):: r
    INTEGER(INT8):: L, U
    !
    L = TRANSFER(LOWER, L)
    U = TRANSFER(UPPER, U)
    if( L > U ) then
                block
                    INTEGER(INT8):: t
                    t = L
                    L = U
                    U = t
                end block
    end if
    if( L<33_int8  ) L = 33_int8
    if( U>126_int8 ) U = 126_int8
    !
    ldim = len(rnd)
    shft = 0
    do i=1, ldim
       CALL GET_CHAR_LU(rg, shft, r, L, U, rnd(i:i))
    end do
    !
  END SUBROUTINE
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  FUNCTION GENERATE_RANDOM_CHAR_FN(rg, lower_case) RESULT(rnd)
    CLASS(RANDOM_GENERATOR),  intent(inout):: rg
    LOGICAL,        optional, intent(in   ):: lower_case
    CHARACTER:: rnd
    
    if(present(lower_case)) then
           if( lower_case ) then
                            call GENERATE_RANDOM_CHAR_0D_LU(rg, rnd, 'a', 'z')
                            return
           end if
    end if
    !
    call GENERATE_RANDOM_CHAR_0D_LU(rg, rnd, 'A', 'Z')
    !  
  END FUNCTION
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  FUNCTION GENERATE_RANDOM_CHAR_SIZ_FN(rg, siz, lower_case) RESULT(rnd)
    CLASS(RANDOM_GENERATOR),  intent(inout):: rg
    integer,                  intent(in   ):: siz
    LOGICAL,        optional, intent(in   ):: lower_case
    CHARACTER(siz):: rnd
    
    if(present(lower_case)) then
           if( lower_case ) then
                            call GENERATE_RANDOM_CHAR_0D_LU(rg, rnd, 'a', 'z')
                            return
           end if
    end if
    !
    call GENERATE_RANDOM_CHAR_0D_LU(rg, rnd, 'A', 'Z')
    !  
  END FUNCTION
  !
  !#############################################################################################################################################################
  !	
  FUNCTION GENERATE_RANDOM_COIN_FN(rg)  RESULT(COIN)
    CLASS(RANDOM_GENERATOR), intent(inout):: rg
    LOGICAL:: COIN
    INTEGER(INT64):: r
    !
    CALL GET_RAND_INT64(rg%seed, r) 
    !
    COIN = BTEST(r, 60)
    !
  END FUNCTION
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  FUNCTION GENERATE_RANDOM_BINARY_FN(rg)  RESULT(BIN)
    CLASS(RANDOM_GENERATOR), intent(inout):: rg
    INTEGER(INT32):: BIN
    INTEGER(INT64):: r
    !
    CALL GET_RAND_INT64(rg%seed, r) 
    !
    IF( BTEST(r, 60) ) THEN
          BIN = ONE
    ELSE
          BIN = Z
    END IF
    !
  END FUNCTION
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  ! Generate Random Values for 1D Arrays
  !	
  PURE SUBROUTINE GENERATE_RANDOM_INT32_1D_LU(rg, A, LOWER, UPPER)
    CLASS(RANDOM_GENERATOR),                  intent(inout):: rg
    INTEGER(INT32), dimension(:), contiguous, intent(inout):: A
    INTEGER(INT32),                           intent(in   ):: LOWER, UPPER
    INTEGER(INT64):: r, range
    INTEGER:: i 
    IF( UPPER <= LOWER) THEN
                        A = LOWER
                        RETURN
    END IF
    !
    range = UPPER - LOWER + ONE
    !
    DO i=1, SIZE(A)
            CALL GET_RAND_INT64(rg%seed, r)
            r = SHIFTR(r, 31)
            !
            A(i) = int(MODULO( r, range ), int32) + LOWER
    END DO
    !!!REAL(REL64):: drand, range
    !!!INTEGER:: i 
    !!!IF( UPPER <= LOWER) THEN
    !!!                    A = LOWER
    !!!                    RETURN
    !!!END IF
    !!!!
    !!!range = REAL(UPPER - LOWER + ONE, REL64)
    !!!!
    !!!DO i=1, SIZE(A)
    !!!        CALL GET_RAND_INT64(rg%seed, r)
    !!!        !
    !!!        drand = RG_TO_01_REL64(r) * range
    !!!        !
    !!!        A(i) = LOWER + NINT( drand, INT32)
    !!!END DO
    !!!!
    !!!DO CONCURRENT(i=1:SIZE(A), A(i) > UPPER)
    !!!                           A(i) = UPPER  ! Only true if RG_TO_01_REL64(r) returns exactly 1.0, which has a near zero change
    !!!END DO
    !  
  END SUBROUTINE
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  PURE SUBROUTINE GENERATE_RANDOM_INT32_1D_RG(rg, A)
    CLASS(RANDOM_GENERATOR),                  intent(inout):: rg
    INTEGER(INT32), dimension(:), contiguous, intent(inout):: A
    INTEGER(INT64):: r
    INTEGER:: i 
    !
    DO i=1, SIZE(A)
            CALL GET_RAND_INT64(rg%seed, r)
            !
            A(i) = RG_TO_INT31(r)
    END DO
    !  
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !	
  PURE SUBROUTINE GENERATE_RANDOM_INT64_1D_LU(rg, A, LOWER, UPPER)
    CLASS(RANDOM_GENERATOR),                  intent(inout):: rg
    INTEGER(INT64), dimension(:), contiguous, intent(inout):: A
    INTEGER(INT64),                           intent(in   ):: LOWER, UPPER
    INTEGER(INT64):: r, range
    INTEGER:: i 
    IF( UPPER <= LOWER) THEN
                        A = LOWER
                        RETURN
    END IF
    !
    range = UPPER - LOWER + ONE
    !
    DO i=1, SIZE(A)
            CALL GET_RAND_INT64(rg%seed, r)
            !
            A(i) = MODULO( r, range ) + LOWER
    END DO
    !  
  END SUBROUTINE
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  PURE SUBROUTINE GENERATE_RANDOM_INT64_1D_LU32(rg, A, LOWER, UPPER)
    CLASS(RANDOM_GENERATOR),                  intent(inout):: rg
    INTEGER(INT64), dimension(:), contiguous, intent(inout):: A
    INTEGER(INT32),                           intent(in   ):: LOWER, UPPER
    INTEGER(INT64):: L, U
    !
    L = int(LOWER, int64)
    U = int(UPPER, int64)
    CALL GENERATE_RANDOM_INT64_1D_LU(rg, A, L, U)
    !  
  END SUBROUTINE
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  PURE SUBROUTINE GENERATE_RANDOM_INT64_1D_RG(rg, A)
    CLASS(RANDOM_GENERATOR),                  intent(inout):: rg
    INTEGER(INT64), dimension(:), contiguous, intent(inout):: A
    INTEGER:: i 
    !
    DO i=1, SIZE(A)
            CALL GET_RAND_INT64(rg%seed, A(i))
    END DO
    !  
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !	
  PURE SUBROUTINE GENERATE_RANDOM_REL64_1D_LU(rg, A, LOWER, UPPER)
    CLASS(RANDOM_GENERATOR),               intent(inout):: rg
    REAL(REL64), dimension(:), contiguous, intent(inout):: A
    REAL(REL64),                           intent(in   ):: LOWER, UPPER
    REAL(REL64):: range
    INTEGER(INT64):: r
    INTEGER:: i 
    IF( UPPER <= LOWER) THEN
                        A = LOWER
                        RETURN
    END IF
    !
    range = UPPER - LOWER
    !
    DO i=1, SIZE(A)
            CALL GET_RAND_INT64(rg%seed, r)
            !
            A(i) = RG_TO_01_REL64(r) * range + LOWER
    END DO
    !  
  END SUBROUTINE
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  PURE SUBROUTINE GENERATE_RANDOM_REL64_1D_LU32(rg, A, LOWER, UPPER)
    CLASS(RANDOM_GENERATOR),               intent(inout):: rg
    REAL(REL64), dimension(:), contiguous, intent(inout):: A
    REAL(REL32),                           intent(in   ):: LOWER, UPPER
    REAL(REL64):: L, U
    !
    L = real(LOWER, rel64)
    U = real(UPPER, rel64)
    !
    CALL GENERATE_RANDOM_REL64_1D_LU(rg, A, L, U)
    !  
  END SUBROUTINE
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  PURE SUBROUTINE GENERATE_RANDOM_REL64_1D_LUI32(rg, A, LOWER, UPPER)
    CLASS(RANDOM_GENERATOR),               intent(inout):: rg
    REAL(REL64), dimension(:), contiguous, intent(inout):: A
    INTEGER(INT32),                        intent(in   ):: LOWER, UPPER
    REAL(REL64):: L, U
    !
    L = real(LOWER, rel64)
    U = real(UPPER, rel64)
    !
    CALL GENERATE_RANDOM_REL64_1D_LU(rg, A, L, U)
    !  
  END SUBROUTINE
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  PURE SUBROUTINE GENERATE_RANDOM_REL64_1D_RG(rg, A)
    CLASS(RANDOM_GENERATOR),               intent(inout):: rg
    REAL(REL64), dimension(:), contiguous, intent(inout):: A
    INTEGER(INT64):: r
    INTEGER:: i 
    !
    DO i=1, SIZE(A)
            CALL GET_RAND_INT64(rg%seed, r)
            !
            A(i) = RG_TO_01_REL64(r)
    END DO
    !  
  END SUBROUTINE
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  PURE SUBROUTINE GENERATE_RANDOM_REL64_1D_SN(rg, A, sign)
    CLASS(RANDOM_GENERATOR),               intent(inout):: rg
    REAL(REL64), dimension(:), contiguous, intent(inout):: A
    LOGICAL,                               intent(in   ):: sign
    INTEGER(INT64):: r
    INTEGER:: i 
    !
    IF(sign) THEN
             DO i=1, SIZE(A)
                     CALL GET_RAND_INT64(rg%seed, r)
                     !
                     A(i) = RG_TO_11_REL64(r, sign)
             END DO
    ELSE
       DO i=1, SIZE(A)
               CALL GET_RAND_INT64(rg%seed, r)
               !
               A(i) = RG_TO_01_REL64(r)
       END DO
    END IF
    !  
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !	
  PURE SUBROUTINE GENERATE_RANDOM_REL32_1D_LU(rg, A, LOWER, UPPER)
    CLASS(RANDOM_GENERATOR),               intent(inout):: rg
    REAL(REL32), dimension(:), contiguous, intent(inout):: A
    REAL(REL32),                           intent(in   ):: LOWER, UPPER
    REAL(REL32):: range
    INTEGER(INT64):: r
    INTEGER:: i 
    IF( UPPER <= LOWER) THEN
                        A = LOWER
                        RETURN
    END IF
    !
    range = UPPER - LOWER
    !
    DO i=1, SIZE(A)
            CALL GET_RAND_INT64(rg%seed, r)
            !
            A(i) = REAL(RG_TO_01_REL64(r), REL32) * range + LOWER
    END DO
    !  
  END SUBROUTINE
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  PURE SUBROUTINE GENERATE_RANDOM_REL32_1D_RG(rg, A)
    CLASS(RANDOM_GENERATOR),               intent(inout):: rg
    REAL(REL32), dimension(:), contiguous, intent(inout):: A
    INTEGER(INT64):: r
    INTEGER:: i 
    !
    DO i=1, SIZE(A)
            CALL GET_RAND_INT64(rg%seed, r)
            !
            A(i) = REAL(RG_TO_01_REL64(r), REL32)
    END DO
    !  
  END SUBROUTINE
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  PURE SUBROUTINE GENERATE_RANDOM_REL32_1D_SN(rg, A, sign)
    CLASS(RANDOM_GENERATOR),               intent(inout):: rg
    REAL(REL32), dimension(:), contiguous, intent(inout):: A
    LOGICAL,                               intent(in   ):: sign
    INTEGER(INT64):: r
    INTEGER:: i 
    !
    IF(sign) THEN
             DO i=1, SIZE(A)
                     CALL GET_RAND_INT64(rg%seed, r)
                     !
                     A(i) = REAL(RG_TO_11_REL64(r, sign), REL32)
             END DO
    ELSE
       DO i=1, SIZE(A)
               CALL GET_RAND_INT64(rg%seed, r)
               !
               A(i) = REAL(RG_TO_01_REL64(r), REL32)
       END DO
    END IF
    !  
  END SUBROUTINE
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  PURE SUBROUTINE GENERATE_RANDOM_REL32_1D_LUI32(rg, A, LOWER, UPPER)
    CLASS(RANDOM_GENERATOR),               intent(inout):: rg
    REAL(REL32), dimension(:), contiguous, intent(inout):: A
    INTEGER(INT32),                        intent(in   ):: LOWER, UPPER
    REAL(REL32):: L, U
    !
    L = real(LOWER, rel32)
    U = real(UPPER, rel32)
    !
    CALL GENERATE_RANDOM_REL32_1D_LU(rg, A, L, U)
    !  
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !
  PURE SUBROUTINE GENERATE_RANDOM_CHAR_1D_RG(rg, rnd)
    CLASS(RANDOM_GENERATOR),    intent(inout) :: rg
    CHARACTER(*), dimension(:), intent(  out) :: rnd
    INTEGER:: ldim, dim, shft, i, j
    INTEGER(INT64):: r
    INTEGER(INT8):: L, U
    !
    L = 33_int8 
    U = 126_int8
    !
    dim  = size(rnd)
    ldim = len(rnd)
    shft = 0
    do j=1, dim
    do i=1, ldim
       CALL GET_CHAR_LU(rg, shft, r, L, U, rnd(j)(i:i))
    end do
    end do
    !
  END SUBROUTINE
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  PURE SUBROUTINE GENERATE_RANDOM_CHAR_1D_CH(rg, rnd, str)
    CLASS(RANDOM_GENERATOR),    intent(inout) :: rg
    CHARACTER(*), dimension(:), intent(  out) :: rnd
    CHARACTER(*),               intent(in   ) :: str ! Max size of 127
    INTEGER:: ldim, dim, shft, i, j, lstr
    INTEGER(INT64):: r
    !
    lstr = len(str)
    !
    dim  = size(rnd)
    ldim = len(rnd)
    shft = 0
    do j=1, dim
    do i=1, ldim
       CALL GET_CHAR_STR(rg, shft, r, lstr, str, rnd(j)(i:i))
    end do
    end do
    !
  END SUBROUTINE
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  PURE SUBROUTINE GENERATE_RANDOM_CHAR_1D_LU(rg, rnd, LOWER, UPPER)
    CLASS(RANDOM_GENERATOR),    intent(inout) :: rg
    CHARACTER(*), dimension(:), intent(  out) :: rnd
    CHARACTER,                  intent(in   ) :: LOWER, UPPER
    INTEGER:: ldim, dim, shft, i, j
    INTEGER(INT64):: r
    INTEGER(INT8):: L, U
    !
    L = TRANSFER(LOWER, L)
    U = TRANSFER(UPPER, U)
    if( L > U ) then
                block
                    INTEGER(INT8):: t
                    t = L
                    L = U
                    U = t
                end block
    end if
    if( L<33_int8  ) L = 33_int8
    if( U>126_int8 ) U = 126_int8
    !
    dim  = size(rnd)
    ldim = len(rnd)
    shft = 0
    do j=1, dim
    do i=1, ldim
       CALL GET_CHAR_LU(rg, shft, r, L, U, rnd(j)(i:i))
    end do
    end do
    !
  END SUBROUTINE
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  ! Shuffle values in array -> Modified SATTOLO Shuffle Method  
  !		
  PURE SUBROUTINE SHUFFLE_INT32_1D_RG(rg, A, npass)
    CLASS(RANDOM_GENERATOR),                  intent(inout) :: rg
    INTEGER(INT32), dimension(:), contiguous, intent(inout) :: A
    INTEGER,                        optional, intent(in   ) :: npass
    INTEGER(INT64):: r
    INTEGER(INT32):: T
    INTEGER:: k, np
    INTEGER:: I, J, DIM 
    INTEGER(INT64):: LDIM
    np = 1
    if(present(npass)) np = npass
    !
    DIM  = SIZE(A)
    LDIM = INT(DIM, INT64)
    !
    do k=1, np
    DO I=DIM, TWO, NEG
        !
        CALL GET_RAND_INT64(rg%seed, r)
        !
        J = INT( MODULO(r, LDIM) + LONG_ONE, INT32)
        !
        T    = A(I)
        A(I) = A(J)
        A(J) = T
        !
    END DO
    end do
    !  
  END SUBROUTINE
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  PURE SUBROUTINE SHUFFLE_INT64_1D_RG(rg, A, npass)
    CLASS(RANDOM_GENERATOR),                  intent(inout) :: rg
    INTEGER(INT64), dimension(:), contiguous, intent(inout) :: A
    INTEGER,                        optional, intent(in   ) :: npass
    INTEGER(INT64):: r
    INTEGER(INT64):: T
    INTEGER:: k, np
    INTEGER:: I, J, DIM 
    INTEGER(INT64):: LDIM
    np = 1
    if(present(npass)) np = npass
    !
    DIM  = SIZE(A)
    LDIM = INT(DIM, INT64)
    !
    do k=1, np
    DO I=DIM, TWO, NEG
        !
        CALL GET_RAND_INT64(rg%seed, r)
        !
        J = INT( MODULO(r, LDIM) + LONG_ONE, INT32)
        !
        T    = A(I)
        A(I) = A(J)
        A(J) = T
        !
    END DO
    end do
    !  
  END SUBROUTINE
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  PURE SUBROUTINE SHUFFLE_REL64_1D_RG(rg, A, npass)
    CLASS(RANDOM_GENERATOR),               intent(inout) :: rg
    REAL(REL64), dimension(:), contiguous, intent(inout) :: A
    INTEGER,                     optional, intent(in   ) :: npass
    INTEGER(INT64):: r
    REAL(REL64):: T
    INTEGER:: k, np
    INTEGER:: I, J, DIM 
    INTEGER(INT64):: LDIM
    np = 1
    if(present(npass)) np = npass
    !
    DIM  = SIZE(A)
    LDIM = INT(DIM, INT64)
    !
    do k=1, np
    DO I=DIM, TWO, NEG
        !
        CALL GET_RAND_INT64(rg%seed, r)
        !
        J = INT( MODULO(r, LDIM) + LONG_ONE, INT32)
        !
        T    = A(I)
        A(I) = A(J)
        A(J) = T
        !
    END DO
    end do
    !  
  END SUBROUTINE
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  PURE SUBROUTINE SHUFFLE_REL32_1D_RG(rg, A, npass)
    CLASS(RANDOM_GENERATOR),               intent(inout) :: rg
    REAL(REL32), dimension(:), contiguous, intent(inout) :: A
    INTEGER,                     optional, intent(in   ) :: npass
    INTEGER(INT64):: r
    REAL(REL32):: T
    INTEGER:: k, np
    INTEGER:: I, J, DIM 
    INTEGER(INT64):: LDIM
    np = 1
    if(present(npass)) np = npass
    !
    DIM  = SIZE(A)
    LDIM = INT(DIM, INT64)
    !
    do k=1, np
    DO I=DIM, TWO, NEG
        !
        CALL GET_RAND_INT64(rg%seed, r)
        !
        J = INT( MODULO(r, LDIM) + LONG_ONE, INT32)
        !
        T    = A(I)
        A(I) = A(J)
        A(J) = T
        !
    END DO
    end do
    !  
  END SUBROUTINE
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  PURE SUBROUTINE SHUFFLE_CHAR_1D_RG(rg, A, npass)
    CLASS(RANDOM_GENERATOR),                intent(inout) :: rg
    CHARACTER(*), dimension(:), contiguous, intent(inout) :: A
    INTEGER,                      optional, intent(in   ) :: npass
    INTEGER(INT64):: r
    CHARACTER(len(A)):: T
    INTEGER:: k, np
    INTEGER:: I, J, DIM 
    INTEGER(INT64):: LDIM
    np = 1
    if(present(npass)) np = npass
    !
    DIM  = SIZE(A)
    LDIM = INT(DIM, INT64)
    !
    do k=1, npass
    DO I=DIM, TWO, NEG
        !
        CALL GET_RAND_INT64(rg%seed, r)
        !
        J = INT( MODULO(r, LDIM) + LONG_ONE, INT32)
        !
        T    = A(I)
        A(I) = A(J)
        A(J) = T
        !
    END DO
    end do
    !  
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !	
  PURE SUBROUTINE SHUFFLE_INT32_DIM_RG(rg, DIM, A, npass)
    CLASS(RANDOM_GENERATOR),        intent(inout) :: rg
    INTEGER,                        intent(in   ) :: DIM
    INTEGER(INT32), dimension(DIM), intent(inout) :: A
    INTEGER,                        intent(in   ) :: npass
    INTEGER(INT64):: r
    INTEGER(INT32):: T
    INTEGER:: I, J, K
    INTEGER(INT64):: LDIM
    !
    LDIM = INT(DIM, INT64)
    !
    do k=1, npass
    DO I=DIM, TWO, NEG
        !
        CALL GET_RAND_INT64(rg%seed, r)
        !
        J = INT( MODULO(r, LDIM) + LONG_ONE, INT32)
        !
        T    = A(I)
        A(I) = A(J)
        A(J) = T
        !
    END DO
    end do
    !  
  END SUBROUTINE
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  PURE SUBROUTINE SHUFFLE_INT64_DIM_RG(rg, DIM, A, npass)
    CLASS(RANDOM_GENERATOR),        intent(inout) :: rg
    INTEGER,                        intent(in   ) :: DIM
    INTEGER(INT64), dimension(DIM), intent(inout) :: A
    INTEGER,                        intent(in   ) :: npass
    INTEGER(INT64):: r
    INTEGER(INT64):: T
    INTEGER:: I, J, K
    INTEGER(INT64):: LDIM
    !
    LDIM = INT(DIM, INT64)
    !
    do k=1, npass
    DO I=DIM, TWO, NEG
        !
        CALL GET_RAND_INT64(rg%seed, r)
        !
        J = INT( MODULO(r, LDIM) + LONG_ONE, INT32)
        !
        T    = A(I)
        A(I) = A(J)
        A(J) = T
        !
    END DO
    end do
    !  
  END SUBROUTINE
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  PURE SUBROUTINE SHUFFLE_REL64_DIM_RG(rg, DIM, A, npass)
    CLASS(RANDOM_GENERATOR),     intent(inout) :: rg
    INTEGER,                     intent(in   ) :: DIM
    REAL(REL64), dimension(DIM), intent(inout) :: A
    INTEGER,                     intent(in   ) :: npass
    INTEGER(INT64):: r
    REAL(REL64):: T
    INTEGER:: I, J, K
    INTEGER(INT64):: LDIM
    !
    LDIM = INT(DIM, INT64)
    !
    do k=1, npass
    DO I=DIM, TWO, NEG
        !
        CALL GET_RAND_INT64(rg%seed, r)
        !
        J = INT( MODULO(r, LDIM) + LONG_ONE, INT32)
        !
        T    = A(I)
        A(I) = A(J)
        A(J) = T
        !
    END DO
    end do
    !  
  END SUBROUTINE
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  PURE SUBROUTINE SHUFFLE_REL32_DIM_RG(rg, DIM, A, npass)
    CLASS(RANDOM_GENERATOR),     intent(inout) :: rg
    INTEGER,                     intent(in   ) :: DIM
    REAL(REL32), dimension(DIM), intent(inout) :: A
    INTEGER,                     intent(in   ) :: npass
    INTEGER(INT64):: r
    REAL(REL32):: T
    INTEGER:: I, J, K
    INTEGER(INT64):: LDIM
    !
    LDIM = INT(DIM, INT64)
    !
    do k=1, npass
    DO I=DIM, TWO, NEG
        !
        CALL GET_RAND_INT64(rg%seed, r)
        !
        J = INT( MODULO(r, LDIM) + LONG_ONE, INT32)
        !
        T    = A(I)
        A(I) = A(J)
        A(J) = T
        !
    END DO
    end do
    !  
  END SUBROUTINE
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  PURE SUBROUTINE SHUFFLE_CHAR_DIM_RG(rg, lenA, DIM, A, npass)
    CLASS(RANDOM_GENERATOR),         intent(inout) :: rg
    INTEGER,                         intent(in   ) :: lenA, DIM
    CHARACTER(lenA), dimension(DIM), intent(inout) :: A
    INTEGER,                         intent(in   ) :: npass
    INTEGER(INT64):: r
    CHARACTER(lenA):: T
    INTEGER:: I, J, K
    INTEGER(INT64):: LDIM
    !
    LDIM = INT(DIM, INT64)
    !
    do k=1, npass
    DO I=DIM, TWO, NEG
        !
        CALL GET_RAND_INT64(rg%seed, r)
        !
        J = INT( MODULO(r, LDIM) + LONG_ONE, INT32)
        !
        T    = A(I)
        A(I) = A(J)
        A(J) = T
        !
    END DO
    end do
    !  
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !	
  PURE SUBROUTINE SHUFFLE_INT32_2D_RG(rg, A, npass)
    CLASS(RANDOM_GENERATOR),                    intent(inout) :: rg
    INTEGER(INT32), dimension(:,:), contiguous, intent(inout) :: A
    INTEGER,                          optional, intent(in   ) :: npass
    INTEGER:: np
    np = 1
    if(present(npass)) np = npass
    !
    CALL SHUFFLE_INT32_DIM_RG(rg, SIZE(A), A, np)
    !
  END SUBROUTINE
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  PURE SUBROUTINE SHUFFLE_INT64_2D_RG(rg, A, npass)
    CLASS(RANDOM_GENERATOR),                    intent(inout) :: rg
    INTEGER(INT64), dimension(:,:), contiguous, intent(inout) :: A
    INTEGER,                          optional, intent(in   ) :: npass
    INTEGER:: np
    np = 1
    if(present(npass)) np = npass
    !
    CALL SHUFFLE_INT64_DIM_RG(rg, SIZE(A), A, np)
    !
  END SUBROUTINE
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  PURE SUBROUTINE SHUFFLE_REL64_2D_RG(rg, A, npass)
    CLASS(RANDOM_GENERATOR),                 intent(inout) :: rg
    REAL(REL64), dimension(:,:), contiguous, intent(inout) :: A
    INTEGER,                       optional, intent(in   ) :: npass
    INTEGER:: np
    np = 1
    if(present(npass)) np = npass
    !
    CALL SHUFFLE_REL64_DIM_RG(rg, SIZE(A), A, np)
    !
  END SUBROUTINE
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  PURE SUBROUTINE SHUFFLE_REL32_2D_RG(rg, A, npass)
    CLASS(RANDOM_GENERATOR),                 intent(inout) :: rg
    REAL(REL32), dimension(:,:), contiguous, intent(inout) :: A
    INTEGER,                       optional, intent(in   ) :: npass
    INTEGER:: np
    np = 1
    if(present(npass)) np = npass
    !
    CALL SHUFFLE_REL32_DIM_RG(rg, SIZE(A), A, np)
    !
  END SUBROUTINE
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  PURE SUBROUTINE SHUFFLE_CHAR_2D_RG(rg, A, npass)
    CLASS(RANDOM_GENERATOR),                  intent(inout) :: rg
    CHARACTER(*), dimension(:,:), contiguous, intent(inout) :: A
    INTEGER,                        optional, intent(in   ) :: npass
    INTEGER:: np
    INTEGER:: lenA, dim
    np = 1
    if(present(npass)) np = npass
    !
    dim = SIZE(A)
    lenA = len(A)
    !
    CALL SHUFFLE_CHAR_DIM_RG(rg, lenA, dim, A, np)
    !
  END SUBROUTINE
  !
  !#############################################################################################################################################################
  !	
  PURE SUBROUTINE SHUFFLE_INT32_3D_RG(rg, A, npass)
    CLASS(RANDOM_GENERATOR),                      intent(inout) :: rg
    INTEGER(INT32), dimension(:,:,:), contiguous, intent(inout) :: A
    INTEGER,                            optional, intent(in   ) :: npass
    INTEGER:: np
    np = 1
    if(present(npass)) np = npass
    !
    CALL SHUFFLE_INT32_DIM_RG(rg, SIZE(A), A, np)
    !
  END SUBROUTINE
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  PURE SUBROUTINE SHUFFLE_INT64_3D_RG(rg, A, npass)
    CLASS(RANDOM_GENERATOR),                      intent(inout) :: rg
    INTEGER(INT64), dimension(:,:,:), contiguous, intent(inout) :: A
    INTEGER,                            optional, intent(in   ) :: npass
    INTEGER:: np
    np = 1
    if(present(npass)) np = npass
    !
    CALL SHUFFLE_INT64_DIM_RG(rg, SIZE(A), A, np)
    !
  END SUBROUTINE
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  PURE SUBROUTINE SHUFFLE_REL64_3D_RG(rg, A, npass)
    CLASS(RANDOM_GENERATOR),                   intent(inout):: rg
    REAL(REL64), dimension(:,:,:), contiguous, intent(inout):: A
    INTEGER,                         optional, intent(in   ) :: npass
    INTEGER:: np
    np = 1
    if(present(npass)) np = npass
    !
    CALL SHUFFLE_REL64_DIM_RG(rg, SIZE(A), A, np)
    !
  END SUBROUTINE
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  PURE SUBROUTINE SHUFFLE_REL32_3D_RG(rg, A, npass)
    CLASS(RANDOM_GENERATOR),                   intent(inout):: rg
    REAL(REL32), dimension(:,:,:), contiguous, intent(inout):: A
    INTEGER,                         optional, intent(in   ) :: npass
    INTEGER:: np
    np = 1
    if(present(npass)) np = npass
    !
    CALL SHUFFLE_REL32_DIM_RG(rg, SIZE(A), A, np)
    !
  END SUBROUTINE
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  PURE SUBROUTINE SHUFFLE_CHAR_3D_RG(rg, A, npass)
    CLASS(RANDOM_GENERATOR),                    intent(inout) :: rg
    CHARACTER(*), dimension(:,:,:), contiguous, intent(inout) :: A
    INTEGER,                          optional, intent(in   ) :: npass
    INTEGER:: lenA, dim
    INTEGER:: np
    np = 1
    if(present(npass)) np = npass
    !
    dim = SIZE(A)
    lenA = len(A)
    !
    CALL SHUFFLE_CHAR_DIM_RG(rg, lenA, dim, A, np)
    !
  END SUBROUTINE
  !	
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  ! 
  FUNCTION COIN_TOSS(SEED)  RESULT(TIS_TRUE)
    INTEGER,  optional, intent(in):: SEED
    LOGICAL:: TIS_TRUE
    TYPE(RANDOM_GENERATOR), ALLOCATABLE, SAVE:: rg
    !
    IF( .NOT. ALLOCATED(RG) ) THEN
               ALLOCATE(RG)
               IF(.NOT. PRESENT(SEED)) CALL SET_RANDOM_PROPERTIES_RG(rg)
    END IF
    IF(PRESENT(SEED)) CALL SET_RANDOM_PROPERTIES_RG(rg, SEED)
    !
    TIS_TRUE = rg%FLIP_COIN()
    !
  END FUNCTION
  !
  !#############################################################################################################################################################
  !	
  FUNCTION BINARY_TOSS(SEED)  RESULT(BINARY_RESULT)
    INTEGER,  optional, intent(in):: SEED
    INTEGER:: BINARY_RESULT
    TYPE(RANDOM_GENERATOR), ALLOCATABLE, SAVE:: rg
    !
    IF( .NOT. ALLOCATED(RG) ) THEN
               ALLOCATE(RG)
               IF(.NOT. PRESENT(SEED)) CALL SET_RANDOM_PROPERTIES_RG(rg)
    END IF
    IF(PRESENT(SEED)) CALL SET_RANDOM_PROPERTIES_RG(rg, SEED)
    !
    BINARY_RESULT = rg%GET_BINARY()
    !
  END FUNCTION
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  ! 
  FUNCTION randInt(LOWER, UPPER, SEED)  RESULT(ival)
    INTEGER(INT32),    intent(in):: LOWER, UPPER
    INTEGER, optional, intent(in):: SEED
    INTEGER(INT32):: ival
    TYPE(RANDOM_GENERATOR), ALLOCATABLE, SAVE:: rg
    !
    IF( .NOT. ALLOCATED(RG) ) THEN
               ALLOCATE(RG)
               IF(.NOT. PRESENT(SEED)) CALL SET_RANDOM_PROPERTIES_RG(rg)
    END IF
    IF(PRESENT(SEED)) CALL SET_RANDOM_PROPERTIES_RG(rg, SEED)
    !
    CALL rg%GET(ival, LOWER, UPPER)
    !
  END FUNCTION
  !
  !#############################################################################################################################################################
  !	
  SUBROUTINE randBase(rnd, LOWER, UPPER, SIGN, SEED)
    REAL(REL64),           intent(out):: rnd
    REAL(REL64), optional, intent(in ):: LOWER, UPPER
    LOGICAL,     optional, intent(in ):: SIGN
    INTEGER,     optional, intent(in ):: SEED
    TYPE(RANDOM_GENERATOR), ALLOCATABLE, SAVE:: rg
    !
    IF( .NOT. ALLOCATED(RG) ) THEN
               ALLOCATE(RG)
               IF(.NOT. PRESENT(SEED)) CALL SET_RANDOM_PROPERTIES_RG(rg)
    END IF
    IF(PRESENT(SEED)) CALL SET_RANDOM_PROPERTIES_RG(rg, SEED)
    !
    IF    (PRESENT(LOWER)) THEN
                           CALL rg%GET(rnd, LOWER, UPPER)
    ELSEIF(PRESENT(SIGN)) THEN
                          CALL rg%GET(rnd, SIGN)
    ELSE
        CALL rg%GET(rnd)
    END IF
    !
  END SUBROUTINE
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  FUNCTION randVal(SEED) RESULT(rnd)
    INTEGER, optional, intent(in ):: SEED
    REAL(REL64):: rnd
    CALL randBase(rnd, SEED=SEED)
  END FUNCTION
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  FUNCTION randLU(LOWER, UPPER, SEED) RESULT(rnd)
    REAL(REL64),           intent(in ):: LOWER, UPPER
    INTEGER,     optional, intent(in ):: SEED
    REAL(REL64):: rnd
    CALL randBase(rnd, LOWER, UPPER, SEED=SEED)
  END FUNCTION
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  FUNCTION randLUI(LOWER, UPPER, SEED) RESULT(rnd)
    INTEGER(INT32),    intent(in ):: LOWER, UPPER
    INTEGER, optional, intent(in ):: SEED
    REAL(REL64):: rnd
    REAL(REL64):: L, U
    L = real(LOWER, rel64)
    U = real(UPPER, rel64)
    CALL randBase(rnd, L, U, SEED=SEED)
  END FUNCTION
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  FUNCTION randSN(SIGN, SEED) RESULT(rnd)
    LOGICAL,               intent(in ):: SIGN
    INTEGER,     optional, intent(in ):: SEED
    REAL(REL64):: rnd
    CALL randBase(rnd, SIGN=SIGN, SEED=SEED)
  END FUNCTION
  !	
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
!  !
!  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!  ! 
!  SUBROUTINE randARRAY_INT(A, LOWER, UPPER, SEED)
!    INTEGER(INT32), dimension(:), contiguous, intent(inout):: A
!    INTEGER(INT32),                 optional, intent(in   ):: LOWER, UPPER
!    INTEGER,                      optional, intent(in   ):: SEED
!    INTEGER:: I
!    TYPE(RANDOM_GENERATOR), ALLOCATABLE, SAVE:: LCG_BETWEEN_ARRAY
!    !
!    IF( .NOT. ALLOCATED(LCG_BETWEEN_ARRAY) ) THEN
!                                    ALLOCATE( LCG_BETWEEN_ARRAY )
!                                    CALL LCG_BETWEEN_ARRAY%INIT()
!    END IF
!    IF(PRESENT(SEED)) CALL LCG_BETWEEN_ARRAY%SET_SEED(SEED)
!    !
!    DO I=1, SIZE(A)
!                    CALL LCG_BETWEEN_ARRAY%GEN( A(I), LOWER, UPPER )
!    END DO
!    !
!  END SUBROUTINE
!  !	
!  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
!  !	
!  SUBROUTINE randARRAY_DBL(A, LOWER, UPPER, SEED)
!    REAL(DBL), dimension(:), contiguous, intent(inout):: A
!    REAL(DBL),                 optional, intent(in   ):: LOWER, UPPER
!    INTEGER,                   optional, intent(in   ):: SEED
!    INTEGER:: I
!    TYPE(RANDOM_GENERATOR), ALLOCATABLE, SAVE:: LCG_BETWEEN_ARRAY
!    !
!    IF( .NOT. ALLOCATED(LCG_BETWEEN_ARRAY) ) THEN
!                                    ALLOCATE( LCG_BETWEEN_ARRAY )
!                                    CALL LCG_BETWEEN_ARRAY%INIT()
!    END IF
!    IF(PRESENT(SEED)) CALL LCG_BETWEEN_ARRAY%SET_SEED(SEED)
!    !
!    DO I=1, SIZE(A)
!                    CALL LCG_BETWEEN_ARRAY%GEN( A(I), LOWER, UPPER )
!    END DO
!    !
!  END SUBROUTINE
!  !	
!  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
!  !	
!  SUBROUTINE randARRAY_SNG(A, LOWER, UPPER, SEED)
!    REAL(SNG), dimension(:), contiguous, intent(inout):: A
!    REAL(SNG),                 optional, intent(in   ):: LOWER, UPPER
!    INTEGER,                   optional, intent(in   ):: SEED
!    INTEGER:: I
!    TYPE(RANDOM_GENERATOR), ALLOCATABLE, SAVE:: LCG_BETWEEN_ARRAY
!    !
!    IF( .NOT. ALLOCATED(LCG_BETWEEN_ARRAY) ) THEN
!                                    ALLOCATE( LCG_BETWEEN_ARRAY )
!                                    CALL LCG_BETWEEN_ARRAY%INIT()
!    END IF
!    IF(PRESENT(SEED)) CALL LCG_BETWEEN_ARRAY%SET_SEED(SEED)
!    !
!    DO I=1, SIZE(A)
!                    CALL LCG_BETWEEN_ARRAY%GEN( A(I), LOWER, UPPER )
!    END DO
!    !
!  END SUBROUTINE
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  ! 
  SUBROUTINE SHUFFLER(I32,R64,I64,R32,CH,SEED)  
    INTEGER(INT32), dimension(:), contiguous, optional, intent(inout):: I32
    REAL   (REL64), dimension(:), contiguous, optional, intent(inout):: R64
    INTEGER(INT64), dimension(:), contiguous, optional, intent(inout):: I64
    REAL   (REL32), dimension(:), contiguous, optional, intent(inout):: R32
    CHARACTER(*),   dimension(:), contiguous, optional, intent(inout):: CH
    INTEGER,                                  optional, intent(in   ):: SEED
    TYPE(RANDOM_GENERATOR), ALLOCATABLE, SAVE:: rg
    !
    IF( .NOT. ALLOCATED(RG) ) THEN
               ALLOCATE(RG)
               IF(.NOT. PRESENT(SEED)) CALL SET_RANDOM_PROPERTIES_RG(rg)
    END IF
    IF(PRESENT(SEED)) CALL SET_RANDOM_PROPERTIES_RG(rg, SEED)
    !
    if    (present(I32)) then
                         CALL SHUFFLE_INT32_1D_RG(rg, I32)
    elseif(present(R64)) then
                         CALL SHUFFLE_REL64_1D_RG(rg, R64)
    elseif(present(I64)) then
                         CALL SHUFFLE_INT64_1D_RG(rg, I64)
    elseif(present(CH) ) then
                         CALL SHUFFLE_CHAR_1D_RG(rg, CH)
    else
                         CALL SHUFFLE_REL32_1D_RG(rg, R32)
    end if
    !
  END SUBROUTINE
  !
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  SUBROUTINE SHUFFLE_INT32(I32,SEED)  
    INTEGER(INT32), dimension(:), contiguous, intent(inout):: I32
    INTEGER,                        optional, intent(in   ):: SEED
    CALL SHUFFLER(I32,SEED=SEED)  
  END SUBROUTINE
  !
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  SUBROUTINE SHUFFLE_REL64(R64,SEED)  
    REAL(REL64), dimension(:), contiguous, intent(inout):: R64
    INTEGER,                     optional, intent(in   ):: SEED
    CALL SHUFFLER(R64=R64,SEED=SEED)  
  END SUBROUTINE
  !
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  SUBROUTINE SHUFFLE_INT64(I64,SEED)  
    INTEGER(INT64), dimension(:), contiguous, intent(inout):: I64
    INTEGER,                        optional, intent(in   ):: SEED
    CALL SHUFFLER(I64=I64,SEED=SEED)  
  END SUBROUTINE
  !
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  SUBROUTINE SHUFFLE_REL32(R32,SEED)  
    REAL   (REL32), dimension(:), contiguous, intent(inout):: R32
    INTEGER,                        optional, intent(in   ):: SEED
    CALL SHUFFLER(R32=R32,SEED=SEED)  
  END SUBROUTINE
  !
  ! ------------------------------------------------------------------------------------------------------------------------------------------------------------
  !	
  SUBROUTINE SHUFFLE_CHAR(CH,SEED)  
    CHARACTER(*), dimension(:), contiguous, intent(inout):: CH
    INTEGER,                      optional, intent(in   ):: SEED
    CALL SHUFFLER(CH=CH,SEED=SEED)  
  END SUBROUTINE
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  ! Helper routine for setting the internal Fortran seed or making the seed random
  ! 
  SUBROUTINE SET_FORTRAN_SEED(SEED)
     INTEGER, optional, intent(in):: SEED
     TYPE(RANDOM_GENERATOR):: rg
     INTEGER, dimension(:), allocatable:: S
     INTEGER:: I, DIM
     !
     CALL RANDOM_SEED (SIZE = DIM)
     ALLOCATE(S(DIM))
     !
     IF(PRESENT(SEED)) THEN
                       CALL INITIALIZE_RG(rg, SEED)
     ELSE
                       CALL INITIALIZE_RG(rg)
     END IF
     !
     DO I=ONE, DIM
               S(I) = rg%GET_INT()
     END DO
     !
     CALL RANDOM_SEED (PUT = S)
     !
     DEALLOCATE(S)
     !
  END SUBROUTINE
END MODULE
!  
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
! Routine that generates the tables used by the Ziggurat algorithm as described by McFarland (2016)
!    See for original implementation:
!                      Marsaglia, G., & Tsang, W. W. (2000). The ziggurat method for generating random variables. Journal of statistical software, 5(8), 1-7.
!                      McFarland, C. D. (2016). A modified ziggurat algorithm for generating exponentially and 
!  
!  Modification to REAL128 (QUAD) Precision that is truncated to REAL64 to improve precision.
!  
!    -> These subroutines were needed for MODULE RANDOM_ROUTINES to 
!       generate the following arrays:
!                exp_X,  exp_Y,  exp_map,  exp_ipmf,  nrm_X,  nrm_Y,  nrm_map,  nrm_ipmf
!
! Normal use:
!        CALL CALC_ZIGGURAT_EXPONENTIAL_LAYERS(256) 
!        CALL CALC_ZIGGURAT_NORMAL_LAYERS(256)
!    
! to get properties
!
MODULE ZIGGURAT_LAYERS
  USE, INTRINSIC:: ISO_FORTRAN_ENV, ONLY: int64,  qp => REAL128
  IMPLICIT NONE
  !
  PRIVATE
  !
  PUBLIC:: CALC_ZIGGURAT_EXPONENTIAL_LAYERS, CALC_ZIGGURAT_NORMAL_LAYERS
  !
  CONTAINS
  !########################################################################################
  ! Exponential Distribution Layers
  !########################################################################################
  SUBROUTINE CALC_ZIGGURAT_EXPONENTIAL_LAYERS(NLAY)
    INTEGER, INTENT(IN):: NLAY
    INTEGER:: N, dim
    REAL(qp):: volume, a, b, last_Y, V_tail, V_mean_inv
    REAL(qp), dimension(:), allocatable:: x, y, dx, dy, v, pmf, w  !w is a work array
    INTEGER,  dimension(:), allocatable:: imap
    INTEGER(int64),  dimension(:), allocatable:: ipmf
    INTEGER(int64):: imax, iE_max
    INTEGER:: i
    REAL(qp)::  zer, half, one, two, imax_qp
    zer  = 0.0_qp
    one  = 1.0_qp
    two  = 2.0_qp
    half = 0.5_qp
    !
    imax    = huge(imax)
    imax_qp = real(imax, qp)
    !
    volume = one / REAL(NLAY, qp)
    a =  one
    b = 10.0_qp
    !
    N   = 1
    dim = NLAY
    !
    call resize(x, dim, zer)
    !
    last_Y = zer  ! variable inherited by gen_X
    I = 0
    DO WHILE( a > volume )
        i = i + 1
        if(i > dim) call grow(x, dim)
        !
        x(i) = BISECTION_QP(gen_X, a, b)
        !
        if(x(i) == HUGE(a)) THEN           ! No solution, solve again with a slightly wider range
                        a = a * 0.9_qp
                        i = i - 1
        else
                        last_Y = pdf(x(i))
                        a = x(i) * 0.9_qp
                        b = x(i)
        end if
    END DO
    i = i + 1
    if(i > dim) call grow(x, dim)
    x(i) = zer
    dim = i
    CALL resize(x, dim, zer)
    !
    ALLOCATE(v(dim))
    ALLOCATE(dx(dim-1))  ! dX = -np.diff(X)  # dx_i = x_i-1 - x_i; x_-1 = x: f(x) = 0 = inf
    ALLOCATE(dy(dim-1))
    ALLOCATE(y(dim))
    DO i=1, dim
            y(i) = pdf(x(i))
    END DO
    DO i=1, dim-1
            dx(i) = x(i) - x(i+1)
            dy(i) = y(i+1) - y(i)
    END DO
    DO i=2, dim-1
            if(abs(X(i)*dy(i-1) - volume) > 1e-24_qp) then
               error stop 'abs(X(i)*dy(i-1) - volume) > 1e-24_qp' 
            end if
    END DO
    !
    DO i=1, dim
            v(i) = cdf(x(i)) 
    END DO
    DO i=dim, 2, -1
            v(i) = v(i-1) - v(i)
    END DO
    v(1) =  one - v(1)
    !
    DO i=2, dim
            v(i) = v(i) - y(i-1)*dx(i-1)
    END DO
    !
    V_tail = v(1)/ sum(v)
    !
    WRITE(*,'(A,F0.4,"%")') "Fraction of remainder in tail: ", 100._qp*V_tail
    !
    CALL resize(v, NLAY, zer)
    !
    WRITE(*,'(F0.6, 1x, I0)') sum(v)/volume, NLAY-dim+1
    
    a = zer
    do i=1, NLAY
        a = a + real(i,qp)*v(i)
    end do
    WRITE(*,'(F0.6)') a/real(NLAY,qp)
    !
    V_mean_inv = real(NLAY,qp)/sum(V)
    v = v * V_mean_inv
    !
    call realign_pmf(v, pmf, imap )
    !
    dim = size(pmf)
    ALLOCATE(ipmf(dim))
    !
    do i=1, dim
        if    (pmf(i) >= one ) then
                               ipmf(i) = imax
        elseif(pmf(i) >= half) then
                               ipmf(i) = int( (pmf(i)-half)*two*imax_qp, int64 )
        else
                               ipmf(i) = int( pmf(i)       *two*imax_qp, int64 ) - imax
        end if
    end do 
    WRITE(*,*) 'exp shift: ', x(1)
    !
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    w = dY/dX                         !auto-allocate w instead of: ALLOCATE(w, source=dY); w = w/dX      ! m = dY/dX
    !
    do i=1, size(w)
        w(i) = Y(i+1) - w(i)*(one - X(i+1) - LOG(w(i)))
    end do
    w = w/dY
    iE_max = int( maxval(w(2:))*imax_qp, int64)
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !
    X = X/imax_qp
    Y = Y/imax_qp
    !
    OPEN(NEWUNIT=I, FILE='exponential_layers.txt', STATUS='REPLACE', ACTION='WRITE')
    WRITE(I, '(A, I0)'              ) "iE_max   = ", iE_max
    WRITE(I, '(A, *(ES27.20,:", "))') "exp_X    = ", X
    WRITE(I, '(A, *(ES27.20,:", "))') "exp_Y    = ", Y
    WRITE(I, '(A, *(I0,:", "))')      "exp_map  = ", imap
    WRITE(I, '(A, *(I0,:", "))')      "exp_ipmf = ", ipmf
    CLOSE(I)
    !
    CONTAINS
    !vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
    pure function pdf(x) result(y)          !
      REAL(qp), intent(in):: x              !
      REAL(qp):: y                          !
      y = exp(-x)                           !
    end function                            !
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    pure function cdf(x) result(y)          !
      REAL(qp), intent(in):: x              !
      REAL(qp):: y                          !
      y = 1.0_qp - pdf(x)                   !
    end function                            !
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    PURE REAL(qp) FUNCTION gen_X(x)         !
      REAL(qp), intent(in):: x              !
      gen_X = x*(pdf(x) - last_Y) - volume  !
    END FUNCTION                            !
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
  END SUBROUTINE 
  !########################################################################################
  ! Normal Distribution Layers
  !########################################################################################
  SUBROUTINE CALC_ZIGGURAT_NORMAL_LAYERS(NLAY)
    INTEGER, INTENT(IN):: NLAY
    INTEGER:: N, dim
    REAL(qp), dimension(:), allocatable:: x, y, dx, dy, v, pmf, w  !w is a work array
    INTEGER,  dimension(:), allocatable:: imap
    INTEGER(int64),  dimension(:), allocatable:: ipmf
    INTEGER(int64):: imax, iE_max, iE_min
    INTEGER:: i, inflection_point, i_inflection
    REAL(qp):: volume, a, b, last_Y, V_tail, V_mean_inv, X_max, tmp
    REAL(qp):: alpha, pi, zer, half, one, two, imax_qp, sqrt_half
    pi = 3.14159265358979323846264338327950_qp
    zer  = 0.0_qp
    one  = 1.0_qp
    two  = 2.0_qp
    half = 0.5_qp
    alpha = sqrt(half*pi)
    sqrt_half = sqrt(half)
    !
    imax    = huge(imax)
    imax_qp = real(imax, qp)
    !
    volume = two*REAL(NLAY, qp)   ! volume = 1/nlay with the normal normalizing constant shifted over
    volume = sqrt(two*pi)/volume
    a =  one
    b = 4.0_qp
    !
    N   = 1
    dim = NLAY
    !
    call resize(x, dim, zer)
    !
    last_Y = zer  ! variable inherited by gen_X
    I = 0
    DO WHILE( a > volume )
        i = i + 1
        if(i > dim) call grow(x, dim)
        !
        x(i) = BISECTION_QP(gen_X, a, b)    ! fsolve(lambda x: x*(f(x) - last_Y_i) - vol, lower_bound, upper_bound)
        !
        if(x(i) == HUGE(a)) THEN           ! No solution, solve again
                        a = a * 0.9_qp
                        i = i - 1
        else
                        last_Y = pdf(x(i))
                        a = x(i) * 0.9_qp
                        b = x(i)
        end if
    END DO
    i = i + 1
    if(i > dim) call grow(x, dim)
    x(i) = zer
    dim = i
    CALL resize(x, dim, zer)
    !
    ALLOCATE(v(dim))
    ALLOCATE(dx(dim-1))  ! dX = -np.diff(X)  # dx_i = x_i-1 - x_i; x_-1 = x: f(x) = 0 = inf
    ALLOCATE(dy(dim-1))
    ALLOCATE(y(dim))
    DO i=1, dim
            y(i) = pdf(x(i))
    END DO
    DO i=1, dim-1
            dx(i) = x(i) - x(i+1)
            dy(i) = y(i+1) - y(i)
    END DO
    DO i=2, dim-1
            if(abs(X(i)*dy(i-1) - volume) > 1e-24_qp) then
               error stop 'abs(X(i)*dy(i-1) - volume) > 1e-24_qp' 
            end if
    END DO
    !
    DO i=1, dim
            v(i) = cdf(x(i)) 
    END DO
    DO i=dim, 2, -1
            v(i) = v(i-1) - v(i)
    END DO
    v(1) =  alpha - v(1)
    !
    DO i=2, dim
            v(i) = v(i) - y(i-1)*dx(i-1)
    END DO
    !
    V_tail = v(1)/ sum(v)
    !
    WRITE(*,'(A,F0.4,"%")') "Fraction of remainder in tail: ", 100._qp*V_tail
    !
    CALL resize(v, NLAY, zer)
    !
    WRITE(*,'(F0.6, 1x, I0)') sum(v)/volume, NLAY-dim+1
    
    a = zer
    do i=1, NLAY
        a = a + real(i,qp)*v(i)
    end do
    WRITE(*,'(F0.6)') a/real(NLAY,qp)
    !
    V_mean_inv = real(NLAY,qp)/sum(V)
    v = v * V_mean_inv
    !
    call realign_pmf(v, pmf, imap )
    !
    dim = size(pmf)
    ALLOCATE(ipmf(dim))
    !
    do i=1, dim
        if    (pmf(i) >= one ) then
                               ipmf(i) = imax
        elseif(pmf(i) >= half) then
                               ipmf(i) = int( (pmf(i)-half)*two*imax_qp, int64 )
        else
                               ipmf(i) = int( pmf(i)       *two*imax_qp, int64 ) - imax
        end if
    end do 
    !9223372036854776000
    !
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    WRITE(*,*) 'nrm shift: ', x(1)
    !
    inflection_point = 1
    i_inflection = 1
    DO i=1, size(x)
        if(x(i) > one) i_inflection = i_inflection + 1
    END DO
    IF(X(i_inflection + 1) >= one) THEN
        WRITE(*,*) 'X[i_inflection + 1] < inflection_point, "Inflection point lies exactly on boundary between two layers" '
    END IF
    !
    WRITE(*,*) 'static uint_fast8_t i_inflection = ', i_inflection+1
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ! w is the deviations of f(x) from y_i  
    ! iE_min and iE_max are the Largest deviations of f(x) from y_i  
    !
    !         last_Y -> variable inherited by delta_i
    !
    w = dY/dX            ! auto-allocate w instead of: ALLOCATE(w, source=dY); w = w/dX      ! m = dY/dX
    do i=2, size(X)
        last_Y = w(i-1)
        a = X(i)
        if(i /= i_inflection) then  !  X_max[i_inflection] should be max_x f(x) - Y_i_inflection(x) (not max_x abs(f(x) - Y_i_inflection(x)) ), because we want to sample points in the concave region.
            b = X(i-1)
        else
            b = one
        end if
        X_max = BISECTION_QP(delta_i, a, b)
        tmp = Y(i) - last_Y*(X_max - X(i))
        tmp = tmp - pdf(X_max)
        w(i-1) = abs( tmp ) / dY(i-1)
    end do
    !
    iE_min = zer
    do i=1, i_inflection-1
                       tmp = ceiling( w(i)*imax_qp, int64 )
                       if(iE_min < tmp) iE_min = tmp
    end do
    !
    iE_max = zer
    do i=i_inflection, size(w)
                       tmp = ceiling( w(i)*imax_qp, int64 )
                       if(iE_max < tmp) iE_max = tmp
    end do
    !
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !
    X = X/imax_qp
    Y = Y/imax_qp
    !
    OPEN(NEWUNIT=I, FILE='normal_layers.txt', STATUS='REPLACE', ACTION='WRITE')
    WRITE(I, '(A, I0)'              ) "iE_min   = ", iE_min
    WRITE(I, '(A, I0)'              ) "iE_max   = ", iE_max
    WRITE(I, '(A, *(I0,     :", "))') "iE       = ", ceiling( w*imax_qp, int64 )
    WRITE(I, '(A, *(ES27.20,:", "))') "nrm_X    = ", X
    WRITE(I, '(A, *(ES27.20,:", "))') "nrm_Y    = ", Y
    WRITE(I, '(A, *(I0,:", "))')      "nrm_map  = ", imap
    WRITE(I, '(A, *(I0,:", "))')      "nrm_ipmf = ", ipmf
    CLOSE(I)
    !
    CONTAINS
    !vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
    pure function pdf(x) result(y)          !
      REAL(qp), intent(in):: x              !
      REAL(qp):: y                          !
      y = exp(-0.5_qp*x*x)                  !
    end function                            !
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    pure function cdf(x) result(y)          !
      REAL(qp), intent(in):: x              !
      REAL(qp):: y                          !
      y = alpha*erf(sqrt_half*x)            !
    end function                            !
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    PURE REAL(qp) FUNCTION gen_X(x)         !
      REAL(qp), intent(in):: x              !
      gen_X = x*(pdf(x) - last_Y) - volume  !
    END FUNCTION                            !
    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
    PURE REAL(qp) FUNCTION delta_i(x)       !
      REAL(qp), intent(in):: x              !
      delta_i = x*pdf(x) - last_Y           !
    END FUNCTION                            !
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^!
  END SUBROUTINE 
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  ! Solves for Alias Method arrays
  ! 
  pure subroutine realign_pmf(v, pmf, imap )
    REAL(qp), dimension(:), allocatable, intent(in   ):: v
    REAL(qp), dimension(:), allocatable, intent(  out):: pmf
    INTEGER,  dimension(:), allocatable, intent(  out):: imap
    REAL(qp), dimension(:), allocatable:: X
    INTEGER, dimension(:), allocatable:: p
    REAL(qp):: pmf_mean_inv, one
    INTEGER:: dim, i, j
    one = 1.0_qp
    dim = size(v)
    ALLOCATE(imap(dim), p(dim+1), X(dim+1))
    do i=1, dim
        imap(i) = i
        p(i) = i
    end do
    p(dim+1) = dim+1
    !
    ALLOCATE(pmf, source=v)
    pmf_mean_inv = real(size(pmf),qp)/sum(pmf)
    pmf = pmf * pmf_mean_inv
    !
    X(1:dim) = pmf 
    X(dim+1) = 2.0_qp
    !
    i = 1
    j = dim
    do while ( X(p(i)) <  one )   ! Note X(dim+1) = 2.0_qp, so i maxes out at dim
                       i = i + 1
    end do
    do while ( X(p(j)) >= one .AND. j > 1)
                       j = j - 1
    end do
    DO WHILE ( i < j )
        call swap(dim, p, i, j)
        do while ( X(p(i)) <  one )
                           i = i + 1
        end do
        do while ( X(p(j)) >= one .AND. j > 1)
                           j = j - 1
        end do
    END DO
    i = j
    j = i + 1
    !
    ! p now partitions x such that x(p(:j-1)) < 1 and x(p(j:)) > 1
    !
    DO WHILE ( i > 0 )
        do while ( X(p(j)) <=  one )      ! Find x_(b_j) that needs probability mass
                           j = j + 1
        end do
        if( j > dim ) exit                ! No more mass to find
        !
        X(p(j)) = X(p(j)) - (1 - X(p(i))) ! Send all of x_(b_i) excess probability mass to x_(b_j)                (x_(b_i) is now done).
        imap(p(i)) = p(j)
        !
        if( X(p(j)) < one ) then           ! If x_(b_j) now has too much probability mass, 
            call swap(dim, p, i, j)        ! Swap, b_i, b_j, it becomes a donor.
            j = j + 1
        else                               ! Otherwise, leave it as an acceptor
            i = i - 1
        end if
    END DO
    !
    pmf = x(1:dim)
    !  
    do i=1, dim  !Check for proper balancing
       x(imap(i)) = x(imap(i)) + one - pmf(i) 
    end do
    do i=1, dim
            if(abs(X(i) - v(i)) > 1e-24_qp) then
               error stop 'abs(X(i) - pmf(i) > 1e-24_qp' 
            end if
    end do
    !
  end subroutine
  !########################################################################################
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  !########################################################################################
  pure subroutine swap(dim, x, i, j)
    INTEGER,                 intent(in   ):: dim, i, j
    INTEGER, dimension(dim), intent(inout):: x
    INTEGER:: tmp
    tmp  = x(i)
    x(i) = x(j) 
    x(j) = tmp
  end subroutine
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  ! Solves for x given F(x) = 0 using secant method
  !
  pure subroutine grow(x, dim, null_value)
    REAL(qp), dimension(:), allocatable, intent(inout):: x
    INTEGER,                   optional, intent(inout):: dim
    REAL(qp),                  optional, intent(in   ):: null_value
    INTEGER:: siz
    !
    if(ALLOCATED(x)) then 
                     siz = 2*size(x)
    else
                     siz = 64
    end if
    call resize(x, siz, null_value)
    !
    IF(present(dim)) dim = size(x)
    !
  end subroutine
  !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  pure subroutine resize(x, dim, null_value)
    REAL(qp), dimension(:), allocatable, intent(inout):: x
    INTEGER,                             intent(in   ):: dim
    REAL(qp),                  optional, intent(in   ):: null_value
    REAL(qp), dimension(:), allocatable:: tmp                 ! auto-deallocates on subroutine exit
    !
    IF    (.not. ALLOCATED(x) ) then
                  if(present(null_value)) then
                                ALLOCATE(x(dim), source=null_value)
                  else          
                                ALLOCATE(x(dim))
                  end if        
    ELSEIF( dim < size(x) ) THEN
                                ALLOCATE(tmp(dim), source=x(1:dim))
                                CALL MOVE_ALLOC(tmp, x)
    ELSEIF( dim > size(x) ) THEN
                                CALL MOVE_ALLOC(x, tmp)
                                ALLOCATE(x(dim))
                                x(1:size(tmp)) = tmp
                                if(present(null_value)) x(size(tmp)+1:dim) = null_value
    END IF
    !
  end subroutine
  !
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  !@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  ! Solves for x given F(x) = 0 using secant method
  !
  PURE FUNCTION BISECTION_QP(func, a, b, tol) result(ans)  ! a<b and sign(f(a)) /= sign(f(b))
    ABSTRACT INTERFACE
       pure function funcQP(x) result(y)
                import:: qp
                real(qp), intent(in):: x
                real(qp):: y
       end function
    END INTERFACE
    procedure(funcQP)             :: func
    real(qp),           intent(in):: a, b
    real(qp), optional, intent(in):: tol
    real(qp):: ans
    real(qp):: x1, x2, f1, f2
    real(qp):: Ftol
    integer:: mxiter, i
    !
    if(present(tol)) then
        Ftol = tol
    else
        Ftol = 1e-24_qp
    end if
    !
    if( a < b ) then                            ! Note x1 < x2
                x1 = a;  x2 = b
    else
                x1 = b;  x2 = a
    end if
    f1 = func(x1)
    f2 = func(x2)
    !
    if(f2*f1 > 0.0_qp) then  !x2 and x1 are not bounded by a root
       ans = HUGE(ans)
       return
    end if
    !
    ans = (x2-x1)/Ftol
    mxiter = CEILING( log(ans)/log(2.0_qp) )  ! Get Log2 of number of calls - Max number of bisections possible
    !
    do i=1, mxiter
         ans = (x1 + x2) * 0.5_qp
         f2  = func(ans)
         !
         if( abs(f2) < Ftol )  return
         !
         if(f2*f1 < 0.0_qp) then
             x2 = ans
        else
             x1 = ans
             f1 = f2
        end if
    end do
    !
    ans = HUGE(ans)
    !
  END FUNCTION 
END MODULE 
