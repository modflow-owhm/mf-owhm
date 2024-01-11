! Program uses GNU gfortran extension EXIT 
!   to have an immediate termination of the program with status number.
!   https://gcc.gnu.org/onlinedocs/gcc-4.7.0/gfortran/EXIT.html
!
! For portability, this can be changed to STOP
!  For example:
!              "call EXIT(66)"  can be changed to 
!              "STOP 66      "
!
! Default operation is to only check files stored in utest  (base unit tests)
!   If command line argument 
!       -a                   or 
!       --all
!   then the problem will also check the etest files (extra files)
!
! This program compares the "_CumHCHG.txt" files in the output and output-true directories.
!   These files are a cumulative positive sum of all head changes in the example model.
!   This provides a poor-man's hash for checking the similarity of the same example, 
!      when run against code changes or different compiler options.
!   
! This program contains three different tolerances for determining 
!   if an example is assumed to pass the unit test.
! HTOL -> Represents a near perfect fit within the tolerance of floating point numbers.
!         If the comparison is less than this ammount, it will pass the unit test and say PERFECT.
! PTOL -> Is the passing tolerance for the base unit tests (utest).
!           PTOL=0.3 was found to be sufficient to pass the unit tests for the same code,
!           but run with different compiler options (-debug, -O2, -O3) and operating systems (Win, Nix) and 
! ETOL -> Is the tolerance used for the extended tests.
!           Only applies when program is run with the "--all" option.
!           These tests have use all the advance packages (SWR, SFR, UZF, NWT, and FMP)
!           And do not meet the convergence criteria for all the time steps, but have sufficiently small mass balances.
!           Due to this, the CumHCHG tends to have  wider variability despite returning the same solution.
!           It was found that ETOL = 10.0 ensures the extended tests pass for different compiler options and operating systems 
!
program validate_example_results
USE, INTRINSIC:: IEEE_ARITHMETIC, ONLY: IEEE_VALUE, IEEE_QUIET_NAN
implicit none
DOUBLE PRECISION, PARAMETER:: HTOL = 0.02d0   ! Cumulative change tolerance (CumHCHG) High Tolerance
DOUBLE PRECISION, PARAMETER:: PTOL = 0.30d0   ! Cumulative change tolerance (CumHCHG) Passing Tolerance
DOUBLE PRECISION, PARAMETER:: ETOL = 10.0d0   ! Cumulative change tolerance (CumHCHG) for extended problems
character(128), dimension(60):: utest
character( 64), dimension( 2):: etest  ! extra tests not meant for validation due long runtimes.
character(16):: arg1
integer:: i, n, iu1, iu2
double precision:: ans1, ans2
logical:: passed
passed = .TRUE.

utest( 1) = "../mf-2005/output/bcf2ss_CumHCHG.txt"
utest( 2) = "../mf-2005/output/etsdrt_CumHCHG.txt"
utest( 3) = "../mf-2005/output/fhb_CumHCHG.txt"
utest( 4) = "../mf-2005/output/ibs2k_CumHCHG.txt"
utest( 5) = "../mf-2005/output/l1a2k_CumHCHG.txt"
utest( 6) = "../mf-2005/output/l1b2k_bath_CumHCHG.txt"
utest( 7) = "../mf-2005/output/l1b2k_CumHCHG.txt"
utest( 8) = "../mf-2005/output/mnw1_CumHCHG.txt"
utest( 9) = "../mf-2005/output/MNW2-Fig28_CumHCHG.txt"
utest(10) = "../mf-2005/output/restest_CumHCHG.txt"
utest(11) = "../mf-2005/output/str_CumHCHG.txt"
utest(12) = "../mf-2005/output/swtex4_CumHCHG.txt"
utest(13) = "../mf-2005/output/tc2hufv4_CumHCHG.txt"
utest(14) = "../mf-2005/output/test1ss_CumHCHG.txt"
utest(15) = "../mf-2005/output/test1tr_CumHCHG.txt"
utest(16) = "../mf-2005/output/testsfr2_CumHCHG.txt"
utest(17) = "../mf-2005/output/testsfr2_tab_CumHCHG.txt"
utest(18) = "../mf-2005/output/tr2k_s3_CumHCHG.txt"
utest(19) = "../mf-2005/output/twrihfb_CumHCHG.txt"
utest(20) = "../mf-2005/output/twrip_CumHCHG.txt"
utest(21) = "../mf-2005/output/twri_CumHCHG.txt"
utest(22) = "../mf-2005/output/UZFtest2_CumHCHG.txt"
utest(23) = "../mf-2005-nwt/output/etsdrt_CumHCHG.txt"
utest(24) = "../mf-2005-nwt/output/swtex4_CumHCHG.txt"
utest(25) = "../mf-2005-nwt/output/test1ss_CumHCHG.txt"
utest(26) = "../mf-2005-nwt/output/test1tr_CumHCHG.txt"
utest(27) = "../mf-2005-nwt/output/testsfr2_CumHCHG.txt"
utest(28) = "../mf-2005-nwt/output/testsfr2_tab_CumHCHG.txt"
utest(29) = "../mf-2005-nwt/output/twrip_CumHCHG.txt"
utest(30) = "../mf-2005-nwt/output/UZFtest2_CumHCHG.txt"
utest(31) = "../mf-nwt/output/Pr1aMFNWT_CumHCHG.txt"
utest(32) = "../mf-nwt/output/Pr1bMFNWT_CumHCHG.txt"
utest(33) = "../mf-nwt/output/Pr2MFNWT_CumHCHG.txt"
utest(34) = "../mf-nwt/output/Pr3_MFNWT_higher_CumHCHG.txt"
utest(35) = "../mf-nwt/output/Pr3_MFNWT_lower_CumHCHG.txt"
utest(36) = "../mf-nwt/output/swi2ex4sww_CumHCHG.txt"
utest(37) = "../mf-rip/output/RIP-ET_EX1_CumHCHG.txt"
utest(38) = "../mf-swi/output/swi2ex1_CumHCHG.txt"
utest(39) = "../mf-swi/output/swi2ex2_cont_CumHCHG.txt"
utest(40) = "../mf-swi/output/swi2ex2_strat_CumHCHG.txt"
utest(41) = "../mf-swi/output/swi2ex3_CumHCHG.txt"
utest(42) = "../mf-swi/output/swi2ex4_2d_CumHCHG.txt"
utest(43) = "../mf-swi/output/swi2ex4_2d_sww_CumHCHG.txt"
utest(44) = "../mf-swi/output/swi2ex5_CumHCHG.txt"
utest(45) = "../mf-swi/output/swi2ex6_1_CumHCHG.txt"
utest(46) = "../mf-swi/output/swi2ex6_2_CumHCHG.txt"
utest(47) = "../mf-swi/output/swi2ex6_3_0.005_CumHCHG.txt"
utest(48) = "../mf-swi/output/swi2ex6_3_0.010_CumHCHG.txt"
utest(49) = "../mf-swi/output/swi2ex6_3_0.100_CumHCHG.txt"
utest(50) = "../mf-swi/output/swi2ex6_3_1.000_CumHCHG.txt"
utest(51) = "../mf-swr/output/SWRSample01.01min_CumHCHG.txt"
utest(52) = "../mf-swr/output/SWRSample01_CumHCHG.txt"
utest(53) = "../mf-swr/output/SWRSample02_CumHCHG.txt"
utest(54) = "../mf-swr/output/SWRSample03_CumHCHG.txt"
utest(55) = "../mf-swr/output/SWRSample04_CumHCHG.txt"
utest(56) = "../mf-swr/output/SWRSample05_CumHCHG.txt"

utest(57) = "../mf-cfp/cfp_mode1_benchmark_examples/exch/output/cfp_bench_exch_CumHCHG.txt"
utest(58) = "../mf-cfp/cfp_mode1_benchmark_examples/q_lam/output/cfp_bench_q_lam_CumHCHG.txt"
utest(59) = "../mf-cfp/cfp_mode1_benchmark_examples/q_turb/output/cfp_bench_q_turb_CumHCHG.txt"
utest(60) = "../mf-cfp/cfp_mode1_example/output/cfp_mode1_CumHCHG.txt"
!utest(61) = "../mf-cfp/cfp_mode2_example/output/CumHCHG.txt"
!utest(62) = "../mf-cfp/cfp_mode3_example/output/CumHCHG.txt"

write(*,'(/,1x, A, 28x, 2x, A)') "File", "Test"
write(*,'(A)') REPEAT('-', 45)

n = size(utest)
do i=1, n
   write(*,'(1x, A32, 2x)', advance="NO") testFile(utest(i))
   !
   iu1  = opener(utest(i))
   iu2  = opener(truePath(utest(i)))
   ans1 = getResult(iu1)
   ans2 = getResult(iu2)
   !
   call valCHECK(ans1, ans2, iu1, iu2)
   !
end do

! Extra tests not meant for validation due long runtimes.
etest(1) = "../mf-owhm-v1/output/owhm_v1_example_a_CumHCHG.txt"
etest(2) = "../mf-owhm-v1/output/owhm_v1_example_b_CumHCHG.txt"
!
! Only process the results of extra tests
! when given the argument:
! -a       or
! --all
CALL GET_COMMAND_ARGUMENT(1, arg1) 
n = LEN_TRIM(arg1)
if( n > 0) then
   if (ArgCheck(n, arg1)) THEN
      write(*,'(/,/,1x, A, 28x, 2x, A)') "File", "Test"
      write(*,'(A)') REPEAT('-', 45)
      n = SIZE(etest)
      do i=1, n
         write(*,'(1x, A32, 2x)', advance="NO") testFile(etest(i))
         !
         iu1  = opener(etest(i))
         iu2  = opener(truePath(etest(i)))
         ans1 = getResult(iu1)
         ans2 = getResult(iu2)
         !
         call valCHECK_Extended(ans1, ans2, iu1, iu2)
         !
      end do
   end if
end if
write(*,'(/,/,/,A)') ""
! GNU Extension 
!   to return error code of either fail or pass
if( .not. passed) call EXIT(66)
call EXIT()

CONTAINS
   !
   function opener(filename) result(iu)
      implicit none
      character(*), intent(in):: filename
      integer:: iu
      integer:: ierr
      !
      open(newunit=iu, file=filename, status='OLD', action='READ', position='REWIND', iostat=ierr)
      !
      if(ierr /= 0) iu = 0
      !
   end function
   !
   function truePath(filename) result(path)
      implicit none
      character(*), intent(in):: filename
      character(len(filename)):: path
      integer:: i
      !
      i = index(filename,'/output/')
      !
      if (i == 0) call EXIT(69)
      !
      i = i + 6
      path=filename(:i)//"-true"//TRIM(filename(i+1:))
      !
   end function
   !
   function testFile(filename) result(path)
      implicit none
      character(*), intent(in):: filename
      character(32):: path
      integer:: i
      !
      i = index(filename,'/output/')
      !
      if (i == 0) call EXIT(69)
      !
      i = i + 7
      path=TRIM(filename(i+1:))
      !
   end function
   !
   function getResult(iu) result(ans)
      implicit none
      integer, intent(in):: iu
      double precision:: ans
      character(16):: num
      integer:: ierr
      !
      if( iu == 0 ) then
                    ans = IEEE_VALUE(ans, IEEE_QUIET_NAN)
                    return
      end if
      !
      rewind(iu)
      read(iu,'(A)', iostat=ierr) num  ! Line 1, skip
      read(iu,'(A)', iostat=ierr) num  ! Line 2, read in cumulative head change number
      !
      if(ierr == 0) read(num,*,iostat=ierr) ans
      !
      if(ierr /= 0) ans = IEEE_VALUE(ans, IEEE_QUIET_NAN)
      !
   end function
   !
   subroutine writeFAIL(iu1,iu2,note)
      integer,      intent(in):: iu1,iu2
      character(*), intent(in):: note
      integer:: ierr
      write(*,'(A, 11x, A)') '    FAIL', note
      if( iu1 /= 0) close(iu1, iostat=ierr)
      if( iu2 /= 0) close(iu2, iostat=ierr)
      passed = .FALSE.
   end subroutine
   !
   subroutine writePASSPerfect(iu1,iu2)
      integer, intent(in):: iu1,iu2
      integer:: ierr
      write(*,'(A)') 'PASS     -> Perfect'
      close(iu1, iostat=ierr)
      close(iu2, iostat=ierr)
   end subroutine
   !
   subroutine writePASS(iu1,iu2)
      integer, intent(in):: iu1,iu2
      integer:: ierr
      write(*,'(A)') 'PASS'
      close(iu1, iostat=ierr)
      close(iu2, iostat=ierr)
   end subroutine
   !
   subroutine valCHECK(ans1, ans2, iu1, iu2)
      double precision, intent(in):: ans1, ans2
      integer,          intent(in):: iu1, iu2
      double precision:: cmp
      !
      if    ( iu1 == 0 ) then
          call writeFAIL(iu1,iu2,"->Test result file not found")
      elseif( iu2 == 0 ) then
          call writeFAIL(iu1,iu2,"->True result file not found")
      elseif( ans1 /= ans1 ) then
          call writeFAIL(iu1,iu2,"->Test result value read error")
      elseif( ans2 /= ans2 ) then
          call writeFAIL(iu1,iu2,"->True result value read error")
      else
          cmp = abs( ans1 - ans2 )
          if (cmp < HTOL) then
              call writePASSPerfect(iu1,iu2)
          elseif (cmp < PTOL) then
              call writePASS(iu1,iu2)
          else
              call writeFAIL(iu1,iu2,"->CumHCHG difference: "//dble2str(cmp)// &
                                     " with True CumHCHG: "//dble2str(ans2))
          end if
      end if
      !
   end subroutine
   !
   subroutine valCHECK_Extended(ans1, ans2, iu1, iu2)
      double precision, intent(in):: ans1, ans2
      integer,          intent(in):: iu1, iu2
      double precision:: cmp
      !
      if    ( iu1 == 0 ) then
          call writeFAIL(iu1,iu2,"->Test result file not found")
      elseif( iu2 == 0 ) then
          call writeFAIL(iu1,iu2,"->True result file not found")
      elseif( ans1 /= ans1 ) then
          call writeFAIL(iu1,iu2,"->Test result file read error")
      elseif( ans2 /= ans2 ) then
          call writeFAIL(iu1,iu2,"->True result file read error")
      else
          cmp = abs( ans1 - ans2 )
          if (cmp < PTOL) then
              call writePASSPerfect(iu1,iu2)
          elseif (cmp < ETOL) then
              call writePASS(iu1,iu2)
          else
              call writeFAIL(iu1,iu2,"->Test and True difference: "//dble2str(cmp))
          end if
      end if
      !
   end subroutine
   !
   function ArgCheck(n, arg) result(ans)          ! gfotran using ADJUSTL raises "internal compiler error: Segmentation fault"
      implicit none
      integer,      intent(in):: n
      character(n), intent(in):: arg
      character:: TAB
      logical:: ans
      integer:: i
      TAB = ACHAR(9)
      i   = 1
      DO WHILE( i <= n )
                IF(arg(i:i) /= TAB .AND. arg(i:i) /= ' ' .AND. arg(i:i) /= '-') EXIT
                i = i+1
      END DO
      ans = .FALSE.
      IF( i <= n ) ans = arg(i:i) == 'a' .OR. arg(i:i) == 'A'
      !
   end function
   !
   function dble2str(val) result(str)
     use, intrinsic:: iso_fortran_env, only: real64
     real(real64),      intent(in) :: val
     character(:),     allocatable :: str
     real(real64):: tmp
     logical :: val10_chk, val1c_chk, val1k_chk, val100k_chk
     real(real64):: tol, one, neg
     character(16):: num                                         ! Largest possible number is 14 characters
     !
     num=''
     !
     if(val /= val) then  ! NaN never equals itself if IEEE float
         num='nan'
     elseif(val >= HUGE(one)) then
         num = 'inf'
     elseif(val <= -HUGE(one)) then
         num = '-inf'
     elseif(val == 0.0_real64) then
         num = '0.0'
     elseif(val>=1.e100_real64 .or. val<=-1.e100_real64) then
        write(num,'(es16.7e3)') val
     elseif(val>=1.e10_real64  .or. val<=-1.e10_real64) then
        write(num,'(es16.7e2)') val
     elseif(val>=1.e6_real64   .or. val<=-1.e6_real64) then
        write(num,'(es16.7e1)') val
     else !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~v
        !
        tol= 1.e-13_real64
        one= 1.0_real64
        neg= -1.0_real64
        tmp =     10._real64*val;   val10_chk = abs(tmp - anint(tmp)) < tmp * tol  .and. (tmp>=one .or. tmp<=neg)
        tmp =    100._real64*val;   val1c_chk = abs(tmp - anint(tmp)) < tmp * tol  .and. (tmp>=one .or. tmp<=neg)
        tmp =   1000._real64*val;   val1k_chk = abs(tmp - anint(tmp)) < tmp * tol  .and. (tmp>=one .or. tmp<=neg)
        tmp = 100000._real64*val; val100k_chk = abs(tmp - anint(tmp)) < tmp * tol  .and. (tmp>=one .or. tmp<=neg)
        !
        if( val10_chk ) then
           write(num,'(f16.1)') val
        elseif( val1c_chk ) then
           write(num,'(f16.2)') val
        elseif( val1k_chk ) then
           write(num,'(f16.3)') val
        elseif(val100k_chk .or. val>=100._real64 .or. val<=-100._real64 ) then
           write(num,'(f16.5)') val
        elseif(val>=0.00099_real64 .or. val<=-0.00099_real64 ) then
           write(num,'(f16.7)') val
        elseif(val>=1.e-9_real64 .or. val<=-1.e-9_real64) then
           write(num,'(es16.7e1)') val
        elseif(val>=1.e-99_real64 .or. val<=-1.e-99_real64) then
           write(num,'(es16.7e2)') val
        else
           write(num,'(es16.7e3)') val
        end if
        !
     end if !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~^
     !
     str = trim(adjustl(num))
     !
   end function
end program