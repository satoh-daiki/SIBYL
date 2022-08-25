! #####################################################################
! This module contains the following subroutines:
!   - INIT_TIMER()              : initialize()
!   - START_TIMER(cstring)      : start(cstring)
!   - WRITE_TIMER()             : write_time()
!   - WRITE_TIMER(SPLIT)        : write_split_time(idummy)
!   - WRITE_TIMER(SPLIT, irank) : write_split_time_rank(idummy, irank)
!   - WRITE_TIMER(cstring)      : write_string_time(cstring)
!   - GET_TIME(x)
! #####################################################################

! =====================================================================
module modtime
  use commondata, only: myrank
! ---------------------------------------------------------------------
  implicit none
! ---------------------------------------------------------------------
  integer, parameter :: SPLIT = 0
! ---------------------------------------------------------------------
  real(8) :: start_time
  real(8), save :: pre_time
  real(8), save :: current_time
  real(8), save :: pre_split_time
! ---------------------------------------------------------------------

!$omp threadprivate(pre_time, current_time, pre_split_time)

interface INIT_TIMER
  module procedure initialize
end interface

interface START_TIMER
  module procedure start
end interface

interface WRITE_TIMER
  module procedure write_time
  module procedure write_split_time
  module procedure write_split_time_rank
  module procedure write_string_time
end interface


contains
  subroutine initialize()
    call GET_TIME(start_time)
    pre_time = start_time
    pre_split_time = start_time
    return
  end subroutine

  subroutine start(cstring)
    character(*) :: cstring
    if (myrank .eq. 0) then
      write(*,'(a)') trim(cstring)
    endif
    call GET_TIME(pre_time)
    pre_split_time = pre_time
    return
  end subroutine

  subroutine write_time()
    call GET_TIME(current_time)
    if (myrank .eq. 0) then
      write(*,'(2f10.2)') current_time-pre_time, current_time-start_time
    endif
    return
  end subroutine

  subroutine write_split_time(idummy)
    integer :: idummy
    call GET_TIME(current_time)
    if (myrank .eq. 0) then
      write(*,'(2f10.2)') current_time-pre_split_time, current_time-start_time
    endif
    pre_split_time = current_time
    return
  end subroutine

  subroutine write_split_time_rank(idummy, irank)
    integer :: idummy, irank
    call GET_TIME(current_time)
#ifdef DEBUG
    write(*,'(2f10.2)',advance='no') current_time-pre_split_time, current_time-start_time
    write(*,'(A2,I5)')' @',irank
#else
    write(*,'(f10.2,A,f10.2,A)') current_time-pre_split_time, '  (', current_time-start_time, ')'
#endif
    pre_split_time = current_time
    return
  end subroutine

  subroutine write_string_time(cstring)
    character(*) :: cstring
    call GET_TIME(current_time)
    if (myrank .eq. 0) then
      write(*,'(a,2f10.2)') trim(cstring), current_time-pre_time, current_time-start_time
    endif
    return
  end subroutine

  subroutine GET_TIME( x )
#ifdef USE_MPI
    use mpi
#endif
!$  use omp_lib
    real(8) :: x

#ifndef USE_MPI
!$  if (.true.) then
!$    x = omp_get_wtime()
!$  else
      call CPU_TIME( x )
!$  endif
#else
   x = MPI_Wtime()
#endif
   return
  end subroutine
! ---------------------------------------------------------------------
end module modtime
