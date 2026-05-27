! #####################################################################
! This file contains the following subroutines:
!   - check_restart()
!   - check_restart_dir()
!   - existence_check_restart_dir(ierr)
!   - create_restart_dir()
!   - rename_restart_dir()
!   - remove_restart_doses()  ### NOT IMPLEMENT ###
!   - remove_restart_dir()    ### NOT IMPLEMENT ###
! #####################################################################

! =====================================================================
subroutine check_restart()
! ---------------------------------------------------------------------
! version history, comments, ...
! =====================================================================
  use commondata
! ---------------------------------------------------------------------
  implicit none
! ---------------------------------------------------------------------
! ---------------------------------------------------------------------
  integer :: ierr
! ---------------------------------------------------------------------
#ifdef USE_MPI
  call MPI_Barrier(MPI_COMM_WORLD, ierr)
#endif
  if (myrank .eq. 0) then
    if (irestart .eq. 1) then
      write (*,'(A)')    '========================'
      write (*,'(A)')    'RESTART: yes'
      call existence_check_restart_dir(ierr)
      if (ierr .eq. 0) then
        call check_restart_parameter(ierr)
        if (ierr .ne. 0) then
          irestart = 0
          write (*,'(A)')    '  restart parameter not match input.data'
        endif
      else
        irestart = 0
        write (*,'(A)')    '  directory for restart not exist'
      endif
      if (irestart .eq. 0) then
        write(*,'(A)')'  warning: restart failed, execute calculation from initial state'
      endif
      write (*,'(A)')    '========================'
    endif
    if (irestart_out .eq. 0) then
      write (*,'(A)') '========================'
      write (*,'(A)') 'OUTPUT for RESTART: no'
      write (*,'(A)') '========================'
    endif
  endif ! myrank

#ifdef USE_MPI
  call MPI_Bcast(irestart, 1, MPI_INTEGER4, 0, MPI_COMM_WORLD, ierr)
#endif

  return
end subroutine check_restart


! =====================================================================
subroutine check_restart_dir(ierr)
! ---------------------------------------------------------------------
! version history, comments, ...
! =====================================================================
  use commondata
! ---------------------------------------------------------------------
  implicit none
! ---------------------------------------------------------------------
  integer, intent(out) :: ierr
! ---------------------------------------------------------------------
  integer :: i
! ---------------------------------------------------------------------

  if (myrank .eq. 0) then
    call existence_check_restart_dir(ierr)
    if (ierr .eq. 0) then
      call rename_restart_dir()
    endif
    call create_restart_dir(ierr)
  endif ! myrank

#ifdef USE_MPI
  call MPI_Bcast(ierr, 1, MPI_INTEGER4, 0, MPI_COMM_WORLD, i)
#endif

  return
end subroutine check_restart_dir


! =====================================================================
!subroutine existence_check_restart_dir(ierr)
! ---------------------------------------------------------------------
! version history, comments, ...
! =====================================================================
!  use commondata
!  use ifposix
! ---------------------------------------------------------------------
!  implicit none
! ---------------------------------------------------------------------
!  integer, intent(out) :: ierr
! ---------------------------------------------------------------------
!--  character(len=7) :: cdirname = 'RESTART'
!  integer :: irid, ierrtmp
! ---------------------------------------------------------------------
!
!  call PXFACCESS(trim(cdirname), len_trim(cdirname), 0, ierr)
!  if (ierr .eq. 0) then
!    call PXFOPENDIR(trim(cdirname), len_trim(cdirname), irid, ierr)
!    if (ierr .eq. 0) then
!      call PXFCLOSEDIR(irid, ierrtmp)
!      return
!    endif
!  endif
!
!  return
!end subroutine existence_check_restart_dir

! =====================================================================
subroutine existence_check_restart_dir(ierr)
! ---------------------------------------------------------------------
! version history, comments, ...
! =====================================================================
  use commondata
  use iso_c_binding, only: c_ptr, c_int, c_char, c_null_char, c_associated
! ---------------------------------------------------------------------
  implicit none
! ---------------------------------------------------------------------

  interface
      function access(path, mode) bind(C, name="access")
          import c_int, c_char
          integer(c_int) :: access
          character(kind=c_char), dimension(*), intent(in) :: path
          integer(c_int), value :: mode
      end function access

      type(c_ptr) function opendir(path) bind(C, name="opendir")
          import c_char, c_ptr
          character(kind=c_char), dimension(*), intent(in) :: path
      end function opendir

      subroutine closedir(dirp) bind(C, name="closedir")
          import c_ptr
          type(c_ptr), value :: dirp
      end subroutine closedir
  end interface

! ---------------------------------------------------------------------
  integer, intent(out) :: ierr
! ---------------------------------------------------------------------
  integer(c_int) :: ierrtmp
  type(c_ptr) :: irid
! ---------------------------------------------------------------------

  ierr = access(trim(cdirname)//c_null_char, 0)
  if (ierr == 0) then
      irid = opendir(trim(cdirname)//c_null_char)
      if (c_associated(irid)) then
          call closedir(irid)
          ierr = 0
          return
      else
          ierr = -1
      end if
  else
      ierr = -1
  end if

  return
end subroutine existence_check_restart_dir

! =====================================================================
!subroutine create_restart_dir(ierr)
! ---------------------------------------------------------------------
! version history, comments, ...
! =====================================================================
!  use commondata
!  use ifposix
! ---------------------------------------------------------------------
!  implicit none
! ---------------------------------------------------------------------
!  integer, intent(out) :: ierr
! ---------------------------------------------------------------------
!--  character(len=7) :: cdirname = 'RESTART'
! ---------------------------------------------------------------------
!
!  call PXFMKDIR(trim(cdirname), len_trim(cdirname), 493, ierr)
!
!  if (ierr .ne. 0) then
!    write(*,*)'error: directory create failed.'
!  endif
!
!  return
!end subroutine create_restart_dir

! =====================================================================
subroutine create_restart_dir(ierr)
! ---------------------------------------------------------------------
! version history, comments, ...
! =====================================================================
  use commondata
  use iso_c_binding, only: c_int, c_char, c_null_char
! ---------------------------------------------------------------------
  implicit none
! ---------------------------------------------------------------------
  integer, intent(out) :: ierr
! ---------------------------------------------------------------------
  interface
      function mkdir(path, mode) bind(C, name="mkdir")
          import c_int, c_char
          integer(c_int) :: mkdir
          character(kind=c_char), dimension(*), intent(in) :: path
          integer(c_int), value :: mode
      end function mkdir
  end interface

  ierr = mkdir(trim(cdirname)//c_null_char, 493)

  if (ierr .ne. 0) then
    write(*,*) 'error: directory create failed.'
  endif

  return
end subroutine create_restart_dir

! =====================================================================
!subroutine rename_restart_dir()
! ---------------------------------------------------------------------
! version history, comments, ...
! =====================================================================
!  use commondata
!  use ifposix
! ---------------------------------------------------------------------
!  implicit none
! ---------------------------------------------------------------------
! ---------------------------------------------------------------------
!  integer :: ierr
! ---------------------------------------------------------------------
!--  character(len=7) :: cdirname = 'RESTART'
!  character(len=150) :: cbakname
!  integer :: idatetime(8)
! ---------------------------------------------------------------------
!  call DATE_AND_TIME(values = idatetime)
!  write(cbakname,'(A,A4,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2)')trim(cdirname),'.bak',idatetime(1),idatetime(2),idatetime(3),idatetime(5),idatetime(6),idatetime(7)
!  call PXFRENAME(trim(cdirname), len_trim(cdirname), trim(cbakname), len_trim(cbakname), ierr)
!
!  if (ierr .ne. 0) then
!    write(*,*)'error: directory rename failed.'
!  endif
!
!  return
!end subroutine rename_restart_dir

! =====================================================================
subroutine rename_restart_dir()
! ---------------------------------------------------------------------
! version history, comments, ...
! =====================================================================
  use commondata
  use iso_c_binding, only: c_int, c_char, c_null_char
! ---------------------------------------------------------------------
  implicit none
! ---------------------------------------------------------------------
  integer :: ierr
! ---------------------------------------------------------------------
  character(len=150) :: cbakname
  integer :: idatetime(8)
! ---------------------------------------------------------------------
  interface
      function rename(oldpath, newpath) bind(C, name="rename")
          import c_int, c_char
          integer(c_int) :: rename
          character(kind=c_char), dimension(*), intent(in) :: oldpath, newpath
      end function rename
  end interface

  call DATE_AND_TIME(values = idatetime)
  write(cbakname,'(A,A4,I4.4,I2.2,I2.2,I2.2,I2.2,I2.2)') trim(cdirname), '.bak', idatetime(1), idatetime(2), idatetime(3), &
                                                            idatetime(5), idatetime(6), idatetime(7)

  ierr = rename(trim(cdirname)//c_null_char, trim(cbakname)//c_null_char)

  if (ierr .ne. 0) then
    write(*,*) 'error: directory rename failed.'
  endif

  return
end subroutine rename_restart_dir

! =====================================================================
subroutine remove_restart_doses()
! ---------------------------------------------------------------------
! version history, comments, ...
! ### NOT IMPLEMENT ###
! =====================================================================
  use commondata
!!!  use modtime
! ---------------------------------------------------------------------
  implicit none
! ---------------------------------------------------------------------
! ---------------------------------------------------------------------

  return
end subroutine remove_restart_doses


! =====================================================================
subroutine remove_restart_dir()
! ---------------------------------------------------------------------
! version history, comments, ...
! ### NOT IMPLEMENT ###
! =====================================================================
  use commondata
!!!  use modtime
! ---------------------------------------------------------------------
  implicit none
! ---------------------------------------------------------------------
! ---------------------------------------------------------------------

  return
end subroutine remove_restart_dir

