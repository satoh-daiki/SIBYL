! #####################################################################
! This file contains the following subroutines:
!   - open_restart_doses_initial(iposy)
!   - open_restart_doses_restart(iposy)
!   - close_restart_doses()
!   - #NOT USE# write_restart_doses2(ix, iy)
!   - write_restart_doses3(ixs, ixe, iy)
!   - write_restart_doses4(ixs, ixe, iys, iye)
! #####################################################################

! =====================================================================
subroutine open_restart_doses_initial(iposy, ierr)
! ---------------------------------------------------------------------
! version history, comments, ...
! =====================================================================
  use commondata
! ---------------------------------------------------------------------
  implicit none
! ---------------------------------------------------------------------
  integer, intent(out) :: iposy, ierr
! ---------------------------------------------------------------------
  integer :: ierrflag
! ---------------------------------------------------------------------
!!!  call START_TIMER( '...' )

#ifdef USE_MPI_SPMD
  write(crestartpar,'(A,A,A,A,I5.5)')trim(cdirname),'/',trim(cfnamepos),'.',myrank
  write(crestartdat,'(A,A,A,A,I5.5)')trim(cdirname),'/',trim(cfnamedos),'.',myrank
#else
  crestartpar = trim(cdirname)//'/'//trim(cfnamepos)
  crestartdat = trim(cdirname)//'/'//trim(cfnamedos)
#endif

#ifdef USE_MPI_MS
  if (myrank .eq. 0) then
#endif
  ierrflag = 0
  ierr = 0
  open(unit=IFDRESPAR,file=trim(crestartpar),status='replace', err=999)
  ierrflag = 1
  open(unit=IFDRESDAT,file=trim(crestartdat),form='unformatted',access='stream',status='replace', err=999)
  ierrflag = 2
999 continue
  if (ierrflag .eq. 0) then
    write(*,*) 'ERROR: open error: '//trim(crestartpar)
    ierr = 1
  else if (ierrflag .eq. 1) then
    close(IFDRESPAR)
    write(*,*) 'ERROR: open error: '//trim(crestartdat)
    ierr = 1
  endif
#ifdef USE_MPI_MS
  endif ! if(myrank.eq.0)
  call MPI_Bcast(ierr, 1, MPI_INTEGER4, 0, MPI_COMM_WORLD, ierrflag)
#endif

  iposy = nc_y_sta

  return
end subroutine open_restart_doses_initial


! =====================================================================
subroutine open_restart_doses_restart(iposy, ierr)
! ---------------------------------------------------------------------
! version history, comments, ...
! =====================================================================
  use commondata
! ---------------------------------------------------------------------
  implicit none
! ---------------------------------------------------------------------
  integer, intent(out) :: iposy, ierr
! ---------------------------------------------------------------------
  integer :: iposx
  integer :: ierrflag
  integer :: i, j
  integer :: istat
! ---------------------------------------------------------------------

#ifdef USE_MPI_SPMD
  write(crestartpar,'(A,A,A,A,I5.5)')trim(cdirname),'/',trim(cfnamepos),'.',myrank
  write(crestartdat,'(A,A,A,A,I5.5)')trim(cdirname),'/',trim(cfnamedos),'.',myrank
#else
  crestartpar = trim(cdirname)//'/'//trim(cfnamepos)
  crestartdat = trim(cdirname)//'/'//trim(cfnamedos)
#endif

#ifdef USE_MPI_MS
  if (myrank .eq. 0) then
#endif
  ierrflag = 0
  ierr = 0
  open(unit=IFDRESPAR,file=trim(crestartpar),action='readwrite',status='old', err=888)
  ierrflag = 1
  open(unit=IFDRESDAT,file=trim(crestartdat),action='readwrite',form='unformatted',access='stream',status='old', err=888)
  ierrflag = 2
888 continue
  if (ierrflag .eq. 0) then
    write(*,*) 'warning: open error: '//trim(crestartpar)
    write(*,*) 'warning: restart failed, execute calculation from initial state'
    if (irestart_out .eq. 1) then
      ierrflag = 0
      open(unit=IFDRESPAR,file=trim(crestartpar),action='write',status='replace', err=777)
      ierrflag = 1
      open(unit=IFDRESDAT,file=trim(crestartdat),action='write',form='unformatted',access='stream',status='replace', err=777)
      ierrflag = 2
777   continue
      if (ierrflag .eq. 0) then
        write(*,*) 'ERROR: open error: '//trim(crestartpar)
        ierr = 1
      else if (ierrflag .eq. 1) then
        close(IFDRESPAR)
        write(*,*) 'ERROR: open error: '//trim(crestartdat)
        ierr = 1
      endif
    endif ! if(irestart_out)
    iposy = nc_y_sta
    return
  else if (ierrflag .eq. 1) then
    close(IFDRESPAR)
    write(*,*) 'warning: open error: '//trim(crestartdat)
    write(*,*) 'warning: restart failed, execute calculation from initial state'
    if (irestart_out .eq. 1) then
      ierrflag = 0
      ierr = 0
      open(unit=IFDRESPAR,file=trim(crestartpar),action='write',status='replace', err=666)
      ierrflag = 1
      open(unit=IFDRESDAT,file=trim(crestartdat),action='write',form='unformatted',access='stream',status='replace', err=666)
      ierrflag = 2
666   continue
      if (ierrflag .eq. 0) then
        write(*,*) 'ERROR: open error: '//trim(crestartpar)
        ierr = 1
      else if (ierrflag .eq. 1) then
        close(IFDRESPAR)
        write(*,*) 'ERROR: open error: '//trim(crestartdat)
        ierr = 1
      endif
    endif ! if(irestart_out)
    iposy = nc_y_sta
    return
  endif

  ierrflag = 0
  ierr = 0
  read(IFDRESPAR, *, iostat=istat, err=555, end=555) iposx, iposy

  ierrflag = 1
  do j=nc_y_sta, iposy
    do i=nc_x_sta, nc_x_end
      read(IFDRESDAT, iostat=istat, err=555, end=555) dout(i, j)
    enddo
  enddo
  iposy = iposy + 1
  ierrflag = 2

555 continue
  if (ierrflag .eq. 0) then
    close(IFDRESPAR)
    close(IFDRESDAT)
    write(*,*) 'ERROR: read error: '//trim(crestartpar)//': ',istat
    ierr = 1
  else if (ierrflag .eq. 1) then
    close(IFDRESPAR)
    close(IFDRESDAT)
    write(*,*) 'ERROR: read error: '//trim(crestartdat)//': ',istat
    ierr = 1
  else
    if (irestart_out .eq. 0) then
      close(IFDRESPAR)
      close(IFDRESDAT)
    endif
  endif
#ifdef USE_MPI_MS
  endif ! if(myrank.eq.0)
  call MPI_Bcast(ierr, 1, MPI_INTEGER4, 0, MPI_COMM_WORLD, ierrflag)
#endif

  return
end subroutine open_restart_doses_restart


! =====================================================================
subroutine close_restart_doses()
! ---------------------------------------------------------------------
! version history, comments, ...
! =====================================================================
  use commondata
! ---------------------------------------------------------------------
  implicit none
! ---------------------------------------------------------------------

#ifdef USE_MPI_MS
  if (myrank .eq. 0) then
#endif
  close(IFDRESDAT)
  close(IFDRESPAR)
#ifdef USE_MPI_MS
  endif ! if(myrank.eq.0)
#endif

  return
end subroutine close_restart_doses


! =====================================================================
subroutine write_restart_doses3(ixs, ixe, iy)
! ---------------------------------------------------------------------
! version history, comments, ...
! =====================================================================
  use commondata
!!!  use modtime
! ---------------------------------------------------------------------
  implicit none
! ---------------------------------------------------------------------
  integer, intent(in) :: ixs, ixe, iy
! ---------------------------------------------------------------------
  integer :: istat
! ---------------------------------------------------------------------

  write(IFDRESDAT, iostat=istat, err=111) dout(ixs:ixe, iy)
  rewind(IFDRESPAR)
  write(IFDRESPAR, *, iostat=istat, err=112) ixe, iy

  return

111 continue
  write(*,*)'ERROR: @write_restart_doses: dose',istat
  call termination
  stop
112 continue
  write(*,*)'ERROR: @write_restart_doses: pos',istat
  call termination
  stop
end subroutine write_restart_doses3


! =====================================================================
subroutine write_restart_doses4(ixs, ixe, iys, iye)
! ---------------------------------------------------------------------
! version history, comments, ...
! =====================================================================
  use commondata
! ---------------------------------------------------------------------
  implicit none
! ---------------------------------------------------------------------
  integer, intent(in) :: ixs, ixe, iys, iye
! ---------------------------------------------------------------------
  integer :: i
  integer :: istat
! ---------------------------------------------------------------------

  do i=iys, iye
    write(IFDRESDAT, iostat=istat, err=333) dout(ixs:ixe, i)
  enddo
  rewind(IFDRESPAR)
  write(IFDRESPAR, *, iostat=istat, err=334) ixe, iye

  return

333 continue
  write(*,*)'ERROR: @write_restart_doses: dose',istat
  call termination
  stop
334 continue
  write(*,*)'ERROR: @write_restart_doses: pos',istat
  call termination
  stop
end subroutine write_restart_doses4

