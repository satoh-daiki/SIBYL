! #####################################################################
! This file contains the following subroutines:
!   - read_restart_data()
!   - write_restart_data()
! #####################################################################

! =====================================================================
subroutine read_restart_data(ierr)
! ---------------------------------------------------------------------
! version history, comments, ...
! =====================================================================
  use commondata
  use modtime
! ---------------------------------------------------------------------
  implicit none
! ---------------------------------------------------------------------
  integer, intent(out) :: ierr
! ---------------------------------------------------------------------
  integer :: i, j, k
  integer :: ierrflag
! ---------------------------------------------------------------------
  call START_TIMER( 'READ DATA for RESTART...' )

#ifndef USE_MPI_SPMD
  crestartdat = trim(cdirname)//'/'//trim(cfnamedat)
#else
  write(crestartdat,'(A,A,A,A,I5.5)')trim(cdirname),'/',trim(cfnamedat),'.',myrank
#endif

#ifdef USE_MPI_MS
  if (myrank .eq. 0) then
#endif
  ierrflag = 0
  open(unit=IFDRESDAT,file=trim(crestartdat),form='unformatted',access='stream',status='old', err=666)
  ierrflag = 1

  read(IFDRESDAT) &
 &(((ish(i,j,k),i=ns_x_sta_all,ns_x_end_all),j=ns_y_sta_all,ns_y_end_all),k=nsh_min,nsh_max),&
 &((ielev(i,j),i=nt_x_sta,nt_x_end),j=nt_y_sta,nt_y_end),&
 &(((din(i,j,k),i=ns_x_sta,ns_x_end),j=ns_y_sta,ns_y_end),k=ns_z_sta,ns_z_end),&
 &(((resp(i,j,k),i=nx_sta,nx_end),j=ny_sta,ny_end),k=ns_z_sta,ns_z_end)

  ierrflag = 2
666 continue
  ierr = 1
  if (ierrflag .eq. 2) then
    close(IFDRESDAT)
    ierr = 0
  else if (ierrflag .eq. 1) then
    close(IFDRESDAT)
    write(*,*) 'ERROR: read error: '//trim(crestartdat)
    ierr = 1
    return
  else
    write(*,*) 'ERROR: open error: '//trim(crestartdat)
    ierr = 1
    return
  endif
#ifdef USE_MPI_MS
  endif ! if(myrank.eq.0)
#endif

  call WRITE_TIMER( 'DONE.' )
  return
end subroutine read_restart_data


! =====================================================================
subroutine write_restart_data()
! ---------------------------------------------------------------------
! version history, comments, ...
! =====================================================================
  use commondata
  use modtime
! ---------------------------------------------------------------------
  implicit none
! ---------------------------------------------------------------------
! ---------------------------------------------------------------------
  integer :: i, j, k
  integer :: ierr
  integer :: ierrflag
! ---------------------------------------------------------------------
  call START_TIMER( 'WRITE DATA for RESTART...' )

#ifndef USE_MPI_SPMD
  crestartdat = trim(cdirname)//'/'//trim(cfnamedat)
#else
  write(crestartdat,'(A,A,A,A,I5.5)')trim(cdirname),'/',trim(cfnamedat),'.',myrank
#endif

#ifdef USE_MPI_MS
  if (myrank .eq. 0) then
#endif
  ierrflag = 0
  open(unit=IFDRESDAT,file=trim(crestartdat),form='unformatted',access='stream',status='replace', err=111)
  ierrflag = 1

  write(IFDRESDAT) &
 &(((ish(i,j,k),i=ns_x_sta_all,ns_x_end_all),j=ns_y_sta_all,ns_y_end_all),k=nsh_min,nsh_max),&
 &((ielev(i,j),i=nt_x_sta,nt_x_end),j=nt_y_sta,nt_y_end),&
 &(((din(i,j,k),i=ns_x_sta,ns_x_end),j=ns_y_sta,ns_y_end),k=ns_z_sta,ns_z_end),&
 &(((resp(i,j,k),i=nx_sta,nx_end),j=ny_sta,ny_end),k=ns_z_sta,ns_z_end)

  ierrflag = 2
111 continue
  ierr = 1
  if (ierrflag .eq. 2) then
    close(IFDRESDAT)
    ierr = 0
  else if (ierrflag .eq. 1) then
    close(IFDRESDAT)
    write(*,*) 'warning: write error: '//trim(crestartdat)
  else
    write(*,*) 'warning: open error: '//trim(crestartdat)
  endif
#ifdef USE_MPI_MS
  endif ! if(myrank.eq.0)
#endif

  call WRITE_TIMER( 'DONE.' )
  return
end subroutine write_restart_data

