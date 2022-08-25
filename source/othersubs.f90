! #####################################################################
! This file contains the following subroutines:
!   - check_abort(ierr)
!   - set_index()
! #####################################################################

! =====================================================================
subroutine check_abort(ierr)
! ---------------------------------------------------------------------
! version history, comments, ...
! =====================================================================
  use commondata
! ---------------------------------------------------------------------
  implicit none
! ---------------------------------------------------------------------
  integer, intent(in) :: ierr
! ---------------------------------------------------------------------
  integer :: i, idatetime(8)
! ---------------------------------------------------------------------
  if (ierr .ne. 0) then
#ifdef USE_MPI
    call MPI_Finalize(i)
#endif
    if (myrank .eq. 0) then
      write (*,*) ''
      write (*,'(A)') 'PROGRAM ABORT.'
      call DATE_AND_TIME(values = idatetime)
      write (*,'(A,I4.4,A,I2.2,A,I2.2,X,I2.2,A,I2.2,A,I2.2,A)') '##### ',idatetime(1),'/',idatetime(2),'/',idatetime(3),idatetime(5),':',idatetime(6),':',idatetime(7),' #########################################'
    endif
    call termination
    stop
  endif
  return

  return
end subroutine check_abort


! =====================================================================
subroutine set_index()
! ---------------------------------------------------------------------
! version history, comments, ...
! =====================================================================
  use commondata
!!!  use modtime
! ---------------------------------------------------------------------
  implicit none
! ---------------------------------------------------------------------

#ifndef USE_MPI_SPMD
  idivx(0) = ns_x_sta - 1
  idivx(1) = ns_x_end
  imapxy(1,0) = ns_y_sta - 1
  imapxy(1,1) = ns_y_end
  npartx = ns_x_end - ns_x_sta + 1
  nparty = ns_y_end - ns_y_sta + 1
  ns_x_sta_all = ns_x_sta
  ns_x_end_all = ns_x_end
  ns_y_sta_all = ns_y_sta
  ns_y_end_all = ns_y_end
  ns_x_sta = max(ns_x_sta_all, nc_x_sta-int(nx/2.0d0))
  ns_x_end = min(ns_x_end_all, nc_x_end+int(nx/2.0d0))
  ns_y_sta = max(ns_y_sta_all, nc_y_sta-int(ny/2.0d0))
  ns_y_end = min(ns_y_end_all, nc_y_end+int(ny/2.0d0))
#else
  npartx = idivx(myxpos) - idivx(myxpos-1)
  nparty = imapxy(myxpos,myypos) - imapxy(myxpos,myypos-1)

  ns_x_sta_all = ns_x_sta
  ns_x_end_all = ns_x_end
  ns_y_sta_all = ns_y_sta
  ns_y_end_all = ns_y_end
  ns_x_sta = idivx(myxpos-1) + 1
  ns_x_end = idivx(myxpos)
  ns_y_sta = imapxy(myxpos, myypos-1) + 1
  ns_y_end = imapxy(myxpos, myypos)

#endif  # ndef USE_MPI

  nx_sta = ns_x_sta - nc_x_end + imid - 1
  if (nx_sta .lt. 1) nx_sta = 1
  nx_end = ns_x_end - nc_x_sta + imid + 1
  if (nx_end .gt. nx) nx_end = nx
  ny_sta = ns_y_sta - nc_y_end + imid - 1
  if (ny_sta .lt. 1) ny_sta = 1
  ny_end = ns_y_end - nc_y_sta + imid + 1
  if (ny_end .gt. ny) ny_end = ny

  if (nx_sta .gt. nx_end) then
    nx_sta = 1    ! 1 or nx
    nx_end = 1    ! 1 or nx
  endif
  if (ny_sta .gt. ny_end) then
    ny_sta = 1    ! 1 or ny
    ny_end = 1    ! 1 or ny
  endif

#ifdef DEBUG
  write(*,'(4(A,I5),A,I3)')' DEBUG: ns_x_sta:end[',ns_x_sta,':',ns_x_end,'], ns_y_sta:end[',ns_y_sta,':',ns_y_end,'] @',myrank
  write(*,'(4(A,I5),A,I3)')' DEBUG: nt_x_sta:end[',nt_x_sta,':',nt_x_end,'], nt_y_sta:end[',nt_y_sta,':',nt_y_end,'] @',myrank
  write(*,'(4(A,I5),A,I3)')' DEBUG: nc_x_sta:end[',nc_x_sta,':',nc_x_end,'], nc_y_sta:end[',nc_y_sta,':',nc_y_end,'] @',myrank
  write(*,'(A,I5,I5,I5,I5,A,I3)')' DEBUG: nx_s,e/ny_s,e=',nx_sta,nx_end,ny_sta,ny_end,' @',myrank
#endif

  return
end subroutine set_index

