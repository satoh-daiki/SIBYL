! #####################################################################
! This file contains the following subroutines:
!   - check_restart_parameter(ierr)
!   - write_restart_parameter()
! #####################################################################

! =====================================================================
subroutine check_restart_parameter(ierr)
! ---------------------------------------------------------------------
! version history, comments, ...
! =====================================================================
  use commondata
! ---------------------------------------------------------------------
  implicit none
! ---------------------------------------------------------------------
  integer, intent(out) :: ierr
! ---------------------------------------------------------------------
! ---------------------------------------------------------------------
  character(len=10) :: cdummy
! ---------------------------------------------------------------------
  integer :: ierrflag
! ---------------------------------------------------------------------
! params
! ---------------------------------------------------------------------
  integer :: imode_chk
  real(8) :: att_chk, den_chk
  character(len=128) :: file_chk
! ---------------------------------------------------------------------
! RESOLUTION
! ---------------------------------------------------------------------
  integer :: irs_chk   ! mesh resolution (m)
! ---------------------------------------------------------------------
! SOURCE REGION
! ---------------------------------------------------------------------
  integer :: ns_x_sta_chk, ns_x_end_chk
  integer :: ns_y_sta_chk, ns_y_end_chk
  integer :: ns_z_sta_chk, ns_z_end_chk
! ---------------------------------------------------------------------
! TARGET REGION
! ---------------------------------------------------------------------
  integer :: nt_x_sta_chk, nt_x_end_chk
  integer :: nt_y_sta_chk, nt_y_end_chk
  integer :: nc_x_sta_chk, nc_x_end_chk
  integer :: nc_y_sta_chk, nc_y_end_chk
! ---------------------------------------------------------------------
! SHIELD
! ---------------------------------------------------------------------
  integer :: nsh_min_chk, nsh_max_chk
#ifdef USE_MPI
! ---------------------------------------------------------------------
! MPI divide parameter
! ---------------------------------------------------------------------
  integer :: ndivx_chk, ndivy_chk
  integer :: idivtype_chk
#endif
! ---------------------------------------------------------------------

  if (myrank .eq. 0) then
!    crestartpar = trim(cdirname)//'/RESTART.param'
    crestartpar = trim(cdirname)//'/'//trim(cfnamepar)

    ierrflag = 0
    open(unit=IFDRESPAR,file=trim(crestartpar),status='old',mode='read', err=999)
    ierrflag = 1
    read(IFDRESPAR,'(A10,A)', err=999, end=999) cdummy, file_chk
    read(IFDRESPAR,'(A10,I10)', err=999, end=999) cdummy, imode_chk
    read(IFDRESPAR,'(A10,1PE12.4)', err=999, end=999) cdummy, att_chk
    read(IFDRESPAR,'(A10,1PE12.4)', err=999, end=999) cdummy, den_chk
    read(IFDRESPAR,'(A10,I10)', err=999, end=999) cdummy, irs_chk
    read(IFDRESPAR,'(A10,I10)', err=999, end=999) cdummy, ns_x_sta_chk
    read(IFDRESPAR,'(A10,I10)', err=999, end=999) cdummy, ns_x_end_chk
    read(IFDRESPAR,'(A10,I10)', err=999, end=999) cdummy, ns_y_sta_chk
    read(IFDRESPAR,'(A10,I10)', err=999, end=999) cdummy, ns_y_end_chk
    read(IFDRESPAR,'(A10,I10)', err=999, end=999) cdummy, ns_z_sta_chk
    read(IFDRESPAR,'(A10,I10)', err=999, end=999) cdummy, ns_z_end_chk
    read(IFDRESPAR,'(A10,I10)', err=999, end=999) cdummy, nt_x_sta_chk
    read(IFDRESPAR,'(A10,I10)', err=999, end=999) cdummy, nt_x_end_chk
    read(IFDRESPAR,'(A10,I10)', err=999, end=999) cdummy, nt_y_sta_chk
    read(IFDRESPAR,'(A10,I10)', err=999, end=999) cdummy, nt_y_end_chk
    read(IFDRESPAR,'(A10,I10)', err=999, end=999) cdummy, nc_x_sta_chk
    read(IFDRESPAR,'(A10,I10)', err=999, end=999) cdummy, nc_x_end_chk
    read(IFDRESPAR,'(A10,I10)', err=999, end=999) cdummy, nc_y_sta_chk
    read(IFDRESPAR,'(A10,I10)', err=999, end=999) cdummy, nc_y_end_chk
    read(IFDRESPAR,'(A10,I10)', err=999, end=999) cdummy, nsh_min_chk
    read(IFDRESPAR,'(A10,I10)', err=999, end=999) cdummy, nsh_max_chk
#ifdef USE_MPI
    read(IFDRESPAR,'(A10,I10)', err=999, end=999) cdummy, ndivx_chk
    read(IFDRESPAR,'(A10,I10)', err=999, end=999) cdummy, ndivy_chk
    read(IFDRESPAR,'(A10,I10)', err=999, end=999) cdummy, idivtype_chk
#endif
    ierrflag = 2
999 continue
    ierr = 1
    if (ierrflag .eq. 2) then
      close(IFDRESPAR)
      ierr = 0
      ! parameter check
      if (file         .ne. file_chk    ) ierr = ierr + 1
      if (imode        .ne. imode_chk   ) ierr = ierr + 1
      if (att          .ne. att_chk     ) ierr = ierr + 1
      if (den          .ne. den_chk     ) ierr = ierr + 1
      if (irs          .ne. irs_chk     ) ierr = ierr + 1
      if (ns_x_sta     .ne. ns_x_sta_chk) ierr = ierr + 1
      if (ns_x_end     .ne. ns_x_end_chk) ierr = ierr + 1
      if (ns_y_sta     .ne. ns_y_sta_chk) ierr = ierr + 1
      if (ns_y_end     .ne. ns_y_end_chk) ierr = ierr + 1
      if (ns_z_sta     .ne. ns_z_sta_chk) ierr = ierr + 1
      if (ns_z_end     .ne. ns_z_end_chk) ierr = ierr + 1
      if (nt_x_sta     .ne. nt_x_sta_chk) ierr = ierr + 1
      if (nt_x_end     .ne. nt_x_end_chk) ierr = ierr + 1
      if (nt_y_sta     .ne. nt_y_sta_chk) ierr = ierr + 1
      if (nt_y_end     .ne. nt_y_end_chk) ierr = ierr + 1
      if (nc_x_sta     .ne. nc_x_sta_chk) ierr = ierr + 1
      if (nc_x_end     .ne. nc_x_end_chk) ierr = ierr + 1
      if (nc_y_sta     .ne. nc_y_sta_chk) ierr = ierr + 1
      if (nc_y_end     .ne. nc_y_end_chk) ierr = ierr + 1
      if (nsh_min      .ne. nsh_min_chk ) ierr = ierr + 1
      if (nsh_max      .ne. nsh_max_chk ) ierr = ierr + 1
#ifdef USE_MPI
      if (ndivx        .ne. ndivx_chk   ) ierr = ierr + 1
      if (ndivy        .ne. ndivy_chk   ) ierr = ierr + 1
      if (idivtype     .ne. idivtype_chk) ierr = ierr + 1
#endif
    else if (ierrflag .eq. 1) then
      close(IFDRESPAR)
      write(*,*) 'warning: read error: '//trim(crestartpar)
    else
      write(*,*) 'warning: open error: '//trim(crestartpar)
    endif
  endif ! myrank

  return
end subroutine check_restart_parameter


! =====================================================================
subroutine write_restart_parameter()
! ---------------------------------------------------------------------
! version history, comments, ...
! =====================================================================
  use commondata
  use modtime
! ---------------------------------------------------------------------
  implicit none
! ---------------------------------------------------------------------
! ---------------------------------------------------------------------
  integer :: ierr
  integer :: ierrflag
! ---------------------------------------------------------------------
  call START_TIMER( 'WRITE PARAMETERs for RESTART...' )

  if (myrank .eq. 0) then
    crestartpar = trim(cdirname)//'/'//trim(cfnamepar)

    ierrflag = 0
    open(unit=IFDRESPAR,file=trim(crestartpar),status='replace', err=111)
    ierrflag = 1
    write(IFDRESPAR,'(A10,A)', err=111)   'file     =', trim(file)
    write(IFDRESPAR,'(A10,I10)', err=111) 'imode    =', imode
    write(IFDRESPAR,'(A10,1PE12.4)', err=111) 'att      =', att
    write(IFDRESPAR,'(A10,1PE12.4)', err=111) 'den      =', den
    write(IFDRESPAR,'(A10,I10)', err=111) 'irs      =', irs
    write(IFDRESPAR,'(A10,I10)', err=111) 'ns_x_sta =', ns_x_sta_all
    write(IFDRESPAR,'(A10,I10)', err=111) 'ns_x_end =', ns_x_end_all
    write(IFDRESPAR,'(A10,I10)', err=111) 'ns_y_sta =', ns_y_sta_all
    write(IFDRESPAR,'(A10,I10)', err=111) 'ns_y_end =', ns_y_end_all
    write(IFDRESPAR,'(A10,I10)', err=111) 'ns_z_sta =', ns_z_sta
    write(IFDRESPAR,'(A10,I10)', err=111) 'ns_z_end =', ns_z_end
    write(IFDRESPAR,'(A10,I10)', err=111) 'nt_x_sta =', nt_x_sta
    write(IFDRESPAR,'(A10,I10)', err=111) 'nt_x_end =', nt_x_end
    write(IFDRESPAR,'(A10,I10)', err=111) 'nt_y_sta =', nt_y_sta
    write(IFDRESPAR,'(A10,I10)', err=111) 'nt_y_end =', nt_y_end
    write(IFDRESPAR,'(A10,I10)', err=111) 'nc_x_sta =', nc_x_sta
    write(IFDRESPAR,'(A10,I10)', err=111) 'nc_x_end =', nc_x_end
    write(IFDRESPAR,'(A10,I10)', err=111) 'nc_y_sta =', nc_y_sta
    write(IFDRESPAR,'(A10,I10)', err=111) 'nc_y_end =', nc_y_end
    write(IFDRESPAR,'(A10,I10)', err=111) '!nsh_min =', nsh_min
    write(IFDRESPAR,'(A10,I10)', err=111) 'nsh_max  =', nsh_max
#ifdef USE_MPI
    write(IFDRESPAR,'(A10,I10)', err=111) 'ndivx    =', ndivx
    write(IFDRESPAR,'(A10,I10)', err=111) 'ndivy    =', ndivy
    write(IFDRESPAR,'(A10,I10)', err=111) 'idivtype =', idivtype
#endif
    ierrflag = 2
111 continue
    ierr = 1
    if (ierrflag .eq. 2) then
      close(IFDRESPAR)
      ierr = 0
    else if (ierrflag .eq. 1) then
      close(IFDRESPAR)
      write(*,*) 'warning: write error: '//trim(crestartpar)
    else
      write(*,*) 'warning: open error: '//trim(crestartpar)
    endif
  endif ! myrank

  call WRITE_TIMER( 'DONE.' )
  return
end subroutine write_restart_parameter

