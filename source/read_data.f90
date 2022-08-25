! #####################################################################
! This file contains the following subroutines:
!   - read_zdata(ierr)
!   - read_din(ierr)
!   - read_initial_data(ierr)
!   - calculate_initial_data()
! #####################################################################

! =====================================================================
subroutine read_zdata(ierr)
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
  integer :: ierrflag, iz
! ---------------------------------------------------------------------
! read z data
  call START_TIMER( 'READING THE Z DATA...' )

  ierr = 0
  if (myrank .eq. 0) then
    ierrflag = 0
    open(unit=10,file=cfname_zda,status='old',mode='read' ,err=111)
    ierrflag = 1
    read(10,*,err=111,end=111) zlow(1), zupp(1)
    if (zlow(1) .ne. 0.0d0) then
      write(*,*)'warning: the value of zlow(1) should be set to 0.0. it was changed to 0.0.'
      zlow(1) = 0.0d0
    endif
    do iz = 2, ns_z_end
      read(10,*,err=111,end=111) zlow(iz), zupp(iz)
    end do
    ierrflag = 2
111 continue
    if (ierrflag .eq. 2) then
      close(10)
    else if (ierrflag .eq. 1) then
      close(10)
      write(*,*) 'ERROR: read error: '//trim(cfname_zda)
      ierr = 1
      return
    else
      write(*,*) 'ERROR: open error: '//trim(cfname_zda)
      ierr = 1
      return
    endif
  endif ! myrank

#ifdef USE_MPI
  call MPI_Bcast(zlow(0:), ns_z_end+1, MPI_REAL8, 0, MPI_COMM_WORLD, ierr)
  call MPI_Bcast(zupp(0:), ns_z_end+1, MPI_REAL8, 0, MPI_COMM_WORLD, ierr)
#endif

  call WRITE_TIMER( 'DONE.' )

  return
end subroutine read_zdata


! =====================================================================
subroutine read_din(ierr)
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
  integer :: istat
  integer :: ierrflag, iz, iy, ix
! ---------------------------------------------------------------------
! read input source data for plume
  call START_TIMER( 'READING THE ACTIVITY DATA OF PLUME...' )

#ifdef USE_MPI
  if (myrank .eq. 0) then
    allocate( din_tmp(ns_x_sta_all:ns_x_end_all,ns_y_sta_all:ns_y_end_all,ns_z_sta:ns_z_end) )
    din_tmp = 0.0d0
  endif
#endif

  isopen_plume = 0
  if (myrank .eq. 0) then
    open(unit=10,file=cfname_plu,iostat=istat,status='old',mode='read')

    if (istat .eq. 0) then
      ierrflag = 1
      do iz = 1, ns_z_end
        do iy = ns_y_sta_all, ns_y_end_all
          do ix = ns_x_sta_all, ns_x_end_all
#ifndef USE_MPI
            read(10,*,err=999,end=999) din(ix,iy,iz)  ! (Bq/m^3)
#else
            read(10,*,err=999,end=999) din_tmp(ix,iy,iz)  ! (Bq/m^3)
#endif
          enddo
        enddo
      enddo
      ierrflag = 2
999 continue
      close(10)
      if (ierrflag .eq. 1) then
        write(*,*) 'ERROR: read error: '//trim(cfname_plu)
        ierr = 1
        return
      endif
      isopen_plume = 1
#ifdef DEBUG
    else
      write(*,*) 'NOTICE: skip read: '//trim(cfname_plu)
#endif
    endif
  endif ! myrank

9 continue
  call WRITE_TIMER( 'DONE.' )
! ---------------------------------------------------------------------
! read input source data for contaminated ground
  call START_TIMER( 'READING THE ACTIVITY DATA OF GROUND...' )

  isopen_ground = 0
  if (myrank .eq. 0) then
    open(unit=10,file=cfname_gnd,iostat=istat,status='old',mode='read')

    if (istat .eq. 0) then
      ierrflag = 1
      do iy = ns_y_sta_all, ns_y_end_all
        do ix = ns_x_sta_all, ns_x_end_all
#ifndef USE_MPI
          read(10,*,err=888,end=888) din(ix,iy,0)   ! (Bq/m^2)
#else
          read(10,*,err=888,end=888) din_tmp(ix,iy,0)   ! (Bq/m^2)
#endif
        enddo
      enddo
      ierrflag = 2
888 continue
      close(10)
      if (ierrflag .eq. 1) then
        write(*,*) 'ERROR: read error: '//trim(cfname_gnd)
        ierr = 1
        return
      endif
      isopen_ground = 1
#ifdef DEBUG
    else
      write(*,*) 'NOTICE: skip read: '//trim(cfname_gnd)
#endif
    endif
  endif ! myrank

8 continue

  call WRITE_TIMER( 'DONE.' )
end subroutine read_din


! =====================================================================
subroutine read_initial_data(ierr)
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
  integer :: istat
  integer :: ierrflag, iz, iy, ix
! ---------------------------------------------------------------------
! read shield data
  call START_TIMER( 'READING THE SHIELD DATA...' )

  isopen_shield = 0
  if (myrank .eq. 0) then
    open(unit=10,file=cfname_sld,iostat=istat,status='old',mode='read')

    if (istat .eq. 0) then
      ierrflag = 1
      do iz = nsh_min, nsh_max
        do iy = ns_y_sta_all, ns_y_end_all
          do ix = ns_x_sta_all, ns_x_end_all
            read(10,*,err=777,end=777) ish(ix,iy,iz)  ! ID
          enddo
        enddo
      enddo
      ierrflag = 2
777 continue
      close(10)
      if (ierrflag .eq. 1) then
        write(*,*) 'ERROR: read error: '//trim(cfname_sld)
        ierr = 1
        return
      endif
      isopen_shield = 1
#ifdef DEBUG
    else
      write(*,*) 'NOTICE: skip read: '//trim(cfname_sld)
#endif
    endif
  endif ! myrank

7 continue

  call WRITE_TIMER( 'DONE.' )
! ---------------------------------------------------------------------
! read elevation data
  call START_TIMER( 'READING THE ELEVATION DATA...' )

  isopen_elevation = 0
  if (myrank .eq. 0) then
    open(unit=10,file=cfname_elv,iostat=istat,status='old',mode='read')

    if (istat .eq. 0 ) then
      allocate( delev(nt_x_sta:nt_x_end,nt_y_sta:nt_y_end) )
      delev(:,:) = 0.0d0

      ierrflag = 1
      do iy = nt_y_sta, nt_y_end
        do ix = nt_x_sta, nt_x_end
          read(10,*,err=666,end=666) delev(ix,iy)   ! (m)
        enddo
      enddo
      ierrflag = 2
666 continue
      close(10)
      if (ierrflag .eq. 1) then
        write(*,*) 'ERROR: read error: '//trim(cfname_elv)
        ierr = 1
        return
      endif

      isopen_elevation = 1
#ifdef DEBUG
    else
      write(*,*) 'NOTICE: skip read: '//trim(cfname_elv)
#endif
    endif
  endif ! myrank

6 continue

  call WRITE_TIMER( 'DONE.' )
! ---------------------------------------------------------------------
  return
end subroutine read_initial_data


! =====================================================================
subroutine calculate_initial_data()
! ---------------------------------------------------------------------
! version history, comments, ...
! =====================================================================
  use commondata
! ---------------------------------------------------------------------
  implicit none
! ---------------------------------------------------------------------
  integer :: iz, iy, ix
  real(8) :: sqirs
! ---------------------------------------------------------------------
  sqirs = dble(irs*irs)*1.0d-3
! ---------------------------------------------------------------------

  if (isopen_plume .eq. 1) then
!$omp parallel do private(iy,ix)
    do iz = 1, ns_z_end
      do iy = ns_y_sta, ns_y_end
        do ix = ns_x_sta, ns_x_end
          din(ix,iy,iz) = din(ix,iy,iz)*sqirs*(zupp(iz)-zlow(iz))        ! (kBq/irs x irs x irs m^3)
        enddo
      enddo
    enddo
!$omp end parallel do
  endif

  if (isopen_ground .eq. 1) then
!$omp parallel do private(ix)
    do iy = ns_y_sta, ns_y_end
      do ix = ns_x_sta, ns_x_end
        din(ix,iy,0) = din(ix,iy,0)*sqirs         ! (kBq/irs x irs m^2)
      enddo
    enddo
!$omp end parallel do
  endif

  if (isopen_shield .eq. 1) then
!$omp parallel do private(iy, ix)
    do iz = nsh_min, nsh_max
      do iy = ns_y_sta_all, ns_y_end_all
        do ix = ns_x_sta_all, ns_x_end_all
          ish(ix,iy,iz) = merge(1, 0, (ish(ix,iy,iz) .eq. ishield))
        enddo
      enddo
    enddo
!$omp end parallel do
  endif

  if (isopen_elevation .eq. 1) then
!$omp parallel do private(ix, iz)
    do iy = nt_y_sta, nt_y_end
      do ix = nt_x_sta, nt_x_end
        if( delev(ix,iy) <= 0.0d0 ) then
          ielev(ix,iy) = 0
        else if( delev(ix,iy) >= zupp(ns_z_end) ) then
          ielev(ix,iy) = ns_z_end + 1
        else
          zmesh_loop :do iz = 1, ns_z_end
            if( delev(ix,iy) >= zlow(iz) .and. delev(ix,iy) < zupp(iz) ) then
              ielev(ix,iy) = iz - 1
              exit zmesh_loop
            endif
          enddo zmesh_loop
        endif
      enddo
    enddo
!$omp end parallel do
    deallocate( delev )
  endif

  return
end subroutine calculate_initial_data

