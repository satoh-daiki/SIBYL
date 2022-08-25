! #####################################################################
! This file contains the following subroutines:
!   - compute_doses(irestarty)
!   - compute_dose(ix, iy)
! #####################################################################

! =====================================================================
subroutine compute_doses(irestarty)
! ---------------------------------------------------------------------
! version history, comments, ...
! =====================================================================
!$ use omp_lib
! ---------------------------------------------------------------------
  use commondata
  use modtime
! ---------------------------------------------------------------------
  implicit none
! ---------------------------------------------------------------------
  integer :: irestarty
! -----------------------------------------------------------------
  integer :: iy, ix
  integer :: icount_restart, ipre_iy
  real(8) :: rescurtime, respretime
  integer :: ierr
! -----------------------------------------------------------------
#ifdef USE_MPI_MS
    integer :: ii
    integer :: icur_iy_restart
    integer :: iflags_dout(irestarty:nc_y_end)
#else
!$  integer :: ii
!$  integer :: icur_iy_restart
!$  integer :: iflags_dout(irestarty:nc_y_end)
#endif
#ifdef USE_MPI
! -----------------------------------------------------------------
    integer :: i, j
    integer :: icur_rank, icur_iy, iremain
    integer :: istatus(MPI_STATUS_SIZE)
    integer :: ipysted(2)
    integer :: icnt, icnt2, irecvfrom
    real(8), allocatable :: dbuf(:)
#endif
! -----------------------------------------------------------------

  call START_TIMER( 'COMPUTING THE GROUND/PLUME DOSES...' )

#ifdef DEBUG
  write(*,'(A,I5,A,I5,A,I5,A,I5,A,I3)')' INFO: nx[',nx_sta,':',nx_end,'], ny[',ny_sta,':',ny_end,'] @',myrank
#endif

  icount_restart = 0
  ipre_iy = irestarty
  respretime = start_time
!$  icur_iy_restart = irestarty
!$  iflags_dout = 0

#ifdef USE_MPI_MS
  if (ncpus .eq. 1) then
#endif
! === SERIAL / OpenMP / MPI master-slave model 1process / 
!     MPI SPMD model / Hybrid Parallel ================================
! ---------------------------------------------------------------------
!$omp parallel do private(iy, ix) copyin(pre_time, pre_split_time) schedule(dynamic)
  LOOP_TGT_PY : do iy = irestarty, nc_y_end
! ---------------------------------------------------------------------
    LOOP_TGT_PX : do ix = nc_x_sta, nc_x_end
      if( ish(ix,iy,1) /= 0 ) cycle LOOP_TGT_PX
! --------------------------------------------------------------------
      call compute_dose(ix, iy)
! --------------------------------------------------------------------
    end do LOOP_TGT_PX
    write(*,'(a,i0,a,i0,a,i0,a)',advance='no') 'TARGET (',nc_x_sta,':',nc_x_end,', ',iy,'): '
!$  if (.false.) then
    call WRITE_TIMER(SPLIT, myrank)
!$  else
!$    call WRITE_TIMER(SPLIT, OMP_GET_THREAD_NUM())
!$  endif

!$  iflags_dout(iy) = 1

!$  if (OMP_GET_THREAD_NUM() .eq. (OMP_GET_NUM_THREADS() - 1)) then
      if (irestart_out .eq. 1) then
!$      do ii = ipre_iy, nc_y_end
!$        if (iflags_dout(ii) .eq. 0) then
!$          icur_iy_restart = ii - 1
!$          exit
!$        endif
!$      enddo

!       ===== output restart data =====
        if (irestart_tim .eq. 1) then
!===== case of TGT_PY lines
!$        if (.false.) then
          icount_restart = icount_restart + 1
!$        else
!$          icount_restart = icur_iy_restart - ipre_iy + 1
!$        endif
          if (icount_restart .ge. irestart_cnt) then
!$          if (.false.) then
!--            call write_restart_doses4(nt_x_sta,nt_x_end,ipre_iy, iy)
            call write_restart_doses4(nc_x_sta,nc_x_end,ipre_iy, iy)
            icount_restart = 0
            ipre_iy = iy + 1
!$          else
!$            call write_restart_doses4(nc_x_sta,nc_x_end,ipre_iy, icur_iy_restart)
!$            icount_restart = 0
!$            ipre_iy = icur_iy_restart + 1
!$          endif
#ifdef DEBUG
!$          if (.false.) then
            write(*,'(a,i0)',advance='no')' DEBUG: write restart doses @iy=',iy
!$          else
!$            write(*,'(a,i0)',advance='no')' DEBUG: write restart doses @iy=',icur_iy_restart
!$          endif
            call WRITE_TIMER(SPLIT, myrank)
#endif
          endif
        else if (irestart_tim .eq. 0) then
!===== case of every TGT_PY , 1-line out
!$        if (.false.) then
!--          call write_restart_doses3(nt_x_sta,nt_x_end,iy)
          call write_restart_doses3(nc_x_sta,nc_x_end,iy)
!$        else
!$          call write_restart_doses4(nc_x_sta,nc_x_end,ipre_iy, icur_iy_restart)
!$          ipre_iy = icur_iy_restart + 1
!$        endif
        else
!===== case of real elapse time
          call GET_TIME(rescurtime)
          icount_restart = int(rescurtime - respretime)
          if (icount_restart .ge. irestart_cnt) then
!$        if (.false.) then
!--            call write_restart_doses4(nt_x_sta,nt_x_end,ipre_iy, iy)
            call write_restart_doses4(nc_x_sta,nc_x_end,ipre_iy, iy)
            ipre_iy = iy + 1
!$        else
!$          call write_restart_doses4(nc_x_sta,nc_x_end,ipre_iy, icur_iy_restart)
!$          ipre_iy = icur_iy_restart + 1
!$        endif
            respretime = rescurtime
#ifdef DEBUG
!$          if (.false.) then
            write(*,'(a,i0)',advance='no')'DEBUG: write restart doses @iy=',iy
!$          else
!$            write(*,'(a,i0)',advance='no')' DEBUG: write restart doses @iy=',icur_iy_restart
!$          endif
            call WRITE_TIMER(SPLIT, myrank)
#endif
          endif
        endif
      endif ! if(restart_out)
!$  endif ! if (OMP_GET...)
!   ===== end: output restart data =====
  end do LOOP_TGT_PY
!$omp end parallel do

#ifdef USE_MPI_MS
  else ! if(ncpus==1)
! === MPI master-slave model / Hybrid parallel  =======================

    if (myrank .ne. 0) then
! ===== SLAVE =========================================================
      do
        ! recv ipyst, ipyed
        call MPI_Recv(ipysted, 2, MPI_INTEGER, 0, 10, MPI_COMM_WORLD, istatus, ierr)
        ! if (FINISH?) exit
        if (ipysted(1) .gt. ipysted(2)) exit
! ---------------------------------------------------------------------
        LOOP_TGT_PY_MS : do iy = ipysted(1), ipysted(2)
! ---------------------------------------------------------------------
!$omp parallel do private(ix) schedule(dynamic)
          LOOP_TGT_PX_MS : do ix = nc_x_sta, nc_x_end
            if( ish(ix,iy,1) /= 0 ) cycle LOOP_TGT_PX_MS
! --------------------------------------------------------------------
            call compute_dose(ix, iy)
! --------------------------------------------------------------------
          end do LOOP_TGT_PX_MS
!$omp end parallel do
          write(*,'(a,i0,a,i0,a,i0,a)',advance='no') 'TARGET (',nc_x_sta,':',nc_x_end,', ',iy,'): '
          call WRITE_TIMER(SPLIT, myrank)
        enddo LOOP_TGT_PY_MS

        icnt = (nc_x_end - nc_x_sta + 1) * (ipysted(2) - ipysted(1) + 1)
        allocate(dbuf(icnt))
        icnt2 = 0
        do iy = ipysted(1), ipysted(2)
          do ix = nc_x_sta, nc_x_end
            icnt2 = icnt2 + 1
            dbuf(icnt2) = dout(ix, iy)
          enddo
        enddo

        if (icnt .ne. icnt2) then
          write(*,*) 'Warning: icnt != icnt2'
        endif
        ! send myrank
        call MPI_Send(myrank, 1, MPI_INTEGER, 0, 20, MPI_COMM_WORLD, ierr)
        ! send ipyst, ipyed
        call MPI_Send(ipysted, 2, MPI_INTEGER, 0, myrank, MPI_COMM_WORLD, ierr)
        ! send dout(:,ipyst:ipyed)
        call MPI_Send(dbuf, icnt, MPI_REAL8, 0, myrank, MPI_COMM_WORLD, ierr)
        deallocate(dbuf)
      enddo
    else ! if(myrank!=0)
! ===== MASTER ========================================================
      iremain = nc_y_end - irestarty + 1
      icur_iy = irestarty
      icur_rank = 1
      do
        if (icur_rank .le. ncpus-1) then
          if (icur_iy .le. nc_y_end) then
            ipysted(1) = icur_iy
            ipysted(2) = icur_iy
            call MPI_Send(ipysted, 2, MPI_INTEGER, icur_rank, 10, MPI_COMM_WORLD, ierr)
            icur_iy = icur_iy + (ipysted(2)-ipysted(1)+1)
          endif
          icur_rank = icur_rank + 1
        else
          ! recv from
          call MPI_Recv(irecvfrom, 1, MPI_INTEGER, MPI_ANY_SOURCE, 20, MPI_COMM_WORLD, istatus, ierr)
          ! recv sted
          call MPI_Recv(ipysted, 2, MPI_INTEGER, irecvfrom, irecvfrom, MPI_COMM_WORLD, istatus, ierr)
          ! recv dbuf
          icnt = (nc_x_end - nc_x_sta + 1) * (ipysted(2) - ipysted(1) + 1)
          allocate(dbuf(icnt))
          call MPI_Recv(dbuf, icnt, MPI_REAL8, irecvfrom, irecvfrom, MPI_COMM_WORLD, istatus, ierr)
          ! dout = dbuf
          icnt2 = 0
          do j = ipysted(1), ipysted(2)
            do i = nc_x_sta, nc_x_end
              icnt2 = icnt2 + 1
              dout(i, j) = dbuf(icnt2)
            enddo
          iflags_dout(j) = 1
          enddo
          deallocate(dbuf)
          iremain = iremain - (ipysted(2)-ipysted(1)+1)
          if (iremain .le. 0) exit
          if (icur_iy .le. nc_y_end) then
            ipysted(1) = icur_iy
            ipysted(2) = icur_iy
            call MPI_Send(ipysted, 2, MPI_INTEGER, irecvfrom, 10, MPI_COMM_WORLD, ierr)
            icur_iy = icur_iy + (ipysted(2)-ipysted(1)+1)
          endif

!         ===== output restart data =====
          if (irestart_out .eq. 1) then
            do ii = ipre_iy, nc_y_end
              if (iflags_dout(ii) .eq. 0) then
                icur_iy_restart = ii - 1
                exit
              endif
            enddo

            if (irestart_tim .eq. 1) then
!===== case of TGT_PY lines
              icount_restart = icur_iy_restart - ipre_iy + 1
              if (icount_restart .ge. irestart_cnt) then
                call write_restart_doses4(nc_x_sta,nc_x_end,ipre_iy, icur_iy_restart)
                icount_restart = 0
                ipre_iy = icur_iy_restart + 1
#ifdef DEBUG
                write(*,'(a,i0)',advance='no')' DEBUG: write restart doses @iy=',icur_iy_restart
                call WRITE_TIMER(SPLIT, myrank)
#endif
              endif
            else if (irestart_tim .eq. 0) then
!===== case of every TGT_PY , 1-line out
              call write_restart_doses4(nc_x_sta,nc_x_end,ipre_iy, icur_iy_restart)
              ipre_iy = icur_iy_restart + 1
            else
!===== case of real elapse time
              call GET_TIME(rescurtime)
              icount_restart = int(rescurtime - respretime)
              if (icount_restart .ge. irestart_cnt) then
                call write_restart_doses4(nc_x_sta,nc_x_end,ipre_iy, icur_iy_restart)
                ipre_iy = icur_iy_restart + 1
                respretime = rescurtime
#ifdef DEBUG
                write(*,'(a,i0)',advance='no')'DEBUG: write restart doses @iy=',icur_iy_restart
                call WRITE_TIMER(SPLIT, myrank)
#endif
              endif
            endif
          endif ! if(restart_out)
!         ===== end: output restart data =====

        endif
      enddo

      do iy = 1, ncpus-1
        ! send ipyst, ipyed for FINISH
        ! send ipyst, ipyed
        ipysted(1) = 1
        ipysted(2) = 0  ! 開始と終了で大小逆転でループ抜け
        call MPI_Send(ipysted, 2, MPI_INTEGER, iy, 10, MPI_COMM_WORLD, ierr)
      enddo




    endif ! if(myrank!=0)
  endif ! if(ncpus==1)
#endif


#ifdef USE_MPI
  call MPI_Barrier(MPI_COMM_WORLD, ierr)
#endif

  call WRITE_TIMER( 'DONE.' )

  return
end subroutine compute_doses


! =====================================================================
subroutine compute_dose(ix, iy)
! ---------------------------------------------------------------------
! version history, comments, ...
! =====================================================================
  use commondata
! ---------------------------------------------------------------------
  implicit none
! ---------------------------------------------------------------------
  integer :: ix, iy
! ---------------------------------------------------------------------
  integer :: i, iz
  integer :: iv
  integer :: ires_x, ires_y, ires_z
  integer :: ipos_x, ipos_y, ipos_z
  integer :: icnt
  real(8) :: fac
! ---------------------------------------------------------------------
  iv     = ielev(ix,iy)
  if (iv .gt. ns_z_end) return

! ---------------------------------------------------------------------
! GROUND
! ---------------------------------------------------------------------
! Added on Mar.02.2020.
! if( iz_sta /= 0 ) goto 10
  if ( iz_sta .eq. 0 ) then

    ires_z = 0
    ipos_z = 0
    LOOP_RESP_GY : do ires_y = 1, ny
      ipos_y = iy-imid+ires_y ! position on the absolute ordinate system
      if( ipos_y < ns_y_sta .or. ipos_y > ns_y_end ) cycle LOOP_RESP_GY

      LOOP_RESP_GX : do ires_x = 1, nx
        ipos_x = ix-imid+ires_x ! position on the absolute ordinate system
        if( ipos_x < ns_x_sta .or. ipos_x > ns_x_end ) cycle LOOP_RESP_GX

!       -----------------------------------------------------------
        if( din(ipos_x,ipos_y,0) <= 0.d0 ) cycle LOOP_RESP_GX
!       -----------------------------------------------------------
!       ATTENUATION
        call attenuation(ix,iy,0,ipos_x,ipos_y,0,icnt)
        fac = dexp(dmuirs*icnt)
!       -----------------------------------------------------------

        dout(ix, iy) = dout(ix, iy) + resp(ires_x,ires_y,0)*din(ipos_x,ipos_y,0)*fac  ! (uSv/h @ irs x irs m^2)

      end do LOOP_RESP_GX
    end do LOOP_RESP_GY

! Added on Mar.02.2020.
! 10 continue
  endif ! if(iz_sta)

! ---------------------------------------------------------------------
! PLUME
! ---------------------------------------------------------------------

    LOOP_RESP_PZ : do iz = 1, iz_end
      ires_z = iz - iv
      ipos_z = iz
      if( (ires_z < 0) .or. (ires_z < ns_z_sta) ) cycle LOOP_RESP_PZ
! ---------------------------------------------------------------------
      LOOP_RESP_PY : do ires_y = 1, ny
        ipos_y = iy-imid+ires_y ! position on the absolute ordinate system
        if( ipos_y < ns_y_sta .or. ipos_y > ns_y_end ) cycle LOOP_RESP_PY

        LOOP_RESP_PX : do ires_x = 1, nx
          ipos_x = ix-imid+ires_x ! position on the absolute ordinate system
          if( ipos_x < ns_x_sta .or. ipos_x > ns_x_end ) cycle LOOP_RESP_PX

!         -----------------------------------------------------------
          if( din(ipos_x,ipos_y,ipos_z) <= 0.d0 ) cycle LOOP_RESP_PX
!         -----------------------------------------------------------
!         ATTENUATION
          call attenuation(ix,iy,iv,ipos_x,ipos_y,ipos_z,icnt)
          fac = dexp(dmuirs*icnt)
!         -----------------------------------------------------------

          dout(ix, iy) = dout(ix, iy) + resp(ires_x,ires_y,ires_z)*din(ipos_x,ipos_y,ipos_z)*fac  ! (uSv/h @ irs x irs m^2)

        end do LOOP_RESP_PX
      end do LOOP_RESP_PY
! ---------------------------------------------------------------------
    end do LOOP_RESP_PZ

  return
end subroutine compute_dose
