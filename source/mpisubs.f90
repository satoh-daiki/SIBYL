! #####################################################################
! This file contains the following subroutines:
!   - check_divnum()
!   - calc_index()
!   - write_partition()
!   - read_partition(ierr)
!   - distribute_initial_data()
!   - distribute_restart_data()
! #####################################################################
! このファイルはUSE_MPI指定時のみコンパイルされる
! #####################################################################

! =====================================================================
subroutine check_divnum(ierr)
! ---------------------------------------------------------------------
! version history, comments, ...
! =====================================================================
  use commondata
!!!  use modtime
! ---------------------------------------------------------------------
  implicit none
! ---------------------------------------------------------------------
  integer, intent(out) :: ierr
! ---------------------------------------------------------------------
! ---------------------------------------------------------------------
!!!  call START_TIMER( '...' )

  ierr = 0
  if ( ((ndivx * ndivy) .ne. ncpus) .or. &
  &    ((ns_y_end - ns_y_sta + 1) .lt. ndivy) .or. &
  &    ((ns_x_end - ns_x_sta + 1) .lt. ndivx) ) then

    if (myrank .eq. 0) then
      if ((ndivx * ndivy) .ne. ncpus) then
!       write (*,'(A)')    '----------------------------------------'
        write (*,'(A)')    ' WARNING:'
        write (*,'(A)')    '   (ndivx * ndivy) is not match ncpus.'
      else if ((ns_y_end - ns_y_sta + 1) .lt. ndivy) then
!       write (*,'(A)')    '----------------------------------------'
        write (*,'(A)')    ' WARNING:'
        write (*,'(A)')    '   ndivy is greater than number of'
        write (*,'(A)')    '                   source elements for Y'
      else
!       write (*,'(A)')    '----------------------------------------'
        write (*,'(A)')    ' WARNING:'
        write (*,'(A)')    '   ndivx is greater than number of'
        write (*,'(A)')    '                   source elements for X'
      endif
    endif

    if ((ns_x_end - ns_x_sta + 1) .ge. ncpus) then
      ndivx = ncpus
      ndivy = 1
      if (myrank .eq. 0) then
        write (*,'(A)')    '   temporary setting;'
        write (*,'(A)')    '     ndivx = ncpus , ndivy = 1'
!       write (*,'(A)')    '----------------------------------------'
!       write (*,'(A,I10)') '!!  ndivx:        ',ndivx
!       write (*,'(A,I10)') '!!  ndivy:        ',ndivy
!       write (*,'(A)')    '----------------------------------------'
      endif
    else if ((ns_y_end - ns_y_sta + 1) .ge. ncpus) then
      ndivx = 1
      ndivy = ncpus
      if (myrank .eq. 0) then
        write (*,'(A)')    '   temporary setting;'
        write (*,'(A)')    '     ndivx = 1 , ndivy = ncpus'
!       write (*,'(A)')    '----------------------------------------'
!       write (*,'(A,I10)') '!!  ndivx:        ',ndivx
!       write (*,'(A,I10)') '!!  ndivy:        ',ndivy
!       write (*,'(A)')    '----------------------------------------'
      endif
    else
      if (myrank .eq. 0) then
        write (*,'(A)')    ' ERROR:'
        write (*,'(A)')    '   could not set temporary number of division'
        write (*,'(A)')    '   for ndivx, ndivy'
!       write (*,'(A)')    '----------------------------------------'
      endif
      ierr = 1  ! error flag ON
    endif
  endif

!!!  call WRITE_TIMER( 'DONE.' )
  return
end subroutine check_divnum


! =====================================================================
subroutine calc_index()
! ---------------------------------------------------------------------
! version history, comments, ...
! =====================================================================
  use commondata
!!!  use modtime
! ---------------------------------------------------------------------
  implicit none
! ---------------------------------------------------------------------
! ---------------------------------------------------------------------
  integer :: i, j, k, icnt, ierr
  integer :: ix, iy
  integer :: itmpx, itmpy
  integer :: idivtmp, imodtmp
  integer(8) :: inum
  integer(8), allocatable :: numdin(:,:)
  integer :: nloop, nflag
  integer :: idistance
  integer :: nx_2, ny_2, ns_x_s, ns_x_e, ns_y_s, ns_y_e
  integer :: icntx, icnty
  integer :: itypeflag
  integer :: icenter_srcx, icenter_srcy
  integer :: icenter_tgtx, icenter_tgty
  integer :: idistancex
  integer :: idistancey
  integer :: lenxsrc, lenysrc, lenxtgt, lenytgt
! ---------------------------------------------------------------------
!!!  call START_TIMER( '...' )

  if (myrank .eq. 0) then
    allocate( numdin(ns_x_sta_all:ns_x_end_all,ns_y_sta_all:ns_y_end_all))
    numdin = 0

    nx_2 = int(nx/2.0d0)
    ny_2 = int(ny/2.0d0)
!--    ns_x_s = max(ns_x_sta_all, nt_x_sta-nx_2)
!--    ns_x_e = min(ns_x_end_all, nt_x_end+nx_2)
!--    ns_y_s = max(ns_y_sta_all, nt_y_sta-ny_2)
!--    ns_y_e = min(ns_y_end_all, nt_y_end+ny_2)
    ns_x_s = max(ns_x_sta_all, nc_x_sta-nx_2)
    ns_x_e = min(ns_x_end_all, nc_x_end+nx_2)
    ns_y_s = max(ns_y_sta_all, nc_y_sta-ny_2)
    ns_y_e = min(ns_y_end_all, nc_y_end+ny_2)

    select case(idivtype)
      case (0)
        ! select dividing method : simple even dividing
        itypeflag = 0
        write(*,*)'NOTICE: select dividing method = even'
      case (1)
        ! select dividing method : elements count of source-region
        itypeflag = 1
        write(*,*)'NOTICE: select dividing method = elements'
      case (2)
        ! select dividing method : 
        !           distance from center of target-region to source-region
        itypeflag = 2
        write(*,*)'NOTICE: select dividing method = distance'
      case (3)
        ! select dividing method : 
        !           overlap area of source-region and target-region
        itypeflag = 3
        write(*,*)'NOTICE: select dividing method = area'
      case default
        ! default is "elements"
        itypeflag = 1
        write(*,*)'NOTICE: select dividing method = elements (default)'
    end select

    if ((itypeflag .eq. 1) .or. (itypeflag .gt. 3) .or. (itypeflag .lt. 0)) then
      ! ===== elements count of source-region ==============================
      ! using type1
!$omp parallel do private(j, i, k)
!CC   do j=ns_y_sta_all, ns_y_end_all
      do j=ns_y_s, ns_y_e
!CC     do i=ns_x_sta_all, ns_x_end_all
        do i=ns_x_s, ns_x_e
          do k=ns_z_sta, ns_z_end
            if (din_tmp(i, j, k) .gt. 0.0d0) then
              numdin(i,j) = numdin(i,j) + 1
            endif
          enddo
        enddo
      enddo
!$omp end parallel do
      ! ----- end by elements ----------------------------------------------
    else if (itypeflag .eq. 2) then
      ! ===== distance from center of target-region to source-region =======
      ! using type3
!--      icenter_tgtx = nt_x_sta + int((nt_x_end-nt_x_sta+1)/2.0d0) + 1
!--      icenter_tgty = nt_y_sta + int((nt_y_end-nt_y_sta+1)/2.0d0) + 1
      icenter_tgtx = nc_x_sta + int((nc_x_end-nc_x_sta+1)/2.0d0) + 1
      icenter_tgty = nc_y_sta + int((nc_y_end-nc_y_sta+1)/2.0d0) + 1
!$omp parallel do private(j, i, ix, iy, idistance, k)
!CC   do j=ns_y_sta_all, ns_y_end_all
      do j=ns_y_s, ns_y_e
!CC     do i=ns_x_sta_all, ns_x_end_all
        do i=ns_x_s, ns_x_e
          ix = i - icenter_tgtx
          iy = j - icenter_tgty
          idistance = sqrt(dble(ix*ix + iy*iy)) - ns_z_sta
          do k=ns_z_sta, ns_z_end
            if (din_tmp(i, j, k) .gt. 0.0d0) then
              numdin(i,j) = numdin(i,j) + idistance + k
            endif
          enddo
        enddo
      enddo
!$omp end parallel do
      ! ----- end by distance ----------------------------------------------
    else if (itypeflag .eq. 3) then
      ! ===== overlap area of source-region and target-region ==============
      ! using type5
!$omp parallel do private(j, i, ix, iy, k, icntx, icnty)
!CC   do j=ns_y_sta_all, ns_y_end_all
      do j=ns_y_s, ns_y_e
!CC     do i=ns_x_sta_all, ns_x_end_all
        do i=ns_x_s, ns_x_e
          do k=ns_z_sta, ns_z_end
            if (din_tmp(i, j, k) .gt. 0.0d0) then
              icntx = 0
!--              if (((i-nx_2) <= nt_x_sta) .and. (nt_x_sta <= (i+nx_2))) then
!--                if (nt_x_end <= (i+nx_2)) then
!--                  icntx = nt_x_end - nt_x_sta + 1
!--                else
!--                  icntx = (i+nx_2) - nt_x_sta + 1
!--                endif
!--              else if (((i-nx_2) <= nt_x_end) .and. (nt_x_end <= (i+nx_2))) then
!--                if ((i-nx_2) <= nt_x_sta) then
!--                  icntx = nt_x_end - nt_x_sta + 1
!--                else
!--                  icntx = nt_x_end - (i-nx_2) + 1
!--                endif
!--              else if ((nt_x_sta <= (i-nx_2)) .and. ((i+nx_2) <= nt_x_end)) then
!--                icntx = nx
!--              endif
              if (((i-nx_2) <= nc_x_sta) .and. (nc_x_sta <= (i+nx_2))) then
                if (nc_x_end <= (i+nx_2)) then
                  icntx = nc_x_end - nc_x_sta + 1
                else
                  icntx = (i+nx_2) - nc_x_sta + 1
                endif
              else if (((i-nx_2) <= nc_x_end) .and. (nc_x_end <= (i+nx_2))) then
                if ((i-nx_2) <= nc_x_sta) then
                  icntx = nc_x_end - nc_x_sta + 1
                else
                  icntx = nc_x_end - (i-nx_2) + 1
                endif
              else if ((nc_x_sta <= (i-nx_2)) .and. ((i+nx_2) <= nc_x_end)) then
                icntx = nx
              endif

              icnty = 0
!--              if (((j-ny_2) <= nt_y_sta) .and. (nt_y_sta <= (j+ny_2))) then
!--                if (nt_y_end <= (j+ny_2)) then
!--                  icnty = nt_y_end - nt_y_sta + 1
!--                else
!--                  icnty = (j+ny_2) - nt_y_sta + 1
!--                endif
!--              else if (((j-ny_2) <= nt_y_end) .and. (nt_y_end <= (j+ny_2))) then
!--                if ((j-ny_2) <= nt_y_sta) then
!--                  icnty = nt_y_end - nt_y_sta + 1
!--                else
!--                  icnty = nt_y_end - (j-ny_2) + 1
!--                endif
!--              else if ((nt_y_sta <= (j-ny_2)) .and. ((j+ny_2) <= nt_y_end)) then
!--                icnty = ny
!--              endif
              if (((j-ny_2) <= nc_y_sta) .and. (nc_y_sta <= (j+ny_2))) then
                if (nc_y_end <= (j+ny_2)) then
                  icnty = nc_y_end - nc_y_sta + 1
                else
                  icnty = (j+ny_2) - nc_y_sta + 1
                endif
              else if (((j-ny_2) <= nc_y_end) .and. (nc_y_end <= (j+ny_2))) then
                if ((j-ny_2) <= nc_y_sta) then
                  icnty = nc_y_end - nc_y_sta + 1
                else
                  icnty = nc_y_end - (j-ny_2) + 1
                endif
              else if ((nc_y_sta <= (j-ny_2)) .and. ((j+ny_2) <= nc_y_end)) then
                icnty = ny
              endif

              numdin(i,j) = numdin(i,j) + (icntx * icnty)
            endif
          enddo
        enddo
      enddo
!$omp end parallel do
      ! ----- end by area --------------------------------------------------
    else if (itypeflag .eq. 0) then
      ! ===== simple even dividing =========================================
      ! ----- end even -----------------------------------------------------
!--    else if (itypeflag .eq. 4) then
!--      ! ===== auto select dividing method ==================================
!--      ! ----- end even -----------------------------------------------------
    endif ! if (itypeflag)

    inum = sum(numdin)
    idivtmp = int(dble(inum / ndivx))

    if (ndivx .ne. 1) then
      if (itypeflag .ne. 0) then
        nflag = 0
        do nloop = 1, 10
          idivx = 0
          idivx(0) = ns_x_sta_all - 1
          j = 1
          inum = 0
          do i=ns_x_sta_all, ns_x_end_all
            inum = inum + sum(numdin(i,:))
            if (inum .ge. idivtmp) then
              idivx(j) = i
              inum = 0
              j = j + 1
              if (j .gt. ndivx) exit
            endif
          enddo
          idivx(ndivx) = ns_x_end_all

          if (j .lt. ndivx) then
            idivtmp = int(idivtmp * 0.97)
          else
            if (sum(numdin(idivx(ndivx-1)+1:idivx(ndivx),:)) .ge. int(idivtmp * 1.5)) then
              idivtmp = int(idivtmp * 1.05)
            else
              nflag = 1
              exit
            endif
          endif
        enddo
      endif

      if (nflag .eq. 0) then
        ! 要素数を平均化する分配に失敗したので、次元数を単純に分割する
        if (itypeflag .ne. 0) then
          write(*,*)'WARNING: X-axis divide failure, divide evenly.'
        endif
        itmpx = ns_x_end_all - ns_x_sta_all + 1
        idivtmp = int(itmpx / ndivx)
        imodtmp = mod(itmpx, ndivx)
        do i=0,ndivx-imodtmp
          idivx(i) = ns_x_sta_all + idivtmp * i - 1
        enddo
        icnt = 1
        do i=ndivx-imodtmp+1,ndivx
          idivx(i) = ns_x_sta_all + idivtmp * i - 1 + icnt
          icnt = icnt + 1
        enddo
      endif
    else ! if (ndivx)
      idivx(0) = ns_x_sta - 1
      idivx(1) = ns_x_end
    endif

    if (ndivy .ne. 1) then
      do ix = 1, ndivx
        if (itypeflag .ne. 0) then
          inum = sum(numdin(idivx(ix-1)+1:idivx(ix),:))
          idivtmp = int(dble(inum / ndivy))

          nflag = 0
          do nloop = 1, 10
            imapxy(ix,:) = 0
            imapxy(ix,0) = ns_y_sta_all - 1
            j = 1
            inum = 0
            do i=ns_y_sta_all, ns_y_end_all
              inum = inum + sum(numdin(idivx(ix-1)+1:idivx(ix),i))
              if (inum .ge. idivtmp) then
                imapxy(ix,j) = i
                inum = 0
                j = j + 1
                if (j .gt. ndivy) exit
              endif
            enddo
            imapxy(ix,ndivy) = ns_y_end_all

            if (j .lt. ndivy) then
              idivtmp = int(idivtmp * 0.97)
            else
              if (sum(numdin(idivx(ix-1)+1:idivx(ix),imapxy(ix,ndivy-1)+1:imapxy(ix,ndivy))) .ge. int(idivtmp * 1.5)) then
                idivtmp = int(idivtmp * 1.05)
              else
                nflag = 1
                exit
              endif
            endif
          enddo
        endif

        if (nflag .eq. 0) then
          ! 要素数を平均化する分配に失敗したので、次元数を単純に分割する
          if (itypeflag .ne. 0) then
            write(*,*)'WARNING: Y-axis divide failure, divide evenly. : x=',ix
          endif
          itmpy = ns_y_end_all - ns_y_sta_all + 1
          idivtmp = int(itmpy / ndivy)
          imodtmp = mod(itmpy, ndivy)
          do i=0,ndivy-imodtmp
            imapxy(ix,i) = ns_y_sta_all + idivtmp * i - 1
          enddo
          icnt = 1
          do i=ndivy-imodtmp+1,ndivy
            imapxy(ix,i) = ns_y_sta_all + idivtmp * i - 1 + icnt
            icnt = icnt + 1
          enddo
        endif
      enddo ! ix
    else ! if (ndivy)
      do ix = 1, ndivx
        imapxy(ix, 0) = ns_y_sta - 1
        imapxy(ix, 1) = ns_y_end
      enddo
    endif

    deallocate( numdin )
  endif ! myrank

  call MPI_Bcast(idivx(0:), ndivx+1, MPI_INTEGER4, 0, MPI_COMM_WORLD, ierr)
  icnt = ndivx * (ndivy + 1)
  call MPI_Bcast(imapxy(1:,0:), icnt, MPI_INTEGER4, 0, MPI_COMM_WORLD, ierr)

!!!  call WRITE_TIMER( 'DONE.' )
  return
end subroutine calc_index


! =====================================================================
subroutine write_partition()
! ---------------------------------------------------------------------
! version history, comments, ...
! =====================================================================
  use commondata
!!!  use modtime
! ---------------------------------------------------------------------
  implicit none
! ---------------------------------------------------------------------
! ---------------------------------------------------------------------
  integer :: ierr
  integer :: ierrflag
  integer :: istat
! ---------------------------------------------------------------------
!!!  call START_TIMER( '...' )

  if (myrank .eq. 0) then
    crestartpar = trim(cdirname)//'/'//trim(cfnamepat)
    ierrflag = 0
    open(unit=IFDRESPAR,file=trim(crestartpar),action='write',form='unformatted',access='stream',status='replace', err=111)
    ierrflag = 1
    write(IFDRESPAR, err=111) idivx(0:ndivx)
    write(IFDRESPAR, err=111) imapxy(1:ndivx,0:ndivy)
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

!-- 特に同期を取る必要もないのでコメントアウトしておく
!--  call MPI_Barrier(MPI_COMM_WORLD, ierr)

!!!  call WRITE_TIMER( 'DONE.' )

  return
end subroutine write_partition


! =====================================================================
subroutine read_partition(ierr)
! ---------------------------------------------------------------------
! version history, comments, ...
! =====================================================================
  use commondata
!!!  use modtime
! ---------------------------------------------------------------------
  implicit none
! ---------------------------------------------------------------------
  integer, intent(out) :: ierr
! ---------------------------------------------------------------------
  integer :: i, ierrflag
! ---------------------------------------------------------------------
!!!  call START_TIMER( '...' )

  if (myrank .eq. 0) then
    crestartpar = trim(cdirname)//'/'//trim(cfnamepat)
    ierrflag = 0
    open(unit=IFDRESPAR,file=trim(crestartpar),action='read',form='unformatted',access='stream',status='old',err=999)
    ierrflag = 1
    read(IFDRESPAR, err=999, end=999) idivx(0:ndivx)
    read(IFDRESPAR, err=999, end=999) imapxy(1:ndivx,0:ndivy)
    ierrflag = 2
999 continue
    ierr = 1
    if (ierrflag .eq. 2) then
      close(IFDRESPAR)
      ierr = 0
    else if (ierrflag .eq. 1) then
      close(IFDRESPAR)
      write(*,*) 'warning: read error: '//trim(crestartpar)
      return
    else
      write(*,*) 'warning: open error: '//trim(crestartpar)
      return
    endif
  endif ! myrank

  call MPI_Bcast(idivx(0:), ndivx+1, MPI_INTEGER4, 0, MPI_COMM_WORLD, ierr)
  i = ndivx * (ndivy + 1)
  call MPI_Bcast(imapxy(1:,0:), i, MPI_INTEGER4, 0, MPI_COMM_WORLD, ierr)

!!!  call WRITE_TIMER( 'DONE.' )

  return
end subroutine read_partition


! =====================================================================
subroutine distribute_initial_data(ierr)
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
  integer :: ix, iy, iz
  integer :: icnt, lsxtmp, lsytmp, isendto
  real(8), allocatable :: dbuf(:)
  integer, allocatable :: istats(:)
! ---------------------------------------------------------------------
  call START_TIMER( 'DISTRIBUTING INITIAL DATA...' )

  call MPI_Bcast(isopen_plume, 1, MPI_INTEGER4, 0, MPI_COMM_WORLD, ierr)
  call MPI_Bcast(isopen_ground, 1, MPI_INTEGER4, 0, MPI_COMM_WORLD, ierr)
  if ((isopen_plume .eq. 1) .or. (isopen_ground .eq. 1)) then
    if (myrank .eq. 0) then
#ifdef USE_MPI_SPMD
      do j = 1, ndivy
        do i = 1, ndivx
          if ((i .eq. 1) .and. (j .eq. 1)) cycle
          isendto = ndivx*(j-1)+i-1     ! ランク番号は0から
#else
      do k = 1, ncpus-1
          i = 1
          j = 1
          isendto = k
#endif
          lsxtmp = idivx(i) - idivx(i-1)
          lsytmp = imapxy(i,j) - imapxy(i,j-1)
          icnt = lsxtmp * lsytmp * (ns_z_end - ns_z_sta + 1)
          allocate(dbuf(icnt))
          dbuf = 0.0d0
!$omp parallel do
          do iz = ns_z_sta, ns_z_end
            do iy = 1, lsytmp
              do ix = 1, lsxtmp
                dbuf(lsxtmp*lsytmp*(iz-ns_z_sta)+lsxtmp*(iy-1)+ix) = din_tmp(idivx(i-1)+ix,imapxy(i,j-1)+iy,iz)
              enddo
            enddo
          enddo
!$omp end parallel do
          call MPI_Send(dbuf, icnt, MPI_REAL8, isendto, isendto, MPI_COMM_WORLD, ierr)
          deallocate(dbuf)
#ifdef USE_MPI_SPMD
        enddo
#endif
      enddo
!$omp parallel do
      do iz = ns_z_sta, ns_z_end
        do iy = 1, nparty
          do ix = 1, npartx
            din(idivx(0)+ix, imapxy(1,0)+iy, iz) = din_tmp(idivx(0)+ix, imapxy(1,0)+iy, iz)
          enddo
        enddo
      enddo
!$omp end parallel do
      deallocate ( din_tmp )
    else    ! myrank
      allocate(istats(MPI_STATUS_SIZE))
      icnt = npartx * nparty * (ns_z_end - ns_z_sta + 1)
      allocate(dbuf(icnt))
      dbuf = 0.0d0
      call MPI_Recv(dbuf, icnt, MPI_REAL8, 0, myrank, MPI_COMM_WORLD, istats, ierr)
!$omp parallel do
      do iz = ns_z_sta, ns_z_end
        do iy = 1, nparty
          do ix = 1, npartx
            din(idivx(myxpos-1)+ix, imapxy(myxpos,myypos-1)+iy, iz) = dbuf(npartx*nparty*(iz-ns_z_sta)+npartx*(iy-1)+ix)
          enddo
        enddo
      enddo
!$omp end parallel do
      deallocate(dbuf)
      deallocate(istats)
    endif
  endif

  call MPI_Bcast(isopen_shield, 1, MPI_INTEGER4, 0, MPI_COMM_WORLD, ierr)
  if (isopen_shield .eq. 1) then
    icnt = (ns_x_end_all - ns_x_sta_all + 1) * (ns_y_end_all - ns_y_sta_all + 1) * (nsh_max - nsh_min + 1)
    call MPI_Bcast(ish(ns_x_sta_all:,ns_y_sta_all:,nsh_min:), icnt, MPI_INTEGER4, 0, MPI_COMM_WORLD, ierr)
  endif

  call MPI_Bcast(isopen_elevation, 1, MPI_INTEGER4, 0, MPI_COMM_WORLD, ierr)
  if (isopen_elevation .eq. 1) then
    if (myrank .ne. 0) then
      allocate( delev(nt_x_sta:nt_x_end,nt_y_sta:nt_y_end) )
      delev(:,:) = 0.0d0
    endif
    icnt = (nt_x_end - nt_x_sta + 1) * (nt_y_end - nt_y_sta + 1)
    call MPI_Bcast(delev(nt_x_sta:,nt_y_sta:), icnt, MPI_REAL8, 0, MPI_COMM_WORLD, ierr)
  endif

!!!  ierr = 0

  call WRITE_TIMER( 'DONE.' )

  return
end subroutine distribute_initial_data


! =====================================================================
subroutine distribute_restart_data(ierr)
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
  integer :: icnt
! ---------------------------------------------------------------------
  call START_TIMER( 'DISTRIBUTING RESTART DATA...' )

  icnt = (ns_x_end_all - ns_x_sta_all + 1) * (ns_y_end_all - ns_y_sta_all + 1) * (nsh_max - nsh_min + 1)
  call MPI_Bcast(ish(ns_x_sta_all:,ns_y_sta_all:,nsh_min:), icnt, MPI_INTEGER4, 0, MPI_COMM_WORLD, ierr)
  icnt = (nt_x_end - nt_x_sta + 1) * (nt_y_end - nt_y_sta + 1)
  call MPI_Bcast(ielev(nt_x_sta:,nt_y_sta:), icnt, MPI_INTEGER4, 0, MPI_COMM_WORLD, ierr)
  icnt = (ns_x_end - ns_x_sta + 1) * (ns_y_end - ns_y_sta + 1) * (ns_z_end - ns_z_sta + 1)
  call MPI_Bcast(din(ns_x_sta:,ns_y_sta:,ns_z_sta:), icnt, MPI_REAL8, 0, MPI_COMM_WORLD, ierr)
  icnt = (nx_end - nx_sta + 1) * (ny_end - ny_sta + 1) * (ns_z_end - ns_z_sta + 1)
  call MPI_Bcast(resp(nx_sta:,ny_sta:,ns_z_sta:), icnt, MPI_REAL8, 0, MPI_COMM_WORLD, ierr)

  call WRITE_TIMER( 'DONE.' )

  return
end subroutine distribute_restart_data

