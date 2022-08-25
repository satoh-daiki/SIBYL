! =====================================================================
subroutine response()
! ---------------------------------------------------------------------
! Programed by d.satoh (2018.05.08)
! Modified  by d.satoh (2018.07.31)
! Modified  by d.satoh (2018.11.06)
! Modified  by d.satoh (2019.04.09)
! Modified  by d.satoh (2019.10.25)
! =====================================================================
  use commondata
  use modtime
! ---------------------------------------------------------------------
  implicit none
! ---------------------------------------------------------------------
  integer :: iz
  integer :: ialt
  integer :: i
  real(8) :: zz, z1, z2, zwidth
! ---------------------------------------------------------------------
  integer :: ix, iy, j, k, l
  integer :: it
  real(8), allocatable :: resp0(:,:), resp1(:,:), resp2(:,:)
  real(8), allocatable :: resp_tmp(:,:,:)
! ---------------------------------------------------------------------
#ifdef USE_MPI
  integer :: icnt, ierr, iii, irankfrom
  integer :: nxtmp, nytmp, nztmp
  integer :: ibuf(4)
  real(8), allocatable :: dbuf(:)
  integer, allocatable :: istats(:)
#endif
! ---------------------------------------------------------------------
  call START_TIMER( 'CONSTRUCTING THE RESPONSE FUNCTIONS...' )

  if (myrank .eq. 0) then
    allocate(resp0(1:nx0,1:ny0))
    resp0 = 0.0d0
    allocate( resp_tmp(1:nx,1:ny,ns_z_sta:ns_z_end) )
    resp_tmp = 0.0d0

    it = dint( (ny0 - ny*irs)/2.d0 )

    LOOP_SRC_Z : do iz = iz_sta, iz_end
      zz = ( zlow(iz) + zupp(iz) ) / 2.0d0
      zwidth = zupp(iz) - zlow(iz)

! ---------------------------------------------------------------------
      if( iz == 0 ) then

        open(unit=100,file='./RESP/000m/'//trim(file),form='unformatted',access='stream',status='old',mode='read')

        read(100) resp0(1:nx0,1:ny0)  ! (mSv/h per kBq/m^2 @ 1.0m x 1.0 m)

        close(100)

! ---------------------------------------------------------------------
      else if( zz < ALT(1) ) then

        open(unit=100,file='./RESP/'//trim(PATH(1))//'/'//trim(file),form='unformatted',access='stream',status='old',mode='read')

        read(100) resp0(1:nx0,1:ny0)  ! (mSv/h per kBq/m^3 @ 1.0m x 1.0 m)

        close(100)

      else if( zz >= ALT(nalt) ) then
        resp0(:,:) = 0.d0
      else
! ---------------------------------------------------------------------
        allocate(resp1(1:nx0,1:ny0))
        allocate(resp2(1:nx0,1:ny0))
        resp1 = 0.0d0
        resp2 = 0.0d0
        LOOP_ALT : do ialt = 1, nalt-1
! ---------------------------------------------------------------------
          if( zz == ALT(ialt) ) then
            open(unit=100,file='./RESP/'//trim(PATH(ialt))//'/'//trim(file),form='unformatted',access='stream',status='old',mode='read')

            read(100) resp0(1:nx0,1:ny0)  ! (mSv/h per kBq/m^3 @ 1.0m x 1.0 m)

            close(100)

            exit
          else if( zz > ALT(ialt) .and. zz < ALT(ialt+1) ) then

            open(unit=200,file='./RESP/'//trim(PATH(ialt))//'/'//trim(file),form='unformatted',access='stream',status='old',mode='read')
            open(unit=300,file='./RESP/'//trim(PATH(ialt+1))//'/'//trim(file),form='unformatted',access='stream',status='old',mode='read')

            read(200) resp1(1:nx0,1:ny0)  ! (mSv/h per kBq/m^3 @ 1.0m x 1.0 m)
            read(300) resp2(1:nx0,1:ny0)  ! (mSv/h per kBq/m^3 @ 1.0m x 1.0 m)

            close(200)
            close(300)

            z1 = dlog(zz/ALT(ialt)) / dlog(ALT(ialt+1)/ALT(ialt))
!$omp parallel do private(i)
            do j=1, ny0
!DIR$ SIMD
              do i=1, nx0
                 resp0(i,j)=resp1(i,j)*dexp( dlog(resp2(i,j)/resp1(i,j)) * z1 )
              enddo
            enddo
!$omp end parallel do

            exit
          end if

! ---------------------------------------------------------------------
        end do LOOP_ALT
        deallocate(resp2)
        deallocate(resp1)
      endif
! ---------------------------------------------------------------------
!$omp parallel do private(iy,ix,l,k)
      do iy = 1, ny
        do ix = 1, nx
!     -----------------------------------------------------------------
          do l = 1, irs
            do k = 1, irs

              resp_tmp(ix,iy,iz) = resp_tmp(ix,iy,iz) + resp0(it+k+irs*(ix-1),it+l+irs*(iy-1))*1.d+3  ! (uSv/h per kBq/m^3 @ irs x irs m^2)

            end do
          end do
!     -----------------------------------------------------------------
        end do
      end do
!$omp end parallel do
! ---------------------------------------------------------------------
    end do LOOP_SRC_Z
    deallocate(resp0)
  endif ! myrank
#ifdef DEBUG
  write(*,'(A)',advance='no') ' DEBUG: read and build resp function done'
  call WRITE_TIMER(SPLIT, myrank)
#endif

#ifdef USE_MPI
  allocate(istats(MPI_STATUS_SIZE))
  if (myrank .eq. 0) then
    do i=1, ncpus-1
      call MPI_Recv(irankfrom, 1, MPI_INTEGER4, MPI_ANY_SOURCE, 135, MPI_COMM_WORLD, istats, ierr)
      call MPI_Recv(ibuf, 4, MPI_INTEGER4, irankfrom, irankfrom, MPI_COMM_WORLD, istats, ierr)

      nztmp = iz_end - iz_sta + 1
      nytmp = ibuf(4) - ibuf(3) + 1
      nxtmp = ibuf(2) - ibuf(1) + 1
      icnt = nxtmp * nytmp * nztmp
      allocate(dbuf(icnt))
      iii=1
      do iz=iz_sta, iz_end
        do iy=ibuf(3), ibuf(4)
          do ix=ibuf(1), ibuf(2)
            dbuf(iii) = resp_tmp(ix, iy, iz)
            iii = iii + 1
          enddo
        enddo
      enddo
      call MPI_Send(dbuf, icnt, MPI_REAL8, irankfrom, 157, MPI_COMM_WORLD, ierr)
      deallocate(dbuf)
    enddo

    do iz=iz_sta, iz_end
      do iy=ny_sta, ny_end
        do ix=nx_sta, nx_end
          resp(ix, iy, iz) = resp_tmp(ix, iy, iz)
        enddo
      enddo
    enddo

    deallocate ( resp_tmp )
  else
    call MPI_Send(myrank, 1, MPI_INTEGER4, 0, 135, MPI_COMM_WORLD, ierr)
    ibuf(1) = nx_sta
    ibuf(2) = nx_end
    ibuf(3) = ny_sta
    ibuf(4) = ny_end
    call MPI_Send(ibuf, 4, MPI_INTEGER4, 0, myrank, MPI_COMM_WORLD, ierr)
    icnt = (nx_end - nx_sta + 1) * (ny_end - ny_sta + 1) * (iz_end - iz_sta + 1)
    allocate(dbuf(icnt))
    call MPI_Recv(dbuf, icnt, MPI_REAL8, 0, 157, MPI_COMM_WORLD, istats, ierr)
    iii = 1
    do iz=iz_sta, iz_end
      do iy=ny_sta, ny_end
        do ix=nx_sta, nx_end
          resp(ix, iy, iz) = dbuf(iii)
          iii = iii + 1
        enddo
      enddo
    enddo
    deallocate(dbuf)
  endif
  deallocate(istats)
#else
    do iz=iz_sta, iz_end
      do iy=ny_sta, ny_end
        do ix=nx_sta, nx_end
          resp(ix, iy, iz) = resp_tmp(ix, iy, iz)
        enddo
      enddo
    enddo
    deallocate ( resp_tmp )
#endif

  call WRITE_TIMER( 'DONE.' )
  return
end subroutine response
