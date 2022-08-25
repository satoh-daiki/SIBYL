! =====================================================================
subroutine output_result()
! ---------------------------------------------------------------------
! version history, comments, ...
! =====================================================================
!$ use omp_lib
  use commondata
! ---------------------------------------------------------------------
  implicit none
! ---------------------------------------------------------------------
! ---------------------------------------------------------------------
  integer :: ix, iy
  real(8) :: xx, yy, sqirs
  real(8) :: t1, t2
  integer :: idatetime(8)
! ---------------------------------------------------------------------
#ifdef USE_MPI_SPMD
  integer :: i, j, k
  integer :: icnt, ierr
  real(8),allocatable :: dout_tmp(:,:)
#endif
! ---------------------------------------------------------------------
!!!  call START_TIMER( 'OUTPUT result.out ...' )

#ifdef USE_MPI_SPMD
  allocate( dout_tmp(ns_x_sta_all:ns_x_end_all,ns_y_sta_all:ns_y_end_all) )
  dout_tmp = 0.0d0
  icnt = (ns_x_end_all - ns_x_sta_all + 1) * (ns_y_end_all - ns_y_sta_all + 1)
  call MPI_Reduce(dout(ns_x_sta_all:,ns_y_sta_all:), &
               &  dout_tmp(ns_x_sta_all:,ns_y_sta_all:), icnt, MPI_REAL8, &
               &  MPI_SUM, 0, MPI_COMM_WORLD, ierr)
  dout = dout_tmp
  deallocate(dout_tmp)
#endif

  if (myrank .eq. 0) then
    sqirs = dble(irs * irs)

    select case(imode)
    case(0)
      open(unit=40,file='result.out',status='unknown')
    case(1)
      open(unit=40,file='result_grd.out',status='unknown')
    case(2)
      open(unit=40,file='result_plu.out',status='unknown')
    end select

    write (40,'(A)') '###################################################################'
    write (40,'(A)') '# SIBYL:                                                          #'
    write (40,'(A)') '#   SImulation system powered BY Lattice dose-response functions  #'
    write (40,'(A,A20,35X,A)') '# Version: ',VERSION,'#'
    write (40,'(A)') '###################################################################'
    write (40,'(A)')     '#'
    write (40,'(A)')     '# ----------------------------------------'
    write (40,'(A,A)')   '# ',  trim(ctitle)
    write (40,'(A)')     '# ----------------------------------------'
    write (40,'(A)')     '# Input Parameters:'
    write (40,'(A,A)')   '#         file:     ',trim(file)
    write (40,'(A,I10)') '#        imode:     ',imode
    write (40,'(A,1PE10.3)') '#          att:     ',att
    write (40,'(A,1PE10.3)') '#          den:     ',den
    write (40,'(A,I10)') '#          irs:     ',irs
    write (40,'(A,I10)') '#     ns_x_sta:     ',ns_x_sta_all
    write (40,'(A,I10)') '#     ns_x_end:     ',ns_x_end_all
    write (40,'(A,I10)') '#     ns_y_sta:     ',ns_y_sta_all
    write (40,'(A,I10)') '#     ns_y_end:     ',ns_y_end_all
    write (40,'(A,I10)') '#     ns_z_sta:     ',ns_z_sta
    write (40,'(A,I10)') '#     ns_z_end:     ',ns_z_end
    write (40,'(A,I10)') '#     nt_x_sta:     ',nt_x_sta
    write (40,'(A,I10)') '#     nt_x_end:     ',nt_x_end
    write (40,'(A,I10)') '#     nt_y_sta:     ',nt_y_sta
    write (40,'(A,I10)') '#     nt_y_end:     ',nt_y_end
    write (40,'(A,I10)') '#     nc_x_sta:     ',nc_x_sta
    write (40,'(A,I10)') '#     nc_x_end:     ',nc_x_end
    write (40,'(A,I10)') '#     nc_y_sta:     ',nc_y_sta
    write (40,'(A,I10)') '#     nc_y_end:     ',nc_y_end
    write (40,'(A,I10)') '#     nsh_min:      ',nsh_min
    write (40,'(A,I10)') '#     nsh_max:      ',nsh_max
    write (40,'(A,I10)') '#     irestart:     ',irestart
    write (40,'(A,I10)') '#     irestart_tim: ',irestart_tim
    write (40,'(A,I10)') '#     irestart_cnt: ',irestart_cnt
    write (40,'(A)')     '# ----------------------------------------'

    do ix = nc_x_sta, nc_x_end
      do iy = nc_y_sta, nc_y_end
        xx = dble(ix * irs)
        yy = dble(iy * irs)
        write(40,'(2f9.1,es11.3)') xx, yy, (dout(ix,iy) / sqirs)
                                           ! (uSv/h @ 1.0 x 1.0 m^2)
      enddo
      write(40,'(a)') '   '
    enddo

    call DATE_AND_TIME(values = idatetime)
    write (40,'(A,I4.4,A,I2.2,A,I2.2,X,I2.2,A,I2.2,A,I2.2)') '# output done. @ ',idatetime(1),'/',idatetime(2),'/',idatetime(3),idatetime(5),':',idatetime(6),':',idatetime(7)

    close(40)
  endif ! myrank

  return
end subroutine output_result

