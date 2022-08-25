! =====================================================================
program main
! ---------------------------------------------------------------------
! SIBYL:
! SImulation system powered BY Lattice dose-response functions
! for the estimation of external doses from a radioactive plume.
! Version 2.1
! ---------------------------------------------------------------------
! size of response matrix:
! mesh = 1001 x 1001 = 1002001
! reso = 1.0 m
! unit = (mSv/h)*(m^3/kBq)
!
! source volume for response matrix:
! 1.0 m x 1.0 m x 1.0 m at altitude z
! ---------------------------------------------------------------------
! size of response matrix for source on the ground:
! mesh = 1001 x 1001 = 1002001
! reso = 1.0 m
! unit = (mSv/h)*(m^2/kBq)
!
! source area for response matrix:
! 1.0 m x 1.0 m on the ground
! ---------------------------------------------------------------------
! Programed by d.satoh (2018.11.06) ! PLUME CODE Ver 1.0
! Modified  by d.satoh (2019.03.06)
! Modified  by d.satoh (2019.04.04) ! PLUME CODE Ver 2.0
! Modified  by d.satoh (2019.04.09) ! 
! Modified  by d.satoh (2019.04.10) ! 
! Modified  by d.satoh (2019.04.24) ! 
! Modified  by d.satoh (2019.08.23) ! PLUME CODE Ver 3.0
! Modified  by d.satoh (2019.10.25) ! PLUME CODE Ver 4.0
! ---------------------------------------------------------------------
! Modified  by d.satoh (2019.12.18) ! SIBYL Ver 1.0
! Modified  by d.satoh (2020.01.09)
! Confirmed by d.satoh (2022.05.16) ! SIBYL ver 2.0 (CCSE Distribution Package)
! Confirmed by d.satoh (2022.07.29) ! SIBYL ver 2.1
! =====================================================================
!$ use omp_lib
! ---------------------------------------------------------------------
  use commondata
  use modtime
! ---------------------------------------------------------------------
  implicit none
! ---------------------------------------------------------------------
  integer :: ierr
  integer :: irestart_y
  integer :: idatetime(8)
! ---------------------------------------------------------------------

! ---------------------------------------------------------------------
#ifdef USE_MPI
  call MPI_Init(ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, myrank, ierr)
  call MPI_Comm_size(MPI_COMM_WORLD, ncpus, ierr)
#else
  myrank = 0
#endif
! ---------------------------------------------------------------------
! START OF SIMULATION
  call INIT_TIMER()                                      !@ modtime.f90
  if (myrank .eq. 0) then
    write (*,'(A)') '###################################################################'
    write (*,'(A)') '# SIBYL:                                                          #'
    write (*,'(A)') '#   SImulation system powered BY Lattice dose-response functions  #'
    write (*,'(A,A20,35X,A)') '# Version: ',VERSION,'#'
    write (*,'(A)') '###################################################################'
    write (*,*) ''
  endif
! ---------------------------------------------------------------------
  call input(ierr)                                         !@ input.f90
  call check_abort(ierr)                               !@ othersubs.f90
! ---------------------------------------------------------------------

! PRINT ENVIRONMENT
  if (myrank .eq. 0) then
    write (*,'(A)')    '----------------------------------------'
    write (*,'(A)')    trim(ctitle)
    write (*,'(A)')    '----------------------------------------'
    write (*,'(A)')    'Parameters:'
    write (*,'(A,A)')  '        file:     ',trim(file)
    write (*,'(A,I10)') '       imode:     ',imode
    write (*,'(A,1PE10.3)') '         att:     ',att
    write (*,'(A,1PE10.3)') '         den:     ',den
    write (*,'(A,I10)') '         irs:     ',irs
    write (*,'(A,I10)') '    ns_x_sta:     ',ns_x_sta
    write (*,'(A,I10)') '    ns_x_end:     ',ns_x_end
    write (*,'(A,I10)') '    ns_y_sta:     ',ns_y_sta
    write (*,'(A,I10)') '    ns_y_end:     ',ns_y_end
    write (*,'(A,I10)') '    ns_z_sta:     ',ns_z_sta
    write (*,'(A,I10)') '    ns_z_end:     ',ns_z_end
    write (*,'(A,I10)') '    nt_x_sta:     ',nt_x_sta
    write (*,'(A,I10)') '    nt_x_end:     ',nt_x_end
    write (*,'(A,I10)') '    nt_y_sta:     ',nt_y_sta
    write (*,'(A,I10)') '    nt_y_end:     ',nt_y_end
    write (*,'(A,I10)') '    nc_x_sta:     ',nc_x_sta
    write (*,'(A,I10)') '    nc_x_end:     ',nc_x_end
    write (*,'(A,I10)') '    nc_y_sta:     ',nc_y_sta
    write (*,'(A,I10)') '    nc_y_end:     ',nc_y_end
    write (*,'(A,I10)') '    nsh_min:      ',nsh_min
    write (*,'(A,I10)') '    nsh_max:      ',nsh_max
    write (*,'(A,I10)') '    irestart:     ',irestart
    write (*,'(A,I10)') '    irestart_out: ',irestart_out
    write (*,'(A,I10)') '    irestart_tim: ',irestart_tim
    write (*,'(A,I10)') '    irestart_cnt: ',irestart_cnt
#ifdef USE_MPI_SPMD
    write (*,'(A,I10)') '    ndivx:        ',ndivx
    write (*,'(A,I10)') '    ndivy:        ',ndivy
    write (*,'(A,I10)') '    idivtype:     ',idivtype
#endif
    write (*,'(A)')    '----------------------------------------'
#ifdef USE_MPI
    write (*,'(A)')    '========================'
!$  if (.true.) then
!$    write (*,'(A)')    'MPI/OpenMP Parameters:'
!$  else
      write (*,'(A)')    'MPI Parameters:'
!$  endif
#ifdef USE_MPI_SPMD
    write (*,'(A)') '    MPI Model:      SPMD'
#else
    write (*,'(A)') '    MPI Model:       M/S'
#endif
    write (*,'(A,I8)') '    ncpus:      ',ncpus
!$  write (*,'(A,I8)') '    nthreads:   ',OMP_GET_MAX_THREADS()
#ifdef USE_MPI_SPMD
    write (*,'(A,I8)') '    ndivx:      ',ndivx
    write (*,'(A,I8)') '    ndivy:      ',ndivy
#endif
    write (*,'(A)')    '========================'
#else
!$  write (*,'(A)')    '========================'
!$  write (*,'(A)')    'OpenMP Parameters:'
!$  write (*,'(A,I8)') '    ncpus:      ',OMP_GET_NUM_PROCS()
!$  write (*,'(A,I8)') '    nprocs:     ',OMP_GET_NUM_THREADS()
!$  write (*,'(A,I8)') '    nthreads:   ',OMP_GET_MAX_THREADS()
!$  write (*,'(A)')    '========================'
#endif
  endif ! myrank

! ---------------------------------------------------------------------
! RESTART check
  call check_restart()                                   !@ restart.f90
! ---------------------------------------------------------------------
  call allocate_arrays_1st()                     !@ allocate_arrays.f90
  if (irestart .eq. 0) then
    if (irestart_out .eq. 1) then
      call check_restart_dir(ierr)                       !@ restart.f90
      call check_abort(ierr)                           !@ othersubs.f90
    endif
    call read_din(ierr)                                !@ read_data.f90
    call check_abort(ierr)                             !@ othersubs.f90
#ifdef USE_MPI_SPMD
    call calc_index()                                    !@ mpisubs.f90
    if (irestart_out .eq. 1) then
      call write_partition()                             !@ mpisubs.f90
    endif
  else
    call read_partition(ierr)                            !@ mpisubs.f90
    call check_abort(ierr)                             !@ othersubs.f90
#endif
  endif ! if (irestart)
  call set_index()                                     !@ othersubs.f90
! ---------------------------------------------------------------------
  call allocate_arrays()                         !@ allocate_arrays.f90
! ---------------------------------------------------------------------
  call read_zdata(ierr)                                !@ read_data.f90
  call check_abort(ierr)                               !@ othersubs.f90
! ---------------------------------------------------------------------
  if (irestart .eq. 0) then
    ! -----------------------------------------------------------------
    call read_initial_data(ierr)                       !@ read_data.f90
    call check_abort(ierr)                             !@ othersubs.f90
#ifdef USE_MPI
    call distribute_initial_data(ierr)                   !@ mpisubs.f90
    call check_abort(ierr)                             !@ othersubs.f90
#endif
    call calculate_initial_data()                      !@ read_data.f90
    ! -----------------------------------------------------------------
    call response()                                     !@ response.f90
    ! -----------------------------------------------------------------
    if (irestart_out .eq. 1) then
      call write_restart_parameter()        !@ rw_restart_parameter.f90
      call write_restart_data()                  !@ rw_restart_data.f90
      call open_restart_doses_initial(irestart_y, ierr) !@ rw_restart_doses.f90
      call check_abort(ierr)                           !@ othersubs.f90
    else
!--      irestart_y = nt_y_sta
      irestart_y = nc_y_sta
    endif
  else
    ! -----------------------------------------------------------------
    call read_restart_data(ierr)                 !@ rw_restart_data.f90
    call check_abort(ierr)                             !@ othersubs.f90
#ifdef USE_MPI_MS
    call distribute_restart_data(ierr)                   !@ mpisubs.f90
    call check_abort(ierr)                             !@ othersubs.f90
#endif
    call open_restart_doses_restart(irestart_y, ierr) !@ rw_restart_doses.f90
    call check_abort(ierr)                             !@ othersubs.f90
    ! -----------------------------------------------------------------
  endif
! ---------------------------------------------------------------------
  call compute_doses(irestart_y)                   !@ compute_doses.f90
! ---------------------------------------------------------------------
  if (irestart_out .eq. 1) then
    call close_restart_doses()                  !@ rw_restart_doses.f90
  endif
! ---------------------------------------------------------------------
  call output_result()                             !@ output_result.f90
! ---------------------------------------------------------------------
  call deallocate_arrays()                       !@ allocate_arrays.f90
! ---------------------------------------------------------------------
#ifdef USE_MPI
  call MPI_Barrier(MPI_COMM_WORLD, ierr)
  call MPI_Finalize(ierr)
#endif
! ---------------------------------------------------------------------
  if (myrank .eq. 0) then
    write (*,'(A)') 'PROGRAM DONE.'
!   write (*,'(A)') '###################################################################'
    call DATE_AND_TIME(values = idatetime)
    write (*,'(A,I4.4,A,I2.2,A,I2.2,X,I2.2,A,I2.2,A,I2.2,A)') '##### ',idatetime(1),'/',idatetime(2),'/',idatetime(3),idatetime(5),':',idatetime(6),':',idatetime(7),' #########################################'
  endif

  call termination
end program main
