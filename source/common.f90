! =====================================================================
module commondata
! ---------------------------------------------------------------------
! Programed by d.satoh (2018.05.08)
! Modified  by d.satoh (2018.07.20)
! Modified  by d.satoh (2018.11.06)
! Modified  by d.satoh (2019.04.09)
! Modified  by d.satoh (2022.05.16)
! Modified  by d.satoh (2022.07.27)
! ---------------------------------------------------------------------
#ifdef USE_MPI
  use mpi
#endif
! =====================================================================
! VERSION
! =====================================================================
  character(len=18), parameter :: VERSION = '02.10 (2022/07/29)'

! =====================================================================
! PARAMETERs
! =====================================================================
! ---------------------------------------------------------------------
! FIXED INPUT DATA FILENAME
! ---------------------------------------------------------------------
  character(len=8),  parameter :: cfname_zda = './Z.data'
  character(len=12), parameter :: cfname_plu = './PLUME.data'
  character(len=13), parameter :: cfname_gnd = './GROUND.data'
  character(len=13), parameter :: cfname_sld = './SHIELD.data'
  character(len=16), parameter :: cfname_elv = './ELEVATION.data'
! ---------------------------------------------------------------------
  integer :: imode  ! 0:Total, 1:Ground, 2:Plume
! ---------------------------------------------------------------------
  character(len=512) :: ctitle
! ---------------------------------------------------------------------
! TAGET NUCLIDE
! ---------------------------------------------------------------------
  character(len=128) :: file
! ---------------------------------------------------------------------
! SHIELD
! ---------------------------------------------------------------------
  real(8) :: att  ! (cm^2/g), total attenuation coefficient for concrete
  real(8) :: den  ! (g/cm^3), effective density of shield mixed with air and concrete
  integer, parameter :: ishield = 9999  ! Special ID for shielding material
! ---------------------------------------------------------------------
! RESPONSE FUNCTION
! ---------------------------------------------------------------------
  integer, parameter :: nx0 = 1001        ! number of x-data in response matrix
  integer, parameter :: ny0 = 1001        ! number of y-data in response matrix
! ---------------------------------------------------------------------
  integer, parameter :: nalt = 17
! ---------------------------------------------------------------------
  character(len=5) :: PATH(1:nalt) = (/ &
  '001m', &
  '005m', &
  '010m', &
  '020m', &
  '030m', &
  '040m', &
  '050m', &
  '060m', &
  '080m', &
  '100m', &
  '125m', &
  '150m', &
  '200m', &
  '350m', &
  '500m', &
  '750m', &
  '1000m' /)
! ---------------------------------------------------------------------
  real(8) :: ALT(1:nalt) = (/ &
  001.d0, &
  005.d0, &
  010.d0, &
  020.d0, &
  030.d0, &
  040.d0, &
  050.d0, &
  060.d0, &
  080.d0, &
  100.d0, &
  125.d0, &
  150.d0, &
  200.d0, &
  350.d0, &
  500.d0, &
  750.d0, &
  1000.d0 /)
! ---------------------------------------------------------------------
! for RESTART
! ---------------------------------------------------------------------
  integer, parameter :: IFDRESPAR = 250
  integer, parameter :: IFDRESDAT = 251
  character(len=128) :: cdirname
  character(len=128) :: crestartpar
  character(len=128) :: crestartdat
  character(len=13), parameter :: cfnamepar = 'RESTART.param'
  character(len=12), parameter :: cfnamepat = 'RESTART.part'
  character(len=12), parameter :: cfnamedat = 'RESTART.data'
  character(len=11), parameter :: cfnamepos = 'RESTART.pos'
  character(len=12), parameter :: cfnamedos = 'RESTART.dose'
! ---------------------------------------------------------------------

! =====================================================================
! VARIABLEs
! =====================================================================
  integer :: iz_sta, iz_end
! ---------------------------------------------------------------------
! RESOLUTION
! ---------------------------------------------------------------------
  integer :: irs   ! mesh resolution (m)
! ---------------------------------------------------------------------
! SOURCE REGION
! ---------------------------------------------------------------------
  integer :: ns_x_sta
  integer :: ns_x_end
  integer :: ns_y_sta
  integer :: ns_y_end
  integer :: ns_z_sta
  integer :: ns_z_end
! ---------------------------------------------------------------------
! SOURCE REGION (all region at time of parallel execution)
! ---------------------------------------------------------------------
  integer :: ns_x_sta_all
  integer :: ns_x_end_all
  integer :: ns_y_sta_all
  integer :: ns_y_end_all
! ---------------------------------------------------------------------
! TARGET REGION for all area
! ---------------------------------------------------------------------
  integer :: nt_x_sta
  integer :: nt_x_end
  integer :: nt_y_sta
  integer :: nt_y_end
! ---------------------------------------------------------------------
! TARGET REGION for calculation area
! ---------------------------------------------------------------------
  integer :: nc_x_sta
  integer :: nc_x_end
  integer :: nc_y_sta
  integer :: nc_y_end
! ---------------------------------------------------------------------
! for RESTART
! ---------------------------------------------------------------------
  ! FLAG : 0= initial start, 1= restart [default = 0]
  integer :: irestart
  ! restart output: 0= no, 1= yes [default = 1]
  integer :: irestart_out
  ! timing: 0= every PY, 1= count PY, other= count sec.(realtime) [def = 1]
  integer :: irestart_tim
  ! counter: number of lines, seconds(realtime) [default = 1]
  integer :: irestart_cnt
! ---------------------------------------------------------------------
! Z DATA
! ---------------------------------------------------------------------
  real(8), allocatable :: zlow(:), zupp(:)
! ---------------------------------------------------------------------
! SHIELD
! ---------------------------------------------------------------------
  real(8) :: dmu
  integer :: nsh_min, nsh_max
  integer, allocatable :: ish(:,:,:)
! ---------------------------------------------------------------------
! SOURCE
! ---------------------------------------------------------------------
  real(8), allocatable :: din(:,:,:)
#ifdef USE_MPI
  real(8), allocatable :: din_tmp(:,:,:)
#endif
! ---------------------------------------------------------------------
! TARGET
! ---------------------------------------------------------------------
  real(8), allocatable :: dout(:,:)   ! Unit: micro-Sv/h at the irs x irs mesh
! ---------------------------------------------------------------------
! ELEVATION
! ---------------------------------------------------------------------
  real(8), allocatable :: delev(:,:)
  integer, allocatable :: ielev(:,:)
! ---------------------------------------------------------------------
! RESPONSE FUNCTION
! ---------------------------------------------------------------------
  integer :: nx, ny, imid
  integer :: nx_sta, nx_end, ny_sta, ny_end
! RESIZED
  real(8), allocatable :: resp(:,:,:)
! ---------------------------------------------------------------------
  real(8) :: dmuirs
! ---------------------------------------------------------------------
! file isopen flag
! ---------------------------------------------------------------------
  integer :: isopen_plume
  integer :: isopen_ground
  integer :: isopen_shield
  integer :: isopen_elevation
! ---------------------------------------------------------------------
! for MPI parallelization, but use SERIAL ver.(exclude ncpus)
! ---------------------------------------------------------------------
  ! number of CPUs
  integer :: ncpus
  ! own MPI process rank number
  integer :: myrank
  ! divided number for X-axis, Y-axis
  integer :: ndivx, ndivy
  ! position number in X,Y-axis direction
  integer :: myxpos, myypos
  ! number of elements for X,Y-axis direction
  integer :: npartx, nparty
  integer, allocatable :: idivx(:)
  integer, allocatable :: imapxy(:,:)
  ! divide method for source region [default = 1]
  ! 0: simple even dividing
  ! 1: elements count of source-region
  ! 2: distance from center of target-region to source-region
  ! 3: overlap area of source-region and target-region
  integer :: idivtype
! ---------------------------------------------------------------------
end module commondata
