! =====================================================================
subroutine input(ierr)
! ---------------------------------------------------------------------
! version history, comments, ...
! =====================================================================
  use commondata
  use modtime
! ---------------------------------------------------------------------
  implicit none
! ---------------------------------------------------------------------
  integer :: ierr
! ---------------------------------------------------------------------
! parameters
  integer :: MAXMANPAR, MAXPARSTRLEN
  parameter(MAXMANPAR = 12, MAXPARSTRLEN = 128)
#ifdef USE_MPI
  integer :: MAXPARAMI, MAXPARAMR
  parameter(MAXPARAMI = 25, MAXPARAMR = 2)
#endif
  character(len=10) :: inputfile
  parameter(inputfile = 'input.data')
! ---------------------------------------------------------------------
  integer :: i
  integer :: iopend
  character(len=1024) :: linebuf, line
  integer :: pos
  character(len=MAXPARSTRLEN) :: key, value
  integer :: ival
  real(8) :: rval
  character(len=MAXPARSTRLEN) :: cval
! ---------------------------------------------------------------------
! for mandatory parameter check
  type keyflag
    character(len=MAXPARSTRLEN) :: key
    integer :: flag      ! 0: param not set, 1: param set
  end type keyflag
  type(keyflag) :: kf(1:MAXMANPAR) 
  integer :: inshflag
  integer :: icxsflag, icxeflag, icysflag, icyeflag
! ---------------------------------------------------------------------
#ifdef USE_MPI
  integer :: ibuf(MAXPARAMI)
  real(8) :: rbuf(MAXPARAMR)
#endif
! ---------------------------------------------------------------------

  call START_TIMER( 'READING INPUT DATA...' )

  ierr = 0

! set MANDATORY PARAMEtERs and reset flags
  kf( 1)%key = 'file'
  kf( 2)%key = 'irs'
  kf( 3)%key = 'ns_x_sta'
  kf( 4)%key = 'ns_x_end'
  kf( 5)%key = 'ns_y_sta'
  kf( 6)%key = 'ns_y_end'
  kf( 7)%key = 'ns_z_sta'
  kf( 8)%key = 'ns_z_end'
  kf( 9)%key = 'nt_x_sta'
  kf(10)%key = 'nt_x_end'
  kf(11)%key = 'nt_y_sta'
  kf(12)%key = 'nt_y_end' 
  do i=1, MAXMANPAR
    kf(i)%flag = 0
  enddo 

! ====================
! MANDATORY PARAMETERs
! ====================
!!!  file         = 'RESP_H_Cs-137.bin'
!!!  irs          =         1  ! mesh resolution (m)
!!!  ns_x_sta     =      -120  ! start point on x axis for SOURCE REGION
!!!  ns_x_end     =       119  ! end   point on x axis
!!!  ns_y_sta     =      -120  ! start point on y axis
!!!  ns_y_end     =       119  ! end   point on y axis
!!!  ns_z_sta     =         0  ! start point on z axis
!!!  ns_z_end     =        80  ! end   point on z axis
!!!  nt_x_sta     =      -120  ! start point on x axis for TARGET REGION
!!!  nt_x_end     =       119  ! end   point on x axis
!!!  nt_y_sta     =      -120  ! start point on y axis
!!!  nt_y_end     =       119  ! end   point on y axis

! =======================
! FIXED PARAMETER
! =======================
  nsh_min      =         1  ! minimum mesh number of SHIELD along z axis

! =======================
! set OPTIONAL PARAMETERs default value
! =======================
  imode        =         0  ! 0:Total, 1:Ground, 2:Plume
  att          =  8.118d-2  ! (cm^2/g), total attenuation coefficient for concrete
  den          =  2.400d-1  ! (g/cm^3), effective density of shield mixed with air and concrete
  ctitle       = 'SIBYL: SImulation system powered BY Latticed dose-response functions'
  cdirname     = 'RESTART'  ! directory name for restart data
  nsh_max      =         0  ! maximum mesh number of SHIELD along z axis
    inshflag   =         0  !   if (inshflag .eq. 0) nsh_max = ns_z_end
  irestart     =         0  ! restart flag:  0= initial, 1= restart
  irestart_out =         1  ! restart out: 0= no, 1=yes
  irestart_tim =         1  ! output timing: 1= count PY, other= count sec.(realtime)
  irestart_cnt =         1  ! num of PY-lines, seconds, etc.
  ndivx        =         1  ! num of divide for X-axis
  ndivy        =         1  !                   Y-axis
  idivtype     =        99  ! divide method for source region: 0= even, 1= elements, 2= distance, 3= area, default= elements
  icxsflag     =         0  ! if (icxsflag .eq. 0) nc_x_sta = nt_x_sta
  icxeflag     =         0  ! if (icxeflag .eq. 0) nc_x_end = nt_x_end
  icysflag     =         0  ! if (icysflag .eq. 0) nc_y_sta = nt_y_sta
  icyeflag     =         0  ! if (icyeflag .eq. 0) nc_y_end = nt_y_end
! ---------------------------------------------------------------------

  if (myrank .eq. 0) then
!   --- read parameter & bcast ---
    iopend = 0
    open(99, file=inputfile, status='old', err=999)
    iopend = 1
    do
      read(99, '(A)', end=888) linebuf

      if ((linebuf(1:1) .eq. '!') .or. (linebuf(1:1) .eq. '&')) cycle

      pos = scan(linebuf, '!')
      if (pos .ne. 0) then
        write(line,'(A)') adjustl(trim(linebuf(1:pos-1)))
      else
        write(line,'(A)') adjustl(trim(linebuf))
      endif

      pos = scan(line, '=')
      if (pos .eq. 0) then
        write(*,*) 'ERROR: Unknown parameter: not KEY=VALUE'
        write(*,*) '         "',trim(line),'"'
        ierr = 1
      else
        write(key,  '(A)') trim(line(1:pos-1))
        write(value,'(A)') trim(adjustl(line(pos+1:)))
! --- INTEGER VARIABLEs ---
        if (trim(key) .eq. 'imode') then
          read(value,*) ival
          imode = ival
        else if (trim(key) .eq. 'irs') then
          read(value,*) ival
          irs = ival
          call setflag(trim(key))
        else if (trim(key) .eq. 'ns_x_sta') then
          read(value,*) ival
          ns_x_sta = ival
          call setflag(trim(key))
        else if (trim(key) .eq. 'ns_x_end') then
          read(value,*) ival
          ns_x_end = ival
          call setflag(trim(key))
        else if (trim(key) .eq. 'ns_y_sta') then
          read(value,*) ival
          ns_y_sta = ival
          call setflag(trim(key))
        else if (trim(key) .eq. 'ns_y_end') then
          read(value,*) ival
          ns_y_end = ival
          call setflag(trim(key))
        else if (trim(key) .eq. 'ns_z_sta') then
          read(value,*) ival
          ns_z_sta = ival
          call setflag(trim(key))
        else if (trim(key) .eq. 'ns_z_end') then
          read(value,*) ival
          ns_z_end = ival
          call setflag(trim(key))
        else if (trim(key) .eq. 'nt_x_sta') then
          read(value,*) ival
          nt_x_sta = ival
          call setflag(trim(key))
        else if (trim(key) .eq. 'nt_x_end') then
          read(value,*) ival
          nt_x_end = ival
          call setflag(trim(key))
        else if (trim(key) .eq. 'nt_y_sta') then
          read(value,*) ival
          nt_y_sta = ival
          call setflag(trim(key))
        else if (trim(key) .eq. 'nt_y_end') then
          read(value,*) ival
          nt_y_end = ival
          call setflag(trim(key))
        else if (trim(key) .eq. 'nc_x_sta') then
          read(value,*) ival
          nc_x_sta = ival
          icxsflag = 1    ! for check VALUE SET
        else if (trim(key) .eq. 'nc_x_end') then
          read(value,*) ival
          nc_x_end = ival
          icxeflag = 1    ! for check VALUE SET
        else if (trim(key) .eq. 'nc_y_sta') then
          read(value,*) ival
          nc_y_sta = ival
          icysflag = 1    ! for check VALUE SET
        else if (trim(key) .eq. 'nc_y_end') then
          read(value,*) ival
          nc_y_end = ival
          icyeflag = 1    ! for check VALUE SET
        else if (trim(key) .eq. 'nsh_min') then
! nsh_min = 1 (fixed parameter)
!         read(value,*) ival
!         nsh_min = ival
          nsh_min = 1
          write(*,*) 'WARNING: "nsh_min" is set, but "nsh_min" is fixed. (nsh_min = 1)'
        else if (trim(key) .eq. 'nsh_max') then
          read(value,*) ival
          nsh_max = ival
          inshflag = 1    ! for check VALUE SET
        else if (trim(key) .eq. 'irestart') then
          read(value,*) ival
          irestart = ival
        else if (trim(key) .eq. 'irestart_out') then
          read(value,*) ival
          irestart_out = ival
        else if (trim(key) .eq. 'irestart_tim') then
          read(value,*) ival
          irestart_tim = ival
        else if (trim(key) .eq. 'irestart_cnt') then
          read(value,*) ival
          irestart_cnt = ival
        else if (trim(key) .eq. 'ndivx') then
#ifdef USE_MPI
#ifdef USE_MPI_SPMD
          read(value,*) ival
          ndivx = ival
#else
          ndivx = 1
          write(*,*) 'WARNING: "ndivx" is set, but "ndivx" is ignored in MPI(master/slave) version.'
#endif
#else
          ndivx = 1
          write(*,*) 'WARNING: "ndivx" is set, but "ndivx" is ignored in Non-MPI version.'
#endif
        else if (trim(key) .eq. 'ndivy') then
#ifdef USE_MPI
#ifdef USE_MPI_SPMD
          read(value,*) ival
          ndivy = ival
#else
          ndivy = 1
          write(*,*) 'WARNING: "ndivy" is set, but "ndivy" is ignored in MPI(master/slave) version.'
#endif
#else
          ndivy = 1
          write(*,*) 'WARNING: "ndivy" is set, but "ndivy" is ignored in Non-MPI version.'
#endif
        else if (trim(key) .eq. 'idivtype') then
          read(value,*) ival
          idivtype = ival
! --- REAL VARIABLEs ---
        else if (trim(key) .eq. 'att') then
          read(value,*) rval
          att = rval
        else if (trim(key) .eq. 'den') then
          read(value,*) rval
          den = rval
! --- CHARACTER VARIABLEs ---
        else if (trim(key) .eq. 'file') then
          read(value,*) cval
          file = trim(cval)
          call setflag(trim(key))
        else if (trim(key) .eq. 'ctitle') then
          read(value,*) cval
          ctitle = trim(cval)
        else if (trim(key) .eq. 'cdirname') then
          read(value,*) cval
          cdirname = trim(cval)
! --- NOT MATCH ---
        else
          write(*,*) 'ERROR: Unknown parameter'
          write(*,*) '         KEY="',trim(key),'"'
          ierr = 1
        endif
!           --------------------------------------------------------------
      endif
    enddo

888 continue
    close(99)
999 continue
    if (iopend .eq. 0) then
!     ----- File Open ERROR -----
      write(*,*) 'ERROR: "',trim(inputfile),'" can not open.'
      ierr = 1
    endif

    ! check mandatory parameters
    do i=1, MAXMANPAR
      if (kf(i)%flag .eq. 0) then
        ierr = 1
        write(*,*) 'ERROR: Missing parameter'
        do ival=1, MAXMANPAR
          if (kf(ival)%flag .eq. 0) then
            write(*,*) '         ',trim(kf(ival)%key)
          endif
        enddo
        exit
      endif
    enddo
  endif ! myrank

#ifdef USE_MPI
  call MPI_Bcast(ierr, 1, MPI_INTEGER4, 0, MPI_COMM_WORLD, i)
#endif
  if (ierr .eq. 1) then
    return
  endif

  if (myrank .eq. 0) then
    ! check "nsh_max" set ?
    if (inshflag .eq. 0) then
      nsh_max = ns_z_end
      write(*,*) 'WARNING: nsh_max not set, nsh_max is set to ns_z_end.'
    endif
    ! check "nc_x_sta" set ?
    if (icxsflag .eq. 0) then
      nc_x_sta = nt_x_sta
      write(*,*) 'WARNING: nc_x_sta not set, nc_x_sta is set to nt_x_sta.'
    endif
    ! check "nc_x_end" set ?
    if (icxeflag .eq. 0) then
      nc_x_end = nt_x_end
      write(*,*) 'WARNING: nc_x_end not set, nc_x_end is set to nt_x_end.'
    endif
    ! check "nc_y_sta" set ?
    if (icysflag .eq. 0) then
      nc_y_sta = nt_y_sta
      write(*,*) 'WARNING: nc_y_sta not set, nc_y_sta is set to nt_y_sta.'
    endif
    ! check "nc_y_end" set ?
    if (icyeflag .eq. 0) then
      nc_y_end = nt_y_end
      write(*,*) 'WARNING: nc_y_end not set, nc_y_end is set to nt_y_end.'
    endif

#ifdef USE_MPI
    ibuf( 1) = imode
    ibuf( 2) = irs
    ibuf( 3) = ns_x_sta
    ibuf( 4) = ns_x_end
    ibuf( 5) = ns_y_sta
    ibuf( 6) = ns_y_end
    ibuf( 7) = ns_z_sta
    ibuf( 8) = ns_z_end
    ibuf( 9) = nt_x_sta
    ibuf(10) = nt_x_end
    ibuf(11) = nt_y_sta
    ibuf(12) = nt_y_end
    ibuf(13) = nc_x_sta
    ibuf(14) = nc_x_end
    ibuf(15) = nc_y_sta
    ibuf(16) = nc_y_end
    ibuf(17) = nsh_min
    ibuf(18) = nsh_max
    ibuf(19) = irestart
    ibuf(20) = irestart_out
    ibuf(21) = irestart_tim
    ibuf(22) = irestart_cnt
    ibuf(23) = ndivx
    ibuf(24) = ndivy
    ibuf(25) = idivtype
    call MPI_Bcast(ibuf, MAXPARAMI, MPI_INTEGER4, 0, MPI_COMM_WORLD, ierr)
    rbuf(1) = att
    rbuf(2) = den
    call MPI_Bcast(rbuf, MAXPARAMR, MPI_REAL8, 0, MPI_COMM_WORLD, ierr)
!   --- need to send CHAR. VAR.s, if using file I/O at other than rank0
  else
!   --- (myrank .ne. 0) recieve parameter ---
    call MPI_Bcast(ibuf, MAXPARAMI, MPI_INTEGER4, 0, MPI_COMM_WORLD, ierr)
    imode        = ibuf( 1)
    irs          = ibuf( 2)
    ns_x_sta     = ibuf( 3)
    ns_x_end     = ibuf( 4)
    ns_y_sta     = ibuf( 5)
    ns_y_end     = ibuf( 6)
    ns_z_sta     = ibuf( 7)
    ns_z_end     = ibuf( 8)
    nt_x_sta     = ibuf( 9)
    nt_x_end     = ibuf(10)
    nt_y_sta     = ibuf(11)
    nt_y_end     = ibuf(12)
    nc_x_sta     = ibuf(13)
    nc_x_end     = ibuf(14)
    nc_y_sta     = ibuf(15)
    nc_y_end     = ibuf(16)
    nsh_min      = ibuf(17)
    nsh_max      = ibuf(18)
    irestart     = ibuf(19)
    irestart_out = ibuf(20)
    irestart_tim = ibuf(21)
    irestart_cnt = ibuf(22)
    ndivx        = ibuf(23)
    ndivy        = ibuf(24)
    idivtype     = ibuf(25)
    call MPI_Bcast(rbuf, MAXPARAMR, MPI_REAL8, 0, MPI_COMM_WORLD, ierr)
    att          = rbuf(1)
    den          = rbuf(2)
#endif
  endif

! ---------------------------------------------------------------------
! calculation of constant values
  dmu = att * den * 1.d+2    ! (1/m)
  nx = dint( (nx0 - irs) / 2.d0 / irs ) * 2 + 1
  ny = dint( (ny0 - irs) / 2.d0 / irs ) * 2 + 1
  dmuirs = -1.0d0 * dmu * irs
  imid = dint( (ny0 - irs) / 2.d0 / irs ) + 1
! ---------------------------------------------------------------------
! CHECKING
  if( nt_x_sta < ns_x_sta ) then
    if (myrank .eq. 0) write(*,*) "ERROR: nt_x_sta < ns_x_sta"
    ierr = 1
  end if
  if( nt_x_end > ns_x_end ) then
    if (myrank .eq. 0) write(*,*) "ERROR: nt_x_end > ns_x_end"
    ierr = 1
  end if
  if( nt_y_sta < ns_y_sta ) then
    if (myrank .eq. 0) write(*,*) "ERROR: nt_y_sta < ns_y_sta"
    ierr = 1
  end if
  if( nt_y_end > ns_y_end ) then
    if (myrank .eq. 0) write(*,*) "ERROR: nt_y_end > ns_y_end"
    ierr = 1
  end if
  if((nc_x_sta < nt_x_sta) .or. (nt_x_end < nc_x_sta)) then
    if (myrank .eq. 0) write(*,*) "ERROR: nc_x_sta is wrong value"
    ierr = 1
  endif
  if((nc_x_end < nt_x_sta) .or. (nt_x_end < nc_x_end)) then
    if (myrank .eq. 0) write(*,*) "ERROR: nc_x_end is wrong value"
    ierr = 1
  endif
  if((nc_y_sta < nt_y_sta) .or. (nt_y_end < nc_y_sta)) then
    if (myrank .eq. 0) write(*,*) "ERROR: nc_y_sta is wrong value"
    ierr = 1
  endif
  if((nc_y_end < nt_y_sta) .or. (nt_y_end < nc_y_end)) then
    if (myrank .eq. 0) write(*,*) "ERROR: nc_y_end is wrong value"
    ierr = 1
  endif
! ---------------------------------------------------------------------
  select case(imode)
  case(0)
    iz_sta = ns_z_sta
    iz_end = ns_z_end
  case(1)
    iz_sta = 0
    iz_end = 0
  case(2)
    iz_sta = 1
    iz_end = ns_z_end
  case default
    write(*,*) 'ERROR; imode =',imode
    ierr = 1
  end select
! ---------------------------------------------------------------------
#ifndef USE_MPI_SPMD
  myxpos = 1
  myypos = 1
#else
  call check_divnum(ierr)                                !@ mpisubs.f90
  call check_abort(ierr)                               !@ othersubs.f90
  myxpos = mod(myrank, ndivx) + 1                 ! 1..ndivx
  myypos = mod(int(myrank / ndivx), ndivy) + 1    ! 1..ndivy
#endif

! ---------------------------------------------------------------------
! temporary value set
  ns_x_sta_all = ns_x_sta
  ns_x_end_all = ns_x_end
  ns_y_sta_all = ns_y_sta
  ns_y_end_all = ns_y_end
! ---------------------------------------------------------------------

  call WRITE_TIMER( 'DONE.' )

  return


contains
  subroutine setflag(str)
    implicit none
    character(*) :: str
    integer :: i
    do i=1, MAXMANPAR
      if (trim(kf(i)%key) .eq. trim(str)) then
        kf(i)%flag = 1
        return
      endif
    enddo
#ifdef DEBUG
    write(*,*) 'set flag fail. : ', trim(str)
#endif
  end subroutine

end subroutine input
