! =====================================================================
program GPM
! ---------------------------------------------------------------------
! GPM:
! Gaussian Plume Model
! ---------------------------------------------------------------------
! Programed by d.satoh (2018.02.20)
! Modified  by d.satoh (2018.05.09)
! Modified  by d.satoh (2018.05.25)
! Modified  by d.satoh (2022.07.15) ! for SIBYL (CCSE Distribution Package)
! =====================================================================
  use commondata
! ---------------------------------------------------------------------
  implicit none
! ---------------------------------------------------------------------
! VARIABLE
! ---------------------------------------------------------------------
  real(8), allocatable :: C(:,:,:)
  real(8), allocatable :: zlo(:)
  real(8), allocatable :: zhi(:)
! ---------------------------------------------------------------------
  real(8):: resol
  real(8):: x, y, z
  real(8):: sigy, sigz
! ---------------------------------------------------------------------
  integer:: ix, iy, iz
  real(8):: dx, dy, dz
! ---------------------------------------------------------------------
  integer:: irx, iry, irz, nresol, io
  real(8):: Act
! ---------------------------------------------------------------------
  real(8):: pi = 4.0*atan(1.0)
! ---------------------------------------------------------------------
  character(len=1):: INP
! ---------------------------------------------------------------------
! READING PARAMETERS
! ---------------------------------------------------------------------
  write (*,'(A)') '#####################################'
  write (*,'(A)') '# GPM:                              #'
  write (*,'(A)') '#   Gaussian Plume Model for SIBYL  #'
  write (*,'(A)') '#####################################'
  write (*,*) ''

777 continue
  write (*,'(A)') 'CHOOSE the PARAMETER-SETTING METHOD:'
  write (*,'(A)') '1 = Standard input, 2 = From the external file...'
  read  (*,'(A1)') INP
  write (*,*) ''
  if( INP == "1" ) then
    call read
  else if( INP == "2" ) then
    call fread
  else
    goto 777
  end if
! ---------------------------------------------------------------------
! WRITING SUMMARY
! ---------------------------------------------------------------------
  write (*,'(A)') '#------------------------------------'
  write (*,'(A)') '# Summary of input parameters        '
  write (*,'(A)') '#------------------------------------'
  write (*,'(A)') '# GPM:                               '
  write (*,'(A)') '#------------------------------------'
  write (*,'(A)') '#  Pasquill-Gifford steady class =  '//PG
  write (*,'(A,f10.3,A)') '#  Emission rate = ', Q, ' (Bq/s) '
  write (*,'(A,f10.3,A)') '#  Wind velocity = ', U, ' (m/s)  '
  write (*,'(A,f10.3,A)') '#  Stack height  = ', H, ' (m)    '
  write (*,'(A)') '#------------------------------------'
  write (*,'(A)') '# Grid system:                       '
  write (*,'(A)') '#------------------------------------'
  write (*,'(A,i10,A)') '#  Resolution  = ', irs, ' (m)    '  
  write (*,'(A,i6,A,i6,A)') '#  (ns_x_sta, ns_x_end)  = (',ns_x_sta,',',ns_x_end,')'
  write (*,'(A,i6,A,i6,A)') '#  (ns_y_sta, ns_y_end)  = (',ns_y_sta,',',ns_y_end,')'
  write (*,'(A,i6,A,i6,A)') '#  (ns_z_sta, ns_z_end)  = (',ns_z_sta,',',ns_z_end,')'
  write (*,'(A)') '#------------------------------------'
! ---------------------------------------------------------------------
  open(unit=20,file='SUMMARY.out',status='unknown')
  write (20,'(A)') '#------------------------------------'
  write (20,'(A)') '# Summary of input parameters        '
  write (20,'(A)') '#------------------------------------'
  write (20,'(A)') '# GPM:                               '
  write (20,'(A)') '#------------------------------------'
  write (20,'(A)') '#  Pasquill-Gifford steady class =  '//PG
  write (20,'(A,f10.3,A)') '#  Emission rate = ', Q, ' (Bq/s) '
  write (20,'(A,f10.3,A)') '#  Wind velocity = ', U, ' (m/s)  '
  write (20,'(A,f10.3,A)') '#  Stack height  = ', H, ' (m)    '
  write (20,'(A)') '#------------------------------------'
  write (20,'(A)') '# Grid system:                       '
  write (20,'(A)') '#------------------------------------'
  write (20,'(A,i10,A)') '#  Resolution  = ', irs, ' (m)      '
  write (20,'(A,i6,A,i6,A)') '#  (ns_x_sta, ns_x_end)  = (',ns_x_sta,',',ns_x_end,')'
  write (20,'(A,i6,A,i6,A)') '#  (ns_y_sta, ns_y_end)  = (',ns_y_sta,',',ns_y_end,')'
  write (20,'(A,i6,A,i6,A)') '#  (ns_z_sta, ns_z_end)  = (',ns_z_sta,',',ns_z_end,')'
  write (20,'(A)') '#------------------------------------'
  close(20)
! ---------------------------------------------------------------------
  allocate( C(ns_x_sta:ns_x_end,ns_y_sta:ns_y_end,ns_z_sta:ns_z_end) )
  C(:,:,:) = 0.0d0

  allocate( zlo(ns_z_sta:ns_z_end) )
  allocate( zhi(ns_z_sta:ns_z_end) )
  open(unit=50,file='Z.data',mode='read',status='old',iostat=io)
  if( io /= 0 ) then
    write(*,'(A)') 'ERROR: Unable to open the file "Z.data"'
    call termination
    stop
  end if
  do iz = ns_z_sta, ns_z_end
    read(50,*) zlo(iz), zhi(iz)
    if( zhi(iz) - zlo(iz) < 1.0 ) then
      write(*,'(A)') 'ERROR: zhi - zlo < 1.0'
      call termination
      stop
    end if
  end do
  close(50)

  resol = dble(irs)
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  X_LOOP : do ix = ns_x_sta, ns_x_end
! ---------------------------------------------------------------------
    x = dble(ix)*resol

    write(*,'(A,f8.2,A)') 'x = ', x, ' (m)'

    if( x <= 0.0 ) then
      C(ix,:,:) = 0.0d0
      cycle X_LOOP
    end if

    if( x == 0.0 ) x = 5.0

    call pasquill(x,sigy,sigz)

!   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    Z_LOOP : do iz = ns_z_sta, ns_z_end
!   -------------------------------------------------------------------
      z = zlo(iz)
      nresol = int( zhi(iz) - zlo(iz) )
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      Y_LOOP : do iy = ns_y_sta, ns_y_end
!     -----------------------------------------------------------------
        y = dble(iy)*resol

        if( z >= H + 6.0*sigz .or. z < H - 6.0*sigz ) then
          C(ix,iy,iz) = 0.d0
        else if( y >= 6.0*sigy .or. y < -6.0*sigy ) then
          C(ix,iy,iz) = 0.d0
        else

          Act = 0.d0

          do irx = 1, irs
            dx = x + irx - 5.0d-1
            call pasquill(dx,sigy,sigz)
            
            do iry = 1, irs
              dy = y + iry - 5.0d-1
              do irz = 1, nresol
                dz = z + irz - 5.0d-1

                Act = Act + Q/(2.0*pi*sigy*sigz*U) * dexp(-1.0*dy**2/(2.0*sigy**2)) &
&                         * ( dexp(-1.0*(dz-H)**2/(2.0*sigz**2)) + dexp(-1.0*(dz+H)**2/(2.0*sigz**2)) )  ! (Bq/m^3)

              end do
            end do
          end do

          C(ix,iy,iz) = Act / (nresol*irs*irs)

        end if

!     -----------------------------------------------------------------
      end do Y_LOOP
!   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    end do Z_LOOP
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  end do X_LOOP
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! ---------------------------------------------------------------------
  write(*,'(A)') 'Outputting the resutls...'
  open(unit=10,file='PLUME.data',status='unknown')
! ---------------------------------------------------------------------
  do iz = 1, ns_z_end
    do iy = ns_y_sta, ns_y_end
      do ix = ns_x_sta, ns_x_end
        write(10,'(es11.4)') C(ix,iy,iz)
      end do
    end do
  end do
! ---------------------------------------------------------------------
  close(10)
  call termination
! ---------------------------------------------------------------------
  stop
end program GPM