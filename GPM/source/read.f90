! =====================================================================
subroutine read
! ---------------------------------------------------------------------
! Purpose is to read parameters from the standard input.
! ---------------------------------------------------------------------
! Programed by d.satoh (2022.07.26)
! =====================================================================
  use commondata
! ---------------------------------------------------------------------
  implicit none
! ---------------------------------------------------------------------
! READ PARAMETER FOR GPM
! ---------------------------------------------------------------------
  write (*,'(A)') 'ENTER the PASQUILL-GIFFORD STABILITY CLASS:'
  write (*,'(A)') '(Unsteady <-- A, B, C, D, E, F, G --> Steady)'
  write (*,'(A)') 'PG; [character]...'
  read  (*,*) PG
  if (PG /= 'A' .and. PG /= 'B' .and. PG /= 'C' .and. PG /= 'D' .and. PG /= 'E' .and. PG /= 'F' .and. PG /= 'G'&
& .and. PG /= 'a' .and. PG /= 'b' .and. PG /= 'c' .and. PG /= 'd' .and. PG /= 'e' .and. PG /= 'f' .and. PG /= 'g') then
    write(*,'(A)') 'ERROR: PG class = '//PG
    call termination
    stop
  end if
  write (*,*) ''

  write (*,'(A)') 'ENTER the PARAMETERS for GPM:'
  write (*,'(A)') 'Q (Bq/s); Emission rate of radioactive material [float]...'
  read  (*,*) Q
  if( Q <= 0 ) then
    write(*,'(A)') 'ERROR: Q <= 0'
    call termination
    stop
  end if
  write (*,'(A)') 'U (m/s); Mean wind velocity along the plume centerline [float]...'
  read  (*,*) U
  if( U <= 0 ) then
    write(*,'(A)') 'ERROR: U <= 0'
    call termination
    stop
  end if
  write (*,'(A)') 'H (m); Height of the plume centerline above the ground [float]...'
  read  (*,*) H
  if( H <= 0 ) then
    write(*,'(A)') 'ERROR: H <= 0'
    call termination
    stop
  end if
  write (*,*) ''

  write (*,'(A)') 'ENTER the PARAMETERS for GRID SYSTEM:'
  write (*,'(A)') 'irs (m); Horizontal resolution of a cell on the grid [integer]...'
  read  (*,*) irs
  if( irs < 1 ) then
    write(*,'(A)') 'ERROR: irs < 1'
    call termination
    stop
  end if

  write (*,'(A)') 'ns_x_sta; Start-cell number of a source region on x axis [integer]...'
  read  (*,*) ns_x_sta
  write (*,'(A)') 'ns_x_end; End-cell number of a source region on x axis [integer]...'
  read  (*,*) ns_x_end
  if( ns_x_sta >= ns_x_end ) then
    write(*,'(A)') 'ERROR: ns_x_sta >= ns_x_end'
    call termination
    stop
  end if

  write (*,'(A)') 'ns_y_sta; Start-cell number of a source region on y axis [integer]...'
  read  (*,*) ns_y_sta
  write (*,'(A)') 'ns_y_end; End-cell number of a source region on y axis [integer]...'
  read  (*,*) ns_y_end
  if( ns_y_sta >= ns_y_end ) then
    write(*,'(A)') 'ERROR: ns_y_sta >= ns_y_end'
    call termination
    stop
  end if

  ns_z_sta = 1
  write (*,'(A)') 'ns_z_sta; Start-cell number of a source region on z axis [integer]...'
  write (*,'(A)') '1 (Fixed value)'
  write (*,'(A)') 'ns_z_end; End-cell number of a source region on z axis [integer]...'
  read  (*,*) ns_z_end
  if( ns_z_end < 1 ) then
    write(*,'(A)') 'ERROR: ns_z_end < 1'
    call termination
    stop
  end if
! ---------------------------------------------------------------------
  write (*,*) ''
! ---------------------------------------------------------------------
end subroutine read