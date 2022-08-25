! =====================================================================
subroutine pasquill(xx,sigy,sigz)
! ---------------------------------------------------------------------
! Purpose is to return the standard deviations of the centerline of 
! Gaussian distributions in the y and z directions based on the 
! Pasquill-Giford curves.
! ---------------------------------------------------------------------
! Programed by d.satoh (2022.07.26)
! =====================================================================
  use commondata
! ---------------------------------------------------------------------
  implicit none
! ---------------------------------------------------------------------
  real(8):: xx
  real(8):: sigy, sigz
  real(8):: gy, ay, gz, az
! ---------------------------------------------------------------------
  select case(PG)
    case('A','a')
      if( xx >= 0.0 .and. xx < 300.0 ) then
        gy = 0.426
        ay = 0.901
        gz = 0.0800
        az = 1.122
      else if( xx >= 300.0 .and. xx < 500.0 ) then
        gy = 0.426
        ay = 0.901
        gz = 0.00855
        az = 1.514
      else if( xx >= 500.0 .and. xx < 1000.0 ) then
        gy = 0.426
        ay = 0.901
        gz = 0.000212
        az = 2.109
      else if( xx >= 1000.0) then
        gy = 0.602
        ay = 0.851
        gz = 0.000212
        az = 2.109
      end if
    case('B','b')
      if( xx >= 0.0 .and. xx < 500.0 ) then
        gy = 0.282
        ay = 0.914
        gz = 0.1272
        az = 0.964
      else if( xx >= 500.0 .and. xx < 1000.0 ) then
        gy = 0.282
        ay = 0.914
        gz = 0.0570
        az = 1.094
      else if( xx >= 1000.0) then
        gy = 0.396
        ay = 0.865
        gz = 0.0570
        az = 1.094
      end if
    case('C','c')
      if( xx >= 0.0 .and. xx < 1000.0 ) then
        gy = 0.1772
        ay = 0.924
        gz = 0.1068
        az = 0.918
      else if( xx >= 1000.0) then
        gy = 0.232
        ay = 0.885
        gz = 0.1068
        az = 0.918
      end if
    case('D','d')
      if( xx >= 0.0 .and. xx < 1000.0 ) then
        gy = 0.1107
        ay = 0.929
        gz = 0.1046
        az = 0.826
      else if( xx >= 1000.0 .and. xx < 10000.0 ) then
        gy = 0.1467
        ay = 0.889
        gz = 0.400
        az = 0.632
      else if( xx >= 10000.0) then
        gy = 0.1467
        ay = 0.889
        gz = 0.811
        az = 0.555
      end if
    case('E','e')
      if( xx >= 0.0 .and. xx < 1000.0 ) then
        gy = 0.0864
        ay = 0.921
        gz = 0.0928
        az = 0.788
      else if( xx >= 1000.0 .and. xx < 10000.0 ) then
        gy = 0.1019
        ay = 0.897
        gz = 0.433
        az = 0.565
      else if( xx >= 10000.0) then
        gy = 0.1019
        ay = 0.897
        gz = 1.732
        az = 0.415
      end if
    case('F','f')
      if( xx >= 0.0 .and. xx < 1000.0 ) then
        gy = 0.0554
        ay = 0.929
        gz = 0.0621
        az = 0.784
      else if( xx >= 1000.0 .and. xx < 10000.0 ) then
        gy = 0.0733
        ay = 0.889
        gz = 0.370
        az = 0.526
      else if( xx >= 10000.0) then
        gy = 0.0733
        ay = 0.889
        gz = 2.41
        az = 0.323
      end if
    case('G','g')
      if( xx >= 0.0 .and. xx < 1000.0 ) then
        gy = 0.0380
        ay = 0.921
        gz = 0.0373
        az = 0.794
      else if( xx >= 1000.0 .and. xx < 2000.0 ) then
        gy = 0.0452
        ay = 0.896
        gz = 0.1105
        az = 0.637
      else if( xx >= 2000.0 .and. xx < 10000.0 ) then
        gy = 0.0452
        ay = 0.896
        gz = 0.529
        az = 0.431
      else if( xx >= 10000.0) then
        gy = 0.0452
        ay = 0.896
        gz = 3.62
        az = 0.222
      end if
  end select
! ---------------------------------------------------------------------
  sigy = gy*xx**ay
  sigz = gz*xx**az
! ---------------------------------------------------------------------
  return
end subroutine pasquill