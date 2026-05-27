! =====================================================================
subroutine merging
! ---------------------------------------------------------------------
  use commondata
  implicit none
! ---------------------------------------------------------------------
  real(8) :: val, sqirs
  integer :: id
  integer :: io
  integer :: ix, iy, iyflag
  integer :: ixout, iyout
! ---------------------------------------------------------------------
  sqirs = dble(irs*irs)
! ---------------------------------------------------------------------
! Result for GUI
  open(unit=123,file="GUI_DOSE.data",status='unknown')
! ---------------------------------------------------------------------
! SHIELD.data
  open(unit=111,file=cfname_sld,iostat=io,status='old')
! ---------------------------------------------------------------------
! There is no SHIELD.data
! ---------------------------------------------------------------------
  if( io /= 0 ) then

    do iy = ns_y_sta_all, ns_y_end_all
      do ix = ns_x_sta_all, ns_x_end_all
        val = dout(ix,iy) /sqirs
        if( val < 1.0d-99 ) val = 0.0d+0
        write(123,'(es15.7)') val
      end do
    end do
!    ixout = 0; iyout = 0
!    do iy = ns_y_sta_all, ns_y_end_all
!      if( iy >= nc_y_sta .and. iy <= nc_y_end ) then
!        iyflag = 1
!        iyout  = iyout + 1
!      else
!        iyflag = 0
!      end if
!
!      do ix = ns_x_sta_all, ns_x_end_all
!        if( iyflag == 1 .and. ix >= nc_x_sta .and. ix <= nc_x_end ) then
!          ixout = ixout + 1
!          val = dout(ixout,iyout) / sqirs
!        else
!          val = 0.0d0
!        end if
!
!        write(123,'(es15.7)') val
!      end do
!
!    end do
!
    return
  end if
! ---------------------------------------------------------------------
! There is SHIELD.data
! ---------------------------------------------------------------------
  do iy = ns_y_sta_all, ns_y_end_all
    do ix = ns_x_sta_all, ns_x_end_all
      val = dout(ix,iy) / sqirs
      if( val < 1.0d-99 ) val = 0.0d+0
      read(111,*,iostat=io) id
      if( id == 9999 ) val = -1.0d0
      write(123,'(es15.7)') val
    end do
  end do
!  ixout = 0; iyout = 0
!  do iy = ns_y_sta_all, ns_y_end_all
!    if( iy >= nc_y_sta .and. iy <= nc_y_end ) then
!      iyflag = 1
!      iyout  = iyout + 1
!    else
!      iyflag = 0
!    end if
!
!    do ix = ns_x_sta_all, ns_x_end_all
!      read(111,*,iostat=io) id
!      if( iyflag == 1 .and. ix >= nc_x_sta .and. ix <= nc_x_end ) then
!        ixout = ixout + 1
!        val = dout(ixout,iyout) / sqirs
!      else
!        val = 0.0d0
!      end if
!
!      if( id == 9999 ) val = -1.0d0
!
!      write(123,'(es15.7)') val
!    end do
!
!  end do
! ---------------------------------------------------------------------
  close(111)
  close(113)
  close(123)
! ---------------------------------------------------------------------
  return
end subroutine merging
! =====================================================================
