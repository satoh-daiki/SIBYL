! =====================================================================
subroutine attenuation(ix,iy,iv,ipos_x,ipos_y,ipos_z,icnt)
! ---------------------------------------------------------------------
! Purpose is to retrun the attenuation factor.
! ---------------------------------------------------------------------
! Programed by d.satoh (2019.08.23)
! Modified  by d.satoh (2019.10.25)
! Modified  by d.satoh (2019.12.03)
! =====================================================================
  use commondata
! ---------------------------------------------------------------------
  implicit none
! ---------------------------------------------------------------------
  integer :: ix, iy, iv, ipos_x, ipos_y, ipos_z, icnt
! ---------------------------------------------------------------------
  real(8) :: x_tgt, y_tgt, z_tgt
  real(8) :: x_src, y_src, z_src
  real(8) :: dx, dy, dz, P
  real(8) :: u, v, w
! ---------------------------------------------------------------------
  real(8) :: x, y, z, d, TD
! ---------------------------------------------------------------------
  integer :: iz
  integer :: isx, isy, isz
  integer :: nz_max
! ---------------------------------------------------------------------
  integer :: i, idivP, ii, istart, iend
  integer :: isz_pre
  real(8) :: dinvirs
  dinvirs = 1.0d0 / dble(irs)
! ---------------------------------------------------------------------
  iz = ipos_z
! ---------------------------------------------------------------------
  if( iz == 0 ) then
    nz_max = 1
  else if( iz <= nsh_max .and. iz /= 0 ) then
    nz_max = iz
  else
    nz_max = nsh_max
  end if
! ---------------------------------------------------------------------
  icnt = 0

  x_tgt = dble(ix*irs) + dble(irs)/2.d0
  y_tgt = dble(iy*irs) + dble(irs)/2.d0
  z_tgt = zupp(iv) + 1.0d-0

  x_src = dble(ipos_x*irs) + dble(irs)/2.d0
  y_src = dble(ipos_y*irs) + dble(irs)/2.d0
  z_src = ( zlow(iz) + zupp(iz) ) /2.d0
! ---------------------------------------------------------------------
  dx = x_tgt - x_src
  dy = y_tgt - y_src
  dz = z_tgt - z_src

  P = dsqrt(dx**2 + dy**2 + dz**2)
! ---------------------------------------------------------------------
! direction vector
  u = dx / P
  v = dy / P
  w = dz / P
! ---------------------------------------------------------------------
#ifndef SKIP_RANGECHECK
  if (u .ge. 0.0d0) then
    if ((ns_x_sta*irs) .gt. x_src) then
      return
    endif
    if (x_tgt .gt. ((ns_x_end_all+1)*irs)) then
      return
    endif
  else
    if ((ns_x_sta_all*irs) .gt. x_tgt) then
      return
    endif
    if (x_src .gt. ((ns_x_end+1)*irs)) then
      return
    endif
  endif
  if (v .ge. 0.0d0) then
    if ((ns_y_sta*irs) .gt. y_src) then
      return
    endif
    if (y_tgt .gt. ((ns_y_end_all+1)*irs)) then
      return
    endif
  else
    if ((ns_y_sta_all*irs) .gt. y_tgt) then
      return
    endif
    if (y_src .gt. ((ns_y_end+1)*irs)) then
      return
    endif
  endif
#endif
! ---------------------------------------------------------------------
! minimum step
  d = dble(irs)
! total step
  TD = d
! ---------------------------------------------------------------------
  idivP = int(P / d)
  if (mod(P,d) .eq. 0) idivP = idivP - 1

  isz_pre  = 1
  istart = 1
  iend   = idivP

  if (w .ge. 0) then
    do i=idivP, 1, -1
      if ((z_src + w * d * dble(i)) .lt. zupp(nz_max)) then
        iend = i
        exit
      endif
    enddo
  else
    do i=1, idivP
      if ((z_src + w * d * dble(i)) .lt. zupp(nz_max)) then
        istart = i
        exit
      endif
    enddo
  endif

  do i=istart, iend
    x = x_src + u * d * dble(i)
    y = y_src + v * d * dble(i)
    z = z_src + w * d * dble(i)

      z_loop : do isz = isz_pre, nz_max
        if( z >= zlow(isz) .and. z < zupp(isz) ) exit z_loop
      end do z_loop
      isz_pre = max(isz-1,1)

      isx = int(x * dinvirs) + merge(-1,0,(x .lt. 0.0d0))

      isy = int(y * dinvirs) + merge(-1,0,(y .lt. 0.0d0))

      icnt = icnt + ish(isx,isy,isz)

  enddo
! ---------------------------------------------------------------------
  return
end subroutine attenuation
