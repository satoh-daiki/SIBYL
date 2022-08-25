! #####################################################################
! This file contains the following subroutines:
!   - allocate_arrays_1st()
!   - allocate_arrays()
!   - deallocate_arrays()
! #####################################################################

! =====================================================================
subroutine allocate_arrays_1st()
! ---------------------------------------------------------------------
! version history, comments, ...
! =====================================================================
  use commondata
! ---------------------------------------------------------------------
  implicit none
! ---------------------------------------------------------------------
! ---------------------------------------------------------------------
  allocate(idivx(0:ndivx))
  idivx = 0
  allocate(imapxy(1:ndivx,0:ndivy))
  imapxy = 0

! ---------------------------------------------------------------------
#ifndef USE_MPI_SPMD
! SOURCE
  allocate( din(ns_x_sta_all:ns_x_end_all,ns_y_sta_all:ns_y_end_all,ns_z_sta:ns_z_end) )
  din(:,:,:) = 0.d0
#endif
! ---------------------------------------------------------------------

  return
end subroutine allocate_arrays_1st


! =====================================================================
subroutine allocate_arrays()
! ---------------------------------------------------------------------
! =====================================================================
  use commondata
! ---------------------------------------------------------------------
  implicit none
! ---------------------------------------------------------------------
! version history, comments, ...
! ---------------------------------------------------------------------
! === move from subroutine "input" ===
! Z DATA
  allocate( zlow(0:ns_z_end) )
  allocate( zupp(0:ns_z_end) )
  zlow(:) = 0.d0
  zupp(:) = 0.d0
! SHIELD
  allocate( ish(ns_x_sta_all:ns_x_end_all,ns_y_sta_all:ns_y_end_all,nsh_min:nsh_max+1) )
  ish(:,:,:) = 0
! RESIZED RESPONSE
  allocate( resp(nx_sta:nx_end,ny_sta:ny_end,ns_z_sta:ns_z_end) )
  resp(:,:,:) = 0.d0
! ---------------------------------------------------------------------
#ifdef USE_MPI_SPMD
! SOURCE
  allocate( din(ns_x_sta:ns_x_end,ns_y_sta:ns_y_end,ns_z_sta:ns_z_end) )
  din(:,:,:) = 0.d0
#endif
! ---------------------------------------------------------------------
! TARGET
  allocate( dout(ns_x_sta_all:ns_x_end_all,ns_y_sta_all:ns_y_end_all) )
  dout(:,:) = 0.d0   ! Unit: micro-Sv/h at the irs x irs mesh
! ---------------------------------------------------------------------
! ELEVATION AT THE TARGET MESH
  allocate( ielev(nt_x_sta:nt_x_end,nt_y_sta:nt_y_end) )
  ielev(:,:) = 0
! ---------------------------------------------------------------------

  return
end subroutine allocate_arrays


! =====================================================================
subroutine deallocate_arrays()
! ---------------------------------------------------------------------
! version history, comments, ...
! =====================================================================
  use commondata
! ---------------------------------------------------------------------
  implicit none
! ---------------------------------------------------------------------
! ---------------------------------------------------------------------

#ifdef DEBUG
  write(*,*)'DEBUG: into deallocate_arrays @',myrank
  if (allocated( ielev )) then
    deallocate( ielev  )
  else
    write(*,*) 'DEBUG: already deallocated: ielev'
  endif
  if (allocated( dout )) then
    deallocate( dout   )
  else
    write(*,*) 'DEBUG: already deallocated: dout'
  endif
  if (allocated( din )) then
    deallocate( din    )
  else
    write(*,*) 'DEBUG: already deallocated: din'
  endif
  if (allocated( resp )) then
    deallocate( resp   )
  else
    write(*,*) 'DEBUG: already deallocated: resp'
  endif
  if (allocated( ish )) then
    deallocate( ish    )
  else
    write(*,*) 'DEBUG: already deallocated: ish'
  endif
  if (allocated( zupp )) then
    deallocate( zupp   )
  else
    write(*,*) 'DEBUG: already deallocated: zupp'
  endif
  if (allocated( zlow )) then
    deallocate( zlow   )
  else
    write(*,*) 'DEBUG: already deallocated: zlow'
  endif
  if (allocated( imapxy )) then
    deallocate( imapxy )
  else
    write(*,*) 'DEBUG: already deallocated: imapxy'
  endif
  if (allocated( idivx )) then
    deallocate( idivx  )
  else
    write(*,*) 'DEBUG: already deallocated: idivx'
  endif
#else
  deallocate( ielev  )
  deallocate( dout   )
  deallocate( din    )
  deallocate( resp   )
  deallocate( ish    )
  deallocate( zupp   )
  deallocate( zlow   )
  deallocate( imapxy )
  deallocate( idivx  )
#endif

  return
end subroutine deallocate_arrays

