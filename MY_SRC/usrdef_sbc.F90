MODULE usrdef_sbc
   !!======================================================================
   !!                     ***  MODULE  usrdef_sbc  ***
   !!
   !!                     ===  WEDDELL configuration  ===
   !!
   !! User defined :   surface forcing of a user configuration
   !!======================================================================
   !! History :  4.0   ! 2016-03  (S. Flavoni, G. Madec)  user defined interface
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   usrdef_sbc    : user defined surface bounday conditions in GYRE case
   !!----------------------------------------------------------------------
   USE oce            ! ocean dynamics and tracers
   USE dom_oce        ! ocean space and time domain
   USE sbc_oce        ! Surface boundary condition: ocean fields
   USE phycst         ! physical constants
   !
   USE in_out_manager ! I/O manager
   USE lib_mpp        ! distribued memory computing library
   USE lbclnk         ! ocean lateral boundary conditions (or mpp link)
   USE lib_fortran    !

   IMPLICIT NONE
   PRIVATE

   PUBLIC   usrdef_sbc_oce       ! routine called in sbcmod module
   PUBLIC   usrdef_sbc_ice_tau   ! routine called by icestp.F90 for ice dynamics
   PUBLIC   usrdef_sbc_ice_flx   ! routine called by icestp.F90 for ice thermo

   !! * Substitutions
#  include "vectopt_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: usrdef_sbc.F90 10425 2018-12-19 21:54:16Z smasson $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE usrdef_sbc_oce( kt )
      !!---------------------------------------------------------------------
      !!                    ***  ROUTINE usrdef_sbc  ***
      !!              
      !! ** Purpose :   provide at each time-step the surface boundary
      !!              condition, i.e. the momentum, heat and freshwater fluxes.
      !!
      !! ** Method  :   all 0 fields, for AMM7 case
      !!                CAUTION : never mask the surface stress field !
      !!
      !! ** Action  : - set to ZERO all the ocean surface boundary condition, i.e.   
      !!                   utau, vtau, taum, wndm, qns, qsr, emp, sfx
      !!
      !!----------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt   ! ocean time step
      INTEGER  ::   ji, jj               ! dummy loop indices
      !!---------------------------------------------------------------------
      !
      IF( kt == nit000 ) THEN
         !
         IF(lwp) WRITE(numout,*)' usr_sbc : AMM7 case: NO surface forcing'
         IF(lwp) WRITE(numout,*)' ~~~~~~~~~~~   utau = vtau = taum = wndm = qns = qsr = emp = sfx = 0'
         !
         !utau(:,:) = 0._wp
         !vtau(:,:) = 0._wp
         ! ... utau, vtau at U- and V_points, resp.
         !     Note the use of 0.5*(2-umask) in order to unmask the stress along coastlines
         !     Note the use of MAX(tmask(i,j),tmask(i+1,j) is to mask tau over ice shelves
         DO jj = 1, jpjm1
            DO ji = 1, fs_jpim1
               utau(ji,jj) = 0.5 * ( 2. - umask(ji,jj,1) ) * 0._wp &
                  &          * MAX(tmask(ji,jj,1),tmask(ji+1,jj,1))
               vtau(ji,jj) = 0.5 * ( 2. - vmask(ji,jj,1) ) * 0._wp &
                  &          * MAX(tmask(ji,jj,1),tmask(ji,jj+1,1))
            END DO
         END DO
         CALL lbc_lnk_multi( 'usrsbc', utau, 'U', -1., vtau, 'V', -1. )
         !taum(:,:) = 0._wp 
         wndm(:,:) = 0._wp * tmask(:,:,1)
         taum(:,:) = wndm(:,:)

         !
         emp (:,:) = 0._wp * tmask(:,:,1)
         sfx (:,:) = 0._wp * tmask(:,:,1)
         qns (:,:) = 0._wp * tmask(:,:,1)
         qsr (:,:) = 0._wp * tmask(:,:,1)
         !
      ENDIF
      !
   END SUBROUTINE usrdef_sbc_oce

   SUBROUTINE usrdef_sbc_ice_tau( kt )
      INTEGER, INTENT(in) ::   kt   ! ocean time step
   END SUBROUTINE usrdef_sbc_ice_tau

   SUBROUTINE usrdef_sbc_ice_flx( kt, phs, phi )
      INTEGER, INTENT(in) ::   kt   ! ocean time step
      REAL(wp), DIMENSION(:,:,:), INTENT(in)  ::   phs    ! snow thickness
      REAL(wp), DIMENSION(:,:,:), INTENT(in)  ::   phi    ! ice thickness
   END SUBROUTINE usrdef_sbc_ice_flx

   !!======================================================================
END MODULE usrdef_sbc
