MODULE usrdef_istate
   !!======================================================================
   !!                   ***  MODULE  usrdef_istate   ***
   !!
   !!                     ===  GYRE configuration  ===
   !!
   !! User defined : set the initial state of a user configuration
   !!======================================================================
   !! History :  4.0  ! 2016-03  (S. Flavoni) Original code
   !!                 ! 2020-11  (S. Techene, G. Madec) separate tsuv from ssh
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!  usr_def_istate : initial state in Temperature and salinity
   !!----------------------------------------------------------------------
   USE par_oce        ! ocean space and time domain
   USE phycst         ! physical constants

  ! USE splines
   !
   USE in_out_manager ! I/O manager
   USE lib_mpp        ! MPP library
   
   IMPLICIT NONE
   PRIVATE

   PUBLIC   usr_def_istate       ! called in istate.F90
   PUBLIC   usr_def_istate_ssh   ! called by domqco.F90

   !! * Substitutions
#  include "do_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: usrdef_istate.F90 14834 2021-05-11 09:24:44Z hadcv $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS
  
   SUBROUTINE usr_def_istate( pdept, ptmask, pts, pu, pv )
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE usr_def_istate  ***
      !! 
      !! ** Purpose :   Initialization of the dynamics and tracers
      !!                Here GYRE configuration example : (double gyre with rotated domain)
      !!
      !! ** Method  : - set temprature field
      !!              - set salinity   field
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(jpi,jpj,jpk)     , INTENT(in   ) ::   pdept   ! depth of t-point               [m]
      REAL(wp), DIMENSION(jpi,jpj,jpk)     , INTENT(in   ) ::   ptmask  ! t-point ocean mask             [m]
      REAL(wp), DIMENSION(jpi,jpj,jpk,jpts), INTENT(  out) ::   pts     ! T & S fields      [Celsius ; g/kg]
      REAL(wp), DIMENSION(jpi,jpj,jpk)     , INTENT(  out) ::   pu      ! i-component of the velocity  [m/s] 
      REAL(wp), DIMENSION(jpi,jpj,jpk)     , INTENT(  out) ::   pv      ! j-component of the velocity  [m/s] 
      !
      INTEGER :: ji, jj, jk  ! dummy loop indices
!AW
      REAL(wp) :: z_a0, z_a1, z_a2, z_b0, z_b1, z_b2
      REAL(wp) :: z_sal_sf, z_mld_sf, z_c0_sf, z_c1_sf, z_maxdep_sf
!AW end
      !!----------------------------------------------------------------------
      !
      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) 'usr_def_istate : analytical definition of initial state '
      IF(lwp) WRITE(numout,*) '~~~~~~~~~~~~~~   Ocean at rest, with an horizontally uniform T and S profiles'
      !
      pu  (:,:,:) = 0._wp           ! ocean at rest
      pv  (:,:,:) = 0._wp
      !
!AW 
      z_a0 = 13.0
      z_a1 = 0.00507078656
      z_a2 = 2.37539619
      z_b0 = -1.1 
      z_b1 = 0.01 
      z_b2 = 34.85
      z_sal_sf = 35.5 
      z_mld_sf = 120
      z_maxdep_sf = 5500
      z_c0_sf = 3
      z_c1_sf = 6 

      DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 1, jpk )
         pts(ji,jj,jk,jp_sal) = z_sal_sf * ptmask(ji,jj,jk)
         pts(ji,jj,jk,jp_tem) = ( z_c0_sf * ( 1-tanh( ( (pdept(ji,jj,jk)-z_mld_sf)/20 )*3.1415927/180 ) ) &
              &              + z_c1_sf * ( ( z_maxdep_sf - pdept(ji,jj,jk) ) / z_maxdep_sf ) ) * ptmask(ji,jj,jk) 
               !pts(ji,jj,:,jp_tem) = ( a0 * exp( -a1 * pdept(ji,jj,:) ) + a2 ) * ptmask(ji,jj,:)
               !pts(ji,jj,:,jp_sal) = ( b0 * exp( -b1 * pdept(ji,jj,:) ) + b2 ) * ptmask(ji,jj,:) 
      END_3D

!      DO_3D( nn_hls, nn_hls, nn_hls, nn_hls, 1, jpk )   ! horizontally uniform T & S profiles
!         pts(ji,jj,jk,jp_tem) =  (  (  16. - 12. * TANH( (pdept(ji,jj,jk) - 400) / 700 ) )   &
!              &           * (-TANH( (500. - pdept(ji,jj,jk)) / 150. ) + 1.) / 2.             &
!              &           + ( 15. * ( 1. - TANH( (pdept(ji,jj,jk)-50.) / 1500.) )            &
!              &           - 1.4 * TANH((pdept(ji,jj,jk)-100.) / 100.)                        &
!              &           + 7.  * (1500. - pdept(ji,jj,jk) ) / 1500.)                        &
!              &           * (-TANH( (pdept(ji,jj,jk) - 500.) / 150.) + 1.) / 2.  ) * ptmask(ji,jj,jk)
!
!         pts(ji,jj,jk,jp_sal) =  (  (  36.25 - 1.13 * TANH( (pdept(ji,jj,jk) - 305) / 460 ) )  &
!              &         * (-TANH((500. - pdept(ji,jj,jk)) / 150.) + 1.) / 2                  &
!              &         + ( 35.55 + 1.25 * (5000. - pdept(ji,jj,jk)) / 5000.                 &
!              &         - 1.62 * TANH( (pdept(ji,jj,jk) - 60.  ) / 650. )                    &
!              &         + 0.2  * TANH( (pdept(ji,jj,jk) - 35.  ) / 100. )                    &
!              &         + 0.2  * TANH( (pdept(ji,jj,jk) - 1000.) / 5000.) )                  &
!              &         * (-TANH( (pdept(ji,jj,jk) - 500.) / 150.) + 1.) / 2  ) * ptmask(ji,jj,jk)
!      END_3D
!AW end     
 !   
   END SUBROUTINE usr_def_istate

   
   SUBROUTINE usr_def_istate_ssh( ptmask, pssh )
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE usr_def_istate_ssh  ***
      !! 
      !! ** Purpose :   Initialization of ssh
      !!
      !! ** Method  :   Set ssh as null, ptmask is required for test cases
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(jpi,jpj,jpk)     , INTENT(in   ) ::   ptmask  ! t-point ocean mask   [m]
      REAL(wp), DIMENSION(jpi,jpj)         , INTENT(  out) ::   pssh    ! sea-surface height   [m]
      !!----------------------------------------------------------------------
      !
      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) 'usr_def_istate_ssh : GYRE configuration, analytical definition of initial state'
      IF(lwp) WRITE(numout,*) '~~~~~~~~~~~~~~~~~~   Ocean at rest, ssh is zero'
      !
      ! Sea level:
      pssh(:,:) = 0._wp
      !
   END SUBROUTINE usr_def_istate_ssh

   !!======================================================================
END MODULE usrdef_istate
