MODULE trddyn
   !!======================================================================
   !!                       ***  MODULE  trddyn  ***
   !! Ocean diagnostics:  ocean dynamic trends
   !!=====================================================================
   !! History :  3.5  !  2012-02  (G. Madec) creation from trdmod: split DYN and TRA trends
   !!                                        and manage  3D trends output for U, V, and KE
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   trd_dyn       : manage the type of momentum trend diagnostics (3D I/O, domain averaged, KE)
   !!   trd_dyn_iom   : output 3D momentum and/or tracer trends using IOM
   !!   trd_dyn_init  : initialization step
   !!----------------------------------------------------------------------
   USE oce            ! ocean dynamics and tracers variables
   USE dom_oce        ! ocean space and time domain variables
   USE phycst         ! physical constants
   USE sbc_oce        ! surface boundary condition: ocean
   USE zdf_oce        ! ocean vertical physics: variables
   USE trd_oce        ! trends: ocean variables
   USE trdken         ! trends: Kinetic ENergy 
   USE trdglo         ! trends: global domain averaged
   USE trdvor         ! trends: vertical averaged vorticity 
   USE trdmxl         ! trends: mixed layer averaged 
   !
   USE in_out_manager ! I/O manager
   USE lbclnk         ! lateral boundary condition 
   USE iom            ! I/O manager library
   USE lib_mpp        ! MPP library

   IMPLICIT NONE
   PRIVATE

   PUBLIC trd_dyn        ! called by all dynXXX modules

   INTERFACE trd_dyn
      module procedure trd_dyn_3d, trd_dyn_2d
   END INTERFACE

   REAL(wp), ALLOCATABLE, DIMENSION(:,:,:), SAVE :: zutrd_hpg, zvtrd_hpg
   REAL(wp), ALLOCATABLE, DIMENSION(:,:,:), SAVE :: zutrd_pvo, zvtrd_pvo
   REAL(wp), ALLOCATABLE, DIMENSION(:,:,:), SAVE :: zutrd_tfre, zvtrd_tfre
   REAL(wp), ALLOCATABLE, DIMENSION(:,:,:), SAVE :: zutrd_bfre, zvtrd_bfre
   REAL(wp), ALLOCATABLE, DIMENSION(:,:,:), SAVE :: zutrd_tfr, zvtrd_tfr
   REAL(wp), ALLOCATABLE, DIMENSION(:,:,:), SAVE :: zutrd_bfr, zvtrd_bfr
   REAL(wp), ALLOCATABLE, DIMENSION(:,:)  , SAVE :: zutrd_iceoc, zvtrd_iceoc
   REAL(wp), ALLOCATABLE, DIMENSION(:,:)  , SAVE :: zutrd_tau2d, zvtrd_tau2d
   REAL(wp), ALLOCATABLE, DIMENSION(:,:)  , SAVE :: zutrd_iceoc2d, zvtrd_iceoc2d
   REAL(wp), ALLOCATABLE, DIMENSION(:,:)  , SAVE :: zutrd_tfr2d, zvtrd_tfr2d
   REAL(wp), ALLOCATABLE, DIMENSION(:,:)  , SAVE :: zutrd_bfr2d, zvtrd_bfr2d

   !! * Substitutions
#  include "vectopt_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id$
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE trd_dyn_3d( putrd, pvtrd, ktrd, kt )
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE trd_dyn_3d  ***
      !! 
      !! ** Purpose :   Dispatch momentum trend computation, e.g. 3D output, 
      !!              integral constraints, barotropic vorticity, kinetic enrgy, 
      !!              and/or mixed layer budget.
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(:,:,:), INTENT(inout) ::   putrd, pvtrd   ! U and V trends 
      INTEGER                   , INTENT(in   ) ::   ktrd           ! trend index
      INTEGER                   , INTENT(in   ) ::   kt             ! time step
      REAL(wp), ALLOCATABLE, DIMENSION(:,:)     ::   zue, zve       ! temporary 2D arrays
      INTEGER                                   ::   jk
      !!----------------------------------------------------------------------
      !
      putrd(:,:,:) = putrd(:,:,:) * umask(:,:,:)                       ! mask the trends
      pvtrd(:,:,:) = pvtrd(:,:,:) * vmask(:,:,:)
      !

!!gm NB : here a lbc_lnk should probably be added

      SELECT CASE( ktrd )
      CASE( jpdyn_hpg_save ) 
         !
         ! save 3D HPG trends to possibly have barotropic part corrected later before writing out
         ALLOCATE( zutrd_hpg(jpi,jpj,jpk), zvtrd_hpg(jpi,jpj,jpk) )
         zutrd_hpg(:,:,:) = putrd(:,:,:)
         zvtrd_hpg(:,:,:) = pvtrd(:,:,:)

      CASE( jpdyn_pvo_save ) 
         !
         ! save 3D coriolis trends to possibly have barotropic part corrected later before writing out
         ALLOCATE( zutrd_pvo(jpi,jpj,jpk), zvtrd_pvo(jpi,jpj,jpk) )
         zutrd_pvo(:,:,:) = putrd(:,:,:)
         zvtrd_pvo(:,:,:) = pvtrd(:,:,:)

      CASE( jpdyn_spg ) 
         ! For explicit scheme SPG trends come here as 3D fields
         ! Add SPG trend to 3D HPG trend and also output as 2D diagnostic in own right.
         CALL trd_dyn_iom_2d( putrd(:,:,1), pvtrd(:,:,1), jpdyn_spg, kt ) 
         zutrd_hpg(:,:,:) = zutrd_hpg(:,:,:) + putrd(:,:,:)  
         zvtrd_hpg(:,:,:) = zvtrd_hpg(:,:,:) + pvtrd(:,:,:)  
         CALL trd_dyn_iom_3d( zvtrd_hpg, zvtrd_hpg, jpdyn_hpg, kt ) 
         DEALLOCATE( zutrd_hpg, zvtrd_hpg )

      CASE( jpdyn_tfre )
         !
         ! Explicit top drag trend calculated in zdf_drg. Save to add to 
         ! ZDF trend later and add to 3D TFR trend. 
         IF( .NOT. ALLOCATED(zutrd_tfre) ) THEN 
            ALLOCATE( zutrd_tfre(jpi,jpj,jpk), zvtrd_tfre(jpi,jpj,jpk) )
            zutrd_tfre(:,:,:) = putrd(:,:,:)
            zvtrd_tfre(:,:,:) = pvtrd(:,:,:)
         ENDIF
         IF( .NOT. ALLOCATED(zutrd_tfr) ) THEN 
            ALLOCATE( zutrd_tfr(jpi,jpj,jpk), zvtrd_tfr(jpi,jpj,jpk) )
            zutrd_tfr(:,:,:) = 0.0
            zvtrd_tfr(:,:,:) = 0.0
         ENDIF
         zutrd_tfr(:,:,:) = zutrd_tfr(:,:,:) + putrd(:,:,:) 
         zvtrd_tfr(:,:,:) = zvtrd_tfr(:,:,:) + pvtrd(:,:,:)

      CASE( jpdyn_tfre_bt, jpdyn_tfri )
         !
         ! Add various top friction terms for baroclinic trend to saved quantity.
         ! Any depth-mean component removed later when TFR trend written out. 
         IF( .NOT. ALLOCATED(zutrd_tfr) ) THEN 
            ALLOCATE( zutrd_tfr(jpi,jpj,jpk), zvtrd_tfr(jpi,jpj,jpk) )
            zutrd_tfr(:,:,:) = 0.0
            zvtrd_tfr(:,:,:) = 0.0
         ENDIF
         zutrd_tfr(:,:,:) = zutrd_tfr(:,:,:) + putrd(:,:,:) 
         zvtrd_tfr(:,:,:) = zvtrd_tfr(:,:,:) + pvtrd(:,:,:)

      CASE( jpdyn_bfre )
         !
         ! Explicit bottom drag trend calculated in zdf_drg. Save to add to 
         ! ZDF trend later and add to 3D BFR trend. 
         IF( .NOT. ALLOCATED(zutrd_bfre) ) THEN 
            ALLOCATE( zutrd_bfre(jpi,jpj,jpk), zvtrd_bfre(jpi,jpj,jpk) )
            zutrd_bfre(:,:,:) = putrd(:,:,:)
            zvtrd_bfre(:,:,:) = pvtrd(:,:,:)
         ENDIF
         IF( .NOT. ALLOCATED(zutrd_bfr) ) THEN 
            ALLOCATE( zutrd_bfr(jpi,jpj,jpk), zvtrd_bfr(jpi,jpj,jpk) )
            zutrd_bfr(:,:,:) = 0.0
            zvtrd_bfr(:,:,:) = 0.0
         ENDIF
         zutrd_bfr(:,:,:) = zutrd_bfr(:,:,:) + putrd(:,:,:) 
         zvtrd_bfr(:,:,:) = zvtrd_bfr(:,:,:) + pvtrd(:,:,:)

      CASE( jpdyn_bfre_bt, jpdyn_bfri )
         !
         ! Add various bottom friction terms for baroclinic trend to saved quantity.
         ! Any depth-mean component removed later when BFR trend written out. 
         IF( .NOT. ALLOCATED(zutrd_bfr) ) THEN 
            ALLOCATE( zutrd_bfr(jpi,jpj,jpk), zvtrd_bfr(jpi,jpj,jpk) )
            zutrd_bfr(:,:,:) = 0.0
            zvtrd_bfr(:,:,:) = 0.0
         ENDIF
         zutrd_bfr(:,:,:) = zutrd_bfr(:,:,:) + putrd(:,:,:) 
         zvtrd_bfr(:,:,:) = zvtrd_bfr(:,:,:) + pvtrd(:,:,:)

      CASE( jpdyn_zdf ) 
         ! ZDF trend: Add explicit top/bottom friction if necessary. If ln_dynspg_ts, remove barotropic component 
         !            and add wind stress, and top and bottom friction trends from dynspg_ts.
         !
         ! If TFRE or BFRE arrays allocated at this stage then they will contain trends due
         ! to explicit top or bottom drag components which need to be added to the ZDF trend. 
         IF( ALLOCATED( zutrd_tfre ) ) THEN
            DO jk = 1, jpkm1
               putrd(:,:,jk) = ( putrd(:,:,jk) + zutrd_tfre(:,:,jk) ) * umask(:,:,jk)
               pvtrd(:,:,jk) = ( pvtrd(:,:,jk) + zvtrd_tfre(:,:,jk) ) * vmask(:,:,jk)
            END DO
            DEALLOCATE( zutrd_tfre, zvtrd_tfre )
         ENDIF
         IF( ALLOCATED( zutrd_bfre ) ) THEN
            DO jk = 1, jpkm1
               putrd(:,:,jk) = ( putrd(:,:,jk) + zutrd_bfre(:,:,jk) ) * umask(:,:,jk)
               pvtrd(:,:,jk) = ( pvtrd(:,:,jk) + zvtrd_bfre(:,:,jk) ) * vmask(:,:,jk)
            END DO
            DEALLOCATE( zutrd_bfre, zvtrd_bfre )
         ENDIF
         IF( ln_dynspg_ts ) THEN
            ALLOCATE( zue(jpi,jpj), zve(jpi,jpj) )
            zue(:,:) = e3u_a(:,:,1) * putrd(:,:,1) * umask(:,:,1)
            zve(:,:) = e3v_a(:,:,1) * pvtrd(:,:,1) * vmask(:,:,1)
            DO jk = 2, jpkm1
               zue(:,:) = zue(:,:) + e3u_a(:,:,jk) * putrd(:,:,jk) * umask(:,:,jk)
               zve(:,:) = zve(:,:) + e3v_a(:,:,jk) * pvtrd(:,:,jk) * vmask(:,:,jk)
            END DO
            DO jk = 1, jpkm1
               putrd(:,:,jk) = ( zutrd_tau2d(:,:) + zutrd_bfr2d(:,:) + putrd(:,:,jk) - zue(:,:) * r1_hu_a(:,:) ) * umask(:,:,jk)
               pvtrd(:,:,jk) = ( zvtrd_tau2d(:,:) + zvtrd_bfr2d(:,:) + pvtrd(:,:,jk) - zve(:,:) * r1_hv_a(:,:) ) * vmask(:,:,jk)
            END DO
            DEALLOCATE( zue, zve, zutrd_tau2d, zvtrd_tau2d, zutrd_bfr2d, zvtrd_bfr2d)
            IF( ALLOCATED( zutrd_tfr2d ) ) THEN
               DO jk = 1, jpkm1
                  putrd(:,:,jk) = ( putrd(:,:,jk) + zutrd_tfr2d(:,:) ) * umask(:,:,jk)
                  pvtrd(:,:,jk) = ( pvtrd(:,:,jk) + zvtrd_tfr2d(:,:) ) * vmask(:,:,jk)
               END DO
               DEALLOCATE( zutrd_tfr2d, zvtrd_tfr2d )
            ENDIF
            !
         ENDIF
         !
      CASE( jpdyn_tot ) 
         ! Don't need to do anything special for TOT trends, but we are at the end of the timestep, so
         ! write out total top and bottom friction "trends" for the surface / bottom layers after 
         ! removing any depth-mean component.
         IF( ALLOCATED( zutrd_tfr ) .OR. ALLOCATED( zutrd_iceoc ) ) THEN
            ! With explicit top and bottom friction, the top friction diagnostic
            ! is initialised here.
            IF( .NOT. ALLOCATED( zutrd_tfr ) ) THEN
               ALLOCATE( zutrd_tfr(jpi,jpj,jpk), zvtrd_tfr(jpi,jpj,jpk) )
               zutrd_tfr(:,:,:) = 0.0
               zvtrd_tfr(:,:,:) = 0.0
            ENDIF   
            IF( ALLOCATED( zutrd_iceoc ) ) THEN
               ! Add trend due to ice-ocean stress at the surface
               zutrd_tfr(:,:,1) = zutrd_tfr(:,:,1) + zutrd_iceoc(:,:)
               zvtrd_tfr(:,:,1) = zvtrd_tfr(:,:,1) + zvtrd_iceoc(:,:)
               DEALLOCATE( zutrd_iceoc, zvtrd_iceoc )
            ENDIF
            ALLOCATE( zue(jpi,jpj), zve(jpi,jpj) )
            zue(:,:) = e3u_a(:,:,1) * zutrd_tfr(:,:,1) * umask(:,:,1)
            zve(:,:) = e3v_a(:,:,1) * zvtrd_tfr(:,:,1) * vmask(:,:,1)
            DO jk = 2, jpkm1
               zue(:,:) = zue(:,:) + e3u_a(:,:,jk) * zutrd_tfr(:,:,jk) * umask(:,:,jk)
               zve(:,:) = zve(:,:) + e3v_a(:,:,jk) * zvtrd_tfr(:,:,jk) * vmask(:,:,jk)
            END DO
            DO jk = 1, jpkm1
               zutrd_tfr(:,:,jk) = ( zutrd_tfr(:,:,jk) - zue(:,:) * r1_hu_a(:,:) ) * umask(:,:,jk)
               zvtrd_tfr(:,:,jk) = ( zvtrd_tfr(:,:,jk) - zve(:,:) * r1_hv_a(:,:) ) * vmask(:,:,jk)
            END DO
            CALL trd_dyn_iom_3d( zutrd_tfr, zvtrd_tfr, jpdyn_tfr, kt )
            DEALLOCATE( zue, zve, zutrd_tfr, zvtrd_tfr )
         ENDIF
         IF( ALLOCATED( zutrd_bfr ) ) THEN
            ALLOCATE( zue(jpi,jpj), zve(jpi,jpj) )
            zue(:,:) = e3u_a(:,:,1) * zutrd_bfr(:,:,1) * umask(:,:,1)
            zve(:,:) = e3v_a(:,:,1) * zvtrd_bfr(:,:,1) * vmask(:,:,1)
            DO jk = 2, jpkm1
               zue(:,:) = zue(:,:) + e3u_a(:,:,jk) * zutrd_bfr(:,:,jk) * umask(:,:,jk)
               zve(:,:) = zve(:,:) + e3v_a(:,:,jk) * zvtrd_bfr(:,:,jk) * vmask(:,:,jk)
            END DO
            DO jk = 1, jpkm1
               zutrd_bfr(:,:,jk) = ( zutrd_bfr(:,:,jk) - zue(:,:) * r1_hu_a(:,:) ) * umask(:,:,jk)
               zvtrd_bfr(:,:,jk) = ( zvtrd_bfr(:,:,jk) - zve(:,:) * r1_hv_a(:,:) ) * vmask(:,:,jk)
            END DO
            CALL trd_dyn_iom_3d( zutrd_bfr, zvtrd_bfr, jpdyn_bfr, kt )
            DEALLOCATE( zue, zve, zutrd_bfr, zvtrd_bfr )
         ENDIF

      END SELECT

      IF ( ktrd <= jptot_dyn ) THEN  ! output of 3D trends and use for other diagnostics
         !
         !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
         !   3D output of momentum and/or tracers trends using IOM interface
         !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
         IF( ln_dyn_trd )   CALL trd_dyn_iom_3d( putrd, pvtrd, ktrd, kt )

         !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
         !  Integral Constraints Properties for momentum and/or tracers trends
         !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
         IF( ln_glo_trd )   CALL trd_glo( putrd, pvtrd, ktrd, 'DYN', kt )

         !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
         !  Kinetic Energy trends
         !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
         IF( ln_KE_trd  )   CALL trd_ken( putrd, pvtrd, ktrd, kt )

         !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
         !  Vorticity trends
         !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
         IF( ln_vor_trd )   CALL trd_vor( putrd, pvtrd, ktrd, kt )

         !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
         !  Mixed layer trends for active tracers
         !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
         !!gm      IF( ln_dyn_mxl )   CALL trd_mxl_dyn   
         !
      ENDIF
      !
   END SUBROUTINE trd_dyn_3d


   SUBROUTINE trd_dyn_2d( putrd, pvtrd, ktrd, kt )
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE trd_mod  ***
      !! 
      !! ** Purpose :   Dispatch momentum trend computation, e.g. 2D output, 
      !!              integral constraints, barotropic vorticity, kinetic enrgy, 
      !!              and/or mixed layer budget.
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(:,:), INTENT(inout) ::   putrd, pvtrd   ! U and V trends 
      INTEGER                 , INTENT(in   ) ::   ktrd           ! trend index
      INTEGER                 , INTENT(in   ) ::   kt             ! time step
      INTEGER                                 ::   jk
      !!----------------------------------------------------------------------
      !
      putrd(:,:) = putrd(:,:) * umask(:,:,1)                       ! mask the trends
      pvtrd(:,:) = pvtrd(:,:) * vmask(:,:,1)
      !

!!gm NB : here a lbc_lnk should probably be added

      SELECT CASE(ktrd)

      CASE ( jpdyn_hpg_corr )
         !
         ! Remove "first-guess" SPG trend from 3D HPG trend. 
         DO jk = 1, jpkm1
            zutrd_hpg(:,:,jk) = zutrd_hpg(:,:,jk) - putrd(:,:)
            zvtrd_hpg(:,:,jk) = zvtrd_hpg(:,:,jk) - pvtrd(:,:)
         ENDDO

      CASE( jpdyn_pvo_corr )
         !
         ! Remove "first-guess" barotropic coriolis trend from 3D PVO trend. 
         DO jk = 1, jpkm1
            zutrd_pvo(:,:,jk) = zutrd_pvo(:,:,jk) - putrd(:,:)
            zvtrd_pvo(:,:,jk) = zvtrd_pvo(:,:,jk) - pvtrd(:,:)
         ENDDO

      CASE( jpdyn_spg )
          !
          ! For split-explicit scheme SPG trends come here as 2D fields
          ! Add SPG trend to 3D HPG trend and also output as 2D diagnostic in own right.
          DO jk = 1, jpkm1
             zutrd_hpg(:,:,jk) = zutrd_hpg(:,:,jk) + putrd(:,:)
             zvtrd_hpg(:,:,jk) = zvtrd_hpg(:,:,jk) + pvtrd(:,:)
          ENDDO
          CALL trd_dyn_3d( zutrd_hpg, zvtrd_hpg, jpdyn_hpg, kt )
          DEALLOCATE( zutrd_hpg, zvtrd_hpg )

      CASE( jpdyn_pvo )
          !
          ! Add 2D PVO trend to 3D PVO trend and also output as diagnostic in own right.
          DO jk = 1, jpkm1
             zutrd_pvo(:,:,jk) = zutrd_pvo(:,:,jk) + putrd(:,:)
             zvtrd_pvo(:,:,jk) = zvtrd_pvo(:,:,jk) + pvtrd(:,:)
          ENDDO
          CALL trd_dyn_3d( zutrd_pvo, zvtrd_pvo, jpdyn_pvo, kt )
          DEALLOCATE( zutrd_pvo, zvtrd_pvo )

      CASE( jpdyn_iceoc )
          !
          ! Save surface ice-ocean stress trend locally to be subtracted from
          ! surface wind stress trend and added to 3D top friction trend. 
          IF( .NOT. ALLOCATED(zutrd_iceoc) ) ALLOCATE( zutrd_iceoc(jpi,jpj), zvtrd_iceoc(jpi,jpj) )
          zutrd_iceoc(:,:) = putrd(:,:)
          zvtrd_iceoc(:,:) = pvtrd(:,:)

      CASE( jpdyn_tau )
          !
          ! Subtract ice-ocean stress from surface wind forcing
          IF( ALLOCATED(zutrd_iceoc) ) THEN
             putrd(:,:) = putrd(:,:) - zutrd_iceoc(:,:) 
             pvtrd(:,:) = pvtrd(:,:) - zvtrd_iceoc(:,:) 
          ENDIF

      CASE( jpdyn_iceoc2d )
          !
          ! Save 2D ice-ocean stress trend locally as the first installment of top friction.
          ! Subtracted from 2D wind stress trend later. 
          IF( .NOT. ALLOCATED(zutrd_tfr2d) ) ALLOCATE( zutrd_tfr2d(jpi,jpj), zvtrd_tfr2d(jpi,jpj) )
          zutrd_tfr2d(:,:) = putrd(:,:)
          zvtrd_tfr2d(:,:) = pvtrd(:,:)

      CASE( jpdyn_tau2d )
          !
          ! Subtract ice-ocean stress from depth-mean trend due to wind forcing
          ! and save to be added to ZDF trend later. Output as a trend in its own right (below).
          ! Note at this stage, zutrd_tfr2d should only contain the contribution to top friction
          ! from (partial) ice-ocean stress.
          ALLOCATE( zutrd_tau2d(jpi,jpj), zvtrd_tau2d(jpi,jpj) )
          IF( ALLOCATED(zutrd_tfr2d) ) THEN
             putrd(:,:) = putrd(:,:) - zutrd_tfr2d(:,:) 
             pvtrd(:,:) = pvtrd(:,:) - zvtrd_tfr2d(:,:) 
          ENDIF
          zutrd_tau2d(:,:) = putrd(:,:)
          zvtrd_tau2d(:,:) = pvtrd(:,:)

      CASE( jpdyn_tfr )
          !
          ! Add ice-ocean stress from depth-mean trend due to top friction
          ! and save to be added to ZDF trend later. Output as a trend in its own right (below).
          IF( .NOT. ALLOCATED(zutrd_tfr2d) ) THEN
             ALLOCATE( zutrd_tfr2d(jpi,jpj), zvtrd_tfr2d(jpi,jpj) )
             zutrd_tfr2d(:,:) = 0._wp ; zvtrd_tfr2d(:,:) = 0._wp 
          ENDIF
          zutrd_tfr2d(:,:) = zutrd_tfr2d(:,:) + putrd(:,:)
          zvtrd_tfr2d(:,:) = zvtrd_tfr2d(:,:) + pvtrd(:,:)
          ! update (putrd,pvtrd) so that total tfr2d trend is output by call to trd_dyn_iom_2d
          putrd(:,:) = zutrd_tfr2d(:,:)
          pvtrd(:,:) = zvtrd_tfr2d(:,:)

      CASE( jpdyn_bfr )
          !
          !  Save 2D field to add to ZDF trend  and also output 2D field as diagnostic in own right (below).
          ALLOCATE( zutrd_bfr2d(jpi,jpj), zvtrd_bfr2d(jpi,jpj) )
          zutrd_bfr2d(:,:) = putrd(:,:)
          zvtrd_bfr2d(:,:) = pvtrd(:,:)

      END SELECT

      IF( ktrd <= jptot_dyn ) THEN ! output of 2D trends and use for other diagnostics

         !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
         !   2D output of momentum and/or tracers trends using IOM interface
         !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
         IF( ln_dyn_trd )   CALL trd_dyn_iom_2d( putrd, pvtrd, ktrd, kt )
         
!!$   CALLS TO THESE ROUTINES FOR 2D DIAGOSTICS NOT CODED YET
!!$         !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
!!$         !  Integral Constraints Properties for momentum and/or tracers trends
!!$         !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
!!$         IF( ln_glo_trd )   CALL trd_glo( putrd, pvtrd, ktrd, 'DYN', kt )
!!$
!!$         !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
!!$         !  Kinetic Energy trends
!!$         !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
!!$         IF( ln_KE_trd  )   CALL trd_ken( putrd, pvtrd, ktrd, kt )
!!$
!!$         !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
!!$         !  Vorticity trends
!!$         !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
!!$         IF( ln_vor_trd )   CALL trd_vor( putrd, pvtrd, ktrd, kt )
!!$
!!$         !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
!!$         !  Mixed layer trends for active tracers
!!$         !<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
!!$         IF( ln_dyn_mxl )   CALL trd_mxl_dyn   

      ENDIF
      !
   END SUBROUTINE trd_dyn_2d


   SUBROUTINE trd_dyn_iom_3d( putrd, pvtrd, ktrd, kt )
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE trd_dyn_iom  ***
      !! 
      !! ** Purpose :   output 3D trends using IOM
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(:,:,:), INTENT(inout) ::   putrd, pvtrd   ! U and V trends
      INTEGER                   , INTENT(in   ) ::   ktrd           ! trend index
      INTEGER                   , INTENT(in   ) ::   kt             ! time step
      !
      INTEGER ::   ji, jj, jk   ! dummy loop indices
      INTEGER ::   ikbu, ikbv   ! local integers
      REAL(wp), ALLOCATABLE, DIMENSION(:,:)   ::   z2dx, z2dy   ! 2D workspace 
      REAL(wp), ALLOCATABLE, DIMENSION(:,:,:) ::   z3dx, z3dy   ! 3D workspace 
      !!----------------------------------------------------------------------
      !
      SELECT CASE( ktrd )
      CASE( jpdyn_hpg )   ;   CALL iom_put( "utrd_hpg", putrd )    ! hydrostatic pressure gradient
                              CALL iom_put( "vtrd_hpg", pvtrd )
      CASE( jpdyn_pvo )   ;   CALL iom_put( "utrd_pvo", putrd )    ! planetary vorticity
                              CALL iom_put( "vtrd_pvo", pvtrd )
      CASE( jpdyn_rvo )   ;   CALL iom_put( "utrd_rvo", putrd )    ! relative  vorticity     (or metric term)
                              CALL iom_put( "vtrd_rvo", pvtrd )
      CASE( jpdyn_keg )   ;   CALL iom_put( "utrd_keg", putrd )    ! Kinetic Energy gradient (or had)
                              CALL iom_put( "vtrd_keg", pvtrd )
                              ALLOCATE( z3dx(jpi,jpj,jpk) , z3dy(jpi,jpj,jpk) )
                              z3dx(:,:,:) = 0._wp                  ! U.dxU & V.dyV (approximation)
                              z3dy(:,:,:) = 0._wp
                              DO jk = 1, jpkm1   ! no mask as un,vn are masked
                                 DO jj = 2, jpjm1
                                    DO ji = 2, jpim1
                                       z3dx(ji,jj,jk) = un(ji,jj,jk) * ( un(ji+1,jj,jk) - un(ji-1,jj,jk) ) / ( 2._wp * e1u(ji,jj) )
                                       z3dy(ji,jj,jk) = vn(ji,jj,jk) * ( vn(ji,jj+1,jk) - vn(ji,jj-1,jk) ) / ( 2._wp * e2v(ji,jj) )
                                    END DO
                                 END DO
                              END DO
                              CALL lbc_lnk_multi( 'trddyn', z3dx, 'U', -1., z3dy, 'V', -1. )
                              CALL iom_put( "utrd_udx", z3dx  )
                              CALL iom_put( "vtrd_vdy", z3dy  )
                              DEALLOCATE( z3dx , z3dy )
      CASE( jpdyn_zad )   ;   CALL iom_put( "utrd_zad", putrd )    ! vertical advection
                              CALL iom_put( "vtrd_zad", pvtrd )
      CASE( jpdyn_ldf )   ;   CALL iom_put( "utrd_ldf", putrd )    ! lateral  diffusion
                              CALL iom_put( "vtrd_ldf", pvtrd )
      CASE( jpdyn_zdf )   ;   CALL iom_put( "utrd_zdf", putrd )    ! vertical diffusion 
                              CALL iom_put( "vtrd_zdf", pvtrd )
      CASE( jpdyn_bfr )   ;   CALL iom_put( "utrd_bfr", putrd )    ! bottom friction for bottom layer
                              CALL iom_put( "vtrd_bfr", pvtrd )
      CASE( jpdyn_tfr )   ;   CALL iom_put( "utrd_tfr", putrd )    ! total top friction for top layer
                              CALL iom_put( "vtrd_tfr", pvtrd )
      CASE( jpdyn_tot )   ;   CALL iom_put( "utrd_tot", putrd )    ! total trends excluding asselin filter
                              CALL iom_put( "vtrd_tot", pvtrd )
      CASE( jpdyn_atf )   ;   CALL iom_put( "utrd_atf", putrd )    ! asselin filter trends 
                              CALL iom_put( "vtrd_atf", pvtrd )
      END SELECT
      !
   END SUBROUTINE trd_dyn_iom_3d


   SUBROUTINE trd_dyn_iom_2d( putrd, pvtrd, ktrd, kt )
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE trd_dyn_iom  ***
      !! 
      !! ** Purpose :   output 2D trends using IOM
      !!----------------------------------------------------------------------
      REAL(wp), DIMENSION(:,:), INTENT(inout) ::   putrd, pvtrd   ! U and V trends
      INTEGER                 , INTENT(in   ) ::   ktrd           ! trend index
      INTEGER                 , INTENT(in   ) ::   kt             ! time step
      !
      INTEGER ::   ji, jj, jk   ! dummy loop indices
      INTEGER ::   ikbu, ikbv   ! local integers
      REAL(wp), ALLOCATABLE, DIMENSION(:,:)   ::   z2dx, z2dy   ! 2D workspace 
      REAL(wp), ALLOCATABLE, DIMENSION(:,:,:) ::   z3dx, z3dy   ! 3D workspace 
      !!----------------------------------------------------------------------
      !
      SELECT CASE( ktrd )
      CASE( jpdyn_spg )      ;   CALL iom_put( "utrd_spg2d", putrd )      ! surface pressure gradient
                                 CALL iom_put( "vtrd_spg2d", pvtrd )
      CASE( jpdyn_pvo )      ;   CALL iom_put( "utrd_pvo2d", putrd )      ! planetary vorticity (barotropic part)
                                 CALL iom_put( "vtrd_pvo2d", pvtrd )
      CASE( jpdyn_frc2d )    ;   CALL iom_put( "utrd_frc2d", putrd )      ! constant forcing term from barotropic calcn.
                                 CALL iom_put( "vtrd_frc2d", pvtrd ) 
      CASE( jpdyn_tau )      ;   CALL iom_put( "utrd_tau", putrd )        ! surface wind stress trend
                                 CALL iom_put( "vtrd_tau", pvtrd )
      CASE( jpdyn_tau2d )    ;   CALL iom_put( "utrd_tau2d", putrd )      ! wind stress depth-mean trend
                                 CALL iom_put( "vtrd_tau2d", pvtrd )
      CASE( jpdyn_bfr )      ;   CALL iom_put( "utrd_bfr2d", putrd )      ! bottom friction depth-mean trend
                                 CALL iom_put( "vtrd_bfr2d", pvtrd )
      CASE( jpdyn_tfr )      ;   CALL iom_put( "utrd_tfr2d", putrd )      ! top friction depth-mean trend
                                 CALL iom_put( "vtrd_tfr2d", pvtrd )
      CASE( jpdyn_tot )      ;   CALL iom_put( "utrd_tot2d", putrd )      ! total 2D trend, excluding time filter
                                 CALL iom_put( "vtrd_tot2d", pvtrd )
      END SELECT
      !
   END SUBROUTINE trd_dyn_iom_2d

   !!======================================================================
END MODULE trddyn
