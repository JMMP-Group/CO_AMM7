MODULE istate
   !!======================================================================
   !!                     ***  MODULE  istate  ***
   !! Ocean state   :  initial state setting
   !!=====================================================================
   !! History :  OPA  !  1989-12  (P. Andrich)  Original code
   !!            5.0  !  1991-11  (G. Madec)  rewritting
   !!            6.0  !  1996-01  (G. Madec)  terrain following coordinates
   !!            8.0  !  2001-09  (M. Levy, M. Ben Jelloul)  istate_eel
   !!            8.0  !  2001-09  (M. Levy, M. Ben Jelloul)  istate_uvg
   !!   NEMO     1.0  !  2003-08  (G. Madec, C. Talandier)  F90: Free form, modules + EEL R5
   !!             -   !  2004-05  (A. Koch-Larrouy)  istate_gyre 
   !!            2.0  !  2006-07  (S. Masson)  distributed restart using iom
   !!            3.3  !  2010-10  (C. Ethe) merge TRC-TRA
   !!            3.4  !  2011-04  (G. Madec) Merge of dtatem and dtasal & suppression of tb,tn/sb,sn 
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !!   istate_init   : initial state setting
   !!   istate_tem    : analytical profile for initial Temperature
   !!   istate_sal    : analytical profile for initial Salinity
   !!   istate_eel    : initial state setting of EEL R5 configuration
   !!   istate_gyre   : initial state setting of GYRE configuration
   !!   istate_uvg    : initial velocity in geostropic balance
   !!----------------------------------------------------------------------
   USE oce             ! ocean dynamics and active tracers 
   USE dom_oce         ! ocean space and time domain 
   USE c1d             ! 1D vertical configuration
   USE daymod          ! calendar
   USE eosbn2          ! eq. of state, Brunt Vaisala frequency (eos     routine)
   USE ldftra_oce      ! ocean active tracers: lateral physics
   USE zdf_oce         ! ocean vertical physics
   USE phycst          ! physical constants
   USE dtatsd          ! data temperature and salinity   (dta_tsd routine)
   USE dtauvd          ! data: U & V current             (dta_uvd routine)
   USE zpshde          ! partial step: hor. derivative (zps_hde routine)
   USE eosbn2          ! equation of state            (eos bn2 routine)
   USE domvvl          ! varying vertical mesh
   USE dynspg_oce      ! pressure gradient schemes
   USE dynspg_flt      ! filtered free surface
   USE sol_oce         ! ocean solver variables
   !
   USE in_out_manager  ! I/O manager
   USE iom             ! I/O library
   USE lib_mpp         ! MPP library
   USE restart         ! restart
   USE wrk_nemo        ! Memory allocation
   USE timing          ! Timing
   USE par_oce
   USE splines

   IMPLICIT NONE
   PRIVATE

   PUBLIC   istate_init   ! routine called by step.F90

   !! * Substitutions
#  include "domzgr_substitute.h90"
#  include "vectopt_loop_substitute.h90"
   !!----------------------------------------------------------------------
   !! NEMO/OPA 3.7 , NEMO Consortium (2014)
   !! $Id: istate.F90 5332 2015-06-01 16:59:09Z mathiot $
   !! Software governed by the CeCILL licence     (NEMOGCM/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------
CONTAINS

   SUBROUTINE istate_init
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE istate_init  ***
      !! 
      !! ** Purpose :   Initialization of the dynamics and tracer fields.
      !!----------------------------------------------------------------------
      INTEGER ::   ji, jj, jk   ! dummy loop indices
      REAL(wp), POINTER, DIMENSION(:,:,:,:) ::   zuvd    ! U & V data workspace
      !!----------------------------------------------------------------------
      !
      IF( nn_timing == 1 )   CALL timing_start('istate_init')
      !

      IF(lwp) WRITE(numout,*) ' '
      IF(lwp) WRITE(numout,*) 'istate_ini : Initialization of the dynamics and tracers'
      IF(lwp) WRITE(numout,*) '~~~~~~~~~~'

      CALL day_init       ! need this to read initial conditions with interpolation

      CALL dta_tsd_init                       ! Initialisation of T & S input data
      IF( lk_c1d ) CALL dta_uvd_init          ! Initialization of U & V input data

      rhd  (:,:,:  ) = 0._wp   ;   rhop (:,:,:  ) = 0._wp      ! set one for all to 0 at level jpk
      rn2b (:,:,:  ) = 0._wp   ;   rn2  (:,:,:  ) = 0._wp      ! set one for all to 0 at levels 1 and jpk
      tsa  (:,:,:,:) = 0._wp                                   ! set one for all to 0 at level jpk
      rab_b(:,:,:,:) = 0._wp   ;   rab_n(:,:,:,:) = 0._wp      ! set one for all to 0 at level jpk

      IF( ln_rstart ) THEN                    ! Restart from a file
         !                                    ! -------------------
         CALL rst_read                           ! Read the restart file
         CALL day_init                           ! model calendar (using both namelist and restart infos)
      ELSE
         !                                    ! Start from rest
         !                                    ! ---------------
         numror = 0                              ! define numror = 0 -> no restart file to read
         neuler = 0                              ! Set time-step indicator at nit000 (euler forward)
         CALL day_init                           ! model calendar (using both namelist and restart infos)
         !                                       ! Initialization of ocean to zero
         !   before fields      !       now fields     
         sshb (:,:)   = 0._wp   ;   sshn (:,:)   = 0._wp
         ub   (:,:,:) = 0._wp   ;   un   (:,:,:) = 0._wp
         vb   (:,:,:) = 0._wp   ;   vn   (:,:,:) = 0._wp  
         rotb (:,:,:) = 0._wp   ;   rotn (:,:,:) = 0._wp
         hdivb(:,:,:) = 0._wp   ;   hdivn(:,:,:) = 0._wp
         !
         IF( cp_cfg == 'eel' ) THEN
            CALL istate_eel                      ! EEL   configuration : start from pre-defined U,V T-S fields
         ELSEIF( cp_cfg == 'gyre' ) THEN         
            CALL istate_gyre                     ! GYRE  configuration : start from pre-defined T-S fields
         ELSE                                    ! Initial T-S, U-V fields read in files
            IF ( ln_tsd_init ) THEN              ! read 3D T and S data at nit000
               CALL dta_tsd( nit000, tsb )  
               tsn(:,:,:,:) = tsb(:,:,:,:)
               !
            ELSE                                 ! Initial T-S fields defined analytically
               CALL istate_t_s
            ENDIF
            IF ( ln_uvd_init .AND. lk_c1d ) THEN ! read 3D U and V data at nit000
               CALL wrk_alloc( jpi, jpj, jpk, 2, zuvd )
               CALL dta_uvd( nit000, zuvd )
               ub(:,:,:) = zuvd(:,:,:,1) ;  un(:,:,:) = ub(:,:,:)
               vb(:,:,:) = zuvd(:,:,:,2) ;  vn(:,:,:) = vb(:,:,:)
               CALL wrk_dealloc( jpi, jpj, jpk, 2, zuvd )
            ENDIF
         ENDIF
         !
         CALL eos( tsb, rhd, rhop, gdept_0(:,:,:) )        ! before potential and in situ densities
#if ! defined key_c1d
         IF( ln_zps .AND. .NOT. ln_isfcav)                                 &
            &            CALL zps_hde    ( nit000, jpts, tsb, gtsu, gtsv,  &    ! Partial steps: before horizontal gradient
            &                                            rhd, gru , grv    )  ! of t, s, rd at the last ocean level
         IF( ln_zps .AND.       ln_isfcav)                                 &
            &            CALL zps_hde_isf( nit000, jpts, tsb, gtsu, gtsv,  &    ! Partial steps for top cell (ISF)
            &                                            rhd, gru , grv , aru , arv , gzu , gzv , ge3ru , ge3rv ,   &
            &                                     gtui, gtvi, grui, grvi, arui, arvi, gzui, gzvi, ge3rui, ge3rvi    ) ! of t, s, rd at the last ocean level
#endif
         !   
         ! - ML - sshn could be modified by istate_eel, so that initialization of fse3t_b is done here
         IF( lk_vvl ) THEN
            DO jk = 1, jpk
               fse3t_b(:,:,jk) = fse3t_n(:,:,jk)
            ENDDO
         ENDIF
         ! 
      ENDIF
      !
      IF( lk_agrif ) THEN                  ! read free surface arrays in restart file
         IF( ln_rstart ) THEN
            IF( lk_dynspg_flt )  THEN      ! read or initialize the following fields
               !                           ! gcx, gcxb for agrif_opa_init
               IF( sol_oce_alloc()  > 0 )   CALL ctl_stop('agrif sol_oce_alloc: allocation of arrays failed')
               CALL flt_rst( nit000, 'READ' )
            ENDIF
         ENDIF                             ! explicit case not coded yet with AGRIF
      ENDIF
      !
      ! 
      ! Initialize "now" and "before" barotropic velocities:
      ! Do it whatever the free surface method, these arrays
      ! being eventually used
      !
      !
      un_b(:,:) = 0._wp ; vn_b(:,:) = 0._wp
      ub_b(:,:) = 0._wp ; vb_b(:,:) = 0._wp
      !
      DO jk = 1, jpkm1
         DO jj = 1, jpj
            DO ji = 1, jpi
               un_b(ji,jj) = un_b(ji,jj) + fse3u_n(ji,jj,jk) * un(ji,jj,jk) * umask(ji,jj,jk)
               vn_b(ji,jj) = vn_b(ji,jj) + fse3v_n(ji,jj,jk) * vn(ji,jj,jk) * vmask(ji,jj,jk)
               !
               ub_b(ji,jj) = ub_b(ji,jj) + fse3u_b(ji,jj,jk) * ub(ji,jj,jk) * umask(ji,jj,jk)
               vb_b(ji,jj) = vb_b(ji,jj) + fse3v_b(ji,jj,jk) * vb(ji,jj,jk) * vmask(ji,jj,jk)
            END DO
         END DO
      END DO
      !
      un_b(:,:) = un_b(:,:) * hur  (:,:)
      vn_b(:,:) = vn_b(:,:) * hvr  (:,:)
      !
      ub_b(:,:) = ub_b(:,:) * hur_b(:,:)
      vb_b(:,:) = vb_b(:,:) * hvr_b(:,:)
      !
      !
      IF( nn_timing == 1 )   CALL timing_stop('istate_init')
      !
   END SUBROUTINE istate_init


   SUBROUTINE istate_t_s
      !!---------------------------------------------------------------------
      !!                  ***  ROUTINE istate_t_s  ***
      !!   
      !! ** Purpose :   Intialization of the temperature field with an 
      !!      analytical profile or a file (i.e. in EEL configuration)
      !!
      !! ** Method  : - temperature: use Philander analytic profile
      !!              - salinity   : use to a constant value 35.5
      !!
      !! References :  Philander ???
      !!----------------------------------------------------------------------
      !
      REAL(wp), DIMENSION(42)  ::   zdep, zsal, ztem
      INTEGER :: ji, jj, jk  ! dummy loop indices
      !!----------------------------------------------------------------------
      !
      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) 'usr_def_istate : analytical definition of initial state '
      IF(lwp) WRITE(numout,*) '~~~~~~~~~~~~~~   Ocean at rest, with an horizontally uniform T and S profiles'
      !
      zdep(:) = (/ &
                   &    5.02,   15.08,   25.16,   35.28,   45.45,   55.69, &
                   &   66.04,   76.55,   87.27,   98.31,  109.81,  121.95, &
                   &  135.03,  149.43,  165.73,  184.70,  207.43,  235.39, &
                   &  270.53,  315.37,  372.97,  446.80,  540.50,  657.32, &
                   &  799.55,  968.00, 1161.81, 1378.66, 1615.29, 1868.07, &
                   & 2133.52, 2408.58, 2690.78, 2978.17, 3269.28, 3563.04, &
                   & 3858.68, 4155.63, 4453.50, 4752.02, 5050.99, 5350.27  &
                   & /)

      SELECT CASE(nn_tsd_zone)

         CASE(1) ! T/S July on-shelf 
            ztem(:) = (/ &
                         & 13.8059, 13.1661, 12.0471, 10.7265,  9.5585,  8.7273, &
                         &  8.2527,  7.9696,  7.7605,  7.5965,  7.5038,  7.5018, &
                         &  7.5609,  7.6436,  7.7050,  7.7011,  7.5887,  7.3471, &
                         &  6.9203,  6.1857,  5.2453,  4.1932,  3.4076,  2.8450, &
                         &  2.6111,  2.5000,  2.5000,  2.5000,  2.5000,  2.5000, &
                         &  2.5000,  2.5000,  2.5000,  2.5000,  2.5000,  2.5000, &
                         &  2.5000,  2.5000,  2.5000,  2.5000,  2.5000,  2.5000  &
                         & /)

            zsal(:) = (/ &
                         & 33.7727, 33.9841, 34.2683, 34.5326, 34.7335, 34.9079, &
                         & 35.0119, 35.0726, 35.0902, 35.1034, 35.1101, 35.1223, &
                         & 35.1436, 35.1718, 35.1893, 35.1929, 35.1828, 35.1670, &
                         & 35.1408, 35.0990, 35.0415, 34.9760, 34.9168, 34.8753, &
                         & 34.8558, 34.8502, 34.8504, 34.8506, 34.8508, 34.8510, &
                         & 34.8512, 34.8514, 34.8516, 34.8518, 34.8520, 34.8522, &
                         & 34.8524, 34.8526, 34.8528, 34.8530, 34.8532, 34.8534  &
                         & /)

         CASE(2) ! T/S July off-shelf

            ztem(:) = (/ &
                         & 13.0669, 12.8587, 12.4760, 11.9986, 11.5363, 11.1627, &
                         & 10.8898, 10.6753, 10.4927, 10.3334, 10.2182, 10.1457, &
                         & 10.1038, 10.0734, 10.0389,  9.9968,  9.9459,  9.8836, &
                         &  9.8069,  9.6953,  9.5345,  9.2901,  8.9319,  8.4192, &
                         &  7.7006,  6.7895,  5.7774,  4.8576,  4.1510,  3.6716, &
                         &  3.3331,  3.0606,  2.8275,  2.6317,  2.4735,  2.3497, &
                         &  2.2601,  2.1973,  2.1555,  2.1237,  2.1072,  2.1000  &
                         & /)

            zsal(:) = (/ &
                         & 35.2001, 35.2052, 35.2186, 35.2411, 35.2661, 35.2873, &
                         & 35.3021, 35.3124, 35.3205, 35.3267, 35.3304, 35.3330, &
                         & 35.3355, 35.3393, 35.3422, 35.3438, 35.3436, 35.3428, &
                         & 35.3413, 35.3374, 35.3313, 35.3239, 35.3192, 35.3171, &
                         & 35.3171, 35.3171, 35.3171, 35.3171, 35.3171, 35.3171, &
                         & 35.3171, 35.3171, 35.3171, 35.3171, 35.3171, 35.3171, &
                         & 35.3171, 35.3171, 35.3171, 35.3171, 35.3171, 35.3171  &
                         & /)


         CASE(3) ! T/S January on-shelf

            ztem(:) = (/ &
                         &  7.8383,  7.9482,  8.0837,  8.1641,  8.1750,  8.1510, &
                         &  8.1110,  8.0510,  7.9648,  7.8804,  7.8221,  7.8020, &
                         &  7.7959,  7.7919,  7.8288,  7.8825,  7.9012,  7.7028, &
                         &  7.2095,  6.3642,  5.2466,  4.0221,  2.9315,  2.1751, &
                         &  1.7760,  1.6325,  1.6008,  1.6000,  1.6000,  1.6000, &
                         &  1.6000,  1.6000,  1.6000,  1.6000,  1.6000,  1.6000, &
                         &  1.6000,  1.6000,  1.6000,  1.6000,  1.6000,  1.6000  &
                         & /)

            zsal(:) = (/ &
                         & 34.0842, 34.1682, 34.3100, 34.4740, 34.6210, 34.7275, &
                         & 34.7861, 34.8231, 34.8661, 34.9293, 34.9936, 35.0405, &
                         & 35.0618, 35.0683, 35.0791, 35.0980, 35.1224, 35.1287, &
                         & 35.1140, 35.0773, 35.0315, 34.9838, 34.9431, 34.9159, &
                         & 34.9019, 34.8971, 34.8960, 34.8962, 34.8964, 34.8966, &
                         & 34.8968, 34.8970, 34.8972, 34.8974, 34.8976, 34.8978, &
                         & 34.8980, 34.8982, 34.8984, 34.8986, 34.8988, 34.8990  &
                         & /)

         CASE(4) ! T/S January off-shelf

            ztem(:) = (/ &
                         &  9.9555,  9.9499,  9.9411,  9.9311,  9.9211,  9.9111, &
                         &  9.9011,  9.8911,  9.8811,  9.8711,  9.8611,  9.8511, &
                         &  9.8411,  9.8311,  9.8211,  9.8111,  9.8011,  9.7911, &
                         &  9.7840,  9.7487,  9.6420,  9.3992,  9.0030,  8.4382, &
                         &  7.7101,  6.8430,  5.9185,  5.0553,  4.3437,  3.8076, &
                         &  3.4096,  3.0998,  2.8462,  2.6397,  2.4862,  2.3868, &
                         &  2.3374,  2.3159,  2.3059,  2.2959,  2.2870,  2.2815  &
                         & /)

            zsal(:) = (/ &
                         & 35.2802, 35.2808, 35.2817, 35.2827, 35.2837, 35.2847, &
                         & 35.2857, 35.2867, 35.2877, 35.2887, 35.2897, 35.2907, &
                         & 35.2917, 35.2927, 35.2937, 35.2947, 35.2957, 35.2967, &
                         & 35.2998, 35.3032, 35.3048, 35.2985, 35.2851, 35.2640, &
                         & 35.2340, 35.1899, 35.1320, 35.0692, 35.0150, 34.9784, &
                         & 34.9598, 34.9527, 34.9502, 34.9474, 34.9436, 34.9397, &
                         & 34.9379, 34.9378, 34.9388, 34.9398, 34.9407, 34.9413  &
                         & /)

      END SELECT

    ! DO jk = 1, jpk             ! horizontally uniform T & S profiles
         DO jj = 1, jpj
            DO ji = 1, jpi
               tsn(ji,jj,:,jp_tem) = spline3(zdep,ztem,fsdept(ji,jj,:)) * tmask(ji,jj,:)
               tsn(ji,jj,:,jp_sal) = spline3(zdep,zsal,fsdept(ji,jj,:)) * tmask(ji,jj,:)
            END DO
         END DO
    ! END DO
      tsb(:,:,:,jp_tem) = tsn(:,:,:,jp_tem)
      tsb(:,:,:,jp_sal) = tsn(:,:,:,jp_sal)


      !
   END SUBROUTINE istate_t_s


   SUBROUTINE istate_eel
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE istate_eel  ***
      !! 
      !! ** Purpose :   Initialization of the dynamics and tracers for EEL R5
      !!      configuration (channel with or without a topographic bump)
      !!
      !! ** Method  : - set temprature field
      !!              - set salinity field
      !!              - set velocity field including horizontal divergence
      !!                and relative vorticity fields
      !!----------------------------------------------------------------------
      USE divcur     ! hor. divergence & rel. vorticity      (div_cur routine)
      USE iom
      !
      INTEGER  ::   inum              ! temporary logical unit
      INTEGER  ::   ji, jj, jk        ! dummy loop indices
      INTEGER  ::   ijloc
      REAL(wp) ::   zh1, zh2, zslope, zcst, zfcor   ! temporary scalars
      REAL(wp) ::   zt1  = 15._wp                   ! surface temperature value (EEL R5)
      REAL(wp) ::   zt2  =  5._wp                   ! bottom  temperature value (EEL R5)
      REAL(wp) ::   zsal = 35.0_wp                  ! constant salinity (EEL R2, R5 and R6)
      REAL(wp) ::   zueel = 0.1_wp                  ! constant uniform zonal velocity (EEL R5)
      REAL(wp), DIMENSION(jpiglo,jpjglo) ::   zssh  ! initial ssh over the global domain
      !!----------------------------------------------------------------------
      !
      SELECT CASE ( jp_cfg ) 
         !                                              ! ====================
         CASE ( 5 )                                     ! EEL R5 configuration
            !                                           ! ====================
            !
            ! set temperature field with a linear profile
            ! -------------------------------------------
            IF(lwp) WRITE(numout,*)
            IF(lwp) WRITE(numout,*) 'istate_eel : EEL R5: linear temperature profile'
            IF(lwp) WRITE(numout,*) '~~~~~~~~~~'
            !
            zh1 = gdept_1d(  1  )
            zh2 = gdept_1d(jpkm1)
            !
            zslope = ( zt1 - zt2 ) / ( zh1 - zh2 )
            zcst   = ( zt1 * ( zh1 - zh2) - ( zt1 - zt2 ) * zh1 ) / ( zh1 - zh2 )
            !
            DO jk = 1, jpk
               tsn(:,:,jk,jp_tem) = ( zt2 + zt1 * exp( - fsdept(:,:,jk) / 1000 ) ) * tmask(:,:,jk)
               tsb(:,:,jk,jp_tem) = tsn(:,:,jk,jp_tem)
            END DO
            !
            IF(lwp) CALL prizre( tsn(:,:,:,jp_tem), jpi   , jpj   , jpk   , jpj/2 ,   &
               &                             1     , jpi   , 5     , 1     , jpk   ,   &
               &                             1     , 1.    , numout                  )
            !
            ! set salinity field to a constant value
            ! --------------------------------------
            IF(lwp) WRITE(numout,*)
            IF(lwp) WRITE(numout,*) 'istate_eel : EEL R5: constant salinity field, S = ', zsal
            IF(lwp) WRITE(numout,*) '~~~~~~~~~~'
            !
            tsn(:,:,:,jp_sal) = zsal * tmask(:,:,:)
            tsb(:,:,:,jp_sal) = tsn(:,:,:,jp_sal)
            !
            ! set the dynamics: U,V, hdiv, rot (and ssh if necessary)
            ! ----------------
            ! Start EEL5 configuration with barotropic geostrophic velocities 
            ! according the sshb and sshn SSH imposed.
            ! we assume a uniform grid (hence the use of e1t(1,1) for delta_y)
            ! we use the Coriolis frequency at mid-channel.   
            ub(:,:,:) = zueel * umask(:,:,:)
            un(:,:,:) = ub(:,:,:)
            ijloc = mj0(INT(jpjglo-1)/2)
            zfcor = ff(1,ijloc)
            !
            DO jj = 1, jpjglo
               zssh(:,jj) = - (FLOAT(jj)- FLOAT(jpjglo-1)/2.)*zueel*e1t(1,1)*zfcor/grav 
            END DO
            !
            IF(lwp) THEN
               WRITE(numout,*) ' Uniform zonal velocity for EEL R5:',zueel
               WRITE(numout,*) ' Geostrophic SSH profile as a function of y:'
               WRITE(numout,'(12(1x,f6.2))') zssh(1,:)
            ENDIF
            !
            DO jj = 1, nlcj
               DO ji = 1, nlci
                  sshb(ji,jj) = zssh( mig(ji) , mjg(jj) ) * tmask(ji,jj,1)
               END DO
            END DO
            sshb(nlci+1:jpi,      :   ) = 0.e0      ! set to zero extra mpp columns
            sshb(      :   ,nlcj+1:jpj) = 0.e0      ! set to zero extra mpp rows
            !
            sshn(:,:) = sshb(:,:)                   ! set now ssh to the before value
            !
            IF( nn_rstssh /= 0 ) THEN  
               nn_rstssh = 0                        ! hand-made initilization of ssh 
               CALL ctl_warn( 'istate_eel: force nn_rstssh = 0' )
            ENDIF
            !
            CALL div_cur( nit000 )                  ! horizontal divergence and relative vorticity (curl)
            ! N.B. the vertical velocity will be computed from the horizontal divergence field
            ! in istate by a call to wzv routine


            !                                     ! ==========================
         CASE ( 2 , 6 )                           ! EEL R2 or R6 configuration
            !                                     ! ==========================
            !
            ! set temperature field with a NetCDF file
            ! ----------------------------------------
            IF(lwp) WRITE(numout,*)
            IF(lwp) WRITE(numout,*) 'istate_eel : EEL R2 or R6: read initial temperature in a NetCDF file'
            IF(lwp) WRITE(numout,*) '~~~~~~~~~~'
            !
            CALL iom_open ( 'eel.initemp', inum )
            CALL iom_get ( inum, jpdom_data, 'initemp', tsb(:,:,:,jp_tem) ) ! read before temprature (tb)
            CALL iom_close( inum )
            !
            tsn(:,:,:,jp_tem) = tsb(:,:,:,jp_tem)                            ! set nox temperature to tb
            !
            IF(lwp) CALL prizre( tsn(:,:,:,jp_tem), jpi   , jpj   , jpk   , jpj/2 ,   &
               &                            1     , jpi   , 5     , 1     , jpk   ,   &
               &                            1     , 1.    , numout                  )
            !
            ! set salinity field to a constant value
            ! --------------------------------------
            IF(lwp) WRITE(numout,*)
            IF(lwp) WRITE(numout,*) 'istate_eel : EEL R5: constant salinity field, S = ', zsal
            IF(lwp) WRITE(numout,*) '~~~~~~~~~~'
            !
            tsn(:,:,:,jp_sal) = zsal * tmask(:,:,:)
            tsb(:,:,:,jp_sal) = tsn(:,:,:,jp_sal)
            !
            !                                    ! ===========================
         CASE DEFAULT                            ! NONE existing configuration
            !                                    ! ===========================
            WRITE(ctmp1,*) 'EEL with a ', jp_cfg,' km resolution is not coded'
            CALL ctl_stop( ctmp1 )
            !
      END SELECT
      !
   END SUBROUTINE istate_eel


   SUBROUTINE istate_gyre
      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE istate_gyre  ***
      !! 
      !! ** Purpose :   Initialization of the dynamics and tracers for GYRE
      !!      configuration (double gyre with rotated domain)
      !!
      !! ** Method  : - set temprature field
      !!              - set salinity field
      !!----------------------------------------------------------------------
      INTEGER :: ji, jj, jk  ! dummy loop indices
      INTEGER            ::   inum          ! temporary logical unit
      INTEGER, PARAMETER ::   ntsinit = 0   ! (0/1) (analytical/input data files) T&S initialization
      !!----------------------------------------------------------------------
      !
      SELECT CASE ( ntsinit)
      !
      CASE ( 0 )                  ! analytical T/S profil deduced from LEVITUS
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) 'istate_gyre : initial analytical T and S profil deduced from LEVITUS '
         IF(lwp) WRITE(numout,*) '~~~~~~~~~~~'
         !
         DO jk = 1, jpk
            DO jj = 1, jpj
               DO ji = 1, jpi
                  tsn(ji,jj,jk,jp_tem) = (  16. - 12. * TANH( (fsdept(ji,jj,jk) - 400) / 700 )         )   &
                       &           * (-TANH( (500-fsdept(ji,jj,jk)) / 150 ) + 1) / 2               &
                       &       + (      15. * ( 1. - TANH( (fsdept(ji,jj,jk)-50.) / 1500.) )       &
                       &                - 1.4 * TANH((fsdept(ji,jj,jk)-100.) / 100.)               &    
                       &                + 7.  * (1500. - fsdept(ji,jj,jk)) / 1500.             )   & 
                       &           * (-TANH( (fsdept(ji,jj,jk) - 500) / 150) + 1) / 2
                  tsn(ji,jj,jk,jp_tem) = tsn(ji,jj,jk,jp_tem) * tmask(ji,jj,jk)
                  tsb(ji,jj,jk,jp_tem) = tsn(ji,jj,jk,jp_tem)

                  tsn(ji,jj,jk,jp_sal) =  (  36.25 - 1.13 * TANH( (fsdept(ji,jj,jk) - 305) / 460 )  )  &
                     &              * (-TANH((500 - fsdept(ji,jj,jk)) / 150) + 1) / 2          &
                     &          + (  35.55 + 1.25 * (5000. - fsdept(ji,jj,jk)) / 5000.         &
                     &                - 1.62 * TANH( (fsdept(ji,jj,jk) - 60.  ) / 650. )       &
                     &                + 0.2  * TANH( (fsdept(ji,jj,jk) - 35.  ) / 100. )       &
                     &                + 0.2  * TANH( (fsdept(ji,jj,jk) - 1000.) / 5000.)    )  &
                     &              * (-TANH((fsdept(ji,jj,jk) - 500) / 150) + 1) / 2 
                  tsn(ji,jj,jk,jp_sal) = tsn(ji,jj,jk,jp_sal) * tmask(ji,jj,jk)
                  tsb(ji,jj,jk,jp_sal) = tsn(ji,jj,jk,jp_sal)
               END DO
            END DO
         END DO
         !
      CASE ( 1 )                  ! T/S data fields read in dta_tem.nc/data_sal.nc files
         IF(lwp) WRITE(numout,*)
         IF(lwp) WRITE(numout,*) 'istate_gyre : initial T and S read from dta_tem.nc/data_sal.nc files'
         IF(lwp) WRITE(numout,*) '~~~~~~~~~~~'
         IF(lwp) WRITE(numout,*) '              NetCDF FORMAT'

         ! Read temperature field
         ! ----------------------
         CALL iom_open ( 'data_tem', inum )
         CALL iom_get ( inum, jpdom_data, 'votemper', tsn(:,:,:,jp_tem) ) 
         CALL iom_close( inum )

         tsn(:,:,:,jp_tem) = tsn(:,:,:,jp_tem) * tmask(:,:,:) 
         tsb(:,:,:,jp_tem) = tsn(:,:,:,jp_tem)

         ! Read salinity field
         ! -------------------
         CALL iom_open ( 'data_sal', inum )
         CALL iom_get ( inum, jpdom_data, 'vosaline', tsn(:,:,:,jp_sal) ) 
         CALL iom_close( inum )

         tsn(:,:,:,jp_sal) = tsn(:,:,:,jp_sal) * tmask(:,:,:) 
         tsb(:,:,:,jp_sal) = tsn(:,:,:,jp_sal)
         !
      END SELECT
      !
      IF(lwp) THEN
         WRITE(numout,*)
         WRITE(numout,*) '              Initial temperature and salinity profiles:'
         WRITE(numout, "(9x,' level   gdept_1d   temperature   salinity   ')" )
         WRITE(numout, "(10x, i4, 3f10.2)" ) ( jk, gdept_1d(jk), tsn(2,2,jk,jp_tem), tsn(2,2,jk,jp_sal), jk = 1, jpk )
      ENDIF
      !
   END SUBROUTINE istate_gyre


   SUBROUTINE istate_uvg
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE istate_uvg  ***
      !!
      !! ** Purpose :   Compute the geostrophic velocities from (tn,sn) fields
      !!
      !! ** Method  :   Using the hydrostatic hypothesis the now hydrostatic 
      !!      pressure is computed by integrating the in-situ density from the
      !!      surface to the bottom.
      !!                 p=integral [ rau*g dz ]
      !!----------------------------------------------------------------------
      USE dynspg          ! surface pressure gradient             (dyn_spg routine)
      USE divcur          ! hor. divergence & rel. vorticity      (div_cur routine)
      USE lbclnk          ! ocean lateral boundary condition (or mpp link)
      !
      INTEGER ::   ji, jj, jk        ! dummy loop indices
      INTEGER ::   indic             ! ???
      REAL(wp) ::   zmsv, zphv, zmsu, zphu, zalfg     ! temporary scalars
      REAL(wp), POINTER, DIMENSION(:,:,:) :: zprn
      !!----------------------------------------------------------------------
      !
      CALL wrk_alloc( jpi, jpj, jpk, zprn)
      !
      IF(lwp) WRITE(numout,*) 
      IF(lwp) WRITE(numout,*) 'istate_uvg : Start from Geostrophy'
      IF(lwp) WRITE(numout,*) '~~~~~~~~~~'

      ! Compute the now hydrostatic pressure
      ! ------------------------------------

      zalfg = 0.5 * grav * rau0
      
      zprn(:,:,1) = zalfg * fse3w(:,:,1) * ( 1 + rhd(:,:,1) )       ! Surface value

      DO jk = 2, jpkm1                                              ! Vertical integration from the surface
         zprn(:,:,jk) = zprn(:,:,jk-1)   &
            &         + zalfg * fse3w(:,:,jk) * ( 2. + rhd(:,:,jk) + rhd(:,:,jk-1) )
      END DO  

      ! Compute geostrophic balance
      ! ---------------------------
      DO jk = 1, jpkm1
         DO jj = 2, jpjm1
            DO ji = fs_2, fs_jpim1   ! vertor opt.
               zmsv = 1. / MAX(  umask(ji-1,jj+1,jk) + umask(ji  ,jj+1,jk)   &
                               + umask(ji-1,jj  ,jk) + umask(ji  ,jj  ,jk) , 1.  )
               zphv = ( zprn(ji  ,jj+1,jk) - zprn(ji-1,jj+1,jk) ) * umask(ji-1,jj+1,jk) / e1u(ji-1,jj+1)   &
                    + ( zprn(ji+1,jj+1,jk) - zprn(ji  ,jj+1,jk) ) * umask(ji  ,jj+1,jk) / e1u(ji  ,jj+1)   &
                    + ( zprn(ji  ,jj  ,jk) - zprn(ji-1,jj  ,jk) ) * umask(ji-1,jj  ,jk) / e1u(ji-1,jj  )   &
                    + ( zprn(ji+1,jj  ,jk) - zprn(ji  ,jj  ,jk) ) * umask(ji  ,jj  ,jk) / e1u(ji  ,jj  )
               zphv = 1. / rau0 * zphv * zmsv * vmask(ji,jj,jk)

               zmsu = 1. / MAX(  vmask(ji+1,jj  ,jk) + vmask(ji  ,jj  ,jk)   &
                               + vmask(ji+1,jj-1,jk) + vmask(ji  ,jj-1,jk) , 1.  )
               zphu = ( zprn(ji+1,jj+1,jk) - zprn(ji+1,jj  ,jk) ) * vmask(ji+1,jj  ,jk) / e2v(ji+1,jj  )   &
                    + ( zprn(ji  ,jj+1,jk) - zprn(ji  ,jj  ,jk) ) * vmask(ji  ,jj  ,jk) / e2v(ji  ,jj  )   &
                    + ( zprn(ji+1,jj  ,jk) - zprn(ji+1,jj-1,jk) ) * vmask(ji+1,jj-1,jk) / e2v(ji+1,jj-1)   &
                    + ( zprn(ji  ,jj  ,jk) - zprn(ji  ,jj-1,jk) ) * vmask(ji  ,jj-1,jk) / e2v(ji  ,jj-1)
               zphu = 1. / rau0 * zphu * zmsu * umask(ji,jj,jk)

               ! Compute the geostrophic velocities
               un(ji,jj,jk) = -2. * zphu / ( ff(ji,jj) + ff(ji  ,jj-1) )
               vn(ji,jj,jk) =  2. * zphv / ( ff(ji,jj) + ff(ji-1,jj  ) )
            END DO
         END DO
      END DO

      IF(lwp) WRITE(numout,*) '         we force to zero bottom velocity'

      ! Susbtract the bottom velocity (level jpk-1 for flat bottom case)
      ! to have a zero bottom velocity

      DO jk = 1, jpkm1
         un(:,:,jk) = ( un(:,:,jk) - un(:,:,jpkm1) ) * umask(:,:,jk)
         vn(:,:,jk) = ( vn(:,:,jk) - vn(:,:,jpkm1) ) * vmask(:,:,jk)
      END DO

      CALL lbc_lnk( un, 'U', -1. )
      CALL lbc_lnk( vn, 'V', -1. )
      
      ub(:,:,:) = un(:,:,:)
      vb(:,:,:) = vn(:,:,:)
      
      ! WARNING !!!!!
      ! after initializing u and v, we need to calculate the initial streamfunction bsf.
      ! Otherwise, only the trend will be computed and the model will blow up (inconsistency).
      ! to do that, we call dyn_spg with a special trick:
      ! we fill ua and va with the velocities divided by dt, and the streamfunction will be brought to the
      ! right value assuming the velocities have been set up in one time step.
      ! we then set bsfd to zero (first guess for next step is d(psi)/dt = 0.)
      !  sets up s false trend to calculate the barotropic streamfunction.

      ua(:,:,:) = ub(:,:,:) / rdt
      va(:,:,:) = vb(:,:,:) / rdt

      ! calls dyn_spg. we assume euler time step, starting from rest.
      indic = 0
      CALL dyn_spg( nit000, indic )       ! surface pressure gradient

      ! the new velocity is ua*rdt

      CALL lbc_lnk( ua, 'U', -1. )
      CALL lbc_lnk( va, 'V', -1. )

      ub(:,:,:) = ua(:,:,:) * rdt
      vb(:,:,:) = va(:,:,:) * rdt
      ua(:,:,:) = 0.e0
      va(:,:,:) = 0.e0
      un(:,:,:) = ub(:,:,:)
      vn(:,:,:) = vb(:,:,:)
       
      ! Compute the divergence and curl

      CALL div_cur( nit000 )            ! now horizontal divergence and curl

      hdivb(:,:,:) = hdivn(:,:,:)       ! set the before to the now value
      rotb (:,:,:) = rotn (:,:,:)       ! set the before to the now value
      !
      CALL wrk_dealloc( jpi, jpj, jpk, zprn)
      !
   END SUBROUTINE istate_uvg

   !!=====================================================================
END MODULE istate
