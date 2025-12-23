!DIR$ FREEFORM
!*******************************************************************************
!  Project Name
!  Copyright (C) 2025 wuwenhao 617082766@qq.com
!
!  This file is part of Project Name.
!
!  This program is free software; you can redistribute it and/or modify
!  it under the terms of the MIT License version 3 as
!  published by the Free Software Foundation.
!
!  You should have received a copy of the MIT License
!  along with this program. If not, see <https://mit-license.org/>.
!
!  @file     umat.f90
!  @brief    ABAQUS UMAT subroutine for SANISAND constitutive model
!  @details  This file implements the user material subroutine (UMAT) for the
!            SANISAND constitutive model in ABAQUS. The UMAT calculates stress
!            updates and consistent tangent moduli for soil materials under
!            various loading conditions. It incorporates elastic predictor -
!            plastic corrector algorithms, fabric evolution, and state-dependent
!            hardening.
!
!  @author   wuwenhao
!  @email    617082766@qq.com
!  @version  1.0.0.1
!  @date     2025/11/17
!  @license  MIT Massachusetts Institute of Technology (MIT)
!-------------------------------------------------------------------------------
!  Remark         : A state variable array of size NSTATV to be
!  updated by the UMAT,which includes following variables:
!  statev(1) = void_ratio : current void ratio
!  statev(2) = harden : current hardening parameter
!  statev(3) = Fabric_[1] : fabric evlution F11
!  statev(4) = Fabric_[2] : fabric evlution F22
!  statev(5) = Fabric_[3] : fabric evlution F33
!  statev(6) = Fabric_[4] : fabric evlution F12,F21
!  statev(7) = Fabric_[5] : fabric evlution F13,F31
!  statev(8) = Fabric_[6] : fabric evlution F23,F32
!  statev(9) = confining pressure
!  statev(10)= shear stress
!  statev(11)= ratio stress
!  statev(12)= the total of volumetric strain
!  statev(13)= the increment of shear strain
!  statev(14)= ANISOTROPIC VARLABLE
!  statev(15)= the norm of fabric tensor
!  statev(16)= Dkp
!  statev(17)= dilatancy
!  statev(18)= axial stress
!-------------------------------------------------------------------------------
!  Change History :
!  <Date>     | <Version> | <Author>       | <Description>
!  2025/11/17 | 1.0.0.1   | wuwenhao      | Create file
!*******************************************************************************
!> @brief ABAQUS UMAT subroutine for SANISAND constitutive model
!>
!> @details This subroutine implements the user material subroutine (UMAT) for
!>          the SANISAND constitutive model in ABAQUS. It calculates stress
!>          updates and consistent tangent moduli for soil materials under
!>          various loading conditions. The algorithm incorporates elastic
!>          predictor - plastic corrector methodology, fabric evolution, and
!>          state-dependent hardening. The subroutine handles both elastic and
!>          plastic loading paths, including return mapping to the yield surface.
!>
!> @param[in,out] stress    Cauchy stress tensor (ntens)
!> @param[in,out] statev    State variable array (nstatv)
!> @param[out]    ddsdde    Jacobian matrix (ntens x ntens)
!> @param[out]    sse       Specific elastic strain energy
!> @param[out]    spd       Specific plastic dissipation
!> @param[out]    scd       Specific creep dissipation
!> @param[out]    rpl       Volumetric heat generation per unit time
!> @param[out]    ddsddt    Stress variation with temperature (ntens)
!> @param[out]    drplde    Variation of rpl with strain increments (ntens)
!> @param[out]    drpldt    Variation of rpl with temperature
!> @param[in]     stran     Total strains at beginning of increment (ntens)
!> @param[in]     dstran    Strain increments (ntens)
!> @param[in]     time      Time array: time(1) = step time, time(2) = total time
!> @param[in]     dtime     Time increment
!> @param[in]     temp      Temperature at beginning of increment
!> @param[in]     dtemp     Temperature increment
!> @param[in]     predef    Predefined field variables array
!> @param[in]     dpred     Increments of predefined field variables
!> @param[in]     cmname    User-defined material name (CHARACTER*80)
!> @param[in]     ndi       Number of direct stress components
!> @param[in]     nshr      Number of engineering shear stress components
!> @param[in]     ntens     Size of stress/strain array (ndi + nshr)
!> @param[in]     nstatv    Number of state variables
!> @param[in]     props     Material properties array (nprops)
!> @param[in]     nprops    Number of material properties
!> @param[in]     coords    Spatial coordinates of integration point
!> @param[in]     drot      Rotation increment matrix (3x3)
!> @param[in,out] pnewdt    Ratio of suggested new time increment
!> @param[in]     celent    Characteristic element length
!> @param[in]     dfgrd0    Deformation gradient at beginning of increment (3x3)
!> @param[in]     dfgrd1    Deformation gradient at end of increment (3x3)
!> @param[in]     noel      Element number
!> @param[in]     npt       Integration point number
!> @param[in]     layer     Layer number (for composite shells and layered solids)
!> @param[in]     kspt      Section point number within the current layer
!> @param[in]     kstep     Step number
!> @param[in]     kinc      Increment number
!>
!> @author wuwenhao
!> @date 2025/11/17
!*******************************************************************************
subroutine Umat(stress, statev, ddsdde, sse, spd, scd, &
                rpl, ddsddt, drplde, drpldt, &
                stran, dstran, time, dtime, temp, dtemp, predef, dpred, &
                cmname, ndi, nshr, ntens, nstatv, props, nprops, &
                coords, drot, pnewdt, celent, dfgrd0, dfgrd1, &
                noel, npt, layer, kspt, kstep, kinc)
  use Base_config
  use presolve_mod
  use tensor_opt_mod
  use elastic_mod
  use Container_mod
  use plastic_mod
  use math_mod
  ! Variable declarations
  implicit none
  !
  character * 80 cmname
  ! Input/Output variables
  integer :: ndi, nshr, ntens, nstatv, nprops, noel, npt, &
             layer, kspt, kstep, kinc
  real(dp) :: stress(ntens), statev(nstatv), &
              ddsdde(ntens, ntens), ddsddt(ntens), drplde(ntens), &
              stran(ntens), dstran(ntens), time(2), props(nprops), &
              coords(3), drot(3, 3), dfgrd0(3, 3), dfgrd1(3, 3), &
              sse, spd, scd, rpl, drpldt, dtime, temp, dtemp, &
              predef, dpred, celent, pnewdt
  !-----------------------------------------------------------------------------
  type(Torch) :: torch_
  type(Elast) :: elast_
  type(Plast) :: plast_
  type(Math) :: math_
  real(DP), dimension(3, 3) :: deplsn, deplsn_ela, res_depsln
  real(DP), dimension(3, 3) :: dsigma, sigma_final
  type(Share_var) :: Shvars_ini, Shvars_ela, Shvars_final
  type(State_var) :: state_ini, stvar_ela, stvar_final
  real(DP) :: voidr_ini, harden_ini, sigma_ini(3, 3), fabric_ini(3, 3)
  real(DP) :: mean_etr, ftol_etr, alout, ftolr
  real(DP), dimension(3, 3, 3, 3) :: dsigde, dsdeyl, dsdetl
  !
  real(DP) :: mean_final, shear, ratio_stress, dEv, dEq, ANIV, Fmag
  real(DP) :: Dkp, dpla
  INTEGER, SAVE :: NUMBER = 0 ! static variable
  !-----------------------------------------------------------------------------
  ! initialize variable
  ftolr = 1.0D-6
  !
  if(noel == 1 .and. npt == 1) then
    write(6, *) '=============================================================='
    write(6, *) 'noel = ', noel, ' npt = ', npt, ' number = ', NUMBER
    write(6, *) '=============================================================='
    ! call abaqus_debug(1, 1, NUMBER, noel, npt, 4, "Umat")
    NUMBER = NUMBER + 1
  endif
  sigma_ini = -convert_array_to_tensor(stress, 1.0_DP)
  harden_ini = statev(2)
  fabric_ini = convert_array_to_tensor(statev(3:8))
  fabric_ini = matmul(drot, matmul(fabric_ini, transpose(drot)))
  ! create Share_var container
  Shvars_ini = Share_var(harden_ini, sigma_ini, fabric_ini)
  !
  deplsn = -convert_array_to_tensor(dstran, 2.0_DP)
  voidr_ini = statev(1)
  state_ini = State_var(voidr_ini, pnewdt)
  !
  if(Shvars_ini%is_low()) then
    ! too low

  else
    mean_etr = math_%mean_with_depsln(Shvars_ini, state_ini, deplsn)
    ftol_etr = math_%ftol_with_depsln(Shvars_ini, state_ini, deplsn)
    if(ftol_etr <= ftolr .and. mean_etr >= tensno) then
      ! elastic updated
      dsigma(:, :) = elast_%calc_dsigma(Shvars_ini, state_ini, deplsn)
      Shvars_final = Share_var(harden_ini, sigma_ini + dsigma, fabric_ini)
      !
      stvar_final = state_ini
      call stvar_final%update_voidr(deplsn)
      !
      dsdetl = elast_%Get_stiffness(Shvars_ini, state_ini)
      !
    elseif(ftol_etr > ftolr .or. (mean_etr < tensno .and. ftol_etr < ftolr)) then
      !     !
      ! call abaqus_debug(1, 1, NUMBER, noel, npt, 0, "Umat")
      alout = math_%Intchc(Shvars_ini, state_ini, deplsn, ftolr)
      ! update variables
      deplsn_ela(:, :) = alout * deplsn(:, :)
      res_depsln = (1.0_DP - alout) * deplsn
      stvar_ela = state_ini
      call stvar_ela%update_voidr(deplsn_ela)
      dsigma(:, :) = elast_%calc_dsigma(Shvars_ini, state_ini, deplsn_ela)
      Shvars_ela = Shvars_ini
      call Shvars_ela%update_sigma(dsigma)
      dsigde(:, :, :, :) = elast_%Get_stiffness(Shvars_ini, state_ini)
      !
      call math_%Onyield(Shvars_ela, stvar_ela, res_depsln, ftolr, Shvars_final, stvar_final, dsdeyl)
      dsdetl = alout * dsigde + (1.0_DP - alout) * dsdeyl
    endif
  endif
  !-----------------------------------------------------------------------------
  !> update variable
  ! update ddsdde
  ddsdde = Convert_tensor4_to_tensor2(dsdetl, ntens)
  ! update state variables
  stress(:) = -Convert_tensor_to_array(Shvars_final%get_sigma(), ntens)
  statev(1) = stvar_final%get_voidr()
  statev(2) = Shvars_final%get_harden()
  statev(3:8) = Convert_tensor_to_array(Shvars_final%get_fabric(), 6)
  !
  pnewdt = stvar_final%get_pnewdt()
  ! confining pressure
  mean_final = torch_%Trace(Shvars_final%get_sigma()) / 3.0_DP
  statev(9) = mean_final
  ! shear_stress
  shear = torch_%Shear(Shvars_final%get_sigma())
  statev(10) = shear
  ! ratio stress
  ratio_stress = torch_%Get_Rm(Shvars_final%get_sigma())
  statev(11) = ratio_stress
  ! the total of volumetric strain
  dEv = torch_%Trace(deplsn)
  statev(12) = statev(12) + dEv * 100
  ! the increment of shear strain
  dEq = torch_%Shear(deplsn)
  statev(13) = statev(13) + dEq * 100
  ! ANISOTROPIC VARLABLE
  ANIV = elast_%Get_anisotropy(Shvars_final)
  statev(14) = ANIV
  ! the norm of fabric tensor
  Fmag = torch_%Norm(Shvars_final%get_fabric())
  statev(15) = Fmag
  ! Dkp
  Dkp = plast_%Get_Dkp(Shvars_final, stvar_final)
  statev(16) = Dkp
  ! dilatancy
  dpla = plast_%Get_dilatancy(Shvars_final, stvar_final)
  statev(17) = dpla
  ! axial stress
  sigma_final = Shvars_final%get_sigma()
  statev(18) = sigma_final(1, 1)
  ! print
  if(noel == 1 .and. npt == 1) then
    call Shvars_final%print()
11  format(A14, F6.2, 2x, A21, F6.2, /, &
           A15, F6.2, /, &
           A14, F6.2, 2x, A14, F6.2, /, &
           A20, F6.2, 2x, A16, F6.2, /, &
           A6, E15.6, 2x, A13, F10.3)
    write(6, 11) "mean stress = ", mean_final, ' deviatoric stress = ', shear, &
      "ratio stress = ", ratio_stress, &
      "fabric norm = ", Fmag, " anisotropy = ", ANIV, &
      "volumetric strain = ", statev(12), " shear strain = ", statev(13), &
      "Dkp = ", Dkp, " dilatancy = ", dpla
  endif
  !
endsubroutine
