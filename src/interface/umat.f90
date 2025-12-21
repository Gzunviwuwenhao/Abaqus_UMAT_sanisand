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
!  @brief    简要说明
!  @details  详细描述
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
!  statev(9) = mean : mean pressure
!  statev(10)=
!-------------------------------------------------------------------------------
!  Change History :
!  <Date>     | <Version> | <Author>       | <Description>
!  2025/11/17 | 1.0.0.1   | wuwenhao      | Create file
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
  type(Torch) :: torch_
  type(Elast) :: elast_
  type(Math) :: math_
  real(DP), dimension(3, 3) :: deplsn, deplsn_ela, res_depsln
  real(DP), dimension(3, 3) :: dsigma
  type(Share_var) :: Shvars_ini, Shvars_ela, Shvars_final
  type(State_var) :: state_ini, stvar_ela, stvar_final
  real(DP) :: voidr_ini, harden_ini, sigma_ini(3, 3), fabric_ini(3, 3)
  real(DP) :: mean_etr, mean_final, ftol_etr, alout, rbd
  real(DP), dimension(3, 3, 3, 3) :: dsigde, dsdeyl, dsdetl
  INTEGER, SAVE :: NUMBER = 0 ! static variable
  !-----------------------------------------------------------------------------
  if(noel == 1 .and. npt == 1) then
    write(6, *) '=============================================================='
    write(6, *) 'noel = ', noel, ' npt = ', npt, ' number = ', NUMBER
    write(6, *) '=============================================================='
    NUMBER = NUMBER + 1
  endif

  sigma_ini(:, :) = -convert_array_to_tensor(stress)
  harden_ini = statev(2)
  fabric_ini(:, :) = convert_array_to_tensor(statev(3:8))
  fabric_ini(:, :) = matmul(drot, matmul(fabric_ini, transpose(drot)))
  ! create Share_var container
  Shvars_ini = Share_var(harden_ini, sigma_ini, fabric_ini)
  !
  deplsn(:, :) = -convert_array_to_tensor(dstran, 2.0_DP)
  voidr_ini = statev(1)
  state_ini = State_var(voidr_ini, pnewdt)
  !
  if(Shvars_ini%is_low()) then
    ! too low

  else
    mean_etr = math_%mean_with_depsln(Shvars_ini, state_ini, deplsn)
    ftol_etr = math_%ftol_with_depsln(Shvars_ini, state_ini, deplsn)
    if(ftol_etr <= EPS .and. mean_etr >= tensno) then
      ! elastic updated
      dsigma(:, :) = elast_%calc_dsigma(Shvars_ini, state_ini, deplsn)
      Shvars_final = Share_var(harden_ini, sigma_ini + dsigma, fabric_ini)
      !
      stvar_final = state_ini
      call stvar_final%update_voidr(deplsn)
      !
      dsdetl = elast_%Get_stiffness(Shvars_ini, state_ini)
      !
    elseif(ftol_etr > EPS .or. (mean_etr < tensno .and. ftol_etr < EPS)) then
      !     !
      call abaqus_debug(1, 1, NUMBER, noel, npt, 0, "Umat")
      call math_%Intchc(Shvars_ini, state_ini, deplsn, rbd, alout)
      if(rbd < 1.0_DP) then
      endif
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
      call math_%Onyield(Shvars_ela, stvar_ela, res_depsln, Shvars_final, stvar_final, dsdeyl)
      dsdetl = alout * dsigde + (1.0_DP - alout * dsdeyl)
    endif
  endif
  ! update ddsdde
  ddsdde = Convert_tensor4_to_tensor2(dsdetl, ntens)
  ! update state variables
  stress(:) = -Convert_tensor_to_array(Shvars_final%get_sigma(), ntens)
  statev(1) = stvar_final%get_voidr()
  statev(2) = Shvars_final%get_harden()
  statev(3:8) = Convert_tensor_to_array(Shvars_final%get_fabric(), 6)
  ! storage mean
  mean_final = torch_%Trace(Shvars_final%get_sigma()) / 3.0_DP
  statev(9) = mean_final
  ! statev(10)
  if(noel == 1 .and. npt == 1) then
    call Shvars_final%print()
    write(6, *) "mean = ", mean_final
  endif
endsubroutine
