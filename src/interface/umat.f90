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
!  @file     sanisand.f90
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
  use share_vars
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
  real(DP), dimension(3, 3) :: deplsn, fabric
  real(DP), dimension(3, 3) :: sigma, dsigetr
  real(DP) :: voidr, voidr_upd, harden
  real(DP) :: ftol_etr
  real(DP) :: pmeini, mean_etr
  real(DP), dimension(3, 3, 3, 3) :: stiffness
  type(Torch) :: torch_
  type(Elast) :: elast_
  type(Share_var) :: Shvars, Shvars_etr, Shvars_upd
  LOGICAL :: FIRSTRUN = .true.
  INTEGER :: TEMPVAR
  INTEGER, SAVE :: NUMBER = 0 ! static variable
  !------------------------
  call abaqus_debug(1, 1, NUMBER, noel, npt, 0, "Umat")
  if(noel == 1 .and. npt == 1) NUMBER = NUMBER + 1
  sigma(:, :) = -convert_array_to_tensor(stress)
  deplsn(:, :) = -convert_array_to_tensor(dstran, 2.0_DP)
  voidr = statev(1)
  harden = statev(2)
  fabric(:, :) = -convert_array_to_tensor(statev(3:8))
  fabric(:, :) = matmul(drot, matmul(fabric, transpose(drot)))
  Shvars = Share_var(harden, sigma, fabric)
  ! initial mean stress
  pmeini = torch_%Trace(sigma) / 3.0_DP
  !

  if(pmeini < tensno) then

  elseif(pmeini > tensno) then
    stiffness = elast_%get_stiffness(Shvars, voidr)
    dsigetr = stiffness.ddot.deplsn
    ! calculate the elastic trial stress
    Shvars_etr = Shvars
    call Shvars_etr%update_sigma(dsigetr)
    ftol_etr = elast_%Yield_distance(Shvars_etr)
    mean_etr = torch_%Trace(Shvars_etr%get_sigma()) / 3.0_dp
    if(ftol_etr <= EPS .and. mean_etr >= tensno) then
      ! elastic updated
      Shvars_upd = Shvars_etr
      voidr_upd = elast_%Update_voidr(voidr, deplsn)
      !
    elseif(ftol_etr > EPS .or. mean_etr < tensno) then
      !
    endif
  endif
  ! update state variables
  stress(:) = -Convert_tensor_to_array(Shvars_upd%get_sigma(), ntens)
  statev(1) = voidr_upd
  statev(2) = Shvars_upd%get_harden()
  statev(3:8) = Convert_tensor_to_array(Shvars_upd%get_fabric(), 6)
  ! storage mean S
  statev(9) = torch_%Trace(Shvars_upd%get_sigma()) / 3.0_DP
  ! statev(10) =
endsubroutine
