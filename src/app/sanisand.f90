!DIR$ FREEFORM
!*****************************************************************************
!  Project Name
!  Copyright (C) 2025 wuwenhao 617082766@qq.com
!
!  This file is part of Project Name.
!
!  This program is free software; you can redistribute it and/or modify
!  it under the terms of the GNU General Public License version 3 as
!  published by the Free Software Foundation.
!
!  You should have received a copy of the GNU General Public License
!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!
!  @file     sanisand.f90
!  @brief    简要说明
!  @details  详细描述
!
!  @author   wuwenhao
!  @email    617082766@qq.com
!  @version  1.0.0.1
!  @date     2025/11/17
!  @license  MIT General Public License (MIT)
!---------------------------------------------------------------------------*
!  Remark         : A state variable array of size NSTATV to be
!  updated by the UMAT,which includes following variables:
!  void_ratio = statev(1) : current void ratio
!  harden     = statev(2) : current hardening parameter
!  Fabric_[1] = statev(3) : fabric evlution F11
!  Fabric_[2] = statev(4) : fabric evlution F22
!  Fabric_[3] = statev(5) : fabric evlution F33
!  Fabric_[4] = statev(6) : fabric evlution F12,F21
!  Fabric_[5] = statev(7) : fabric evlution F13,F31
!  Fabric_[6] = statev(8) : fabric evlution F23,F32
!---------------------------------------------------------------------------*
!  Change History :
!  <Date>     | <Version> | <Author>       | <Description>
!  2025/11/17 | 1.0.0.1   | wuwenhao      | Create file
!*****************************************************************************
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
  real(data_t), dimension(3, 3) :: deplsn, fabric
  real(dp), dimension(3, 3) :: sigma, dsigetr, sigetr, sig_upd
  real(data_t) :: voidr, voidr_upd,harden
  real(dp) :: ftoltr
  real(data_t) :: pmeini, meanetr
  real(dp), dimension(3, 3, 3, 3) :: stiffness
  type(Torch) :: opt_
  type(elast) :: elast_
  !------------------------
  sigma(:, :) = convert_array_to_tensor(stress)
  deplsn(:, :) = convert_array_to_tensor(dstran, two)
  voidr = statev(1)
  harden = statev(2)
  fabric(:, :) = -convert_array_to_tensor(statev(3:8))
  fabric(:, :) = matmul(drot, matmul(fabric, transpose(drot)))
  ! initial mean stress
  pmeini = opt_%Trace(sigma) / three
  !
  if(pmeini < tensno) then

  elseif(pmeini > tensno) then
    stiffness = elast_%get_stiffness(sigma, voidr)
    dsigetr(:, :) = stiffness(:, :, :, :) .ddot.deplsn(:, :)
    ! calculate the elastic trial stress
    sigetr(:, :) = sigma(:, :) + dsigetr(:, :)
    ftoltr = elast_%isyield(sigetr, harden)
    meanetr = opt_%Trace(sigetr) / 3.0_dp
    if(ftoltr <= eps .and. meanetr >= tensno) then
      ! elastic updated
      sig_upd(:, :) = sigetr(:, :)
      voidr_upd = elast_%update_voidr(voidr, deplsn)
    elseif(ftoltr > eps .or.  meanetr < tensno)then
    end if
  endif
endsubroutine
