!DIR$ FREEFORM
!*****************************************************************************
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
!  @file     sdvini.F90
!  @brief    简要说明
!  @details  详细描述
!
!  @author   wuwenhao
!  @email    617082766@qq.com
!  @version  1.0.0.1
!  @date     2025/12/08
!  @license  MIT Massachusetts Institute of Technology (MIT)
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
!  2025/12/08 | 1.0.0.1   | wuwenhao      | Create file
!*****************************************************************************
SUBROUTINE Sdvini(statev, coords, nstatv, ncrds, noel, npt, layer, kspt)
  use Base_config
  use presolve_mod
  use Material_config
  use exception_mod
#include "macro.h"
  IMPLICIT NONE
  integer, intent(in) :: nstatv, ncrds, noel, npt, layer, kspt
  real(DP), intent(in) :: coords(ncrds)
  real(DP), intent(inout) :: statev(nstatv)
  real(DP) :: void_ini, harden_ini, fabric_ini(3, 3), fabric_rot(3, 3)
  real(DP) :: rot_matrix(3, 3), angle, axis(3), temp
  !
  ! CALL abaqus_debug(1, 1, 0, noel, npt, 0, "Sdvini")
  ! check nstatv
  CHECK_TRUE(nstatv >= 8, "Sdvini: nstatv is less than 8.")
  ! Initialize state variables
  void_ini = 0.6_DP  ! Initial void ratio
  harden_ini = 0.01_DP  ! Initial hardening parameter
  !
  angle = 0.0_DP * PI / 180_DP
  axis = [0.0_DP, 0.0_DP, 1.0_DP]
  rot_matrix(:, :) = Get_rotation_matrix(angle, axis)
  fabric_ini(:, :) = 0.0_DP
  temp = dsqrt(2.0_DP / 3.0_DP) * PARAM%F0
  fabric_ini(:, :) = reshape([ &
                             temp, 0.0_DP, 0.0_DP, &
                             0.0_DP, -temp / 2.0_DP, 0.0_DP, &
                             0.0_DP, 0.0_DP, -temp / 2.0_DP],[3, 3])
  fabric_rot(:, :) = matmul(rot_matrix, matmul(fabric_ini, transpose(rot_matrix)))
  ! void ratio initial
  statev(1) = void_ini
  ! harden initial
  statev(2) = harden_ini
  ! fabric initial
  statev(3:8) = Convert_tensor_to_array(fabric_rot, 6)
  ! initialize other state variables to zero
  if(nstatv > 8) statev(9:nstatv) = 0.0_DP
  RETURN
ENDSUBROUTINE Sdvini
