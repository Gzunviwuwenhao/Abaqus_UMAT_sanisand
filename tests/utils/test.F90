
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
!  @file     test.f90
!  @brief    简要说明
!  @details  详细描述
!
!  @author   wuwenhao
!  @email    617082766@qq.com
!  @version  1.0.0.1
!  @date     2025/11/27
!  @license  GNU General Public License (GPL)
!---------------------------------------------------------------------------*
!  Remark         : 说明备注
!---------------------------------------------------------------------------*
!  Change History :
!  <Date>     | <Version> | <Author>       | <Description>
!  2025/11/27 | 1.0.0.1   | wuwenhao      | Create file
!*****************************************************************************
program test
  ! Preprocessor macros
  ! use modules
  use base_config, only: data_t, delta,sp
  use tensor_opt_mod
  use presolve_mod
  use exception_mod
#include "macro.h"
  ! declaration variable
  implicit none
  type(Torch) :: opt_
  real(data_t), dimension(3, 3) :: stress, S_ref, S, R_ref, R, temp
  real(data_t) :: array1(3), array2(4), array3(6)
  real(data_t), dimension(3, 3) :: tensor1, tensor2, tensor3
  real(data_t) :: mean, p, J2, J3, J2_ref, J3_ref, sin3t, sin3t_ref
  integer :: i, j
  ! random_number
  integer :: seed_size
  integer, allocatable :: seed(:)
  ! test context
  call random_seed(size=seed_size)
  allocate(seed(seed_size))
  seed = 1234
  call random_seed(put=seed)
  ! 生成随机矩阵
  call random_number(stress)
  call random_number(array1)
  call random_number(array2)
  call random_number(array3)
  array1 = -array1 * 100
  array2 = -array2 * 100
  array3 = -array3 * 100
  stress = stress * 100
  call opt_%Print(stress)
  ! 矩阵的迹
  mean = opt_%Trace(stress)
  p = (stress(1, 1) + stress(2, 2) + stress(3, 3)) / 3.0d0
  CHECK_FLOAT_EQUAL(mean / 3.d0, p, " trace vertify")
  ! 验证偏应力
  S_ref = stress - p * delta
  s = opt_%Deviatoric(stress)
  do i = 1, 3
    do j = 1, 3
      CHECK_FLOAT_EQUAL(S(i, j), S_ref(i, j), " trace vertify")
    enddo
  enddo
  ! 验证应力比
  R_ref = S(:, :) / p
  R = opt_%Ratio(stress)
  do i = 1, 3
    do j = 1, 3
      CHECK_FLOAT_EQUAL(R(i, j), R_ref(i, j), " trace vertify")
    enddo
  enddo
  ! 验证j2, J3
  J2 = opt_%Get_J2(stress)
  J2_ref = sum(s**2) / 2.0d0
  CHECK_FLOAT_EQUAL(J2, J2_ref, " J2 vertify")
  temp = matmul(S, matmul(S, S))
  J3_ref = sum([(temp(i, i), i=1, 3)]) / 3.0d0
  J3 = opt_%Get_J3(stress)
  CHECK_FLOAT_EQUAL(J3, J3_ref, " J3 vertify")
  ! sin3t
  sin3t = opt_%Sin3theta(stress)
  sin3t_ref = -1.5D0 * DSQRT(3.0D0) * J3_ref / (J2_ref**1.5D0)
  CHECK_FLOAT_EQUAL(sin3t, sin3t_ref, " sin3t vertify")
  !
  tensor1 = convert_array_to_tensor(array1)
  tensor2 = convert_array_to_tensor(array2,2.0d0)
  tensor3 = convert_array_to_tensor(array3,2.0d0)
  call print_array(array1)
  call opt_%Print(tensor1)

endprogram test
