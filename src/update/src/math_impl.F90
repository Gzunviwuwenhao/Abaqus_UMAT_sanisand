!*******************************************************************************
!> @brief math_impl
!>
!> @details 模块详细描述
!>
!> @author wuwenhao
!> @date 2025/11/27
!*******************************************************************************
submodule(math_mod) math_impl
  use exception_mod
  use tensor_opt_mod
  use elastic_mod
  use plastic_mod
#include "macro.h"
  implicit none
  type(Elast) elast_
  type(Plast) plast_
  type(Math) math_
  !
contains
  module procedure Bisection_impl
  type(Share_var) :: shvars0, shvars1, shvarsm
  real(DP) :: fleft, fright, fmid
  real(DP) :: mid, left, right, precision
  real(DP), dimension(3, 3, 3, 3) :: stiff
  real(DP), dimension(3, 3) :: dsigma, dsige0, dsige1, dsigem
  real(DP), dimension(3, 3) :: sigmae
  integer :: iter, iter_max
  !-----------------------------------------------------------------------------
  left = alpha0
  right = alpha1
  ! initial vars
  iter_max = 100
  precision = 1.0D-8
  shvars0 = shvars
  shvars1 = shvars
  shvarsm = shvars
  ! check
  CHECK_TRUE(left >= 0.0_DP .and. right <= 1.0_DP, "left and right should be in [0,1]")
  CHECK_TRUE(left < right, "The interval can not be emptied.")
  !
  stiff = elast_%Get_stiffness(shvars, voidr)
  dsigma = stiff.ddot.depsln
  dsige0 = left * dsigma
  dsige1 = right * dsigma
  ! update
  call shvars0%update_sigma(dsige0)
  call shvars1%update_sigma(dsige1)
  ! calculate ftol
  fleft = elast_%Yield_distance(shvars0)
  fright = elast_%Yield_distance(shvars1)
  CHECK_TRUE(fleft * fright < 0.0_DP, "ValueError:The interval [ a, b ]")
  !
  if(abs(fright) <= EPS) then
    alout = right
    return
  endif
  if(abs(fleft) <= EPS) then
    alout = left
    return
  endif
  !
  do iter = 1, iter_max
    mid = left + (right - left) / 2.0_DP
    dsigem = mid * dsigma
    sigmae(:, :) = shvars%get_sigma() + dsigem(:, :)
    call shvarsm%changed_sigma(sigmae)
    fmid = elast_%Yield_distance(shvarsm)
    ! 检查是否收敛
    if(abs(fmid) <= precision .or. (right - left) <= precision) then
      alout = mid
      return
    endif
    if(fleft * fmid < 0.0_DP) then
      right = mid
      fright = fmid
    else
      left = mid
      fleft = fmid
    endif
  enddo
  end procedure Bisection_impl
  !
  module procedure left_bound_impl
  end procedure left_bound_impl
  module procedure right_bound_impl
  end procedure right_bound_impl
endsubmodule math_impl
