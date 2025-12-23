!*******************************************************************************
!> @brief share_vars_impl
!>
!> @details 模块详细描述
!>
!> @author wuwenhao
!> @date 2025/11/27
!*******************************************************************************
submodule(Container_mod) Container_impl
  use Base_config
  use tensor_opt_mod
  use exception_mod
#include "macro.h"
  implicit none
  type(Torch) torch_
contains
  !=============================================================================
  !
  ! interface Share_var
  !
  !=============================================================================
  module procedure share_construct_param
  real(DP) :: mean
  !-----------------------------------------------------------------------------
  this%harden_ = harden
  this%sigma_(:, :) = sigma(:, :)
  this%fabric_(:, :) = fabric(:, :)
  !
  this%initialized_ = .true.
  !
  mean = torch_%Trace(this%sigma_) / 3.0_DP
  this%is_lowstress = merge(.true., .false., mean <= EPS)
  call this%jugde_nan_inf_impl()
  end procedure share_construct_param
  !*****************************************************************************
  module procedure Share_construct_zero
  this%harden_ = 0.0_DP
  this%sigma_(:, :) = 0.0_DP
  this%fabric_(:, :) = 0.0_DP
  this%initialized_ = .true.
  this%is_lowstress = .true.
  this%is_nan_inf = .false.
  end procedure Share_construct_zero
  !*****************************************************************************
  module procedure get_harden_impl
  CHECK_TRUE(this%initialized_, "container share_vars has not initialized")
  harden = this%harden_
  end procedure get_harden_impl
  !*****************************************************************************
  module procedure get_sigma_impl
  CHECK_TRUE(this%initialized_, "container share_vars has not initialized")
  sigma(:, :) = this%sigma_(:, :)
  end procedure get_sigma_impl
  !*****************************************************************************
  module procedure get_fabric_impl
  CHECK_TRUE(this%initialized_, "container share_vars has not initialized")
  fabric(:, :) = this%fabric_(:, :)
  end procedure get_fabric_impl
  !*****************************************************************************
  module procedure low_impl
  CHECK_TRUE(this%initialized_, "container share_vars has not initialized")
  is_true = this%is_lowstress
  end procedure low_impl
  !*****************************************************************************
  module procedure update_harden_impl
  CHECK_TRUE(this%initialized_, "container share_vars has not initialized")
  this%harden_ = this%harden_ + harden
  call this%jugde_nan_inf_impl()
  end procedure update_harden_impl
  !*****************************************************************************
  module procedure update_sigma_impl
  real(DP) :: mean
  !-----------------------------------------------------------------------------
  CHECK_TRUE(this%initialized_, "container share_vars has not initialized")
  this%sigma_(:, :) = this%sigma_(:, :) + dsigma(:, :)
  mean = torch_%Trace(this%sigma_) / 3.0_DP
  this%is_lowstress = merge(.true., .false., mean <= EPS)
  call this%jugde_nan_inf_impl()
  end procedure update_sigma_impl
  !*****************************************************************************
  module procedure update_fabric_impl
  CHECK_TRUE(this%initialized_, "container share_vars has not initialized")
  this%fabric_(:, :) = this%fabric_(:, :) + fabric(:, :)
  call this%jugde_nan_inf_impl()
  end procedure update_fabric_impl
  !*****************************************************************************
  module procedure update_shvars_impl
  real(DP) :: mean
  !-----------------------------------------------------------------------------
  CHECK_TRUE(this%initialized_, "container share_vars has not initialized")
  this%harden_ = this%harden_ + dharden
  this%sigma_(:, :) = this%sigma_(:, :) + dsigma(:, :)
  this%fabric_(:, :) = this%fabric_(:, :) + dfabric(:, :)
  !
  mean = torch_%Trace(this%sigma_) / 3.0_DP
  this%is_lowstress = merge(.true., .false., mean <= EPS)
  call this%jugde_nan_inf_impl()
  end procedure update_shvars_impl
  !*****************************************************************************
  module procedure changed_harden_impl
  CHECK_TRUE(this%initialized_, "container share_vars has not initialized")
  this%harden_ = harden
  call this%jugde_nan_inf_impl()
  end procedure changed_harden_impl
  !*****************************************************************************
  module procedure changed_sigma_impl
  real(DP) :: mean
  !-----------------------------------------------------------------------------
  CHECK_TRUE(this%initialized_, "container share_vars has not initialized")
  this%sigma_(:, :) = sigma(:, :)
  mean = torch_%Trace(this%sigma_) / 3.0_DP
  this%is_lowstress = merge(.true., .false., mean <= EPS)
  call this%jugde_nan_inf_impl()
  end procedure changed_sigma_impl
  !*****************************************************************************
  module procedure changed_fabric_impl
  CHECK_TRUE(this%initialized_, "container share_vars has not initialized")
  this%fabric_(:, :) = fabric(:, :)
  call this%jugde_nan_inf_impl()
  end procedure changed_fabric_impl
  !*****************************************************************************
  module procedure norm_impl
  res(1) = abs(this%get_harden())
  res(2) = torch_%Norm(this%get_sigma())
  res(3) = torch_%Norm(this%get_fabric())
  end procedure norm_impl
  !*****************************************************************************
  module procedure print_impl
  write(6, *) "Stress Tensor : "
  call torch_%Print(this%get_sigma())
  write(6, *) "Fabric Tensor : "
  call torch_%Print(this%fabric_)
  write(6, '(A12, ES13.6)') "Hardening = ", this%harden_
  end procedure print_impl
  !*****************************************************************************
  module procedure jugde_nan_inf_impl
  logical has_error
  has_error = (this%harden_ /= this%harden_) .or. (abs(this%harden_) > MAX_DATA)
  has_error = has_error .or. any(this%sigma_ /= this%sigma_) &
              .or. any(abs(this%sigma_) > MAX_DATA)
  has_error = has_error .or. any(this%fabric_ /= this%fabric_) &
              .or. any(abs(this%fabric_) > MAX_DATA)
  this%is_nan_inf = has_error
  end procedure jugde_nan_inf_impl
  !*****************************************************************************
  module procedure assign_impl
  CHECK_TRUE(other%initialized_, "container share_vars has not initialized")
  this%harden_ = other%harden_
  this%sigma_(:, :) = other%sigma_(:, :)
  this%fabric_(:, :) = other%fabric_(:, :)
  this%initialized_ = other%initialized_
  this%is_lowstress = other%is_lowstress
  this%is_nan_inf = other%is_nan_inf
  end procedure assign_impl
  !*****************************************************************************
  module procedure binary_add_impl
  real(DP) :: mean
  !-----------------------------------------------------------------------------
  CHECK_TRUE(this%initialized_, "container share_vars has not initialized")
  CHECK_TRUE(other%initialized_, "container share_vars has not initialized")
  res%harden_ = this%harden_ + other%harden_
  res%sigma_ = this%sigma_ + other%sigma_
  res%fabric_ = this%fabric_ + other%fabric_
  res%initialized_ = .true.
  mean = torch_%Trace(res%sigma_) / 3.0_DP
  res%is_lowstress = merge(.true., .false., mean <= EPS)
  call res%jugde_nan_inf_impl()
  end procedure binary_add_impl
  !*****************************************************************************
  module procedure binary_sub_impl
  real(DP) :: mean
  !-----------------------------------------------------------------------------
  CHECK_TRUE(this%initialized_, "container share_vars has not initialized")
  CHECK_TRUE(other%initialized_, "container share_vars has not initialized")
  res%harden_ = this%harden_ - other%harden_
  res%sigma_ = this%sigma_ - other%sigma_
  res%fabric_ = this%fabric_ - other%fabric_
  res%initialized_ = .true.
  mean = torch_%Trace(res%sigma_) / 3.0_DP
  res%is_lowstress = merge(.true., .false., mean <= EPS)
  call res%jugde_nan_inf_impl()
  end procedure binary_sub_impl
  !*****************************************************************************
  module procedure unary_minus_impl
  real(DP) :: mean
  !-----------------------------------------------------------------------------
  CHECK_TRUE(this%initialized_, "container share_vars has not initialized")
  res%harden_ = -this%harden_
  res%sigma_ = -this%sigma_
  res%fabric_ = -this%fabric_
  res%initialized_ = this%initialized_
  mean = torch_%Trace(res%sigma_) / 3.0_DP
  res%is_lowstress = merge(.true., .false., mean <= EPS)
  call res%jugde_nan_inf_impl()
  end procedure unary_minus_impl
  !*****************************************************************************
  module procedure unary_lhs_scalar_impl
  real(DP) :: mean
  !-----------------------------------------------------------------------------
  CHECK_TRUE(this%initialized_, "container share_vars has not initialized")
  res%harden_ = scalar * this%harden_
  res%sigma_ = scalar * this%sigma_
  res%fabric_ = scalar * this%fabric_
  res%initialized_ = .true.
  mean = torch_%Trace(res%sigma_) / 3.0_DP
  res%is_lowstress = merge(.true., .false., mean <= EPS)
  call res%jugde_nan_inf_impl()
  end procedure unary_lhs_scalar_impl
  !*****************************************************************************
  module procedure unary_rhs_scalar_impl
  real(DP) :: mean
  !-----------------------------------------------------------------------------
  CHECK_TRUE(this%initialized_, "container share_vars has not initialized")
  res%harden_ = this%harden_ * scalar
  res%sigma_ = this%sigma_ * scalar
  res%fabric_ = this%fabric_ * scalar
  res%initialized_ = .true.
  mean = torch_%Trace(res%sigma_) / 3.0_DP
  res%is_lowstress = merge(.true., .false., mean <= EPS)
  call res%jugde_nan_inf_impl()
  end procedure unary_rhs_scalar_impl
  !*****************************************************************************
  module procedure unary_div_impl
  real(DP) :: mean, scalar_
  !-----------------------------------------------------------------------------
  CHECK_TRUE(this%initialized_, "container share_vars has not initialized")
  scalar_ = scalar
  if(abs(scalar_) < EPS) scalar_ = sign(scalar_, EPS)
  res%harden_ = this%harden_ / scalar_
  res%sigma_ = this%sigma_ / scalar_
  res%fabric_ = this%fabric_ / scalar_
  res%initialized_ = .true.
  mean = torch_%Trace(res%sigma_) / 3.0_DP
  res%is_lowstress = merge(.true., .false., mean <= EPS)
  call res%jugde_nan_inf_impl()
  end procedure unary_div_impl
  !
  !=============================================================================
  !
  ! interface State_var
  !
  !=============================================================================
  !
  module procedure State_construct_param
  this%voidr_ = voidr
  this%pnewdt_ = pnewdt
  this%initialized_ = .true.
  end procedure State_construct_param
  !*****************************************************************************
  module procedure get_voidr_impl
  CHECK_TRUE(this%initialized_, "container state_vars has not initialized")
  voidr = this%voidr_
  end procedure get_voidr_impl
  !*****************************************************************************
  module procedure get_pnewdt_impl
  CHECK_TRUE(this%initialized_, "container state_vars has not initialized")
  pnewdt = this%pnewdt_
  end procedure get_pnewdt_impl
  !*****************************************************************************
  module procedure update_voidr_impl
  real(DP) :: despv
  !-----------------------------------------------------------------------------
  CHECK_TRUE(this%initialized_, "container state_vars has not initialized")
  despv = torch_%Trace(depsln)
  this%voidr_ = this%voidr_ - (1.0_DP + this%voidr_) * despv
  end procedure update_voidr_impl
  !*****************************************************************************
  module procedure changed_voidr_impl
  CHECK_TRUE(this%initialized_, "container state_vars has not initialized")
  this%voidr_ = voidr
  end procedure changed_voidr_impl
  !*****************************************************************************
  module procedure changed_pnewdt_impl
  CHECK_TRUE(this%initialized_, "container state_vars has not initialized")
  this%pnewdt_ = pnewdt
  end procedure changed_pnewdt_impl
  !*****************************************************************************
  module procedure assign_impl_state
  CHECK_TRUE(other%initialized_, "container state_vars has not initialized")
  this%voidr_ = other%voidr_
  this%pnewdt_ = other%pnewdt_
  this%initialized_ = other%initialized_
  end procedure assign_impl_state
endsubmodule
