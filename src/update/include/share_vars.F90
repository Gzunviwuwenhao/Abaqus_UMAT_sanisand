!*****************************************************************************
!> @brief share_vars
!>
!> @details 模块详细描述
!>
!> @author wuwenhao
!> @date 2025/12/07
!*****************************************************************************
module share_vars
  use Base_config, only: DP
  implicit none
  private
  ! Type definitions
  type,public :: Share_var
    real(DP) :: harden_
    real(DP), dimension(3, 3) :: sigma_
    real(DP), dimension(3, 3) :: fabric_
  endtype Share_var
  interface Share_var
    module procedure :: construct_with_param
  endinterface
contains
  function construct_with_param(harden, sigma, fabric) result(this)
    real(DP) :: harden
    real(DP), dimension(3, 3) :: sigma
    real(DP), dimension(3, 3) :: fabric
    type(Share_var) :: this
    !
    this%harden_ = harden
    this%sigma_ = sigma(:, :)
    this%fabric_ = fabric(:, :)
  endfunction construct_with_param
endmodule share_vars
