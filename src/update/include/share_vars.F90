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
  type, public :: Share_var
    private
    real(DP) :: harden_
    real(DP), dimension(3, 3) :: sigma_
    real(DP), dimension(3, 3) :: fabric_
    logical :: initialized_ = .false.
  contains
    procedure, public, pass(this) :: get_harden => get_harden_impl
    procedure, public, pass(this) :: get_sigma => getsigma_impl
    procedure, public, pass(this) :: update_sigma => update_sigma_impl
    procedure, public, pass(this) :: get_fabric => getfabric_impl
    procedure, public, pass(this) :: update_shvars => update_shvars_impl
    procedure, public, pass(this) :: changed_sigma => changed_sigma_impl
    procedure, pass(this) :: assign => assign_impl
    generic, public :: assignment(=) => assign
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
  !
  function get_harden_impl(this) result(harden)
    class(Share_var), intent(in) :: this
    real(DP) :: harden
    harden = this%harden_
  endfunction
  !
  function getsigma_impl(this) result(sigma)
    class(Share_var), intent(in) :: this
    real(DP), dimension(3, 3) :: sigma
    sigma(:, :) = this%sigma_(:, :)
  endfunction
  !
  subroutine update_sigma_impl(this, dsigma)
    class(Share_var), intent(inout) :: this
    real(DP), dimension(3, 3), intent(in) :: dsigma
    this%sigma_(:, :) = this%sigma_(:, :) + dsigma(:, :)
  endsubroutine update_sigma_impl

  function getfabric_impl(this) result(fabric)
    class(Share_var), intent(in) :: this
    real(DP), dimension(3, 3) :: fabric
    fabric(:, :) = this%fabric_(:, :)
  endfunction
  !
  subroutine update_shvars_impl(dharden, dsigma, dfabric, this)
    real(DP), intent(in) :: dharden
    real(DP), dimension(3, 3), intent(in) :: dsigma
    real(DP), dimension(3, 3), intent(in) :: dfabric
    class(Share_var), intent(inout) :: this
    !
    this%harden_ = this%harden_ + dharden
    this%sigma_(:, :) = this%sigma_(:, :) + dsigma(:, :)
    this%fabric_(:, :) = this%fabric_(:, :) + dfabric(:, :)
  endsubroutine update_shvars_impl
  !
  subroutine changed_sigma_impl(this, sigma)
    class(Share_var), intent(inout) :: this
    real(DP), dimension(3, 3), intent(in) :: sigma
    this%sigma_(:, :) = sigma(:, :)
  endsubroutine changed_sigma_impl
  !
  subroutine assign_impl(this, other)
    class(Share_var), intent(inout) :: this
    type(Share_var), intent(in) :: other
    !
    this%harden_ = other%harden_
    this%sigma_(:, :) = other%sigma_(:, :)
    this%fabric_(:, :) = other%fabric_(:, :)
  endsubroutine assign_impl
endmodule share_vars
