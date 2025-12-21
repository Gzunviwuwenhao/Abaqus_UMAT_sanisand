!*****************************************************************************
!> @brief Container
!>
!> @details 模块详细描述
!>
!> @author wuwenhao
!> @date 2025/12/07
!*****************************************************************************
module Container_mod
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
    logical :: is_lowstress = .false.
    logical :: is_nan_inf = .false.
    integer :: size
  contains
    procedure, public, pass(this) :: get_harden => get_harden_impl
    procedure, public, pass(this) :: get_sigma => get_sigma_impl
    procedure, public, pass(this) :: get_fabric => get_fabric_impl
    procedure, public, pass(this) :: is_low => low_impl
    procedure, public, pass(this) :: update_harden => update_harden_impl
    procedure, public, pass(this) :: update_sigma => update_sigma_impl
    procedure, public, pass(this) :: update_fabric => update_fabric_impl
    procedure, public, pass(this) :: update_shvars => update_shvars_impl
    procedure, public, pass(this) :: changed_harden => changed_harden_impl
    procedure, public, pass(this) :: changed_sigma => changed_sigma_impl
    procedure, public, pass(this) :: changed_fabric => changed_fabric_impl
    procedure, public, pass(this) :: norm => norm_impl
    procedure, public, pass(this) :: print => print_impl
    procedure, private, pass(this) :: jugde_nan_inf_impl
    procedure, pass(this) :: assign => assign_impl
    generic, public :: assignment(=) => assign
    procedure, pass(this) :: binary_add => binary_add_impl
    generic, public :: operator(+) => binary_add
    procedure, pass(this) :: binary_sub => binary_sub_impl
    generic, public :: operator(-) => binary_sub
    procedure, pass(this) :: unary_minus => unary_minus_impl
    generic, public :: operator(-) => unary_minus
    procedure, pass(this) :: unary_lhs_scalar => unary_lhs_scalar_impl
    generic, public :: operator(*) => unary_lhs_scalar
    procedure, pass(this) :: unary_rhs_scalar => unary_rhs_scalar_impl
    generic, public :: operator(*) => unary_rhs_scalar
    procedure, pass(this) :: unary_div_impl => unary_div_impl
    generic, public :: operator(/) => unary_div_impl
  endtype Share_var
  !
  type, public :: State_var
    private
    real(DP) :: voidr_
    real(DP) :: pnewdt_
    logical :: initialized_ = .false.
  contains
    procedure, public, pass(this) :: get_voidr => get_voidr_impl
    procedure, public, pass(this) :: get_pnewdt => get_pnewdt_impl
    procedure, public, pass(this) :: update_voidr => update_voidr_impl
    procedure, public, pass(this) :: changed_voidr => changed_voidr_impl
    procedure, public, pass(this) :: changed_pnewdt => changed_pnewdt_impl
    procedure, pass(this) :: assign => assign_impl_state
    generic, public :: assignment(=) => assign
  endtype State_var
  !
  interface Share_var
    module procedure :: share_construct_param
    module procedure :: Share_construct_zero
  endinterface
  !
  interface State_var
    module procedure :: State_construct_param
  endinterface
  !
  interface
    module function share_construct_param(harden, sigma, fabric) result(this)
      real(DP), intent(in) :: harden
      real(DP), dimension(3, 3), intent(in) :: sigma
      real(DP), dimension(3, 3), intent(in) :: fabric
      type(Share_var) :: this
    endfunction share_construct_param
    !***************************************************************************
    module function Share_construct_zero() result(this)
      type(Share_var) :: this
    endfunction Share_construct_zero
    !***************************************************************************
    module function get_harden_impl(this) result(harden)
      class(Share_var), intent(in) :: this
      real(DP) :: harden
    endfunction get_harden_impl
    !***************************************************************************
    module function get_sigma_impl(this) result(sigma)
      class(Share_var), intent(in) :: this
      real(DP), dimension(3, 3) :: sigma
    endfunction
    !***************************************************************************
    module function get_fabric_impl(this) result(fabric)
      class(Share_var), intent(in) :: this
      real(DP), dimension(3, 3) :: fabric
    endfunction
    module function low_impl(this) result(is_true)
      class(Share_var), intent(in) :: this
      logical :: is_true
    endfunction low_impl
    !***************************************************************************
    module subroutine update_harden_impl(this, harden)
      class(Share_var), intent(inout) :: this
      real(DP), intent(in) :: harden
    endsubroutine update_harden_impl
    !***************************************************************************
    module subroutine update_sigma_impl(this, dsigma)
      class(Share_var), intent(inout) :: this
      real(DP), dimension(3, 3), intent(in) :: dsigma
    endsubroutine update_sigma_impl
    !***************************************************************************
    module subroutine update_fabric_impl(this, fabric)
      class(Share_var), intent(inout) :: this
      real(DP), dimension(3, 3), intent(in) :: fabric
    endsubroutine update_fabric_impl
    !***************************************************************************
    module subroutine update_shvars_impl(dharden, dsigma, dfabric, this)
      real(DP), intent(in) :: dharden
      real(DP), dimension(3, 3), intent(in) :: dsigma
      real(DP), dimension(3, 3), intent(in) :: dfabric
      class(Share_var), intent(inout) :: this
    endsubroutine update_shvars_impl
    !***************************************************************************
    module subroutine changed_harden_impl(this, harden)
      class(Share_var), intent(inout) :: this
      real(DP), intent(in) :: harden
    endsubroutine changed_harden_impl
    !***************************************************************************
    module subroutine changed_sigma_impl(this, sigma)
      class(Share_var), intent(inout) :: this
      real(DP), dimension(3, 3), intent(in) :: sigma
    endsubroutine changed_sigma_impl
    !***************************************************************************
    module subroutine changed_fabric_impl(this, fabric)
      class(Share_var), intent(inout) :: this
      real(DP), dimension(3, 3), intent(in) :: fabric
    endsubroutine changed_fabric_impl
    !***************************************************************************
    module function norm_impl(this) result(res)
      class(Share_var), intent(in) :: this
      real(DP), dimension(3) :: res
    endfunction norm_impl
    !***************************************************************************
    module subroutine print_impl(this)
      class(Share_var), intent(in) :: this
    endsubroutine print_impl
    !***************************************************************************
    module subroutine jugde_nan_inf_impl(this)
      class(Share_var), intent(inout) :: this
    endsubroutine jugde_nan_inf_impl
    !***************************************************************************
    module subroutine assign_impl(this, other)
      class(Share_var), intent(inout) :: this
      type(Share_var), intent(in) :: other
    endsubroutine assign_impl
    !***************************************************************************
    module function binary_add_impl(this, other) result(res)
      class(Share_var), intent(in) :: this
      type(Share_var), intent(in) :: other
      type(Share_var) :: res
    endfunction binary_add_impl
    !***************************************************************************
    module function binary_sub_impl(this, other) result(res)
      class(Share_var), intent(in) :: this
      type(Share_var), intent(in) :: other
      type(Share_var) :: res
    endfunction binary_sub_impl
    !***************************************************************************
    module function unary_minus_impl(this) result(res)
      class(Share_var), intent(in) :: this
      type(Share_var) :: res
    endfunction unary_minus_impl
    !***************************************************************************
    module function unary_lhs_scalar_impl(scalar, this) result(res)
      real(DP), intent(in) :: scalar
      class(Share_var), intent(in) :: this
      type(Share_var) :: res
    endfunction unary_lhs_scalar_impl
    !***************************************************************************
    module function unary_rhs_scalar_impl(this, scalar) result(res)
      class(Share_var), intent(in) :: this
      real(DP), intent(in) :: scalar
      type(Share_var) :: res
    endfunction unary_rhs_scalar_impl
    !***************************************************************************
    module function unary_div_impl(this, scalar) result(res)
      class(Share_var), intent(in) :: this
      real(DP), intent(in) :: scalar
      type(Share_var) :: res
    endfunction unary_div_impl
    ! end interface
  endinterface

  !
  interface
    module function State_construct_param(voidr, pnewdt) result(this)
      real(DP), intent(in) :: voidr
      real(DP), intent(in) :: pnewdt
      type(State_var) :: this
    endfunction State_construct_param
    !***************************************************************************
    module function get_voidr_impl(this) result(voidr)
      class(State_var), intent(in) :: this
      real(DP) :: voidr
    endfunction get_voidr_impl
    module function get_pnewdt_impl(this) result(pnewdt)
      class(State_var), intent(in) :: this
      real(DP) :: pnewdt
    endfunction get_pnewdt_impl
    !***************************************************************************
    module subroutine update_voidr_impl(this, depsln)
      class(State_var), intent(inout) :: this
      real(DP), dimension(3, 3), intent(in) :: depsln
    endsubroutine update_voidr_impl
    !***************************************************************************
    module subroutine changed_voidr_impl(this, voidr)
      class(State_var), intent(inout) :: this
      real(DP), intent(in) :: voidr
    endsubroutine changed_voidr_impl
    !***************************************************************************
    module subroutine changed_pnewdt_impl(this, pnewdt)
      class(State_var), intent(inout) :: this
      real(DP), intent(in) :: pnewdt
    endsubroutine changed_pnewdt_impl
    !***************************************************************************
    module subroutine assign_impl_state(this, other)
      class(State_var), intent(inout) :: this
      type(State_var), intent(in) :: other
    endsubroutine assign_impl_state
  endinterface
contains

endmodule Container_mod
