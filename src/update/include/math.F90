!*****************************************************************************
!> @brief math_mod
!>
!> @details 模块详细描述
!>
!> @author wuwenhao
!> @date 2025/12/11
!*****************************************************************************
module math_mod
  use Base_config
  use share_vars
  implicit none
  !
  private
  type Math
  contains
    procedure, public, nopass :: Get_alout => Bisection_impl
    procedure, private, nopass :: left_bound_impl
    procedure, private, nopass :: right_bound_impl
  endtype

  interface
    !***************************************************************************
    !> @brief Get_alout_impl
    !>
    !> @details 函数详细描述
    !>
    !> @param[in]  stress :current stress tensor
    !> @param[in]  voidr : current void ratio
    !> @param[in]  alpha0 : left boundary
    !> @param[in]  alpha1 : right boundary
    !> @param[out] alout :
    !>
    !> @return 返回值说明
    !***************************************************************************
    module function Bisection_impl(shvars, voidr, depsln, alpha0, alpha1) result(alout)
      type(Share_var), intent(in) :: shvars
      real(DP), intent(in) :: voidr
      real(DP), dimension(3, 3), intent(in) :: depsln
      real(DP), intent(in) :: alpha0
      real(DP), intent(in) :: alpha1
      real(DP) :: alout
    endfunction Bisection_impl
    !***************************************************************************
    !> @brief Get_alout_impl
    !>
    !> @details 函数详细描述
    !>
    !> @param[in]  stress :current stress tensor
    !> @param[in]  voidr : current void ratio
    !> @param[out]  alpha1 : left boundary
    !>
    !> @return 返回值说明
    !***************************************************************************
    module function right_bound_impl(shvars, voidr, depsln) result(alpha1)
      type(Share_var), intent(in) :: shvars
      real(DP), intent(in) :: voidr
      real(DP), dimension(3, 3), intent(in) :: depsln
      real(DP) :: alpha1
    endfunction right_bound_impl
    module function left_bound_impl(shvars, voidr, depsln) result(alpha0)
      type(Share_var), intent(in) :: shvars
      real(DP), intent(in) :: voidr
      real(DP), dimension(3, 3), intent(in) :: depsln
      real(DP) :: alpha1
    endfunction left_bound_impl
  endinterface
contains

endmodule math_mod
