!*******************************************************************************
!> @brief Math module for numerical algorithms
!>
!> @details This module provides mathematical utilities including bisection method,
!>          interval checking, and other numerical algorithms used in the UMAT.
!>
!> @author wuwenhao
!> @date 2025/12/11
!*******************************************************************************
module math_mod
  use Base_config
  use Container_mod
  implicit none
  private
  !*****************************************************************************
  !> @brief Abstract interface for functions with parameters
  !>
  !> @details This abstract interface defines the signature for functions that
  !>          take shared variables, state variables, and an amplitude parameter,
  !>          returning a double precision value.
  !>
  !> @param[in] shvars    Shared variables (stress tensor, etc.)
  !> @param[in] stvars    State variables (void ratio, etc.)
  !> @param[in] amplitude Amplitude parameter (e.g., scaling factor)
  !> @return fval         Function value
  !*****************************************************************************
  abstract interface
    function func_with_param(shvars, stvars, depsln) result(fval)
      use Base_config
      use Container_mod
      implicit none
      type(Share_var), intent(in) :: shvars
      type(State_var), intent(in) :: stvars
      real(DP), dimension(3, 3), intent(in) :: depsln
      real(DP) :: fval
    endfunction func_with_param
  endinterface
  !
  type, public :: Math
  contains
    procedure, public, nopass :: Intchc => intchc_impl
    procedure, public, nopass :: Onyield => Onyield_impl
    procedure, public, nopass :: Bisection_impl
    procedure, public, nopass :: drift_shvars_impl
    procedure, public, nopass :: Get_residual_impl
    procedure, public, nopass :: ftol_with_depsln
    procedure, public, nopass :: mean_with_depsln
    procedure, private, nopass :: is_monotonic
  endtype
  !
  interface
    !***************************************************************************
    !> @brief Interval checking implementation
    !>
    !> @details This subroutine performs interval checking to find the appropriate
    !>          scaling factor for the strain increment. It determines the right
    !>          boundary and the scaling factor for plastic correction.
    !>
    !> @param[in]  shvars Shared variables (stress tensor, etc.)
    !> @param[in]  stvars State variables (void ratio, etc.)
    !> @param[out] rbd    Right boundary of the scaling factor (0 <= rbd <= 1)
    !> @param[out] alout  Scaling factor for plastic correction
    !***************************************************************************
    module subroutine intchc_impl(shvars, stvars, depsln, rbd, alout)
      type(Share_var), intent(in) :: shvars
      type(State_var), intent(in) :: stvars
      real(DP), dimension(3, 3), intent(in) :: depsln
      real(DP), intent(out) :: rbd
      real(DP), intent(out) :: alout
    endsubroutine intchc_impl
    !***************************************************************************
    !> @brief Bisection method implementation
    !>
    !> @details This function implements the bisection method to find the root of
    !>          a function within a given interval [lbd, rbd]. The function must
    !>          have opposite signs at the boundaries. Monotonicity check can be
    !>          enabled in debug mode.
    !>
    !> @param[in] shvars   Shared variables (stress tensor, etc.)
    !> @param[in] stvars   State variables (void ratio, etc.)
    !> @param[in] func     Function to find root of (conforms to func_with_param)
    !> @param[in] lbd      Left boundary of the interval
    !> @param[in] rbd      Right boundary of the interval
    !> @param[in] condition Target value (usually 0 for root finding)
    !> @return alout       Root found within the interval
    !***************************************************************************
    module function Bisection_impl(shvars, stvars, depsln, func, lbd, rbd, condition) &
      result(alout)
      ! declration
      type(Share_var), intent(in) :: shvars
      type(State_var), intent(in) :: stvars
      real(DP), dimension(3, 3), intent(in) :: depsln
      procedure(func_with_param) :: func
      real(DP), intent(in) :: lbd
      real(DP), intent(in) :: rbd
      real(DP), intent(in) :: condition
      real(DP) :: alout
    endfunction Bisection_impl
    !***************************************************************************
    module function Get_residual_impl(shfor, shsec, shtmp) result(residual)
      type(Share_var), intent(in) :: shfor
      type(Share_var), intent(in) :: shsec
      type(Share_var), intent(in) :: shtmp
      real(DP) :: residual
    endfunction Get_residual_impl
    !***************************************************************************
    module subroutine Onyield_impl(shvars, stvars, depsln, shvar_upd, stvar_upd, dempx)
      type(Share_var), intent(in) :: shvars
      type(State_var), intent(in) :: stvars
      real(DP), dimension(3, 3), intent(in) :: depsln
      type(Share_var), intent(out) :: shvar_upd
      type(State_var), intent(out) :: stvar_upd
      real(DP), dimension(3, 3, 3, 3) :: dempx
    endsubroutine Onyield_impl
    !
    module subroutine drift_shvars_impl(shtmp, sttmp, depsln, shdrt, stdrt)
      type(Share_var), intent(in) :: shtmp
      type(State_var), intent(in) :: sttmp
      real(DP), dimension(3, 3), intent(in) :: depsln
      type(Share_var), intent(out) :: shdrt
      type(State_var), intent(out) :: stdrt
    endsubroutine drift_shvars_impl
    !***************************************************************************
    !> @brief Yield distance with strain increment
    !>
    !> @details This function calculates the yield distance after applying a
    !>          scaled strain increment. It computes the stress increment from
    !>          the strain increment using the elasticity tensor, updates the
    !>          stress, and returns the yield distance.
    !>
    !> @param[in] shvars    Shared variables (stress tensor, etc.)
    !> @param[in] stvars    State variables (void ratio, etc.)
    !> @param[in] amplitude Scaling factor for the strain increment
    !> @return ftol         Yield distance after applying scaled strain increment
    !***************************************************************************
    module function ftol_with_depsln(shvars, stvars, depsln) result(ftol)
      type(Share_var), intent(in) :: shvars
      type(State_var), intent(in) :: stvars
      real(DP), dimension(3, 3), intent(in) :: depsln
      real(DP) :: ftol
    endfunction ftol_with_depsln
    !***************************************************************************
    !> @brief Mean stress with strain increment
    !>
    !> @details This function calculates the mean stress after applying a
    !>          scaled strain increment. It computes the stress increment from
    !>          the strain increment using the elasticity tensor, updates the
    !>          stress, and returns the mean stress (trace/3).
    !>
    !> @param[in] shvars    Shared variables (stress tensor, etc.)
    !> @param[in] stvars    State variables (void ratio, etc.)
    !> @param[in] amplitude Scaling factor for the strain increment
    !> @return res          Mean stress after applying scaled strain increment
    !***************************************************************************
    module function mean_with_depsln(shvars, stvars, depsln) result(res)
      type(Share_var), intent(in) :: shvars
      type(State_var), intent(in) :: stvars
      real(DP), dimension(3, 3), intent(in) :: depsln
      real(DP) :: res
    endfunction mean_with_depsln
    !***************************************************************************
    !> @brief Monotonicity check for a function
    !>
    !> @details This function checks whether a given function is monotonic
    !>          (either non-decreasing or non-increasing) within the interval
    !>          [lbd, rbd]. It samples the function at multiple points and
    !>          verifies monotonic behavior.
    !>
    !> @param[in] shvars Shared variables (stress tensor, etc.)
    !> @param[in] stvars State variables (void ratio, etc.)
    !> @param[in] func   Function to check (conforms to func_with_param)
    !> @param[in] lbd    Left boundary of the interval
    !> @param[in] rbd    Right boundary of the interval
    !> @return           .true. if function is monotonic, .false. otherwise
    !***************************************************************************
    module logical function is_monotonic(shvars, stvars, depsln, func, lbd, rbd)
      type(Share_var), intent(in) :: shvars
      type(State_var), intent(in) :: stvars
      real(DP), dimension(3, 3), intent(in) :: depsln
      procedure(func_with_param) :: func
      real(DP), intent(in) :: lbd
      real(DP), intent(in) :: rbd
    endfunction is_monotonic
  endinterface

contains
!*******************************************************************************
endmodule math_mod
