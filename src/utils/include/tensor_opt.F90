!*****************************************************************************
!> @brief tensor_opt_mod
!>
!> @details 模块详细描述
!>
!> @author wuwenhao
!> @date 2025/11/27
!*****************************************************************************
module tensor_opt_mod
  use Base_config, only: DP
  implicit none
  public :: operator(.ddot.), Torch
  !---------------------------------------------------------------------------
  !> @brief torch :
  !>
  !> @details 类型详细描述
  !>
  !> @author wuwenhao
  !> @date 2025/12/05
  !---------------------------------------------------------------------------
  type :: Torch
  contains
    private
    ! 打印数组
    procedure, public, nopass :: Print => Print_Impl
    procedure, public, nopass :: Trace => Trace_Impl
    procedure, public, nopass :: Get_J2 => Sec_dev_invar_impl
    procedure, public, nopass :: Get_J3 => Trd_dev_invar_impl
    procedure, public, nopass :: Deviatoric => Deviatoric_impl
    procedure, public, nopass :: Ratio => Ratio_impl
    procedure, public, nopass :: Sin3theta => Sin3theta_impl
    procedure, public, nopass :: Shear => Shear_impl

  endtype Torch
  !
  interface operator(.ddot.)
    module procedure Tensor4_ddot_tensor2
    module procedure Tensor2_ddot_tensor4
  endinterface
  !======================================================================
  ! Abstract interface definition (implemented in the sub-module)
  !======================================================================
  interface
    !*****************************************************************************
    !> @brief Print_Impl
    !>
    !> @details Print a stress tensor
    !>
    !> @param[in]  stress  Stress tensor to print
    !>
    !*****************************************************************************
    module Subroutine Print_Impl(stress)
      real(DP), intent(in), dimension(3, 3) :: stress
    endSubroutine
    !*****************************************************************************
    !> @brief Trace_Impl
    !>
    !> @details Calculate the trace of a stress tensor
    !>
    !> @param[in]  stress  Stress tensor
    !>
    !> @return Trace of the stress tensor
    !*****************************************************************************
    module function Trace_Impl(stress) result(val)
      real(DP), intent(in), dimension(3, 3) :: stress
      real(DP) :: val
    endfunction Trace_Impl
    !*****************************************************************************
    !> @brief Sec_dev_invar_impl
    !>
    !> @details Calculate the second deviatoric invariant (J2) of a stress tensor
    !>
    !> @param[in]  stress  Stress tensor
    !>
    !> @return Second deviatoric invariant (J2)
    !*****************************************************************************
    module function Sec_dev_invar_impl(stress) result(val)
      real(DP), intent(in), dimension(3, 3) :: stress
      real(DP) :: val
    endfunction Sec_dev_invar_impl
    !*****************************************************************************
    !> @brief Trd_dev_invar_impl
    !>
    !> @details Calculate the third deviatoric invariant (J3) of a stress tensor
    !>
    !> @param[in]  stress  Stress tensor
    !>
    !> @return Third deviatoric invariant (J3)
    !*****************************************************************************
    module function Trd_dev_invar_impl(stress) result(val)
      real(DP), intent(in), dimension(3, 3) :: stress
      real(DP) :: val
    endfunction Trd_dev_invar_impl
    !*****************************************************************************
    !> @brief Deviatoric_impl
    !>
    !> @details Calculate the deviatoric part of a stress tensor
    !>
    !> @param[in]  stress  Stress tensor
    !>
    !> @return Deviatoric stress tensor
    !*****************************************************************************
    module function Deviatoric_impl(stress) result(tensor)
      real(DP), intent(in), dimension(3, 3) :: stress
      real(DP), dimension(3, 3) :: tensor
    endfunction Deviatoric_impl
    !*****************************************************************************
    !> @brief Ratio_impl
    !>
    !> @details Calculate the stress ratio tensor (deviatoric stress divided by
    !> mean stress)
    !> @param[in]  stress  Stress tensor
    !>
    !> @return Stress ratio tensor
    !*****************************************************************************
    module function Ratio_impl(stress) result(tensor)
      real(DP), intent(in), dimension(3, 3) :: stress
      real(DP), dimension(3, 3) :: tensor
    endfunction Ratio_impl
    !*****************************************************************************
    !> @brief Sin3theta_impl
    !>
    !> @details Calculate sin(3θ) where θ is the Lode angle
    !>
    !> @param[in]  stress  Stress tensor
    !>
    !> @return sin(3θ) value
    !*****************************************************************************
    module function Sin3theta_impl(stress) result(sin3t)
      real(DP), intent(in), dimension(3, 3) :: stress
      real(DP) :: sin3t
    endfunction Sin3theta_impl
    !*****************************************************************************
    !> @brief Shear_impl
    !>
    !> @details Calculate the shear stress (sqrt(J2))
    !>
    !> @param[in]  stress  Stress tensor
    !>
    !> @return Shear stress value
    !*****************************************************************************
    module function Shear_impl(stress) result(shear)
      real(DP), dimension(3, 3), intent(in) :: stress
      real(DP) :: shear
    endfunction Shear_impl
    !
    !*****************************************************************************
    !> @brief Tensor4_ddot_tensor2
    !>
    !> @details Calculate the double dot product of a fourth-order tensor and
    !> a second-order tensor
    !> @param[in]  tensor4  Fourth-order tensor
    !> @param[in]  tensor2  Second-order tensor
    !>
    !> @return Resulting second-order tensor
    !*****************************************************************************
    module function Tensor4_ddot_tensor2(tensor4, tensor2) result(res)
      real(DP), dimension(3, 3, 3, 3), intent(in) :: tensor4
      real(DP), dimension(3, 3), intent(in) :: tensor2
      real(DP), dimension(3, 3) :: res
    endfunction Tensor4_ddot_tensor2
    !*****************************************************************************
    !> @brief Tensor2_ddot_tensor4
    !>
    !> @details Calculate the double dot product of a second-order tensor and
    !> a fourth-order tensor
    !> @param[in]  tensor2  Second-order tensor
    !> @param[in]  tensor4  Fourth-order tensor
    !>
    !> @return Resulting second-order tensor
    !*****************************************************************************
    module function Tensor2_ddot_tensor4(tensor2, tensor4) result(res)
      real(DP), dimension(3, 3, 3, 3), intent(in) :: tensor4
      real(DP), dimension(3, 3), intent(in) :: tensor2
      real(DP), dimension(3, 3) :: res
    endfunction Tensor2_ddot_tensor4
  endinterface
contains

endmodule tensor_opt_mod
