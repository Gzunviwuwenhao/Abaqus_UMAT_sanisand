!*****************************************************************************
!> @brief tensor_opt_mod
!>
!> @details 模块详细描述
!>
!> @author wuwenhao
!> @date 2025/11/27
!*****************************************************************************
module tensor_opt_mod
  implicit none
  public :: operator(.ddot.)
  type, public :: torch
  contains
    private
    procedure, public, nopass :: trace => trace_impl_
    procedure, public, nopass :: get_J2 => sec_dev_invar_impl_
    procedure, public, nopass :: get_J3 => trd_dev_invar_impl_
    procedure, public, nopass :: deviatoric => deviatoric_impl_
    procedure, public, nopass :: ratio => ratio_impl_
    procedure, public, nopass :: sin3theta => sin3theta_impl_
    procedure, public, nopass :: shear => shear_impl
    ! 打印数组
    procedure, public, nopass :: print => print_impl_
  endtype torch
  !
  interface operator(.ddot.)
    module procedure tensor4_ddot_tensor2
    module procedure tensor2_ddot_tensor4
  endinterface
  !======================================================================
  ! 抽象接口定义（在子模块中实现）
  !======================================================================
  interface
    ! print tensor
    module Subroutine print_impl_(stress)
      use Base_config, only: data_t
      real(data_t), intent(in), dimension(3, 3) :: stress
    endSubroutine
    ! tensor trace
    module function trace_impl_(stress) result(val)
      use Base_config, only: data_t
      real(data_t), intent(in), dimension(3, 3) :: stress
      real(data_t) :: val
    endfunction trace_impl_
    ! second-order invariant
    module function sec_dev_invar_impl_(stress) result(val)
      use Base_config, only: data_t
      real(data_t), intent(in), dimension(3, 3) :: stress
      real(data_t) :: val
    endfunction sec_dev_invar_impl_
    ! third_order deviatoric invariant
    module function trd_dev_invar_impl_(stress) result(val)
      use Base_config, only: data_t
      real(data_t), intent(in), dimension(3, 3) :: stress
      real(data_t) :: val
    endfunction trd_dev_invar_impl_
    ! deviatoric stress
    module function deviatoric_impl_(stress) result(tensor)
      use Base_config, only: data_t
      real(data_t), intent(in), dimension(3, 3) :: stress
      real(data_t), dimension(3, 3) :: tensor
    endfunction deviatoric_impl_
    ! stress ratio
    module function ratio_impl_(stress) result(tensor)
      use Base_config, only: data_t
      real(data_t), intent(in), dimension(3, 3) :: stress
      real(data_t), dimension(3, 3) :: tensor
    endfunction ratio_impl_
    ! sin3theta
    module function sin3theta_impl_(stress) result(sin3t)
      use Base_config, only: data_t
      real(data_t), intent(in), dimension(3, 3) :: stress
      real(data_t) :: sin3t
    endfunction sin3theta_impl_
    ! shear
    module function shear_impl(stress) result(shear)
      use Base_config, only: data_t
      real(data_t), dimension(3, 3), intent(in) :: stress
      real(data_t) :: shear
    endfunction shear_impl
    !
    module function tensor4_ddot_tensor2(tensor4, tensor2) result(res)
      use Base_config
      implicit none
      real(data_t), dimension(3, 3, 3, 3), intent(in) :: tensor4
      real(data_t), dimension(3, 3), intent(in) :: tensor2
      real(data_t), dimension(3, 3) :: res
    endfunction tensor4_ddot_tensor2
    module function tensor2_ddot_tensor4(tensor2, tensor4) result(res)
      use Base_config
      implicit none
      real(data_t), dimension(3, 3, 3, 3), intent(in) :: tensor4
      real(data_t), dimension(3, 3), intent(in) :: tensor2
      real(data_t), dimension(3, 3) :: res
    endfunction tensor2_ddot_tensor4
  endinterface
contains

endmodule tensor_opt_mod

