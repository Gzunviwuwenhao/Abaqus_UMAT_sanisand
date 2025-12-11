!DIR$ FREEFORM
!*****************************************************************************
!> @brief presolve_mod
!>
!> @details 模块详细描述
!>
!> @author wuwenhao
!> @date 2025/11/17
!*****************************************************************************
module presolve_mod
  use Base_config, only: DP
  implicit none
  public :: Convert_array_to_tensor, Convert_tensor_to_array, Print_array, Get_rotation_matrix
  public :: abaqus_debug
  private
  ! Public interface
  interface
    !**************************************************************************
    !> @brief convert_stress_to_tensor
    !>
    !> @details: this function is used to convert stress into a 3×3 two-dimensional tensor
    !>
    !> @param[in]  array: 输入的向量
    !> @param[out] tensor: 应力张量
    !>
    !> @return 返回值说明
    !**************************************************************************
    module function Convert_array_to_tensor(array, scalar) result(tensor)
      real(DP), intent(in) :: array(:)
      real(DP), intent(in), optional :: scalar
      real(DP), dimension(3, 3) :: tensor
    endfunction Convert_array_to_tensor
    !**************************************************************************
    !> @brief convert_stress_to_tensor
    !>
    !> @details: this function is used to convert a 3×3 two-dimensional tensor into array
    !>
    !> @param[in]  tensor: 输入的向量
    !> @param[in]  size: 向量的大小
    !> @param[out] array: 输出数组
    !>
    !> @return 返回值说明
    !**************************************************************************
    module function Convert_tensor_to_array(tensor, size, scalar) result(array)
      real(DP), dimension(3, 3), intent(in) :: tensor
      integer, intent(in) :: size
      integer, intent(in), optional :: scalar
      real(DP), dimension(size) :: array
    endfunction Convert_tensor_to_array
    module function Get_rotation_matrix(angle, axis) result(rot_matrix)
      real(DP), intent(in) :: angle
      real(DP), intent(in), dimension(3) :: axis
      real(DP), dimension(3, 3) :: rot_matrix
    endfunction Get_rotation_matrix
    ! print the array
    module Subroutine Print_array(array)
      real(DP), intent(in), dimension(:) :: array
    endSubroutine
    ! abaqus_debug
    module subroutine abaqus_debug(noel_num, npt_num, num, noel, npt, iteration, names)
      integer, intent(in) :: noel_num, npt_num, num
      integer, intent(in) :: noel, npt,iteration
      character(len=*), intent(in) :: names
    endsubroutine abaqus_debug
  endinterface ! end interface
contains

endmodule presolve_mod
