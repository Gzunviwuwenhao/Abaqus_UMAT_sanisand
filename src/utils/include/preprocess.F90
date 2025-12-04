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
  implicit none
  public convert_array_to_tensor, print_array
  private

  !
  interface print_array
    module procedure print_impl_
  endinterface
  ! Public interface
  interface
    !**************************************************************************
    !> @brief convert_stress_to_tensor
    !>
    !> @details: this function is used to convert stress into a 3×3 two-dimensional tensor
    !>
    !> @param[in]  array: 输入的向量
    !> @param[in]  size: 向量的大小
    !> @param[out] tensor: 应力张量
    !>
    !> @return 返回值说明
    !**************************************************************************
    module function convert_array_to_tensor(array, scalar) result(tensor)
      use Base_config, only: data_t
      implicit none
      real(data_t), intent(in) :: array(:)
      real(data_t), intent(in), optional :: scalar
      real(data_t), dimension(3, 3) :: tensor
    endfunction convert_array_to_tensor
    ! print the array
    module Subroutine print_impl_(array)
      use Base_config, only: data_t
      implicit none
      real(data_t), intent(in), dimension(:) :: array
    endSubroutine
    !
  endinterface ! end interface
contains

endmodule presolve_mod
