!DIR$ FREEFORM
!*****************************************************************************
!> @brief Preprocessing module
!>
!> @details This module provides utilities for data conversion and
!> preprocessing operations, including tensor-array conversions,
!> rotation matrix generation, and debugging utilities.
!>
!> @author wuwenhao
!> @date 2025/11/17
!*****************************************************************************
module presolve_mod
  use Base_config, only: DP
  implicit none
  public :: Convert_array_to_tensor, Convert_tensor_to_array, Print_array
  public :: Convert_tensor4_to_tensor2
  public :: abaqus_debug, Get_rotation_matrix
  private
  ! Public interface
  interface
    !**************************************************************************
    !> @brief Convert array to tensor
    !>
    !> @details Convert a 1D array to a 3x3 2D tensor representation.
    !> Commonly used for converting stress/strain vectors to tensor form.
    !>
    !> @param[in]  array Input vector (size 6 for symmetric tensor)
    !> @param[in]  scalar Optional scaling factor
    !>
    !> @return 3x3 tensor representation
    !**************************************************************************
    module function Convert_array_to_tensor(array, scalar) result(tensor)
      real(DP), intent(in) :: array(:)
      real(DP), intent(in), optional :: scalar
      real(DP), dimension(3, 3) :: tensor
    endfunction Convert_array_to_tensor
    !**************************************************************************
    !> @brief Convert tensor to array
    !>
    !> @details Convert a 3x3 2D tensor to a 1D array representation.
    !> For symmetric tensors, returns Voigt notation vector.
    !>
    !> @param[in]  tensor Input 3x3 tensor
    !> @param[in]  size Output array size (6 for symmetric, 9 for full)
    !> @param[in]  scalar Optional integer parameter
    !>
    !> @return 1D array representation of the tensor
    !**************************************************************************
    module function Convert_tensor_to_array(tensor, size, scalar) result(array)
      real(DP), dimension(3, 3), intent(in) :: tensor
      integer, intent(in) :: size
      integer, intent(in), optional :: scalar
      real(DP), dimension(size) :: array
    endfunction Convert_tensor_to_array
    !**************************************************************************
    !> @brief Convert fourth-order tensor to second-order matrix
    !>
    !> @details Convert a 3x3x3x3 fourth-order tensor to a 2D matrix
    !> representation using Voigt notation.
    !>
    !> @param[in]  tensor4 Input fourth-order tensor (3x3x3x3)
    !> @param[in]  size Output matrix size (6x6 for symmetric)
    !>
    !> @return 2D matrix representation
    !**************************************************************************
    module function Convert_tensor4_to_tensor2(tensor4, size) result(tensor2)
      real(DP), dimension(3, 3, 3, 3), intent(in) :: tensor4
      integer, intent(in) :: size
      real(DP), dimension(size, size) :: tensor2
    endfunction Convert_tensor4_to_tensor2
    !**************************************************************************
    !> @brief Generate rotation matrix
    !>
    !> @details Generate a 3x3 rotation matrix for given angle and axis
    !> using Rodrigues' rotation formula.
    !>
    !> @param[in]  angle Rotation angle in radians
    !> @param[in]  axis  Rotation axis vector (3D)
    !>
    !> @return 3x3 rotation matrix
    !**************************************************************************
    module function Get_rotation_matrix(angle, axis) result(rot_matrix)
      real(DP), intent(in) :: angle
      real(DP), intent(in), dimension(3) :: axis
      real(DP), dimension(3, 3) :: rot_matrix
    endfunction Get_rotation_matrix
    !**************************************************************************
    !> @brief Print array contents
    !>
    !> @details Print the contents of a 1D array to standard output
    !> for debugging purposes.
    !>
    !> @param[in]  array Input array to print
    !**************************************************************************
    module Subroutine Print_array(array)
      real(DP), intent(in), dimension(:) :: array
    endSubroutine
    !**************************************************************************
    !> @brief ABAQUS debugging utility
    !>
    !> @details Print debugging information for ABAQUS UMAT simulations,
    !> including element number, integration point, and iteration count.
    !>
    !> @param[in]  noel_num  Number of elements
    !> @param[in]  npt_num   Number of integration points
    !> @param[in]  num       Additional numerical parameter
    !> @param[in]  noel      Current element number
    !> @param[in]  npt       Current integration point number
    !> @param[in]  iteration Current iteration number
    !> @param[in]  names     Descriptive name for debugging output
    !**************************************************************************
    module subroutine abaqus_debug(noel_num, npt_num, num, noel, npt, iteration, names)
      integer, intent(in) :: noel_num, npt_num, num
      integer, intent(in) :: noel, npt, iteration
      character(len=*), intent(in) :: names
    endsubroutine abaqus_debug
  endinterface ! end interface
contains

endmodule presolve_mod
