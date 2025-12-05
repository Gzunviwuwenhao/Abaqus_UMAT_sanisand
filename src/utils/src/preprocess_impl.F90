!*****************************************************************************
!> @brief presolve_impl
!>
!> @details 模块详细描述
!>
!> @author wuwenhao
!> @date 2025/11/27
!*****************************************************************************
submodule(presolve_mod) presolve_impl
  use Base_config, only: DP
  implicit none
contains
  !**************************************************************************
  !> @brief convert_array_to_tensor
  !>
  !> @details: this function is used to convert array into a 3×3 two-dimensional tensor
  !>
  !> @param[in]  array: 输入的应力向量
  !> @param[in]  size: 应力向量的大小
  !> @param[out] tensor: 应力张量
  !>
  !> @return 返回值说明
  !**************************************************************************
  module procedure convert_array_to_tensor
  real(DP) :: scalar_
  if(present(scalar)) then
    scalar_ = scalar
  else
    scalar_ = 1.0d0
  endif
  select case(size(array))
  case(3)
    tensor(1, 1) = -array(1)
    tensor(2, 2) = -array(2)
    tensor(1, 2) = -array(3) / scalar_
    tensor(2, 1) = -array(3) / scalar_
  case(4)
    tensor(1, 1) = -array(1)
    tensor(2, 2) = -array(2)
    tensor(3, 3) = -array(3)
    tensor(1, 2) = -array(4) / scalar_
    tensor(2, 1) = -array(4) / scalar_
  case(6)
    tensor(1, 1) = -array(1)
    tensor(2, 2) = -array(2)
    tensor(3, 3) = -array(3)
    tensor(1, 2) = -array(4) / scalar_
    tensor(2, 1) = -array(4) / scalar_
    tensor(1, 3) = -array(5) / scalar_
    tensor(3, 1) = -array(5) / scalar_
    tensor(2, 3) = -array(6) / scalar_
    tensor(3, 2) = -array(6) / scalar_
  endselect
  end procedure convert_array_to_tensor
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
  module procedure Convert_tensor_to_array
  real(DP) :: scalar_
  if(present(scalar)) then
    scalar_ = scalar
  else
    scalar_ = 1.0_DP
  endif
  select case(size)
  case(3)
    array(1) = -tensor(1, 1)
    array(2) = -tensor(2, 2)
    array(3) = -tensor(1, 2) * scalar_
  case(4)
    array(1) = -tensor(1, 1)
    array(2) = -tensor(2, 2)
    array(3) = -tensor(3, 3)
    array(4) = -tensor(1, 2) * scalar_
  case(6)
    array(1) = -tensor(1, 1)
    array(2) = -tensor(2, 2)
    array(3) = -tensor(3, 3)
    array(4) = -tensor(1, 2) * scalar_
    array(5) = -tensor(1, 3) * scalar_
    array(6) = -tensor(2, 3) * scalar_
  endselect
  end procedure Convert_tensor_to_array
  ! print array
  module procedure print_array
  implicit none
  integer :: i, n

  n = size(array)
  print *, "Array size: ", n
  print *, "Array elements:"
  do i = 1, n
    print *, "  array(", i, ") = ", array(i)
  enddo
  end procedure print_array
endsubmodule
