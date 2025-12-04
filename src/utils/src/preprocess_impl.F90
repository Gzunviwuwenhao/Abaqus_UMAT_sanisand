!*****************************************************************************
!> @brief presolve_impl
!>
!> @details 模块详细描述
!>
!> @author wuwenhao
!> @date 2025/11/27
!*****************************************************************************
submodule(presolve_mod) presolve_impl
  use Base_config,only:data_t
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
  !
  implicit none
  real(data_t)::scalar_
  if (present(scalar) ) then
    scalar_ = scalar
  else
    scalar_ = 1.0d0
  end if
  select case(size(array))
  case(3)
    tensor(1, 1) = -array(1)
    tensor(2, 2) = -array(2)
    tensor(1, 2) = -array(3)/scalar_
    tensor(2, 1) = -array(3)/scalar_
  case(4)
    tensor(1, 1) = -array(1)
    tensor(2, 2) = -array(2)
    tensor(3, 3) = -array(3)
    tensor(1, 2) = -array(4)/scalar_
    tensor(2, 1) = -array(4)/scalar_
  case(6)
    tensor(1, 1) = -array(1)
    tensor(2, 2) = -array(2)
    tensor(3, 3) = -array(3)
    tensor(1, 2) = -array(4)/scalar_
    tensor(2, 1) = -array(4)/scalar_
    tensor(1, 3) = -array(5)/scalar_
    tensor(3, 1) = -array(5)/scalar_
    tensor(2, 3) = -array(6)/scalar_
    tensor(3, 2) = -array(6)/scalar_
  endselect
  end procedure convert_array_to_tensor
  !
  module procedure print_impl_
    implicit none
    integer :: i, n

    n = size(array)
    print *, "Array size: ", n
    print *, "Array elements:"
    do i = 1, n
      print *, "  array(", i, ") = ", array(i)
    end do
  end procedure print_impl_
endsubmodule
