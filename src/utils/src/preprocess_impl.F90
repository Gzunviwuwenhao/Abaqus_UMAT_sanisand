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
  use tensor_opt_mod
  implicit none
  type(Torch) :: torch_
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
    tensor(1, 1) = array(1)
    tensor(2, 2) = array(2)
    tensor(1, 2) = array(3) / scalar_
    tensor(2, 1) = array(3) / scalar_
  case(4)
    tensor(1, 1) = array(1)
    tensor(2, 2) = array(2)
    tensor(3, 3) = array(3)
    tensor(1, 2) = array(4) / scalar_
    tensor(2, 1) = array(4) / scalar_
  case(6)
    tensor(1, 1) = array(1)
    tensor(2, 2) = array(2)
    tensor(3, 3) = array(3)
    tensor(1, 2) = array(4) / scalar_
    tensor(2, 1) = array(4) / scalar_
    tensor(1, 3) = array(5) / scalar_
    tensor(3, 1) = array(5) / scalar_
    tensor(2, 3) = array(6) / scalar_
    tensor(3, 2) = array(6) / scalar_
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
    array(1) = tensor(1, 1)
    array(2) = tensor(2, 2)
    array(3) = tensor(1, 2) * scalar_
  case(4)
    array(1) = tensor(1, 1)
    array(2) = tensor(2, 2)
    array(3) = tensor(3, 3)
    array(4) = tensor(1, 2) * scalar_
  case(6)
    array(1) = tensor(1, 1)
    array(2) = tensor(2, 2)
    array(3) = tensor(3, 3)
    array(4) = tensor(1, 2) * scalar_
    array(5) = tensor(1, 3) * scalar_
    array(6) = tensor(2, 3) * scalar_
  endselect
  end procedure Convert_tensor_to_array
  !*****************************************************************************
  module procedure Convert_tensor4_to_tensor2
  integer, parameter :: FST(6) = [1, 2, 3, 1, 1, 2]
  integer, parameter :: SCD(6) = [1, 2, 3, 2, 3, 3]
  integer :: i1, i2, j1, j2, i, j
  !
  do j = 1, size
    j1 = FST(j)
    j2 = SCD(j)
    do i = 1, size
      i1 = FST(i)
      i2 = SCD(i)
      tensor2(i, j) = 0.25d0 * (tensor4(i1, i2, j1, j2) + tensor4(i1, i2, j2, j1) + &
                                tensor4(i2, i1, j1, j2) + tensor4(i2, i1, j2, j1))
    enddo
  enddo
  end procedure Convert_tensor4_to_tensor2
  !*****************************************************************************
  module procedure Get_rotation_matrix
  real(DP), dimension(3) :: uaxis
  real(DP) :: cos, sin, temp, ux, uy, uz
  !
  if(axis(1) == 0.0_DP .and. axis(2) == 0.0_DP .and. axis(3) == 0.0_DP) then
    write(6, *) "Error: Rotation axis cannot be zero vector."
    call exit(1)
  endif
  uaxis = axis / dsqrt(sum(axis**2))
  ux = uaxis(1)
  uy = uaxis(2)
  uz = uaxis(3)
  cos = dcos(angle)
  sin = dsin(angle)
  temp = 1.0_DP - cos
  rot_matrix(1, 1) = cos + ux * ux * temp
  rot_matrix(1, 2) = ux * uy * temp - uz * sin
  rot_matrix(1, 3) = ux * uz * temp + uy * sin
  rot_matrix(2, 1) = uy * ux * temp + uz * sin
  rot_matrix(2, 2) = cos + uy * uy * temp
  rot_matrix(2, 3) = uy * uz * temp - ux * sin
  rot_matrix(3, 1) = uz * ux * temp - uy * sin
  rot_matrix(3, 2) = uz * uy * temp + ux * sin
  rot_matrix(3, 3) = cos + uz * uz * temp
  end procedure Get_rotation_matrix
  !**************************************************************************
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
  ! abaqus_debug
  module procedure abaqus_debug
  logical :: firstrun = .true.
  integer :: tempvar
  if((noel_num == noel) .and. (npt_num == npt) .and. num >= iteration) then
    write(*, *) "debug in ", trim(names)
    if(firstrun) then
      write(*, *) "please input an integer"
      read(*, *) tempvar
    endif
  endif
  end procedure abaqus_debug
endsubmodule
