!*****************************************************************************
!> @brief Preprocessing implementation submodule
!>
!> @details Implementation submodule for preprocessing utilities,
!> containing actual implementations of tensor-array conversions,
!> rotation matrix generation, and debugging functions.
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
  !> @brief Convert array to tensor implementation
  !>
  !> @details Convert a 1D array to a 3x3 2D tensor representation.
  !> Supports different array sizes (3, 4, 6) for different tensor
  !> representations (2D plane stress, axisymmetric, full 3D).
  !>
  !> @param[in]  array  Input vector in Voigt notation
  !> @param[in]  scalar Optional scaling factor for shear components
  !>
  !> @return 3x3 tensor representation
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
  !> @brief Convert tensor to array implementation
  !>
  !> @details Convert a 3x3 2D tensor to a 1D array representation.
  !> Supports different output sizes (3, 4, 6) for different tensor
  !> representations. Applies optional scaling to shear components.
  !>
  !> @param[in]  tensor  Input 3x3 tensor
  !> @param[in]  size    Output array size (3, 4, or 6)
  !> @param[in]  scalar  Optional integer scaling parameter
  !>
  !> @return 1D array in Voigt notation
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
  !> @brief Convert fourth-order tensor to second-order matrix implementation
  !>
  !> @details Convert a 3x3x3x3 fourth-order tensor to a 2D matrix
  !> using Voigt notation mapping. Applies symmetrization for
  !> minor symmetries in the fourth-order tensor.
  !>
  !> @param[in]  tensor4  Input fourth-order tensor (3x3x3x3)
  !> @param[in]  size     Output matrix size (6 for symmetric tensors)
  !>
  !> @return 2D matrix representation
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
  !> @brief Generate rotation matrix implementation
  !>
  !> @details Generate a 3x3 rotation matrix using Rodrigues' rotation
  !> formula for given angle and axis. Validates input axis and
  !> normalizes it before computation.
  !>
  !> @param[in]  angle      Rotation angle in radians
  !> @param[in]  axis       Rotation axis vector (3D)
  !>
  !> @return 3x3 rotation matrix
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
  !> @brief Print array implementation
  !>
  !> @details Print the contents of a 1D array to standard output
  !> with formatting for debugging purposes.
  !>
  !> @param[in]  array  Input array to print
  !**************************************************************************
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
  !**************************************************************************
  !> @brief ABAQUS debugging utility implementation
  !>
  !> @details Implementation of debugging function for ABAQUS UMAT
  !> simulations. Checks if current element and integration point
  !> match specified debugging targets, then prints debug information
  !> and optionally pauses for user input on first run.
  !>
  !> @param[in]  noel_num   Number of elements
  !> @param[in]  npt_num    Number of integration points
  !> @param[in]  num        Additional numerical parameter
  !> @param[in]  noel       Current element number
  !> @param[in]  npt        Current integration point number
  !> @param[in]  iteration  Current iteration number
  !> @param[in]  names      Descriptive name for debugging output
  !**************************************************************************
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
