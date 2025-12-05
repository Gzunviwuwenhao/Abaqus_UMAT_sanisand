!*****************************************************************************
!> @brief tensor_opt_impl
!>
!> @details 模块详细描述
!>
!> @author wuwenhao
!> @date 2025/11/27
!*****************************************************************************
submodule(tensor_opt_mod) tensor_opt_impl
  use Base_config, only: DP, TWO, THREE, EPS
  implicit none
contains
  !*****************************************************************************
  !> @brief print_impl_
  !>
  !> @details 打印3x3应力张量的内容
  !>
  !> @param[in] stress 应力张量
  !*****************************************************************************
  module procedure Print_impl
  implicit none
  integer :: i, j
  ! declaration
  write(*, '(A)') "tensor:"
  do i = 1, 3
    write(*, '(A, I1, A)', advance='no') "Row ", i, ": ["
    do j = 1, 3
      write(*, '(F10.6)', advance='no') stress(i, j)
      if(j < 3) write(*, '(A)', advance='no') ", "
    enddo
    write(*, '(A)') "]"
  enddo
  write(*, *)
  end procedure Print_impl
  !*****************************************************************************
  !> @brief trace_impl_
  !>
  !> @details 计算矩阵的迹
  !>
  !> @param[in]  stress 应力张量
  !> @param[out] val 矩阵的迹
  !>
  !> @return 矩阵的迹
  !*****************************************************************************
  module procedure Trace_impl
  implicit none
  integer :: i
  val = sum([(stress(i, i), i=1, 3)])
  end procedure Trace_impl
  !*****************************************************************************
  !> @brief sec_invariant_impl_
  !>
  !> @details 计算偏张量
  !>
  !> @param[in]  stress 应力张量
  !> @param[out] val 矩阵的迹
  !>
  !> @return 矩阵的迹
  !*****************************************************************************
  module procedure Sec_dev_invar_impl
  implicit none
  type(Torch) :: opt_
  real(DP), dimension(3, 3) :: S
  S = opt_%Deviatoric(stress)
  val = sum(S**2) / two
  end procedure Sec_dev_invar_impl
  !*****************************************************************************
  module procedure Trd_dev_invar_impl
  implicit none
  type(Torch) :: opt_
  real(DP), dimension(3, 3) :: S, temp
  ! implementation
  ! deviatoric stress
  S = opt_%Deviatoric(stress)
  temp = matmul(S, matmul(S, S))
  val = opt_%Trace(temp) / three
  end procedure Trd_dev_invar_impl
  !*****************************************************************************
  module procedure Deviatoric_impl
  implicit none
  real(dp), parameter :: delta1(3, 3) = reshape( &
                         [1.D0, 0.D0, 0.D0, &
                          0.D0, 1.D0, 0.D0, &
                          0.D0, 0.D0, 1.D0],[3, 3])
  type(Torch) :: opt_
  real(DP) :: mean
  ! mean is mean stress which must be larger than zero
  mean = opt_%Trace(stress) / three
  tensor = stress - mean * delta1
  end procedure Deviatoric_impl
  !*****************************************************************************
  !> @brief ratio_impl_
  !>
  !> @details 计算偏张量
  !>
  !> @param[in]  stress 应力张量
  !> @param[out] val 矩阵的迹
  !>
  !> @return 矩阵的迹
  !*****************************************************************************
  module procedure Ratio_impl
  ! declaration
  implicit none
  type(Torch) :: opt_
  real(DP), dimension(3, 3) :: S
  real(DP) :: mean
  ! implementation
  mean = opt_%Trace(stress) / three
  IF(ABS(mean) .LT. eps) mean = SIGN(eps, mean)
  ! deviatoric stress
  S = opt_%Deviatoric(stress)
  ! return tensor
  tensor = S / mean
  end procedure Ratio_impl
  !*****************************************************************************
  module procedure Sin3theta_impl
  ! declaration
  implicit none
  type(Torch) :: opt_
  real(DP) :: J2, J3
  ! implementation
  ! compute J2 and J3
  J2 = opt_%Get_J2(stress)
  IF(ABS(J2) .LT. eps) J2 = SIGN(eps, J2)
  J3 = opt_%Get_J3(stress)
  IF(ABS(J3) .LT. eps) J3 = SIGN(eps, J3)
  sin3t = -1.5_dp * DSQRT(3.0_dp) * J3 / (J2**1.5_dp)
  if(ABS(sin3t) .GT. 1.0_dp) sin3t = SIGN(1.0_dp, sin3t)
  end procedure Sin3theta_impl
  !*****************************************************************************
  module procedure Shear_impl
  use Base_config, only: three
  type(Torch) :: opt_
  real(DP) :: J2
  J2 = opt_%Get_J2(stress)
  shear = dsqrt(three * J2)
  end procedure Shear_impl
  !*****************************************************************************
  module procedure Tensor4_ddot_tensor2
  implicit none
  integer :: I, J
  DO J = 1, 3
    DO I = 1, 3
      res(I, J) = SUM(TENSOR4(I, J, :, :) * TENSOR2(:, :))
    ENDDO
  ENDDO
  end procedure Tensor4_ddot_tensor2
  !
  module procedure Tensor2_ddot_tensor4
  integer :: I, J
  DO J = 1, 3
    DO I = 1, 3
      res(I, J) = SUM( TENSOR2(:, :)* TENSOR4(:, :, I, J))
    ENDDO
  ENDDO
  end procedure Tensor2_ddot_tensor4
  !
endsubmodule tensor_opt_impl
