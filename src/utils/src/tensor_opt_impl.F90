!*****************************************************************************
!> @brief tensor_opt_impl
!>
!> @details 模块详细描述
!>
!> @author wuwenhao
!> @date 2025/11/27
!*****************************************************************************
submodule(tensor_opt_mod) tensor_opt_impl
  ! 这是tensor_opt 模块的实现子模块
  ! 可以在实现文件的开头使用 use 语句导入所需模块
  ! 类似于C++ using namespace, 导入子模块不会污染命名空间，这是安全的
  use Base_config, only: dp, data_t, index_t, two, three, eps
  implicit none
contains
  !*****************************************************************************
  !> @brief print_impl_
  !>
  !> @details 打印3x3应力张量的内容
  !>
  !> @param[in] stress 应力张量
  !*****************************************************************************
  module procedure print_impl_
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

  end procedure print_impl_
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
  module procedure trace_impl_
  implicit none
  integer(index_t) :: i
  val = sum([(stress(i, i), i=1, 3)])
  end procedure trace_impl_
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
  module procedure sec_dev_invar_impl_
  implicit none
  type(torch) :: opt_
  real(data_t), dimension(3, 3) :: S
  S = opt_%deviatoric(stress)
  val = sum(S**2) / two
  end procedure sec_dev_invar_impl_
  !*****************************************************************************
  module procedure trd_dev_invar_impl_
  implicit none
  type(torch) :: opt_
  real(data_t), dimension(3, 3) :: S, temp
  ! implementation
  ! deviatoric stress
  S = opt_%deviatoric(stress)
  temp = matmul(S, matmul(S, S))
  val = opt_%trace(temp) / three
  end procedure trd_dev_invar_impl_
  !*****************************************************************************
  module procedure deviatoric_impl_
  implicit none
  real(dp), parameter :: delta1(3, 3) = reshape( &
                         [1.D0, 0.D0, 0.D0, &
                          0.D0, 1.D0, 0.D0, &
                          0.D0, 0.D0, 1.D0],[3, 3])
  type(torch) :: opt_
  real(data_t) :: mean
  ! mean is mean stress which must be larger than zero
  mean = opt_%trace(stress) / three
  tensor = stress - mean * delta1
  end procedure deviatoric_impl_
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
  module procedure ratio_impl_
  ! declaration
  implicit none
  type(torch) :: opt_
  real(data_t), dimension(3, 3) :: S
  real(data_t) :: mean
  ! implementation
  mean = opt_%trace(stress) / three
  IF(ABS(mean) .LT. eps) mean = SIGN(eps, mean)
  ! deviatoric stress
  S = opt_%deviatoric(stress)
  ! return tensor
  tensor = S / mean
  end procedure ratio_impl_
  !*****************************************************************************
  module procedure sin3theta_impl_
  ! declaration
  implicit none
  type(torch) :: opt_
  real(data_t) :: J2, J3
  ! implementation
  ! compute J2 and J3
  J2 = opt_%get_J2(stress)
  IF(ABS(J2) .LT. eps) J2 = SIGN(eps, J2)
  J3 = opt_%get_J3(stress)
  IF(ABS(J3) .LT. eps) J3 = SIGN(eps, J3)
  sin3t = -1.5_dp * DSQRT(3.0_dp) * J3 / (J2**1.5_dp)
  if(ABS(sin3t) .GT. 1.0_dp) sin3t = SIGN(1.0_dp, sin3t)
  end procedure sin3theta_impl_
  !*****************************************************************************
  module procedure shear_impl
  use Base_config, only: three
  type(torch) :: opt_
  real(data_t) :: J2
  J2 = opt_%get_J2(stress)
  shear = dsqrt(three * J2)
  end procedure shear_impl
  !*****************************************************************************
  module procedure tensor4_ddot_tensor2
  implicit none
  integer :: I, J
  DO J = 1, 3
    DO I = 1, 3
      res(I, J) = SUM(TENSOR4(I, J, :, :) * TENSOR2(:, :))
    ENDDO
  ENDDO
  end procedure tensor4_ddot_tensor2
  !
  module procedure tensor2_ddot_tensor4
  integer :: I, J
  DO J = 1, 3
    DO I = 1, 3
      res(I, J) = SUM( TENSOR2(:, :)* TENSOR4(:, :, I, J))
    ENDDO
  ENDDO
  end procedure tensor2_ddot_tensor4
endsubmodule tensor_opt_impl
