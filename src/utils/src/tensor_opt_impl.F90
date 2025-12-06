!*****************************************************************************
!> @brief tensor_torch_mod
!>
!> @details Module for tensor operations in continuum mechanics
!>
!> @author wuwenhao
!> @date 2025/11/27
!*****************************************************************************
submodule(tensor_opt_mod) tensor_torch_impl
  use Base_config, only: DP, TWO, THREE, EPS, DELTA
  implicit none
contains
  !***************************************************************************
  !> @brief Print_Impl
  !>
  !> @details Print a stress tensor
  !>
  !> @param[in]  stress  Stress tensor to print
  !
  !***************************************************************************
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
  !***************************************************************************
  !> @brief Trace_Impl
  !>
  !> @details Calculate the trace of a stress tensor
  !>
  !> @param[in]  stress  Stress tensor
  !>
  !> @return Trace of the stress tensor
  !***************************************************************************
  module procedure Trace_impl
  implicit none
  integer :: i
  val = sum([(stress(i, i), i=1, 3)])
  end procedure Trace_impl
  !***************************************************************************
  !> @brief Sec_dev_invar_impl
  !>
  !> @details Calculate the second deviatoric invariant (J2) of a stress tensor
  !>
  !> @param[in]  stress  Stress tensor
  !>
  !> @return Second deviatoric invariant (J2)
  !***************************************************************************
  module procedure Sec_dev_invar_impl
  implicit none
  type(Torch) :: torch_
  real(DP), dimension(3, 3) :: S
  S = torch_%Deviatoric(stress)
  val = sum(S**2) / two
  end procedure Sec_dev_invar_impl
  !***************************************************************************
  !> @brief Trd_dev_invar_impl
  !>
  !> @details Calculate the third deviatoric invariant (J3) of a stress tensor
  !>
  !> @param[in]  stress  Stress tensor
  !>
  !> @return Third deviatoric invariant (J3)
  !***************************************************************************
  module procedure Trd_dev_invar_impl
  implicit none
  type(Torch) :: torch_
  real(DP), dimension(3, 3) :: S, temp
  ! implementation
  ! deviatoric stress
  S = torch_%Deviatoric(stress)
  temp = matmul(S, matmul(S, S))
  val = torch_%Trace(temp) / three
  end procedure Trd_dev_invar_impl
  !***************************************************************************
  !> @brief Deviatoric_impl
  !>
  !> @details Calculate the deviatoric part of a stress tensor
  !>
  !> @param[in]  stress  Stress tensor
  !>
  !> @return Deviatoric stress tensor
  !***************************************************************************
  module procedure Deviatoric_impl
  implicit none
  type(Torch) :: torch_
  real(DP) :: mean
  ! mean is mean stress which must be larger than zero
  mean = torch_%Trace(stress) / three
  tensor = stress - mean * DELTA
  end procedure Deviatoric_impl
  !***************************************************************************
  !> @brief Ratio_impl
  !>
  !> @details Calculate the stress ratio tensor (deviatoric stress divided by
  !> mean stress)
  !> @param[in]  stress  Stress tensor
  !>
  !> @return Stress ratio tensor
  !***************************************************************************
  module procedure Ratio_impl
  ! declaration
  implicit none
  type(Torch) :: torch_
  real(DP), dimension(3, 3) :: S
  real(DP) :: mean
  ! implementation
  mean = torch_%Trace(stress) / three
  IF(ABS(mean) .LT. eps) mean = SIGN(eps, mean)
  ! deviatoric stress
  S = torch_%Deviatoric(stress)
  ! return tensor
  tensor = S / mean
  end procedure Ratio_impl
  !***************************************************************************
  !> @brief Sin3theta_impl
  !>
  !> @details Calculate sin(3θ) where θ is the Lode angle
  !>
  !> @param[in]  stress  Stress tensor
  !>
  !> @return sin(3θ) value
  !***************************************************************************
  module procedure Sin3theta_impl
  ! declaration
  implicit none
  type(Torch) :: torch_
  real(DP) :: J2, J3
  ! implementation
  ! compute J2 and J3
  J2 = torch_%Get_J2(stress)
  IF(ABS(J2) .LT. eps) J2 = SIGN(eps, J2)
  J3 = torch_%Get_J3(stress)
  IF(ABS(J3) .LT. eps) J3 = SIGN(eps, J3)
  sin3t = -1.5_dp * DSQRT(3.0_dp) * J3 / (J2**1.5_dp)
  if(ABS(sin3t) .GT. 1.0_dp) sin3t = SIGN(1.0_dp, sin3t)
  end procedure Sin3theta_impl
  !***************************************************************************
  !> @brief Shear_impl
  !>
  !> @details Calculate the shear stress (sqrt(J2))
  !>
  !> @param[in]  stress  Stress tensor
  !>
  !> @return Shear stress value
  !***************************************************************************
  module procedure Shear_impl
  use Base_config, only: three
  type(Torch) :: torch_
  real(DP) :: J2
  J2 = torch_%Get_J2(stress)
  shear = dsqrt(three * J2)
  end procedure Shear_impl
  !***************************************************************************
  !> @brief Normalize_impl
  !>
  !> @details Calculate the normalize of the tensor
  !>
  !> @param[in]  a 3x3 size of tensor
  !>
  !> @return Normalized tensor
  !***************************************************************************
  module procedure Normalize_impl
  real(DP) :: norm
  norm = sum(tensor**2)
  norm = MAX(DSQRT(norm), EPS)
  res(:, :) = tensor(:, :) / norm
  end procedure Normalize_impl
  !***************************************************************************
  !> @brief Normalize_impl
  !>
  !> @details Calculate the normalize of the tensor
  !>
  !> @param[in]  a 3x3 size of tensor
  !>
  !> @return the norm of tensor
  !***************************************************************************
  module procedure Norm_impl
    real(DP) :: temp
    temp = sum(tensor**2)
    res = max(dsqrt(temp),EPS)
  end procedure Norm_impl
  !***************************************************************************
  !> @brief Tensor4_ddot_tensor2
  !>
  !> @details Calculate the double dot product of a fourth-order tensor and
  !> a second-order tensor
  !> @param[in]  tensor4  Fourth-order tensor
  !> @param[in]  tensor2  Second-order tensor
  !>
  !> @return Resulting second-order tensor
  !***************************************************************************
  module procedure Tensor4_ddot_tensor2
  implicit none
  integer :: I, J
  DO J = 1, 3
    DO I = 1, 3
      res(I, J) = SUM(TENSOR4(I, J, :, :) * TENSOR2(:, :))
    ENDDO
  ENDDO
  end procedure Tensor4_ddot_tensor2
  !***************************************************************************
  !> @brief Tensor2_ddot_tensor4
  !>
  !> @details Calculate the double dot product of a second-order tensor and
  !> a fourth-order tensor
  !> @param[in]  tensor2  Second-order tensor
  !> @param[in]  tensor4  Fourth-order tensor
  !>
  !> @return Resulting second-order tensor
  !***************************************************************************
  module procedure Tensor2_ddot_tensor4
  integer :: I, J
  DO J = 1, 3
    DO I = 1, 3
      res(I, J) = SUM(TENSOR2(:, :) * TENSOR4(:, :, I, J))
    ENDDO
  ENDDO
  end procedure Tensor2_ddot_tensor4
  !
endsubmodule tensor_torch_impl
