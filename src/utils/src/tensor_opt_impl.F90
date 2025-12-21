!*******************************************************************************
!> @brief tensor_torch_mod
!>
!> @details Module for tensor operations in continuum mechanics
!>
!> @author wuwenhao
!> @date 2025/11/27
!*******************************************************************************
submodule(tensor_opt_mod) tensor_torch_impl
  use Base_config
  implicit none
  type(Torch) :: torch_
contains
  !*****************************************************************************
  !> @brief Tensor4_ddot_tensor2
  !>
  !> @details Calculate the double dot product of a fourth-order tensor and
  !> a second-order tensor
  !> @param[in]  tensor4  Fourth-order tensor
  !> @param[in]  tensor2  Second-order tensor
  !>
  !> @return Resulting second-order tensor
  !*****************************************************************************
  module procedure Tensor4_ddot_tensor2
  integer :: i, j
  do j = 1, 3
    do i = 1, 3
      res(i, j) = sum(tensor4(i, j, :, :) * tensor2(:, :))
    enddo
  enddo
  end procedure Tensor4_ddot_tensor2
  !*****************************************************************************
  !> @brief Tensor2_ddot_tensor4
  !>
  !> @details Calculate the double dot product of a second-order tensor and
  !> a fourth-order tensor
  !> @param[in]  tensor2  Second-order tensor
  !> @param[in]  tensor4  Fourth-order tensor
  !>
  !> @return Resulting second-order tensor
  !*****************************************************************************
  module procedure Tensor2_ddot_tensor4
  integer :: i, j
  do j = 1, 3
    do i = 1, 3
      res(i, j) = sum(tensor2(:, :) * tensor4(:, :, i, j))
    enddo
  enddo
  end procedure Tensor2_ddot_tensor4
  !*****************************************************************************
  module procedure Tensor2_dyad_tensor2
  integer :: i, j, k, l
  DO l = 1, 3
    DO k = 1, 3
      DO j = 1, 3
        DO i = 1, 3
          res(i, j, k, l) = tensorA(i, j) * tensorB(k, l)
        ENDDO
      ENDDO
    ENDDO
  ENDDO
  end procedure Tensor2_dyad_tensor2
  !*****************************************************************************
  !> @brief Print_Impl
  !>
  !> @details Print a stress tensor
  !>
  !> @param[in]  stress  Stress tensor to print
  !
  !*****************************************************************************
  module procedure Print_impl
  integer :: i, j
  ! declaration
  do i = 1, 3
    write(6, '(A, I1, A)', advance='no') "Row ", i, ": ["
    do j = 1, 3
      write(6, '(F6.2)', advance='no') tensor(i, j)
      if(j < 3) write(6, '(A)', advance='no') ", "
    enddo
    write(6, '(A)') "]"
  enddo
  end procedure Print_impl
  !*****************************************************************************
  !> @brief Trace_Impl
  !>
  !> @details Calculate the trace of a stress tensor
  !>
  !> @param[in]  stress  Stress tensor
  !>
  !> @return Trace of the stress tensor
  !*****************************************************************************
  module procedure Trace_impl
  integer :: i
  !-----------------------------------------------------------------------------
  val = sum([(tensor(i, i), i=1, 3)])
  end procedure Trace_impl
  !*****************************************************************************
  !> @brief Deviatoric_impl
  !>
  !> @details Calculate the deviatoric part of a stress tensor
  !>
  !> @param[in]  stress  Stress tensor
  !>
  !> @return Deviatoric stress tensor
  !*****************************************************************************
  module procedure Deviatoric_impl
  real(DP) :: mean
  !-----------------------------------------------------------------------------
  mean = torch_%Trace(tensor) / 3.0_DP
  res = tensor - mean * DELTA
  end procedure Deviatoric_impl
  !*****************************************************************************
  !> @brief Sec_dev_invar_impl
  !>
  !> @details Calculate the second deviatoric invariant (J2) of a stress tensor
  !>
  !> @param[in]  stress  Stress tensor
  !>
  !> @return Second deviatoric invariant (J2)
  !*****************************************************************************
  module procedure Get_J2_impl
  real(DP), dimension(3, 3) :: dev_tensor
  !-----------------------------------------------------------------------------
  dev_tensor = torch_%Deviatoric(tensor)
  val = sum(dev_tensor**2) / 2.0_DP
  end procedure Get_J2_impl
  !*****************************************************************************
  !> @brief Trd_dev_invar_impl
  !>
  !> @details Calculate the third deviatoric invariant (J3) of a stress tensor
  !>
  !> @param[in]  stress  Stress tensor
  !>
  !> @return Third deviatoric invariant (J3)
  !*****************************************************************************
  module procedure Get_J3_impl
  real(DP), dimension(3, 3) :: dev_tensor, temp
  !-----------------------------------------------------------------------------
  dev_tensor = torch_%Deviatoric(tensor)
  temp = matmul(dev_tensor, matmul(dev_tensor, dev_tensor))
  val = torch_%Trace(temp) / 3.0_DP
  end procedure Get_J3_impl
  !*****************************************************************************
  !> @brief Ratio_impl
  !>
  !> @details Calculate the stress ratio tensor (deviatoric stress divided by
  !> mean stress)
  !> @param[in]  stress  Stress tensor
  !>
  !> @return Stress ratio tensor
  !*****************************************************************************
  module procedure Ratio_impl
  ! declaration
  real(DP), dimension(3, 3) :: dev_tensor
  real(DP) :: mean
  ! implementation
  mean = torch_%Trace(tensor) / 3.0_DP
  IF(ABS(mean) .LT. eps) mean = SIGN(eps, mean)
  ! deviatoric stress
  dev_tensor = torch_%Deviatoric(tensor)
  ! return tensor
  res = dev_tensor / mean
  end procedure Ratio_impl
  !*****************************************************************************
  !> @brief Get_sin3t_impl
  !>
  !> @details Calculate sin(3θ) where θ is the Lode angle
  !>
  !> @param[in]  stress  Stress tensor
  !>
  !> @return sin(3θ) value
  !*****************************************************************************
  module procedure Get_sin3t_impl
  ! declaration
  real(DP) :: J2, J3
  ! implementation
  ! compute J2 and J3
  J2 = torch_%Get_J2(tensor)
  IF(ABS(J2) .LT. eps) J2 = SIGN(eps, J2)
  J3 = torch_%Get_J3(tensor)
  val = -1.5_dp * DSQRT(3.0_dp) * J3 / (J2**1.5_dp)
  if(ABS(val) .GT. 1.0_dp) val = SIGN(1.0_dp, val)
  end procedure Get_sin3t_impl
  !*****************************************************************************
  !> @brief Shear_impl
  !>
  !> @details Calculate the shear stress (sqrt(J2))
  !>
  !> @param[in]  stress  Stress tensor
  !>
  !> @return Shear stress value
  !*****************************************************************************
  module procedure Shear_impl
  real(DP) :: J2
  J2 = torch_%Get_J2(tensor)
  res = dsqrt(three * J2)
  end procedure Shear_impl
  !*****************************************************************************
  !> @brief Normalize_impl
  !>
  !> @details Calculate the normalize of the tensor
  !>
  !> @param[in]  a 3x3 size of tensor
  !>
  !> @return Normalized tensor
  !*****************************************************************************
  module procedure Normalize_impl
  real(DP) :: norm
  norm = sum(tensor**2)
  norm = max(dsqrt(norm), eps)
  res(:, :) = tensor(:, :) / norm
  end procedure Normalize_impl
  !*****************************************************************************
  !> @brief Normalize_impl
  !>
  !> @details Calculate the normalize of the tensor
  !>
  !> @param[in]  a 3x3 size of tensor
  !>
  !> @return the norm of tensor
  !*****************************************************************************
  module procedure Norm_impl
  real(DP) :: temp
  temp = sum(tensor**2)
  res = max(dsqrt(temp), 0.0_DP)
  end procedure Norm_impl
  !*****************************************************************************
  module procedure Get_cost_impl
  real(DP) :: norm_A, norm_B, dot_product
  !
  norm_A = torch_%norm(tensorA)
  norm_A = max(norm_A, EPS)
  norm_B = torch_%Norm(tensorB)
  norm_B = max(norm_B, EPS)
  dot_product = sum(tensorA * tensorB)
  !
  val = dot_product / (norm_A * norm_B)
  if(abs(val) >= 1.0_dp) val = sign(1.0_dp, val)
  end procedure Get_cost_impl
  !*****************************************************************************
  module procedure Get_Rm_impl
  real(DP), dimension(3, 3) :: ratio
  ratio = torch_%Get_ratio(tensor)
  val = dsqrt(3.0_DP * torch_%Get_J2(ratio))
  end procedure Get_Rm_impl
!*******************************************************************************
endsubmodule tensor_torch_impl
