submodule(elastic_mod) elastic_impl
  use Base_config, only: DP, DELTA, EPS, PA
  use Material_config, only: PARAM
  implicit none

contains
!*****************************************************************************
!> @brief 函数简要说明
!>
!> @details 函数详细描述
!>
!> @param[in]  参数名 输入参数说明
!> @param[out] 参数名 输出参数说明
!>
!> @return 返回值说明
!*****************************************************************************
  module procedure Get_gtheta_impl
  use tensor_opt_mod
  implicit none
  type(Torch) :: torch_
  real(DP) :: gtheta1, sin3t
  !
  sin3t = torch_%Sin3theta(stress)
  gtheta1 = (-dsqrt((1.0_DP + PARAM%C**2)**2 + 4.0_dp * PARAM%C * &
            (1.0_DP - PARAM%C**2) * sin3t) - (1.0_DP + PARAM%C**2)) / 2.0_DP / &
            (1.0_DP - PARAM%C) / sin3t
  gtheta = -PARAM%C * (1.0_DP + PARAM%C) / (1.0_DP - PARAM%C) / sin3t / gtheta1
  IF((gtheta) .GE. 1.0D0) gtheta = 1.0D0
  IF((gtheta) .LE. PARAM%C) gtheta = PARAM%C
  atheta = (PARAM%C - 1.0_DP) * gtheta**2 / (2.0_DP * (1.0_DP - PARAM%C) &
                                       * sin3t * gtheta + (1.0_DP + PARAM%C**2))
  IF(ABS(atheta) .LT. EPS) atheta = SIGN(EPS, atheta)
  sdgth = (4.0_DP*(PARAM%C - 1.0_DP)*gtheta*atheta + 2.0_DP*(PARAM%C - 1.0_DP) &
           * sin3t * atheta) / (2.0_DP * (1.0_DP - PARAM%C) * sin3t * gtheta + &
                                (1.0_DP + PARAM%C**2))
  IF(ABS(sdgth) .LT. EPS) sdgth = SIGN(EPS, sdgth)
  end procedure Get_gtheta_impl
!*****************************************************************************
!> @brief 函数简要说明
!>
!> @details 函数详细描述
!>
!> @param[in]  参数名 输入参数说明
!> @param[out] 参数名 输出参数说明
!>
!> @return 返回值说明
!*****************************************************************************
  module procedure Get_pfratio_impl
  use tensor_opt_mod
  real(DP) :: rm, gtheta, atheta, sdgth, sin3t
  real(DP), dimension(3, 3) :: ratio
  integer :: i
  type(Torch) :: torch_
  type(Elast) :: elast_
  call elast_%Get_gtheta(stress, gtheta, atheta, sdgth)
  sin3t = torch_%Sin3theta(stress)
  ratio = torch_%Get_ratio(stress)
  rm = dsqrt(3.0_DP * torch_%Get_J2(stress))
  pfratio(:, :) = 1.5_DP / (rm * gtheta)**2 * &
                 ((rm * gtheta + 3.0_DP * rm * sin3t * atheta) * ratio(:, :) + &
                 9.0_DP * atheta * (sum([(ratio(:, i) * ratio(i, :), i=1, 3)])))
  end procedure Get_pfratio_impl
!*****************************************************************************
!> @brief 函数简要说明
!>
!> @details 函数详细描述
!>
!> @param[in]  参数名 输入参数说明
!> @param[out] 参数名 输出参数说明
!>
!> @return 返回值说明
!*****************************************************************************
  module procedure Yield_distance_impl
  use tensor_opt_mod
  type(Torch) :: torch_
  type(elast) :: elast_
  real(DP) :: mean, RM, gtheta, atheta, sdgth
  ! implementation
  ! check the input variable
  mean = torch_%Trace(stress) / 3.0_DP
  IF(ABS(mean) .LT. eps) mean = SIGN(eps, mean)
  !
  RM = torch_%Shear(stress) / mean
  call elast_%Get_gtheta(stress, gtheta, atheta, sdgth)
  distance = RM / gtheta - harden
  end procedure Yield_distance_impl
  !*****************************************************************************
  !> @brief get_stiffness_impl
  !>
  !> @details 函数详细描述
  !>
  !> @param[in] stress : current stress tensor(3x3)
  !> @param[in] void_ratio : current void ratio(scalar)
  !> @param[out] stiffness : a stiffness tensor of size 3x3x3x3
  !>
  !> @return 返回值说明
  !*****************************************************************************
  module procedure get_stiffness_impl
  use tensor_opt_mod
  implicit none
  type(Torch) :: torch_
  real(DP) :: shear, bulk, mean
  integer :: i, j, k, l
  ! mean effective stress
  mean = torch_%Trace(stress)
  ! shear modulus
shear = PARAM%G0*(2.973_dp - void_ratio)**2/(1.0_DP + void_ratio)/dsqrt(mean*PA)
  ! bulk modulus
  bulk = shear * 2.0_DP * (1.0_DP + PARAM%NU) / 3.0_DP / (1.0_DP - 2.0_DP * PARAM%NU)
  ! stiffness tensor
  do l = 1, 3
    do k = 1, 3
      do j = 1, 3
        do i = 1, 3
          stiffness(i, j, k, l) = &
            (bulk - 2.0_DP / 3.0_DP * shear) * DELTA(i, j) * DELTA(k, l) + &
            shear * (DELTA(i, k) * DELTA(j, l) + DELTA(i, l) * DELTA(j, k))
        enddo
      enddo
    enddo
  enddo
  end procedure get_stiffness_impl
  !*****************************************************************************
  !> @brief update_voidr_impl
  !>
  !> @details 函数详细描述
  !>
  !> @param[in] voidr : Current void ratio(scalar)
  !> @param[in] dstrain : Increment of the strain tensor
  !> @param[out] new_voidr : Updated void ratio(scalar)
  !>
  !> @return 返回值说明
  !*****************************************************************************
  module procedure update_voidr_impl
  use tensor_opt_mod
  implicit none
  type(Torch) :: torch_
  real(DP) :: despv
  !
  despv = torch_%Trace(dstrain)
  new_voidr = voidr - (1.0_dp + voidr) * despv
  end procedure update_voidr_impl
endsubmodule elastic_impl
