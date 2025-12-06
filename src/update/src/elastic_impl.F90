submodule(elastic_mod) elastic_impl
  use Base_config, only: DP, DELTA, EPS, PA
  use Material_config, only: PARAM
  use tensor_opt_mod
  implicit none
  type(Torch) torch_
  type(Elast) elast_
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
  real(DP) :: gtheta1, sin3t
  !
  sin3t = torch_%Sin3theta(shvars%sigma_(:,:))
  gtheta1 = (-dsqrt((1.0_DP + PARAM%C**2)**2 + 4.0_dp * PARAM%C * &
                    (1.0_DP - PARAM%C**2) * sin3t) - (1.0_DP + PARAM%C**2)) / 2.0_DP / &
            (1.0_DP - PARAM%C) / sin3t
  gtheta = -PARAM%C * (1.0_DP + PARAM%C) / (1.0_DP - PARAM%C) / sin3t / gtheta1
  IF((gtheta) .GE. 1.0D0) gtheta = 1.0D0
  IF((gtheta) .LE. PARAM%C) gtheta = PARAM%C
  atheta = (PARAM%C - 1.0_DP) * gtheta**2 / (2.0_DP * (1.0_DP - PARAM%C) &
                                             * sin3t * gtheta + (1.0_DP + PARAM%C**2))
  IF(ABS(atheta) .LT. EPS) atheta = SIGN(EPS, atheta)
  sdgth = (4.0_DP * (PARAM%C - 1.0_DP) * gtheta * atheta + 2.0_DP * (PARAM%C - 1.0_DP) &
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
  module procedure Get_pFpr_impl
  real(DP) :: rm, gtheta, atheta, sdgth, sin3t
  real(DP), dimension(3, 3) :: ratio
  integer :: i
  call elast_%Get_gtheta(shvars, gtheta, atheta, sdgth)
  sin3t = torch_%Sin3theta(shvars%sigma_(:,:))
  ratio = torch_%Get_ratio(shvars%sigma_(:,:))
  rm = dsqrt(3.0_DP * torch_%Get_J2(shvars%sigma_(:,:)))
  pfratio(:, :) = 1.5_DP / (rm * gtheta)**2 * &
                  ((rm * gtheta + 3.0_DP * rm * sin3t * atheta) * ratio(:, :) + &
                   9.0_DP * atheta * (sum([(ratio(:, i) * ratio(i, :), i=1, 3)])))
  end procedure Get_pFpr_impl
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
  real(DP) :: mean, RM, gtheta, atheta, sdgth
  ! implementation
  ! check the input variable
  mean = torch_%Trace(shvars%sigma_) / 3.0_DP
  IF(ABS(mean) .LT. eps) mean = SIGN(eps, mean)
  !
  RM = torch_%Shear(shvars%sigma_) / mean
  call elast_%Get_gtheta(shvars, gtheta, atheta, sdgth)
  ftol = RM / gtheta - shvars%harden_
  end procedure Yield_distance_impl
  !*****************************************************************************
  !> @brief get_stiffness_impl
  !>
  !> @details 函数详细描述
  !>
  !> @param[in] shvars%sigma_(:,:) : current shvars%sigma_(:,:) tensor(3x3)
  !> @param[in] void_ratio : current void ratio(scalar)
  !> @param[out] stiffness : a stiffness tensor of size 3x3x3x3
  !>
  !> @return 返回值说明
  !*****************************************************************************
  module procedure get_stiffness_impl
  real(DP) :: shear, bulk, mean
  integer :: i, j, k, l
  ! mean effective shvars%sigma_(:,:)
  mean = torch_%Trace(shvars%sigma_(:,:))
  ! shear modulus
  shear = PARAM%G0 * (2.973_dp - voidr)**2 / (1.0_DP + voidr) / dsqrt(mean * PA)
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
  real(DP) :: despv
  !
  despv = torch_%Trace(dstrain)
  new_voidr = voidr - (1.0_dp + voidr) * despv
  end procedure update_voidr_impl
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
  module procedure Get_anisotropy_impl
  real(DP), dimension(3, 3) :: pFpr, dnorm
  pFpr(:, :) = elast_%Get_pFpr(shvars)
  dnorm(:, :) = torch_%Normalize(torch_%Deviatoric(pFpr(:, :)))
  abase = sum(dnorm(:, :) * shvars%fabric_(:, :))
  abase = max(min(abase, 1.0_DP), -1.0_DP)
  end procedure Get_anisotropy_impl
endsubmodule elastic_impl
