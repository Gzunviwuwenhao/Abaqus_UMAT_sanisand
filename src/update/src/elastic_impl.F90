submodule(elastic_mod) elastic_impl
  use Base_config
  use Material_config
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
  sin3t = torch_%Sin3theta(shvars%get_sigma())
  if(abs(sin3t) .lt. EPS) sin3t = sign(EPS, sin3t)
  gtheta1 = (-dsqrt((1.0_DP + PARAM%C**2)**2 + 4.0_DP * PARAM%C * &
                    (1.0_DP - PARAM%C**2) * sin3t) - (1.0_DP + PARAM%C**2)) &
            / 2.0_DP / (1.0_DP - PARAM%C) / sin3t
  gtheta(1) = -PARAM%C * (1.0_DP + PARAM%C) / (1.0_DP - PARAM%C) / sin3t &
              / gtheta1
  IF((gtheta(1)) .GE. 1.0D0) gtheta(1) = 1.0D0
  IF((gtheta(1)) .LE. PARAM%C) gtheta(1) = PARAM%C
  gtheta(2) = (PARAM%C - 1.0_DP) * gtheta(1)**2 &
              / (2.0_DP * (1.0_DP - PARAM%C) &
                 * sin3t * gtheta(1) + (1.0_DP + PARAM%C**2))
  IF(ABS(gtheta(2)) .LT. EPS) gtheta(2) = SIGN(EPS, gtheta(2))
  gtheta(3) = (4.0_DP * (PARAM%C - 1.0_DP) * gtheta(1) * gtheta(2) &
               + 2.0_DP * (PARAM%C - 1.0_DP) * sin3t * gtheta(2)) &
              / (sin3t * gtheta(1) * 2.0_DP * (1.0_DP - PARAM%C) + (1.0_DP + PARAM%C**2))
  IF(ABS(gtheta(3)) .LT. EPS) gtheta(3) = SIGN(EPS, gtheta(3))
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
  real(DP) :: RM, gtheta(3), sin3t
  real(DP), dimension(3, 3) :: r
  integer :: p
  gtheta = elast_%Get_gtheta(shvars)
  sin3t = torch_%Sin3theta(shvars%get_sigma())
  r = torch_%Get_ratio(shvars%get_sigma())
  RM = torch_%Get_Rm(shvars%get_sigma())
  pfpr = 1.5_DP / (RM * gtheta(1))**2 * &
         ((RM * gtheta(1) + 3.0_DP * RM * sin3t * gtheta(2)) * r(:, :) + &
          9.0_DP * gtheta(2) * (sum([(r(:, p) * r(p, :), p=1, 3)])))
  end procedure Get_pFpr_impl
  !*****************************************************************************
  module procedure Get_anisotropy_impl
  real(DP), dimension(3, 3) :: pFpr, dev_pfpr, norm_dev_pfpr
  pFpr(:, :) = elast_%Get_pFpr(shvars)
  dev_pfpr = torch_%Deviatoric(pFpr(:, :))
  norm_dev_pfpr = torch_%Normalize(dev_pfpr)
  abase = sum(norm_dev_pfpr * shvars%get_fabric())
  abase = max(min(abase, 1.0_DP), -1.0_DP)
  end procedure Get_anisotropy_impl
  !*****************************************************************************
  module procedure Get_shear_impl
  real(DP) :: mean
  ! mean effective shvars%get_sigma()(:,:)
  mean = torch_%Trace(shvars%get_sigma()) / 3.0_DP
  ! shear modulus
  shear = PARAM%G0 * (2.973_DP - stvars%get_voidr())**2 &
          / (1.0_DP + stvars%get_voidr()) * dsqrt(mean * PA)
  end procedure Get_shear_impl
  !*****************************************************************************
  module procedure Get_bulk_impl
  real(DP) :: shear
  shear = elast_%Get_shear(shvars, stvars)
  bulk = shear * 2.0_DP * (1.0_DP + PARAM%NU) / 3.0_DP &
         / (1.0_DP - 2.0_DP * PARAM%NU)
  end procedure Get_bulk_impl
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
  real(DP) :: RM, gtheta(3)
  !
  RM = torch_%Get_Rm(shvars%get_sigma())
  gtheta = elast_%Get_gtheta(shvars)
  ftol = RM / gtheta(1) - shvars%get_harden()
  end procedure Yield_distance_impl
  !*****************************************************************************
  !> @brief get_stiffness_impl
  !>
  !> @details 函数详细描述
  !>
  !> @param[in] shvars%get_sigma()(:,:) : current shvars%get_sigma()(:,:) tensor(3x3)
  !> @param[in] void_ratio : current void ratio(scalar)
  !> @param[out] stiffness : a stiffness tensor of size 3x3x3x3
  !>
  !> @return 返回值说明
  !*****************************************************************************
  module procedure get_stiffness_impl
  real(DP) :: shear, bulk
  integer :: i, j, k, l
  shear = elast_%Get_shear(shvars, stvars)
  bulk = elast_%Get_bulk(shvars, stvars)
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
  module procedure calc_dsigma_impl
  real(DP), dimension(3, 3, 3, 3) :: stiff
  !
  stiff(:, :, :, :) = get_stiffness_impl(shvars, stvars)
  dsigma(:, :) = stiff(:, :, :, :) .ddot.depsln
  end procedure calc_dsigma_impl
!*******************************************************************************
endsubmodule elastic_impl
