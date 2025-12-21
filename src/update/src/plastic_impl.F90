submodule(plastic_mod) plastic_impl
  use Base_config
  use Material_config
  use tensor_opt_mod
  use elastic_mod
  implicit none
  type(Torch) :: torch_
  type(Elast) :: elast_
  type(plast) :: plast_
contains
  !*****************************************************************************
  !> @brief get_pfsig_impl
  !>
  !> @details 函数详细描述
  !>
  !> @param[in]  stress :current stress tensor
  !> @param[out] pfsig :Return a yield function corresponding to the partial
  !> derivative of stress
  !>
  !> @return 返回值说明
  !*****************************************************************************
  module procedure get_pfsig_impl
  real(DP), dimension(3, 3, 3, 3) :: prpsig
  real(DP) :: mean
  real(DP), dimension(3, 3) :: pfpr, sigma
  integer :: i, j, k, l
  sigma = shvars%get_sigma()
  mean = torch_%Trace(sigma) / 3.0_dp
  do l = 1, 3
    do k = 1, 3
      do j = 1, 3
        do i = 1, 3
          prpsig(i, j, k, l) = DELTA(i, k) * DELTA(j, l) / mean - &
                               sigma(i, j) * DELTA(k, l) / mean**2 / 3.0_dp
        enddo
      enddo
    enddo
  enddo
  !
  pfpr = elast_%Get_pFpr(shvars)
  pfsig = pfpr.ddot.prpsig
  end procedure get_pfsig_impl
  !*****************************************************************************
  !> @brief Get_pgsig_impl
  !>
  !> @details 函数详细描述
  !>
  !> @param[in]  stress :current stress tensor
  !> @param[out] pfsig :Return a yield function corresponding to the partial
  !> derivative of stress
  !>
  !> @return 返回值说明
  !*****************************************************************************
  module procedure Get_pgsig_impl
  real(DP), dimension(3, 3, 3, 3) :: pnpr
  real(DP), dimension(3, 3) :: r, pfpr, norm_dev_pfpr, pApr, pGpA, pgpr
  real(DP), dimension(3, 3) :: dev_pfpr, dev_pgpr, norm_dev_pgpr
  real(DP) :: sin3t, gtheta(3), RM, frmag, abase, A, hardg
  real(DP) :: dpla
  real(DP), dimension(7) :: scalar
  real(DP), dimension(3, 3, 3, 3, 7) :: tensor4

  integer :: i, j, k, l, ip, iq
  !
  sin3t = torch_%Sin3theta(shvars%get_sigma())
  gtheta = elast_%Get_gtheta(shvars)
  RM = torch_%Get_Rm(shvars%get_sigma())
  r = torch_%Get_ratio(shvars%get_sigma())
  pfpr(:, :) = elast_%Get_pFpr(shvars)
  !
  scalar(1) = (1.0_DP + 3.0_DP * sin3t / gtheta(1) * gtheta(2)) &
              * 3.0_DP / 4.0_DP / RM / gtheta(1)
  scalar(2) = 27.0_DP / 4.0_DP / (RM * gtheta(1))**2 * gtheta(2)
  scalar(3) = +9.0_DP / 4.0_DP / RM**3 / gtheta(1) &
              * (-1.0_DP + 9.0_DP * sin3t / gtheta(1) &
                 * (2.0_DP * sin3t / gtheta(1) * gtheta(2)**2 &
                    - gtheta(2) - sin3t * gtheta(3)))
  scalar(4) = 81.0_DP / 4.0_DP / RM**4 / gtheta(1)**2 &
              * (6.0_DP * sin3t / gtheta(1) * gtheta(2)**2 &
                 - 2.0_DP * gtheta(2) - 3.0_DP * sin3t * gtheta(3))
  scalar(5) = 729.0_DP / 4.0_DP / RM**5 / gtheta(1)**3 &
              * (2.0_DP * gtheta(2)**2 - gtheta(1) * gtheta(3))
  scalar(6) = -27.0_DP * sin3t / 2.0_DP / RM**2 / gtheta(1)**2 &
              * (2.D0 / gtheta(1) * gtheta(2)**2 - gtheta(3))
  scalar(7) = -81.0_DP / 2.0_DP / RM**3 / gtheta(1)**2 &
              * (2.0_DP / gtheta(1) * gtheta(2)**2 - gtheta(3))
  do l = 1, 3
    do k = 1, 3
      do j = 1, 3
        do i = 1, 3
          tensor4(i, j, k, l, 1) = &
            (DELTA(I, K) * DELTA(J, L) + DELTA(I, L) * DELTA(J, K))
          tensor4(i, j, k, l, 2) = &
            (R(I, K) * DELTA(J, L) + R(I, L) * DELTA(J, K) &
             + R(L, J) * DELTA(I, K) + R(K, J) * DELTA(I, L))
          tensor4(i, j, k, l, 3) = R(i, j) * R(k, l)
          tensor4(i, j, k, l, 4) = &
            R(i, j) * sum([(R(k, ip) * R(ip, l), ip=1, 3)]) &
            + sum([(R(i, ip) * R(ip, j), ip=1, 3)]) * R(k, l)
          tensor4(i, j, k, l, 5) = &
            sum([(R(k, ip) * R(ip, l), ip=1, 3)]) &
            * sum([(R(i, iq) * R(iq, j), iq=1, 3)])
          tensor4(i, j, k, l, 6) = DELTA(i, j) * R(k, l)
          tensor4(i, j, k, l, 7) = &
            DELTA(i, j) * sum([(R(k, ip) * R(ip, l), ip=1, 3)])
        enddo
      enddo
    enddo
  enddo
  pnpr(:, :, :, :) = sum([(scalar(i) * tensor4(:, :, :, :, i), i=1, 7)])
  dev_pfpr = torch_%Deviatoric(pfpr)
  frmag = torch_%Norm(dev_pfpr)
  norm_dev_pfpr(:, :) = torch_%Normalize(dev_pfpr)
  abase = elast_%Get_anisotropy(shvars)
  A = -PARAM%KH * (abase - 1)**2
  pApr(:, :) = ((shvars%get_fabric() - abase * norm_dev_pfpr) / frmag) .ddot.pnpr
  hardg = RM / gtheta(1) / exp(A)
  pGpA = 2.0_DP * PARAM%KH * hardg * (abase - 1.0_DP) * exp(A)
  pgpr = pfpr + pGpA * pApr
  dpla = plast_%Get_dilatancy(shvars, stvars)
  dev_pgpr = torch_%Deviatoric(pgpr)
  norm_dev_pgpr = torch_%Normalize(dev_pgpr)
  pgsig = norm_dev_pgpr + dsqrt(2.0_DP / 3.0_DP) * dpla * DELTA(:, :) / 3.0_DP
  end procedure Get_pgsig_impl
  !*****************************************************************************
  !> @brief Get_psim_impl
  !>
  !> @details 函数详细描述
  !>
  !> @param[in] sigma : current stress tensor
  !> @param[in] fabric : current fabric tensor
  !> @param[in]  voidr : current void ratio
  !> @param[out] voidc : psim
  !>
  !> @return 返回值说明
  !*****************************************************************************
  module procedure Get_psim_impl
  real(DP) :: e_c, mean, psi, abase
  mean = torch_%Trace(shvars%get_sigma()) / 3.0_DP
  abase = elast_%Get_anisotropy(shvars)
  e_c = PARAM%VOIDC - PARAM%LAC * (mean / PA)**PARAM%KSI
  psi = stvars%get_voidr() - e_c
  psim = psi - PARAM%EA * (abase - 1.0_DP)
  end procedure Get_psim_impl
  !*****************************************************************************
  !> @brief Get_dilatancy_impl
  !>
  !> @details 函数详细描述
  !>
  !> @param[in] sigma : current stress tensor
  !> @param[in] fabric : current fabric tensor
  !> @param[in]  voidr : current void ratio
  !> @param[out] voidc : psim
  !>
  !> @return 返回值说明
  !*****************************************************************************
  module procedure Get_dilatancy_impl
  real(DP) :: psim, M_d, gtheta(3), RM
  !
  RM = torch_%Get_Rm(shvars%get_sigma())
  gtheta = elast_%Get_gtheta(shvars)
  psim = plast_%Get_psim(shvars, stvars)
  M_d = PARAM%MCS * gtheta(1) * exp(PARAM%DM * psim)
  dpla = PARAM%D1 / PARAM%MCS / gtheta(1) &
         * (1.0_DP + RM / PARAM%MCS / gtheta(1)) * (M_d - RM)
  end procedure Get_dilatancy_impl
  !*****************************************************************************
  !> @brief Get_dilatancy_impl
  !>
  !> @details 函数详细描述
  !>
  !> @param[in] sigma : current stress tensor
  !> @param[in] fabric : current fabric tensor
  !> @param[in]  voidr : current void ratio
  !> @param[out] voidc : psim
  !>
  !> @return 返回值说明
  !*****************************************************************************
  module procedure Get_evolution_impl
  real(DP) :: psim, gtheta(3), RM
  real(DP) :: M_p, abase, frmag, shear
  real(DP), dimension(3, 3) :: norm_dev_pfpr, pfpr
  !-----------------------------------------------------------------------------
  pfpr = elast_%Get_pFpr(shvars)
  frmag = torch_%Norm(pfpr)
  norm_dev_pfpr = torch_%Normalize(torch_%Deviatoric(pfpr))
  ! shear modulus
  shear = elast_%Get_shear(shvars, stvars)
  ! an
  abase = elast_%Get_anisotropy(shvars)
  RM = torch_%Get_Rm(shvars%get_sigma())
  gtheta = elast_%Get_gtheta(shvars)
  psim = plast_%Get_psim(shvars, stvars)
  M_p = PARAM%MCS * gtheta(1) * exp(-PARAM%NKP * psim)
  !
  Rh = frmag * shear * (1.0_DP - PARAM%CH * stvars%get_voidr()) / RM * (M_p - RM)
  RF = PARAM%FEVR * (norm_dev_pfpr(:, :) - shvars%get_fabric())
  end procedure Get_evolution_impl
  !*****************************************************************************
  module procedure Get_Dkp_impl
  real(DP) :: mean, Rh, Rf(3, 3)
  !-----------------------------------------------------------------------------
  mean = torch_%Trace(shvars%get_sigma()) / 3.0_DP
  call plast_%Get_evolution(shvars, stvars, Rh, Rf)
  Dkp = mean * Rh
  end procedure Get_Dkp_impl
  !*****************************************************************************
  module procedure Elstop_impl
  real(DP), dimension(3, 3) :: pfsig, xm, theta
  real(DP), dimension(3, 3, 3, 3) :: stiff
  real(DP) :: dnmetr, Dkp, frnde, lamda, hdl, Rf(3, 3), Rh
  !-----------------------------------------------------------------------------
  pfsig = plast_%Get_pfsig(shvars)
  xm = plast_%Get_pgsig(shvars, stvars)
  Dkp = plast_%Get_Dkp(shvars, stvars)
  stiff = elast_%Get_stiffness(shvars, stvars)
  dnmetr = sum(pfsig * (stiff.ddot.xm))
  frnde = dnmetr + Dkp
  if(abs(frnde) <= EPS) frnde = sign(frnde, EPS)
  !
  theta = (pfsig.ddot.stiff) / frnde
  !
  lamda = sum(theta * depsln)
  ! hdl = lamda > 0.0_DP ? 1.0_DP : 0.0_DP
  hdl = merge(1.0_DP, 0.0_DP, lamda > 0.0_DP)
  dempx = stiff - hdl * ((stiff.ddot.xm) .dyad.theta)
  !
  call plast_%Get_evolution(shvars, stvars, Rh, Rf)
  Rshvars = Share_var(hdl * lamda * Rh, dempx.ddot.depsln, hdl * lamda * Rf)
  end procedure Elstop_impl
  !*****************************************************************************
  !
!*******************************************************************************
endsubmodule plastic_impl
