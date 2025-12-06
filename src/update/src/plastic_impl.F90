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
  real(DP), dimension(3, 3, 3, 3) :: prpsigma
  real(DP) :: mean
  real(DP), dimension(3, 3) :: pfratio
  integer :: i, j, k, l

  mean = torch_%Trace(shvars%sigma_(:,:)) / 3.0_dp
  do l = 1, 3
    do k = 1, 3
      do j = 1, 3
        do i = 1, 3
          prpsigma(i, j, k, l) = DELTA(i, k) * DELTA(j, l) / mean - &
                                 shvars%sigma_(i, j) * DELTA(k, l) / mean**2 / 3.0_dp
        enddo
      enddo
    enddo
  enddo
  !
  pfratio(:, :) = elast_%Get_pFpr(shvars)
  pfsig(:, :) = pfratio.ddot.prpsigma
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
  real(DP), dimension(3, 3) :: R, pfratio, dnorm, pApr, pGpA, pgpr, EP
  real(DP) :: sin3t, gtheta, atheta, sdgth, rm, frmag, abase, A, hardg
  real(DP) :: dpla
  real(DP), dimension(7) :: scalar
  real(DP), dimension(3, 3, 3, 3, 7) :: tensor4

  integer :: i, j, k, l, ip, iq
  !
  sin3t = torch_%Sin3theta(shvars%sigma_)
  call elast_%Get_gtheta(shvars, gtheta, atheta, sdgth)
  rm = dsqrt(3.0_DP * torch_%Get_J2(shvars%sigma_))
  R(:, :) = torch_%Get_ratio(shvars%sigma_)
  pfratio(:, :) = elast_%Get_pFpr(shvars)
  !
  scalar(1) = 3.0_DP / 4.0_DP / rm / gtheta * (1.0_DP + 3.0_DP * sin3t / gtheta * atheta)
  scalar(2) = 27.0_DP / 4.0_DP / rm**2 / gtheta**2 * atheta
  scalar(3) = +9.0_DP / 4.0_DP / rm**3 / gtheta * (-1.0_DP + 9.0_DP * sin3t / gtheta * (2.0_DP * sin3t / gtheta * atheta**2 - atheta - sin3t * sdgth))
  scalar(4) = 81.0_DP / 4.0_DP / rm**4 / gtheta**2 * (6.0_DP * sin3t / gtheta * atheta**2 - 2.0_DP * atheta - 3.0_DP * sin3t * sdgth)
  scalar(5) = 729.0_DP / 4.0_DP / rm**5 / gtheta**3 * (2.0_DP * atheta**2 - gtheta * sdgth)
  scalar(6) = -27.0_DP * sin3t / 2.0_DP / rm**2 / gtheta**2 * (2.D0 / gtheta * atheta**2 - sdgth)
  scalar(7) = -81.0_DP / 2.0_DP / rm**3 / gtheta**2 * (2.0_DP / gtheta * atheta**2 - sdgth)
  do l = 1, 3
    do k = 1, 3
      do j = 1, 3
        do i = 1, 3
          tensor4(i, j, k, l, 1) = (DELTA(I, K) * DELTA(J, L) + DELTA(I, L) * DELTA(J, K))
          tensor4(i, j, k, l, 2) = (R(I, K) * DELTA(J, L) + R(I, L) * DELTA(J, K) + R(L, J) * DELTA(I, K) + R(K, J) * DELTA(I, L))
          tensor4(i, j, k, l, 3) = R(i, j) * R(k, l)
          tensor4(i, j, k, l, 4) = R(i, j) * sum([(R(k, ip) * R(ip, l), ip=1, 3)]) + sum([(R(i, ip) * R(ip, j), ip=1, 3)])
          tensor4(i, j, k, l, 5) = sum([(R(k, ip) * R(ip, l), ip=1, 3)]) * sum([(R(i, iq) * R(iq, j), iq=1, 3)])
          tensor4(i, j, k, l, 6) = DELTA(i, j) * R(k, l)
          tensor4(i, j, k, l, 7) = DELTA(i, j) * sum([(R(k, ip) * R(ip, l), ip=1, 3)])
        enddo
      enddo
    enddo
  enddo
  pnpr(:, :, :, :) = sum([(scalar(i) * tensor4(:, :, :, :, i), i=1, 7)])
  frmag = torch_%Norm(torch_%Deviatoric(pfratio))
  dnorm(:, :) = torch_%Normalize(torch_%Deviatoric(pfratio))
  abase = elast_%Get_anisotropy(shvars)
  A = -PARAM%KH * (abase - 1)**2
  pApr(:, :) = ((shvars%fabric_(:, :) - abase * dnorm(:, :)) / frmag) .ddot.pnpr(:, :, :, :)
  hardg = rm / gtheta / exp(A)
  pGpA = 2.0_DP * PARAM%KH * hardg * (abase - 1.0_DP) * exp(A)
  pgpr = pfratio(:, :) + pGpA * pApr(:, :)
  dpla = plast_%Get_dilatancy(shvars, voidr)

  ep = torch_%Normalize(torch_%Deviatoric(pgpr))
  xm = ep(:, :) + dsqrt(2.0_DP / 3.0_DP) * dpla * DELTA(:, :) / 3.0_DP
  end procedure Get_pgsig_impl
  !
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
  mean = torch_%Trace(shvars%sigma_) / 3.0_DP
  abase = elast_%Get_anisotropy(shvars)
  e_c = PARAM%VOIDC - PARAM%LAC * (mean / PA)**PARAM%KSI
  psi = voidr - e_c
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
  real(DP) :: psim, M_d, gtheta, atheta, sdgth, Rm
  !
  Rm = dsqrt(3.0_DP * torch_%Get_J2(shvars%sigma_))
  call elast_%Get_gtheta(shvars, gtheta, atheta, sdgth)
  psim = plast_%Get_psim(shvars, voidr)
  M_d = PARAM%MCS * gtheta * exp(PARAM%DM * psim)
  dpla = PARAM%D1 / PARAM%MCS / gtheta * (1.0_DP + Rm / PARAM%MCS / gtheta) * (M_d - Rm)
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
  real(DP) :: psim, M_d, gtheta, atheta, sdgth, Rm
  real(DP) :: M_p, abase, frmag, shear, mean
  real(DP), dimension(3, 3) :: dnorm, pfratio
  pfratio(:, :) = elast_%Get_pFpr(shvars)
  frmag = torch_%Norm(pfratio)
  dnorm = torch_%Normalize(torch_%Deviatoric(pfratio))
  ! mean effective stress
  mean = torch_%Trace(shvars%sigma_)
  ! shear modulus
  shear = PARAM%G0 * (2.973_dp - voidr)**2 / (1.0_DP + voidr) / dsqrt(mean * PA)
  ! an
  abase = elast_%Get_anisotropy(shvars)
  Rm = dsqrt(3.0_DP * torch_%Get_J2(shvars%sigma_))
  call elast_%Get_gtheta(shvars, gtheta, atheta, sdgth)
  psim = plast_%Get_psim(shvars, voidr)
  M_p = PARAM%MCS * gtheta * exp(-PARAM%NKP * psim)
  Rh = frmag * shear * (1.0_DP - PARAM%CH * voidr) / Rm * (M_p - Rm)
  RF(:,:) = PARAM%FEVR*exp(abase)*(dnorm(:,:) - shvars%fabric_(:,:))
  end procedure Get_evolution_impl
endsubmodule plastic_impl
