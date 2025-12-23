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
  module procedure Get_pfpr_impl
  real(DP) :: sin3t, gtheta(3), RM, frmag, abase, A, hardg, pfpA
  real(DP), dimension(3, 3) :: Fr, r, dnorm, pApr, r_squared
  real(DP), dimension(3, 3, 3, 3) :: pnpr
  integer :: i, j, k, l
  real(DP), dimension(7) :: scalar
  real(DP), dimension(3, 3, 3, 3, 7) :: tensor4
  !-----------------------------------------------------------------------------
  Fr = elast_%Get_Fr(shvars)
  !
  sin3t = torch_%Sin3theta(shvars%get_sigma())
  gtheta = elast_%Get_gtheta(shvars)
  RM = torch_%Get_Rm(shvars%get_sigma())
  r = torch_%Get_ratio(shvars%get_sigma())
  r_squared = matmul(r, r)
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

        enddo
      enddo
    enddo
  enddo
  tensor4(i, j, k, l, 3) = R(i, j) * R(k, l)
  tensor4(:, :, :, :, 4) = &
    2.0 * R(:, :) .dyad.r_squared
  tensor4(:, :, :, :, 5) = r_squared.dyad.r_squared
  tensor4(:, :, :, :, 6) = DELTA.dyad.R
  tensor4(:, :, :, :, 7) = DELTA(:, :) .dyad.r_squared
  !
  pnpr(:, :, :, :) = sum([(scalar(i) * tensor4(:, :, :, :, i), i=1, 7)])
  frmag = torch_%Norm(Fr)
  dnorm = elast_%Get_dnorm(shvars)
  abase = elast_%Get_anisotropy(shvars)
  pApr(:, :) = ((shvars%get_fabric() - abase * dnorm) / frmag) .ddot.pnpr
  A = -PARAM%KH * (abase - 1)**2
  hardg = RM / gtheta(1) / exp(A)
  pfpA = 2.0_DP * PARAM%KH * hardg * (abase - 1.0_DP) * exp(A)
  res = Fr + pfpA * pApr
  end procedure Get_pfpr_impl
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
  pfpr = plast_%Get_pfpr(shvars)
  res = pfpr.ddot.prpsig
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
  real(DP), dimension(3, 3) :: pgpr, dev_pgpr, norm_dev_pgpr
  real(DP) :: dpla
  !
  pgpr = plast_%Get_pfpr(shvars)
  !
  dpla = plast_%Get_dilatancy(shvars, stvars)
  dev_pgpr = torch_%Deviatoric(pgpr)
  norm_dev_pgpr = torch_%Normalize(dev_pgpr)
  res = norm_dev_pgpr + dsqrt(2.0_DP / 3.0_DP) * dpla * DELTA / 3.0_DP
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
  real(DP) :: e_c, P, psi, abase
  P = torch_%Trace(shvars%get_sigma()) / 3.0_DP
  abase = elast_%Get_anisotropy(shvars)
  e_c = PARAM%VOIDC - PARAM%LAC * (P / PA)**PARAM%KSI
  psi = stvars%get_voidr() - e_c
  res = psi - PARAM%EA * (abase - 1.0_DP)
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
  res = PARAM%D1 / (PARAM%MCS * gtheta(1)) &
        * (1.0_DP + RM / (PARAM%MCS * gtheta(1))) * (M_d - RM)
  end procedure Get_dilatancy_impl
  !*****************************************************************************
  !> @brief Get_evolution_impl
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
  real(DP) :: psim, gtheta(3), RM, P
  real(DP) :: M_p, abase, G
  real(DP), dimension(3, 3) :: dnorm, Fr
  !-----------------------------------------------------------------------------
  Fr = elast_%Get_Fr(shvars)
  dnorm = elast_%Get_dnorm(shvars)
  ! shear modulus
  G = elast_%Get_shear(shvars, stvars)
  ! an
  abase = elast_%Get_anisotropy(shvars)
  RM = torch_%Get_Rm(shvars%get_sigma())
  gtheta = elast_%Get_gtheta(shvars)
  psim = plast_%Get_psim(shvars, stvars)
  M_p = PARAM%MCS * gtheta(1) * exp(-PARAM%NKP * psim)
  P = torch_%Trace(shvars%get_sigma()) / 3.0_DP
  !
  Rh = G * (1.0_DP - PARAM%CH * stvars%get_voidr()) * (M_p - RM) / (P * RM)
  RF = PARAM%FEVR * (dnorm - shvars%get_fabric())
  end procedure Get_evolution_impl
  !*****************************************************************************
  module procedure Get_Dkp_impl
  real(DP) :: Rh, Rf(3, 3), gtheta(3), RM
  real(DP) :: pfpA, pfph, abase, A, hardg, P
  real(DP), dimension(3, 3) :: Fr, pApF
  !-----------------------------------------------------------------------------
  call plast_%Get_evolution(shvars, stvars, Rh, Rf)
  gtheta = elast_%Get_gtheta(shvars)
  RM = torch_%Get_Rm(shvars%get_sigma())
  abase = elast_%Get_anisotropy(shvars)
  A = -PARAM%KH * (abase - 1)**2
  hardg = RM / gtheta(1) / exp(A)
  pfpA = 2.0_DP * PARAM%KH * hardg * (abase - 1.0_DP) * exp(A)
  Fr = elast_%Get_Fr(shvars)
  pApf = torch_%Get_unit_devivator(Fr)
  pfph = -exp(A)
  P = torch_%Trace(shvars%get_sigma()) / 3.0_DP
  Dkp = -(pfph * Rh + pfpA * sum(pApf * Rf))
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
