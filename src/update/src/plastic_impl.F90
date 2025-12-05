submodule(plastic_mod) plastic_impl
  use Base_config
  use Material_config
  implicit none

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
  use tensor_opt_mod
  use elastic_mod
  real(DP), dimension(3, 3, 3, 3) :: prpsigma
  real(DP) :: mean
  real(DP), dimension(3, 3) :: pfratio
  integer :: i, j, k, l
  type(Torch) :: torch_
  type(Elast) :: elast_
  mean = torch_%Trace(sigma) / 3.0_dp
  do l = 1, 3
    do k = 1, 3
      do j = 1, 3
        do i = 1, 3
          prpsigma(i, j, k, l) = DELTA(i, k) * DELTA(j, l) / mean - &
                                 sigma(i, j) * DELTA(k, l) / mean**2 / 3.0_dp
        enddo
      enddo
    enddo
  enddo
  !
  pfratio(:, :) = elast_%Get_pfratio(sigma)
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
  use tensor_opt_mod
  use elastic_mod
  real(DP), dimension(3, 3, 3, 3) :: pnpr
  real(DP), dimension(3, 3) :: R, pfratio, dnorm, papr
  real(DP) :: sin3t, gtheta, atheta, sdgth, rm, frmag
  real(DP), dimension(7) :: temp_scalar

  type(Torch) :: torch_
  type(Elast) :: elast_
  integer :: i, j, k, l, ip, iq
  !
  sin3t = torch_%Sin3theta(sigma)
  call elast_%Get_gtheta(sigma, gtheta, atheta, sdgth)
  rm = dsqrt(3.0_DP * torch_%Get_J2(sigma))
  R = torch_%Get_ratio(sigma)
  !
  temp_scalar(1) = 3.0_DP/4.0_DP/rm/gtheta*(1.0_DP + 3.0_DP*sin3t/gtheta*atheta)
  temp_scalar(2) = 27.0_DP / 4.0_DP / rm**2 / gtheta**2 * atheta
  temp_scalar(3) = +9.0_DP / 4.0_DP / rm**3 / gtheta &
                    *(-1.0_DP + 9.0_DP*sin3t/gtheta*(2.0_DP*sin3t/gtheta*atheta**2 - atheta - sin3t*sdgth))
  temp_scalar(4) = 81.0_DP / 4.0_DP / rm**4 / gtheta**2 &
   *(6.0_DP * sin3t / gtheta * atheta**2 - 2.0_DP * atheta - 3.0_DP * sin3t * sdgth)
  temp_scalar(5) = 729.0_DP/4.0_DP/rm**5/gtheta**3*(2.0_DP*atheta**2 - gtheta*sdgth)
  temp_scalar(6) = - 27.0_DP*sin3t/2.0_DP/rm**2/gtheta**2*(2.D0/gtheta*atheta**2 - sdgth)
  temp_scalar(7) = - 81.0_DP/2.0_DP/rm**3/gtheta**2*(2.0_DP/gtheta*atheta**2 - sdgth)
  !
  do l = 1, 3
    do k = 1, 3
      do j = 1, 3
        do i = 1, 3
          pnpr(i, j, k, l) = &
      temp_scalar(1) * (DELTA(I, K) * DELTA(J, L) + DELTA(I, L) * DELTA(J, K)) &
            + temp_scalar(2) * (R(I, K) * DELTA(J, L) + R(I, L) * DELTA(J, K) &
                              + R(L, J) * DELTA(I, K) + R(K, J) * DELTA(I, L)) &
            + temp_scalar(3) * R(i, j) * R(k, l) &
           + temp_scalar(4) * (R(i, j) * sum([(R(k, ip) * R(ip, l), ip=1, 3)]) &
                            + sum([(R(i, ip) * R(ip, j), ip=1, 3)])) * R(k, l) &
              +temp_scalar(5) *sum([(R(k,ip)*R(ip,l), ip=1,3)])*sum([(R(i,iq)*R(iq,j), iq=1,3)]) &
            + temp_scalar(6) * DELTA(i, j) * R(k, l) + &
           +temp_scalar(7) * DELTA(i, j) * sum([(R(k, ip) * R(ip, l), ip=1, 3)])
        enddo
      enddo
    enddo
  enddo
  pfratio(:, :) = elast_%Get_pfratio(sigma)
  frmag = sum((torch_%Deviatoric(pfratio))**2)
  dnorm(:, :) = torch_%Normalize(torch_%Deviatoric(pfratio))
papr(:,:) = (fabric(:,:)- sum(fabric*dnorm)*dnorm(:,:)).ddot.pnpr(:,:,:,:)/frmag
  end procedure Get_pgsig_impl
endsubmodule plastic_impl
