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
    implicit none
    real(dp),dimension(3,3,3,3) :: prpsigma
    real(dp) :: mean
    integer :: i,j,k,l
    type(torch) :: torch_
    mean = torch_%trace(sigma)/3.0_dp
    do l = 1, 3
      do k = 1, 3
        do j = 1, 3
          do i = 1, 3
            prpsigma(i, j, k, l) = delta(i, k)*delta(j, l)/mean - &
                                 sigma(i, j)*delta(k, l)/mean**2/3.0_dp
          end do
        end do
      end do
    end do
    !
    ! pfsig(:,:) =
  end procedure get_pfsig_impl
endsubmodule plastic_impl
