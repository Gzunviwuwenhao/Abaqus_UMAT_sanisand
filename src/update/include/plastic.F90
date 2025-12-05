!*****************************************************************************
!> @brief plastic_mod
!>
!> @details 模块详细描述
!>
!> @author wuwenhao
!> @date 2025/12/05
!*****************************************************************************
module plastic_mod

  implicit none
  private
  type, public :: plast
  contains
    procedure, public, nopass :: Get_pfsig => Get_pfsig_impl
    procedure, public, nopass :: Get_voidc => Voidc_impl
  endtype plast
  !
  interface
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
    module function Get_pfsig_impl(sigma) result(pfsig)
      use Base_config
      implicit none
      real(dp), dimension(3, 3), intent(in) :: sigma
      real(dp) :: pfsig
    endfunction Get_pfsig_impl
    !*****************************************************************************
    !> @brief voidc_impl
    !>
    !> @details 函数详细描述
    !>
    !> @param[in]  sigma : current stress tensor
    !> @param[in]  voidr : current void ratio
    !> @param[out] voidc : critial state void ratio
    !>
    !> @return 返回值说明
    !*****************************************************************************
    module function Voidc_impl(sigma,voidr) result(voidc)
      use Base_config
      implicit none
      ! input
      real(dp),dimension(3,3),intent(in) :: sigma
      real(dp),intent(in) :: voidr
      ! output
      real(dp) :: voidc
    end function Voidc_impl
  endinterface ! end interface
contains

endmodule plastic_mod
