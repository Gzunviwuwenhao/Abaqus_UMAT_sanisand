!*******************************************************************************
!> @brief plastic_mod
!>
!> @details 模块详细描述
!>
!> @author wuwenhao
!> @date 2025/12/05
!*******************************************************************************
module plastic_mod
  use Base_config
  use share_vars
  implicit none
  private
  type, public :: plast
  contains
    procedure, public, nopass :: Get_pfsig => Get_pfsig_impl
    procedure, public, nopass :: Get_pgsig => Get_pgsig_impl
    procedure, public, nopass :: Get_psim => Get_psim_impl
    procedure, public, nopass :: Get_dilatancy => Get_dilatancy_impl
    procedure, public, nopass :: Get_evolution => Get_evolution_impl
    procedure, public, nopass :: Get_Dkp => Get_Dkp_impl
    ! procedure, public, nopass :: Get_lamda => Get_lamda_impl
  endtype plast
  !
  interface
    !***************************************************************************
    !> @brief get_pfsig_impl
    !>
    !> @details 函数详细描述
    !>
    !> @param[in]  stress :current stress tensor
    !> @param[out] pfsig :Return a yield function corresponding to the partial
    !> derivative of stress
    !>
    !> @return 返回值说明
    !***************************************************************************
    module function Get_pfsig_impl(shvars) result(pfsig)
      implicit none
      type(Share_var) :: shvars
      real(DP), dimension(3, 3) :: pfsig
    endfunction Get_pfsig_impl
    !***************************************************************************
    !> @brief Get_pgsig_impl
    !>
    !> @details 函数详细描述
    !>
    !> @param[in]  stress :current stress tensor
    !> @param[out] pfsig :Return a yield function corresponding to the partial
    !> derivative of stress
    !>
    !> @return 返回值说明
    !***************************************************************************
    module function Get_pgsig_impl(shvars, voidr) result(xm)
      implicit none
      type(Share_var) :: shvars
      real(DP), intent(in) :: voidr
      real(DP), dimension(3, 3) :: xm
    endfunction Get_pgsig_impl
    !***************************************************************************
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
    !***************************************************************************
    module function Get_psim_impl(shvars, voidr) result(psim)
      implicit none
      ! input
      type(Share_var) :: shvars
      real(DP), intent(in) :: voidr
      ! output
      real(DP) :: psim
    endfunction Get_psim_impl
    !***************************************************************************
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
    !***************************************************************************
    module function Get_dilatancy_impl(shvars, voidr) result(dpla)
      type(Share_var) :: shvars
      real(DP), intent(in) :: voidr
      real(DP) :: dpla
    endfunction Get_dilatancy_impl
    !***************************************************************************
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
    !***************************************************************************
    module subroutine Get_evolution_impl(shvars, voidr, Rh, RF)
      type(Share_var) :: shvars
      real(DP), intent(in) :: voidr
      real(DP) :: Rh, RF(3, 3)
    endsubroutine Get_evolution_impl
  endinterface ! end interface
contains
  function Get_Dkp_impl(shvars, voidr) result(Dkp)
      type(Share_var) :: shvars
    real(DP), intent(in) :: voidr
    real(DP) :: Dkp, RH, RF(3, 3)
    type(plast) plast_
    !
    call plast_%Get_evolution(shvars, voidr, RH, RF)
    Dkp = RH
  endfunction Get_Dkp_impl
endmodule plastic_mod
