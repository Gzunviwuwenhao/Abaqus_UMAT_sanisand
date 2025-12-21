!*******************************************************************************
!> @brief plastic_mod
!>
!> @details 模块详细描述
!>
!> @author wuwenhao
!> @date 2025/12/05
!*******************************************************************************
module plastic_mod
  use Base_config, only: DP
  use Container_mod
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
    procedure, public, nopass :: Elstop => Elstop_impl
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
    module function Get_pfsig_impl(shvars) result(pFsig)
      type(Share_var), intent(in) :: shvars
      real(DP), dimension(3, 3) :: pFsig
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
    module function Get_pgsig_impl(shvars, stvars) result(pgsig)
      implicit none
      type(Share_var), intent(in) :: shvars
      type(State_var), intent(in) :: stvars
      real(DP), dimension(3, 3) :: pgsig
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
    module function Get_psim_impl(shvars, stvars) result(psim)
      ! input
      type(Share_var), intent(in) :: shvars
      type(State_var), intent(in) :: stvars
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
    module function Get_dilatancy_impl(shvars, stvars) result(dpla)
      type(Share_var), intent(in) :: shvars
      type(State_var), intent(in) :: stvars
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
    module subroutine Get_evolution_impl(shvars, stvars, Rh, RF)
      type(Share_var), intent(in) :: shvars
      type(State_var), intent(in) :: stvars
      real(DP) :: Rh, RF(3, 3)
    endsubroutine Get_evolution_impl
    !
    module function Get_Dkp_impl(shvars, stvars) result(Dkp)
      type(Share_var), intent(in) :: shvars
      type(State_var), intent(in) :: stvars
      real(DP) :: Dkp
    endfunction Get_Dkp_impl
    !
    module subroutine Elstop_impl(shvars, stvars, depsln, Rshvars, dempx)
      type(Share_var), intent(in) :: shvars
      type(State_var), intent(in) :: stvars
      real(DP), dimension(3, 3), intent(in) :: depsln
      type(Share_var), intent(out) :: Rshvars
      real(DP), dimension(3, 3, 3, 3), intent(out) :: dempx
    endsubroutine Elstop_impl
    !
  endinterface ! end interface
contains
!*******************************************************************************
endmodule plastic_mod
