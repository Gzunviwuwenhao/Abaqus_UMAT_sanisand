!*****************************************************************************
!> @brief elastic_mod
!>
!> @details 模块详细描述
!>
!> @author wuwenhao
!> @date 2025/12/02
!*****************************************************************************
module elastic_mod
  use Base_config, only: DP
  use Container_mod
  implicit none
  private
  !-----------------------------------------------------------------------------
  !> @brief ela_opt
  !>
  !> @details 类型详细描述
  !>
  !> @author wuwenhao
  !> @date 2025/12/02
  !-----------------------------------------------------------------------------
  type, public :: Elast
  contains
    procedure, public, nopass :: Get_gtheta => Get_gtheta_impl
    procedure, public, nopass :: Get_pFpr => Get_pFpr_impl
    procedure, public, nopass :: Get_anisotropy => Get_anisotropy_impl
    procedure, public, nopass :: Get_shear => Get_shear_impl
    procedure, public, nopass :: Get_bulk => Get_bulk_impl
    procedure, public, nopass :: Yield_distance => yield_distance_impl
    procedure, public, nopass :: Get_stiffness => Get_stiffness_impl
    procedure, public, nopass :: Update_voidr => Update_voidr_impl
    procedure, public, nopass :: calc_dsigma => calc_dsigma_impl
  endtype Elast
  !=============================================================================
  ! Abstract interface definition (implemented in the sub-module)
  !=============================================================================
  interface
    !***************************************************************************
    !> @brief Get_gtheta_impl
    !>
    !> @details 函数详细描述
    !>
    !> @param[in]  参数名 输入参数说明
    !> @param[out] 参数名 输出参数说明
    !>
    !> @return 返回值说明
    !***************************************************************************

    module function Get_gtheta_impl(shvars) result(gtheta)
      type(Share_var), intent(in) :: shvars
      real(DP), dimension(3) :: gtheta
    endfunction Get_gtheta_impl
    !***************************************************************************
    !> @brief Get_pfratio_impl
    !>
    !> @details 函数详细描述
    !>
    !> @param[in]  参数名 输入参数说明
    !> @param[out] 参数名 输出参数说明
    !>
    !> @return 返回值说明
    !***************************************************************************
    module function Get_pFpr_impl(shvars) result(pfpr)
      type(Share_var), intent(in) :: shvars
      real(DP), dimension(3, 3) :: pfpr
    endfunction Get_pFpr_impl
    !***************************************************************************
    !> @brief Get_gtheta_impl
    !>
    !> @details 函数详细描述
    !>
    !> @param[in]  参数名 输入参数说明
    !> @param[out] 参数名 输出参数说明
    !>
    !> @return 返回值说明
    !***************************************************************************
    module function Get_anisotropy_impl(shvars) result(abase)
      type(Share_var), intent(in) :: shvars
      real(DP) :: abase
    endfunction Get_anisotropy_impl
    !***************************************************************************
    module function Get_shear_impl(shvars, stvars) result(shear)
      type(Share_var), intent(in) :: shvars
      type(State_var), intent(in) :: stvars
      real(DP) :: shear
    endfunction Get_shear_impl

    !***************************************************************************
    module function Get_bulk_impl(shvars, stvars) result(bulk)
      type(Share_var), intent(in) :: shvars
      type(State_var), intent(in) :: stvars
      real(DP) :: bulk
    endfunction Get_bulk_impl
    !***************************************************************************
    module function Yield_distance_impl(shvars) result(ftol)
      type(Share_var), intent(in) :: shvars
      real(DP) :: ftol
    endfunction Yield_distance_impl

    !***************************************************************************
    !> @brief get_stiffness_impl
    !>
    !> @details 函数详细描述
    !>
    !> @param[in] stress : current stress tensor(3x3)
    !> @param[in] void_ratio : current void ratio(scalar)
    !> @param[out] stiffness : a stiffness tensor of size 3x3x3x3
    !>
    !> @return 返回值说明
    !***************************************************************************
    module function Get_stiffness_impl(shvars, stvars) result(stiffness)
      type(Share_var), intent(in) :: shvars
      type(State_var), intent(in) :: stvars
      real(DP), dimension(3, 3, 3, 3) :: stiffness
    endfunction Get_stiffness_impl
    !***************************************************************************
    !> @brief update_voidr_impl
    !>
    !> @details 函数详细描述
    !>
    !> @param[in] voidr : Current void ratio(scalar)
    !> @param[in] dstrain : Increment of the strain tensor
    !> @param[out] new_voidr : Updated void ratio(scalar)
    !>
    !> @return 返回值说明
    !***************************************************************************
    module function Update_voidr_impl(voidr, dstrain) result(new_voidr)
      real(dp), intent(in) :: voidr
      real(dp), intent(in), dimension(3, 3) :: dstrain
      real(dp) :: new_voidr
    endfunction Update_voidr_impl
    !***************************************************************************
    module function calc_dsigma_impl(shvars, stvars, depsln) result(dsigma)
      type(Share_var), intent(in) :: shvars
      type(State_var), intent(in) :: stvars
      real(DP), dimension(3, 3), intent(in) :: depsln
      real(DP), dimension(3, 3) :: dsigma
    endfunction calc_dsigma_impl
    ! end interface
  endinterface
contains
!*******************************************************************************
endmodule elastic_mod
