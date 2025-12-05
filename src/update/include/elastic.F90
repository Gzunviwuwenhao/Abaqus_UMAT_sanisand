
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
  implicit none
  private
  !---------------------------------------------------------------------------
  !> @brief ela_opt
  !>
  !> @details 类型详细描述
  !>
  !> @author wuwenhao
  !> @date 2025/12/02
  !---------------------------------------------------------------------------
  type, public :: Elast
  contains
    procedure, public, nopass :: Get_gtheta => Get_gtheta_impl
    procedure, public, nopass :: Get_pfratio => Get_pfratio_impl
    procedure, public, nopass :: Yield_distance => yield_distance_impl
    procedure, public, nopass :: Get_stiffness => Get_stiffness_impl
    procedure, public, nopass :: Update_voidr => Update_voidr_impl
  endtype Elast
  !======================================================================
  ! Abstract interface definition (implemented in the sub-module)
  !======================================================================
  interface
    module function Yield_distance_impl(stress, harden) result(distance)
      real(DP), intent(in), dimension(3, 3) :: stress
      real(DP), intent(in) :: harden
      real(DP) :: distance
    endfunction Yield_distance_impl
    !*****************************************************************************
    !> @brief Get_gtheta_impl
    !>
    !> @details 函数详细描述
    !>
    !> @param[in]  参数名 输入参数说明
    !> @param[out] 参数名 输出参数说明
    !>
    !> @return 返回值说明
    !*****************************************************************************
    module subroutine Get_gtheta_impl(stress, gtheta, atheta, sdgth)
      real(DP), dimension(3, 3), intent(in) :: stress
      real(DP) :: gtheta, atheta, sdgth
    endsubroutine Get_gtheta_impl
    !*****************************************************************************
    !> @brief Get_pfratio_impl
    !>
    !> @details 函数详细描述
    !>
    !> @param[in]  参数名 输入参数说明
    !> @param[out] 参数名 输出参数说明
    !>
    !> @return 返回值说明
    !*****************************************************************************
    module function Get_pfratio_impl(stress) result(pfratio)
      real(DP),dimension(3,3),intent(in) :: stress
      real(DP),dimension(3,3) :: pfratio
    end function Get_pfratio_impl
    !*****************************************************************************
    !> @brief get_stiffness_impl
    !>
    !> @details 函数详细描述
    !>
    !> @param[in] stress : current stress tensor(3x3)
    !> @param[in] void_ratio : current void ratio(scalar)
    !> @param[out] stiffness : a stiffness tensor of size 3x3x3x3
    !>
    !> @return 返回值说明
    !*****************************************************************************
    module function Get_stiffness_impl(stress, void_ratio) result(stiffness)
      real(DP), dimension(3, 3), intent(in) :: stress
      real(DP), intent(in) :: void_ratio
      real(DP), dimension(3, 3, 3, 3) :: stiffness
    endfunction Get_stiffness_impl
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
    module function Update_voidr_impl(voidr, dstrain) result(new_voidr)
      real(dp), intent(in) :: voidr
      real(dp), intent(in), dimension(3, 3) :: dstrain
      real(dp) :: new_voidr
    endfunction Update_voidr_impl
  endinterface ! end interface

contains
endmodule elastic_mod
