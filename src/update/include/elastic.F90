
!*****************************************************************************
!> @brief elastic_mod
!>
!> @details 模块详细描述
!>
!> @author wuwenhao
!> @date 2025/12/02
!*****************************************************************************
module elastic_mod
  implicit none
  private
  ! Type definitions
  !---------------------------------------------------------------------------
  !> @brief ela_opt
  !>
  !> @details 类型详细描述
  !>
  !> @author wuwenhao
  !> @date 2025/12/02
  !---------------------------------------------------------------------------
  type, public :: elast
  contains
    procedure, public, nopass :: interpolate => interpolate_impl
    procedure, public, nopass :: isyield => isyield_impl
    procedure, public, nopass :: get_stiffness => get_stiffness_impl
    procedure, public, nopass :: update_voidr => update_voidr_impl
  endtype elast
  !======================================================================
  ! 抽象接口定义（在子模块中实现）
  !======================================================================
  interface
    module function isyield_impl(stress, harden) result(distance)
      use Base_config, only: data_t, dp
      implicit none
      real(data_t), intent(in), dimension(3, 3) :: stress
      real(data_t), intent(in) :: harden
      real(dp) :: distance
    endfunction isyield_impl
    !*****************************************************************************
    !> @brief interpolate_impl
    !>
    !> @details 函数详细描述
    !>
    !> @param[in]  参数名 输入参数说明
    !> @param[out] 参数名 输出参数说明
    !>
    !> @return 返回值说明
    !*****************************************************************************
    module function interpolate_impl(stress) result(gtheta)
      use Base_config, only: data_t
      implicit none
      real(data_t), dimension(3, 3), intent(in) :: stress
      real(data_t) :: gtheta
    endfunction interpolate_impl
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
    module function get_stiffness_impl(stress, void_ratio) result(stiffness)
      use Base_config, only: data_t
      implicit none
      real(data_t), dimension(3, 3), intent(in) :: stress
      real(data_t), intent(in) :: void_ratio
      real(data_t), dimension(3, 3, 3, 3) :: stiffness
    endfunction get_stiffness_impl
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
    module function update_voidr_impl(voidr, dstrain) result(new_voidr)
      use Base_config
      implicit none
      real(dp),intent(in) :: voidr
      real(dp),intent(in),dimension(3,3) :: dstrain
      real(dp):: new_voidr
    end function update_voidr_impl
  endinterface ! end interface

contains
endmodule elastic_mod
