!*****************************************************************************
!> @brief material_config
!>
!> @details 模块详细描述
!>
!> @author wuwenhao
!> @date 2025/10/24
!*****************************************************************************
module Material_config
  use Base_config, only: data_t
  implicit none
  public :: param_
  private
  ! Module variables
  !---------------------------------------------------------------------------
  !> @brief param
  !>
  !> @details 类型详细描述
  !>
  !> @author wuwenhao
  !> @date 2025/11/27
  !---------------------------------------------------------------------------
  type param
    real(data_t) :: G0, nu
    real(data_t) :: c
  endtype param

  type(param), parameter :: param_ = param(G0=125, nu=0.2, c=0.75)
contains

endmodule Material_config
