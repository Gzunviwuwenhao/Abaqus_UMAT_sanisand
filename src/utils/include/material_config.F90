!*****************************************************************************
!> @brief material_config
!>
!> @details 模块详细描述
!>
!> @author wuwenhao
!> @date 2025/10/24
!*****************************************************************************
module Material_config
  use Base_config, only: DP
  implicit none
  public :: PARAM
  private
  !---------------------------------------------------------------------------
  !> @brief param_
  !>
  !> @details 类型详细描述
  !>
  !> @author wuwenhao
  !> @date 2025/11/27
  !---------------------------------------------------------------------------
  type param_
    real(DP) :: G0, NU
    real(DP) :: C
  endtype param_
  ! initial type
  type(param_), parameter :: PARAM = param_(G0=125, NU=0.2, C=0.75)
contains

endmodule Material_config
