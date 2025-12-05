!*****************************************************************************
!> @brief material_config
!>
!> @details 该模块为材料常数模块,包括以下的参数
!> (1) G0 : shear modulus constant
!> (2) NU : Possion's ratio
!> (3) C  : The ratio between the critical state stress ratio in triaxial
!> extension M_e and that in triaxial compression M_c
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
