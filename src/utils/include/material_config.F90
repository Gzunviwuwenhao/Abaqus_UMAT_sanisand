!*****************************************************************************
!> @brief Material configuration module
!>
!> @details This module defines material constants for the constitutive model
!> including the following parameters:
!> (1) G0 : Shear modulus constant
!> (2) NU : Poisson's ratio
!> (3) C  : Ratio between critical state stress ratio in triaxial extension
!>          M_e and that in triaxial compression M_c
!> @author wuwenhao
!> @date 2025/10/24
!*****************************************************************************
module Material_config
  use Base_config, only: DP
  implicit none
  public :: PARAM
  private
  !---------------------------------------------------------------------------
  !> @brief Parameter container type
  !>
  !> @details Derived type containing all material parameters for the
  !> constitutive model. Includes elastic, plastic, and hardening parameters.
  !>
  !> @author wuwenhao
  !> @date 2025/11/27
  !---------------------------------------------------------------------------
  type param_
    real(DP) :: G0, NU
    real(DP) :: C
    real(DP) :: KH
    real(DP) :: VOIDC, KSI, LAC, MCS, EA
    real(DP) :: D1, DM
    real(DP) :: CH, NKP
    real(DP) :: FEVR, F0
  endtype param_
  ! initial type
  type(param_), parameter :: PARAM = param_(G0=125.0_DP, NU=0.1_DP, C=0.75_DP, &
                                            KH=0.03_DP, KSI=0.7_DP, LAC=0.05_DP, &
                                            MCS=1.25_DP, EA=0.1_DP, VOIDC=0.934_DP, &
                                            D1=0.1_DP, DM=3.5_DP, &
                                            CH=0.9_DP, NKP=3.0_DP, FEVR=5.7_DP, F0=0.45_DP)
contains

endmodule Material_config
