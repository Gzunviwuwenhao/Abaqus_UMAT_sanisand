!DIR$ FREEFORM
!*****************************************************************************
!> @brief base_config
!>
!> @details
!>
!> @author wuwenhao
!> @date 2025/11/17
!*****************************************************************************
module Base_config
  use, intrinsic :: iso_fortran_env, only: real32, real64, int32, int64, int8
  implicit none
  private
  public :: PA, PI, EPS, DELTA, ZERO, ONE, TWO, THREE, FOUR
  public :: DP, SP, I4, I8
  public :: TENSNO
  integer, parameter :: DP = real64 ! double precision
  integer, parameter :: SP = real32 ! single precision
  integer, parameter :: I4 = int32
  integer, parameter :: I8 = int64
  !
  integer, parameter :: MAX_INDEX = huge(I4)
  ! constants
  real(DP), parameter :: PA = 101.325_DP ! atmospheric pressure, unit kPa
  real(DP), parameter :: PI = 3.1415927_DP ! circular constant
  real(DP), parameter :: EPS = 1.0D-12 ! a small number
  real(DP), parameter :: ZERO = 0.0_DP
  real(DP), parameter :: ONE = 1.0_DP
  real(DP), parameter :: TWO = 2.0_DP
  real(DP), parameter :: THREE = 3.0_DP
  real(DP), parameter :: FOUR = 4.0_DP
  real(DP), parameter :: DELTA(3, 3) = reshape( &
                         [1.0_DP, 0.0_DP, 0.0_DP, &
                          0.0_DP, 1.0_DP, 0.0_DP, &
                          0.0_DP, 0.0_DP, 1.0_DP],[3, 3]) ! Kronecker delta
  real(DP), parameter :: TENSNO = 1.0D-2
contains

endmodule Base_config ! module base_config
