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
  public :: pa, pi, eps, delta, zero, one, two, three, four
  public :: dp, sp, I4, I8
  public :: index_t, char_t, data_t
  public :: tensno
  integer, parameter :: dp = real64 ! double precision
  integer, parameter :: sp = real32 ! single precision
  integer, parameter :: I4 = int32
  integer, parameter :: I8 = int64
  integer(int8), parameter :: char_t = int8
  integer(I4), parameter :: index_t = I4
  integer(I4), parameter :: data_t = dp

  ! constants
  real(dp), parameter :: pa = 101.325d0 ! atmospheric pressure, unit kPa
  real(dp), parameter :: pi = 3.1415927D0 ! circular constant
  real(dp), parameter :: eps = 1.0d-12 ! a small number
  real(dp), parameter :: zero = 0.0_dp ! zero value
  real(dp), parameter :: one = 1.0_dp
  real(dp), parameter :: two = 2.0_dp
  real(dp), parameter :: three = 3.0_dp
  real(dp), parameter :: four = 4.0_dp
  real(dp), parameter :: delta(3, 3) = reshape( &
                         [1.D0, 0.D0, 0.D0, &
                          0.D0, 1.D0, 0.D0, &
                          0.D0, 0.D0, 1.D0],[3, 3]) ! Kronecker delta
  real(dp), parameter :: tensno = 1.0d-2
contains
endmodule Base_config ! module base_config
