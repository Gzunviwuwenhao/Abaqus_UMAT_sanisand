
!DIR$ FREEFORM
!*****************************************************************************
!> @brief Base configuration module
!>
!> @details
!> This module defines fundamental constants, precision types, and
!> configuration parameters used throughout the UMAT implementation.
!> It provides standardized precision types (DP, SP), integer types (I4, I8),
!> mathematical constants (PI, EPS), and physical constants (PA).
!> The module also defines tensor-related constants and utility parameters.
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
  public :: TENSNO, MAX_INDEX, MAX_DATA
  !> Precision type definitions
  integer, parameter :: DP = real64 !< Double precision (64-bit real)
  integer, parameter :: SP = real32 !< Single precision (32-bit real)
  integer, parameter :: I4 = int32  !< 32-bit integer
  integer, parameter :: I8 = int64  !< 64-bit integer
  !> Maximum value constants
  integer, parameter :: MAX_INDEX = huge(I4) !< Maximum value for 32-bit integer
  integer, parameter :: MAX_DATA = huge(DP)  !< Maximum value for double precision

  !> Fundamental mathematical and physical constants
  real(DP), parameter :: PA = 101.325_DP   !< Atmospheric pressure (kPa)
  real(DP), parameter :: PI = 3.1415927_DP !< Pi constant (π)
  real(DP), parameter :: EPS = 1.0D-12     !< Small tolerance value for comparisons
  real(DP), parameter :: ZERO = 0.0_DP     !< Zero constant
  real(DP), parameter :: ONE = 1.0_DP      !< One constant
  real(DP), parameter :: TWO = 2.0_DP      !< Two constant
  real(DP), parameter :: THREE = 3.0_DP    !< Three constant
  real(DP), parameter :: FOUR = 4.0_DP     !< Four constant
  !> Kronecker delta tensor (identity matrix for 3D)
  real(DP), parameter :: DELTA(3, 3) = reshape( &
                         [1.0_DP, 0.0_DP, 0.0_DP, &
                          0.0_DP, 1.0_DP, 0.0_DP, &
                          0.0_DP, 0.0_DP, 1.0_DP],[3, 3]) !< 3x3 identity matrix

  !> Small value for tensor operations (tolerance for near-zero tensors)
  real(DP), parameter :: TENSNO = 1.0D-2 !< Tensor near-zero threshold
contains

endmodule Base_config ! module base_config
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
!*****************************************************************************
!> @brief Container module for UMAT state variables
!>
!> @details
!> This module defines container types for storing and managing state
!> variables in ABAQUS UMAT implementations. It provides two main types:
!> 1. Share_var: Contains hardening parameters, stress tensors, and fabric
!>    tensors with associated update operations.
!> 2. State_var: Contains void ratio and time increment parameters for
!>    state-dependent calculations.
!> The module supports arithmetic operations, assignment, and various
!> utility functions for state variable management.
!>
!> @author wuwenhao
!> @date 2025/12/07
!*****************************************************************************
module Container_mod
  use Base_config, only: DP
  implicit none
  private
  ! Type definitions
  type, public :: Share_var
    private
    real(DP) :: harden_
    real(DP), dimension(3, 3) :: sigma_
    real(DP), dimension(3, 3) :: fabric_
    logical :: initialized_ = .false.
    logical :: is_lowstress = .false.
    logical :: is_nan_inf = .false.
  contains
    procedure, public, pass(this) :: get_harden => get_harden_impl
    procedure, public, pass(this) :: get_sigma => get_sigma_impl
    procedure, public, pass(this) :: get_fabric => get_fabric_impl
    procedure, public, pass(this) :: is_low => low_impl
    procedure, public, pass(this) :: update_harden => update_harden_impl
    procedure, public, pass(this) :: update_sigma => update_sigma_impl
    procedure, public, pass(this) :: update_fabric => update_fabric_impl
    procedure, public, pass(this) :: update_shvars => update_shvars_impl
    procedure, public, pass(this) :: changed_harden => changed_harden_impl
    procedure, public, pass(this) :: changed_sigma => changed_sigma_impl
    procedure, public, pass(this) :: changed_fabric => changed_fabric_impl
    procedure, public, pass(this) :: norm => norm_impl
    procedure, public, pass(this) :: print => print_impl
    procedure, private, pass(this) :: jugde_nan_inf_impl
    procedure, pass(this) :: assign => assign_impl
    generic, public :: assignment(=) => assign
    procedure, pass(this) :: binary_add => binary_add_impl
    generic, public :: operator(+) => binary_add
    procedure, pass(this) :: binary_sub => binary_sub_impl
    generic, public :: operator(-) => binary_sub
    procedure, pass(this) :: unary_minus => unary_minus_impl
    generic, public :: operator(-) => unary_minus
    procedure, pass(this) :: unary_lhs_scalar => unary_lhs_scalar_impl
    generic, public :: operator(*) => unary_lhs_scalar
    procedure, pass(this) :: unary_rhs_scalar => unary_rhs_scalar_impl
    generic, public :: operator(*) => unary_rhs_scalar
    procedure, pass(this) :: unary_div_impl => unary_div_impl
    generic, public :: operator(/) => unary_div_impl
  endtype Share_var
  !
  type, public :: State_var
    private
    real(DP) :: voidr_
    real(DP) :: pnewdt_
    logical :: initialized_ = .false.
  contains
    procedure, public, pass(this) :: get_voidr => get_voidr_impl
    procedure, public, pass(this) :: get_pnewdt => get_pnewdt_impl
    procedure, public, pass(this) :: update_voidr => update_voidr_impl
    procedure, public, pass(this) :: changed_voidr => changed_voidr_impl
    procedure, public, pass(this) :: changed_pnewdt => changed_pnewdt_impl
    procedure, pass(this) :: assign => assign_impl_state
    generic, public :: assignment(=) => assign
  endtype State_var
  !
  interface Share_var
    module procedure :: share_construct_param
    module procedure :: Share_construct_zero
  endinterface
  !
  interface State_var
    module procedure :: State_construct_param
  endinterface
  !
  interface
    module function share_construct_param(harden, sigma, fabric) result(this)
      real(DP), intent(in) :: harden
      real(DP), dimension(3, 3), intent(in) :: sigma
      real(DP), dimension(3, 3), intent(in) :: fabric
      type(Share_var) :: this
    endfunction share_construct_param
    !***************************************************************************
    module function Share_construct_zero() result(this)
      type(Share_var) :: this
    endfunction Share_construct_zero
    !***************************************************************************
    module function get_harden_impl(this) result(harden)
      class(Share_var), intent(in) :: this
      real(DP) :: harden
    endfunction get_harden_impl
    !***************************************************************************
    module function get_sigma_impl(this) result(sigma)
      class(Share_var), intent(in) :: this
      real(DP), dimension(3, 3) :: sigma
    endfunction
    !***************************************************************************
    module function get_fabric_impl(this) result(fabric)
      class(Share_var), intent(in) :: this
      real(DP), dimension(3, 3) :: fabric
    endfunction
    module function low_impl(this) result(is_true)
      class(Share_var), intent(in) :: this
      logical :: is_true
    endfunction low_impl
    !***************************************************************************
    module subroutine update_harden_impl(this, harden)
      class(Share_var), intent(inout) :: this
      real(DP), intent(in) :: harden
    endsubroutine update_harden_impl
    !***************************************************************************
    module subroutine update_sigma_impl(this, dsigma)
      class(Share_var), intent(inout) :: this
      real(DP), dimension(3, 3), intent(in) :: dsigma
    endsubroutine update_sigma_impl
    !***************************************************************************
    module subroutine update_fabric_impl(this, fabric)
      class(Share_var), intent(inout) :: this
      real(DP), dimension(3, 3), intent(in) :: fabric
    endsubroutine update_fabric_impl
    !***************************************************************************
    module subroutine update_shvars_impl(dharden, dsigma, dfabric, this)
      real(DP), intent(in) :: dharden
      real(DP), dimension(3, 3), intent(in) :: dsigma
      real(DP), dimension(3, 3), intent(in) :: dfabric
      class(Share_var), intent(inout) :: this
    endsubroutine update_shvars_impl
    !***************************************************************************
    module subroutine changed_harden_impl(this, harden)
      class(Share_var), intent(inout) :: this
      real(DP), intent(in) :: harden
    endsubroutine changed_harden_impl
    !***************************************************************************
    module subroutine changed_sigma_impl(this, sigma)
      class(Share_var), intent(inout) :: this
      real(DP), dimension(3, 3), intent(in) :: sigma
    endsubroutine changed_sigma_impl
    !***************************************************************************
    module subroutine changed_fabric_impl(this, fabric)
      class(Share_var), intent(inout) :: this
      real(DP), dimension(3, 3), intent(in) :: fabric
    endsubroutine changed_fabric_impl
    !***************************************************************************
    module function norm_impl(this) result(res)
      class(Share_var), intent(in) :: this
      real(DP), dimension(3) :: res
    endfunction norm_impl
    !***************************************************************************
    module subroutine print_impl(this)
      class(Share_var), intent(in) :: this
    endsubroutine print_impl
    !***************************************************************************
    module subroutine jugde_nan_inf_impl(this)
      class(Share_var), intent(inout) :: this
    endsubroutine jugde_nan_inf_impl
    !***************************************************************************
    module subroutine assign_impl(this, other)
      class(Share_var), intent(inout) :: this
      type(Share_var), intent(in) :: other
    endsubroutine assign_impl
    !***************************************************************************
    module function binary_add_impl(this, other) result(res)
      class(Share_var), intent(in) :: this
      type(Share_var), intent(in) :: other
      type(Share_var) :: res
    endfunction binary_add_impl
    !***************************************************************************
    module function binary_sub_impl(this, other) result(res)
      class(Share_var), intent(in) :: this
      type(Share_var), intent(in) :: other
      type(Share_var) :: res
    endfunction binary_sub_impl
    !***************************************************************************
    module function unary_minus_impl(this) result(res)
      class(Share_var), intent(in) :: this
      type(Share_var) :: res
    endfunction unary_minus_impl
    !***************************************************************************
    module function unary_lhs_scalar_impl(scalar, this) result(res)
      real(DP), intent(in) :: scalar
      class(Share_var), intent(in) :: this
      type(Share_var) :: res
    endfunction unary_lhs_scalar_impl
    !***************************************************************************
    module function unary_rhs_scalar_impl(this, scalar) result(res)
      class(Share_var), intent(in) :: this
      real(DP), intent(in) :: scalar
      type(Share_var) :: res
    endfunction unary_rhs_scalar_impl
    !***************************************************************************
    module function unary_div_impl(this, scalar) result(res)
      class(Share_var), intent(in) :: this
      real(DP), intent(in) :: scalar
      type(Share_var) :: res
    endfunction unary_div_impl
    ! end interface
  endinterface

  !
  interface
    module function State_construct_param(voidr, pnewdt) result(this)
      real(DP), intent(in) :: voidr
      real(DP), intent(in) :: pnewdt
      type(State_var) :: this
    endfunction State_construct_param
    !***************************************************************************
    module function get_voidr_impl(this) result(voidr)
      class(State_var), intent(in) :: this
      real(DP) :: voidr
    endfunction get_voidr_impl
    module function get_pnewdt_impl(this) result(pnewdt)
      class(State_var), intent(in) :: this
      real(DP) :: pnewdt
    endfunction get_pnewdt_impl
    !***************************************************************************
    module subroutine update_voidr_impl(this, depsln)
      class(State_var), intent(inout) :: this
      real(DP), dimension(3, 3), intent(in) :: depsln
    endsubroutine update_voidr_impl
    !***************************************************************************
    module subroutine changed_voidr_impl(this, voidr)
      class(State_var), intent(inout) :: this
      real(DP), intent(in) :: voidr
    endsubroutine changed_voidr_impl
    !***************************************************************************
    module subroutine changed_pnewdt_impl(this, pnewdt)
      class(State_var), intent(inout) :: this
      real(DP), intent(in) :: pnewdt
    endsubroutine changed_pnewdt_impl
    !***************************************************************************
    module subroutine assign_impl_state(this, other)
      class(State_var), intent(inout) :: this
      type(State_var), intent(in) :: other
    endsubroutine assign_impl_state
  endinterface
contains

endmodule Container_mod
!*****************************************************************************
!> @brief tensor_opt_mod
!>
!> @details Module for tensor operations in continuum mechanics
!>
!> @author wuwenhao
!> @date 2025/11/27
!*****************************************************************************
module tensor_opt_mod
  use Base_config
  implicit none
  public :: operator(.ddot.), operator(.dyad.)
  ! default private
  private
  !---------------------------------------------------------------------------
  !> @brief Torch
  !>
  !> @details Type for tensor operations providing various stress tensor
  !> calculations and utilities
  !>
  !> @author wuwenhao
  !> @date 2025/12/05
  !---------------------------------------------------------------------------
  type, public :: Torch
  contains
    private
    ! static function method
    procedure, public, nopass :: Print => Print_Impl
    procedure, public, nopass :: Trace => Trace_Impl
    procedure, public, nopass :: Deviatoric => Deviatoric_impl
    procedure, public, nopass :: Get_J2 => Get_J2_impl
    procedure, public, nopass :: Get_J3 => Get_J3_impl
    procedure, public, nopass :: Get_ratio => Ratio_impl
    procedure, public, nopass :: Sin3theta => Get_sin3t_impl
    procedure, public, nopass :: Shear => Shear_impl
    procedure, public, nopass :: Normalize => normalize_impl
    procedure, public, nopass :: Norm => Norm_impl
    procedure, public, nopass :: Get_cost => Get_cost_impl
    procedure, public, nopass :: Get_Rm => Get_Rm_impl
    procedure, public, nopass :: Get_unit_devivator => Get_unit_devivator_impl
  endtype Torch
  !
  interface operator(.ddot.)
    module procedure Tensor4_ddot_tensor2
    module procedure Tensor2_ddot_tensor4
  endinterface
  interface operator(.dyad.)
    module procedure Tensor2_dyad_tensor2
  endinterface
  !=============================================================================
  ! Abstract interface definition (implemented in the sub-module)
  !=============================================================================
  interface
    !***************************************************************************
    !> @brief Tensor4_ddot_tensor2
    !>
    !> @details Calculate the double dot product of a fourth-order tensor and
    !> a second-order tensor
    !> @param[in]  tensor4  Fourth-order tensor
    !> @param[in]  tensor2  Second-order tensor
    !>
    !> @return Resulting second-order tensor
    !***************************************************************************
    module function Tensor4_ddot_tensor2(tensor4, tensor2) result(res)
      real(DP), dimension(3, 3, 3, 3), intent(in) :: tensor4
      real(DP), dimension(3, 3), intent(in) :: tensor2
      real(DP), dimension(3, 3) :: res
    endfunction Tensor4_ddot_tensor2
    !***************************************************************************
    !> @brief Tensor2_ddot_tensor4
    !>
    !> @details Calculate the double dot product of a second-order tensor and
    !> a fourth-order tensor
    !> @param[in]  tensor2  Second-order tensor
    !> @param[in]  tensor4  Fourth-order tensor
    !>
    !> @return Resulting second-order tensor
    !***************************************************************************
    module function Tensor2_ddot_tensor4(tensor2, tensor4) result(res)
      real(DP), dimension(3, 3, 3, 3), intent(in) :: tensor4
      real(DP), dimension(3, 3), intent(in) :: tensor2
      real(DP), dimension(3, 3) :: res
    endfunction Tensor2_ddot_tensor4
    !***************************************************************************
    !> @brief Dyadic product of two second-order tensors
    !>
    !> @details Calculate the dyadic (outer) product of two second-order
    !> tensors, resulting in a fourth-order tensor.
    !>
    !> @param[in]  tensorA First second-order tensor
    !> @param[in]  tensorB Second second-order tensor
    !>
    !> @return Fourth-order tensor result
    !***************************************************************************
    module function Tensor2_dyad_tensor2(tensorA, tensorB) result(res)
      real(DP), dimension(3, 3), intent(in) :: tensorA
      real(DP), dimension(3, 3), intent(in) :: tensorB
      real(DP), dimension(3, 3, 3, 3) :: res
    endfunction Tensor2_dyad_tensor2
    !***************************************************************************
    !> @brief Print tensor
    !>
    !> @details Print the components of a 3x3 tensor to standard output
    !> in matrix format for debugging and visualization.
    !>
    !> @param[in]  tensor  3x3 tensor to print
    !***************************************************************************
    module Subroutine Print_Impl(tensor)
      real(DP), intent(in), dimension(3, 3) :: tensor
    endSubroutine
    !***************************************************************************
    !> @brief Trace_Impl
    !>
    !> @details Calculate the trace of a stress tensor
    !>
    !> @param[in]  stress  Stress tensor
    !>
    !> @return Trace of the stress tensor
    !***************************************************************************
    module function Trace_Impl(tensor) result(val)
      real(DP), intent(in), dimension(3, 3) :: tensor
      real(DP) :: val
    endfunction Trace_Impl
    !***************************************************************************
    !> @brief Sec_dev_invar_impl
    !>
    !> @details Calculate the second deviatoric invariant (J2) of a stress tensor
    !>
    !> @param[in]  stress  Stress tensor
    !>
    !> @return Second deviatoric invariant (J2)
    !***************************************************************************
    module function Get_J2_impl(tensor) result(val)
      real(DP), intent(in), dimension(3, 3) :: tensor
      real(DP) :: val
    endfunction Get_J2_impl
    !***************************************************************************
    !> @brief Trd_dev_invar_impl
    !>
    !> @details Calculate the third deviatoric invariant (J3) of a stress tensor
    !>
    !> @param[in]  stress  Stress tensor
    !>
    !> @return Third deviatoric invariant (J3)
    !***************************************************************************
    module function Get_J3_impl(tensor) result(val)
      real(DP), intent(in), dimension(3, 3) :: tensor
      real(DP) :: val
    endfunction Get_J3_impl
    !***************************************************************************
    !> @brief Deviatoric_impl
    !>
    !> @details Calculate the deviatoric part of a stress tensor
    !>
    !> @param[in]  stress  Stress tensor
    !>
    !> @return Deviatoric stress tensor
    !***************************************************************************
    module function Deviatoric_impl(tensor) result(res)
      real(DP), intent(in), dimension(3, 3) :: tensor
      real(DP), dimension(3, 3) :: res
    endfunction Deviatoric_impl
    !***************************************************************************
    !> @brief Ratio_impl
    !>
    !> @details Calculate the stress ratio tensor (deviatoric stress divided by
    !> mean stress)
    !> @param[in]  stress  Stress tensor
    !>
    !> @return Stress ratio tensor
    !***************************************************************************
    module function Ratio_impl(tensor) result(res)
      real(DP), intent(in), dimension(3, 3) :: tensor
      real(DP), dimension(3, 3) :: res
    endfunction Ratio_impl
    !***************************************************************************
    !> @brief Sin3theta_impl
    !>
    !> @details Calculate sin(3θ) where θ is the Lode angle
    !>
    !> @param[in]  stress  Stress tensor
    !>
    !> @return sin(3θ) value
    !***************************************************************************
    module function Get_sin3t_impl(tensor) result(val)
      real(DP), intent(in), dimension(3, 3) :: tensor
      real(DP) :: val
    endfunction Get_sin3t_impl
    !***************************************************************************
    !> @brief Shear_impl
    !>
    !> @details Calculate the shear stress (sqrt(J2))
    !>
    !> @param[in]  stress  Stress tensor
    !>
    !> @return Shear stress value
    !***************************************************************************
    module function Shear_impl(tensor) result(res)
      real(DP), dimension(3, 3), intent(in) :: tensor
      real(DP) :: res
    endfunction Shear_impl
    !***************************************************************************
    !> @brief Normalize tensor
    !>
    !> @details Normalize a tensor by dividing by its Frobenius norm,
    !> resulting in a unit tensor with the same direction.
    !>
    !> @param[in]  tensor  Input 3x3 tensor
    !>
    !> @return Normalized unit tensor
    !***************************************************************************
    module function Normalize_impl(tensor) result(res)
      real(DP), dimension(3, 3), intent(in) :: tensor
      real(DP), dimension(3, 3) :: res
    endfunction Normalize_impl
    !***************************************************************************
    !> @brief Calculate tensor norm
    !>
    !> @details Calculate the Frobenius norm (Euclidean norm) of a 3x3 tensor.
    !>
    !> @param[in]  tensor  Input 3x3 tensor
    !>
    !> @return Frobenius norm of the tensor
    !***************************************************************************
    module function Norm_impl(tensor) result(res)
      real(DP), dimension(3, 3), intent(in) :: tensor
      real(DP) :: res
    endfunction Norm_impl
    !***************************************************************************
    !> @brief Calculate cosine of angle between tensors
    !>
    !> @details Calculate the cosine of the angle between two tensors
    !> using their double dot product and norms.
    !>
    !> @param[in]  tensorA  First 3x3 tensor
    !> @param[in]  tensorB  Second 3x3 tensor
    !>
    !> @return Cosine of angle between tensors
    !***************************************************************************
    module function Get_cost_impl(tensorA, tensorB) result(val)
      real(DP), dimension(3, 3), intent(in) :: tensorA
      real(DP), dimension(3, 3), intent(in) :: tensorB
      real(DP) :: val
    endfunction Get_cost_impl
    !***************************************************************************
    !> @brief Calculate R_m parameter
    !>
    !> @details Calculate the R_m parameter used in critical state soil
    !> mechanics, related to the Lode angle and stress invariants.
    !>
    !> @param[in]  tensor  Stress tensor
    !>
    !> @return R_m parameter value
    !***************************************************************************
    module function Get_Rm_impl(tensor) result(val)
      real(DP), dimension(3, 3), intent(in) :: tensor
      real(DP) :: val
    endfunction Get_Rm_impl
    !***************************************************************************
    !> @brief Calculate unit deviatoric tensor
    !>
    !> @details Calculate the unit deviatoric tensor by normalizing the
    !> deviatoric part of a stress tensor.
    !>
    !> @param[in]  tensor  Input stress tensor
    !>
    !> @return Unit deviatoric tensor
    !***************************************************************************
    module function Get_unit_devivator_impl(tensor) result(res)
      real(DP), dimension(3, 3), intent(in) :: tensor
      real(DP), dimension(3, 3) :: res
    endfunction Get_unit_devivator_impl
  endinterface
contains
!*******************************************************************************
endmodule tensor_opt_mod
!DIR$ FREEFORM
!*****************************************************************************
!> @brief Preprocessing module
!>
!> @details This module provides utilities for data conversion and
!> preprocessing operations, including tensor-array conversions,
!> rotation matrix generation, and debugging utilities.
!>
!> @author wuwenhao
!> @date 2025/11/17
!*****************************************************************************
module presolve_mod
  use Base_config, only: DP
  implicit none
  public :: Convert_array_to_tensor, Convert_tensor_to_array, Print_array
  public :: Convert_tensor4_to_tensor2
  public :: abaqus_debug, Get_rotation_matrix
  private
  ! Public interface
  interface
    !**************************************************************************
    !> @brief Convert array to tensor
    !>
    !> @details Convert a 1D array to a 3x3 2D tensor representation.
    !> Commonly used for converting stress/strain vectors to tensor form.
    !>
    !> @param[in]  array Input vector (size 6 for symmetric tensor)
    !> @param[in]  scalar Optional scaling factor
    !>
    !> @return 3x3 tensor representation
    !**************************************************************************
    module function Convert_array_to_tensor(array, scalar) result(tensor)
      real(DP), intent(in) :: array(:)
      real(DP), intent(in), optional :: scalar
      real(DP), dimension(3, 3) :: tensor
    endfunction Convert_array_to_tensor
    !**************************************************************************
    !> @brief Convert tensor to array
    !>
    !> @details Convert a 3x3 2D tensor to a 1D array representation.
    !> For symmetric tensors, returns Voigt notation vector.
    !>
    !> @param[in]  tensor Input 3x3 tensor
    !> @param[in]  size Output array size (6 for symmetric, 9 for full)
    !> @param[in]  scalar Optional integer parameter
    !>
    !> @return 1D array representation of the tensor
    !**************************************************************************
    module function Convert_tensor_to_array(tensor, size, scalar) result(array)
      real(DP), dimension(3, 3), intent(in) :: tensor
      integer, intent(in) :: size
      integer, intent(in), optional :: scalar
      real(DP), dimension(size) :: array
    endfunction Convert_tensor_to_array
    !**************************************************************************
    !> @brief Convert fourth-order tensor to second-order matrix
    !>
    !> @details Convert a 3x3x3x3 fourth-order tensor to a 2D matrix
    !> representation using Voigt notation.
    !>
    !> @param[in]  tensor4 Input fourth-order tensor (3x3x3x3)
    !> @param[in]  size Output matrix size (6x6 for symmetric)
    !>
    !> @return 2D matrix representation
    !**************************************************************************
    module function Convert_tensor4_to_tensor2(tensor4, size) result(tensor2)
      real(DP), dimension(3, 3, 3, 3), intent(in) :: tensor4
      integer, intent(in) :: size
      real(DP), dimension(size, size) :: tensor2
    endfunction Convert_tensor4_to_tensor2
    !**************************************************************************
    !> @brief Generate rotation matrix
    !>
    !> @details Generate a 3x3 rotation matrix for given angle and axis
    !> using Rodrigues' rotation formula.
    !>
    !> @param[in]  angle Rotation angle in radians
    !> @param[in]  axis  Rotation axis vector (3D)
    !>
    !> @return 3x3 rotation matrix
    !**************************************************************************
    module function Get_rotation_matrix(angle, axis) result(rot_matrix)
      real(DP), intent(in) :: angle
      real(DP), intent(in), dimension(3) :: axis
      real(DP), dimension(3, 3) :: rot_matrix
    endfunction Get_rotation_matrix
    !**************************************************************************
    !> @brief Print array contents
    !>
    !> @details Print the contents of a 1D array to standard output
    !> for debugging purposes.
    !>
    !> @param[in]  array Input array to print
    !**************************************************************************
    module Subroutine Print_array(array)
      real(DP), intent(in), dimension(:) :: array
    endSubroutine
    !**************************************************************************
    !> @brief ABAQUS debugging utility
    !>
    !> @details Print debugging information for ABAQUS UMAT simulations,
    !> including element number, integration point, and iteration count.
    !>
    !> @param[in]  noel_num  Number of elements
    !> @param[in]  npt_num   Number of integration points
    !> @param[in]  num       Additional numerical parameter
    !> @param[in]  noel      Current element number
    !> @param[in]  npt       Current integration point number
    !> @param[in]  iteration Current iteration number
    !> @param[in]  names     Descriptive name for debugging output
    !**************************************************************************
    module subroutine abaqus_debug(noel_num, npt_num, num, noel, npt, iteration, names)
      integer, intent(in) :: noel_num, npt_num, num
      integer, intent(in) :: noel, npt, iteration
      character(len=*), intent(in) :: names
    endsubroutine abaqus_debug
  endinterface ! end interface
contains

endmodule presolve_mod

!*****************************************************************************
!> @brief exception_mod
!>
!> @details 模块详细描述
!>
!> @author wuwenhao
!> @date 2025/11/27
!*****************************************************************************
module exception_mod
  implicit none
  private
  public :: ASSERT_TRUE, ASSERT_EQUAL, ASSERT_FLOAT_EQUAL
  type ErrorContext
    character(len=:), allocatable :: file_
    integer :: line_
    character(len=:), allocatable :: prefix_
  contains
    procedure, public, pass(this) :: throw_error => throw_error_impl
  endtype ErrorContext

  interface ErrorContext
    module procedure create_error_context
  endinterface ErrorContext

  interface
    ! ErrorContext(std::string file, int line, std::string prefix);
    !*****************************************************************************
    !> @brief Create an error context object
    !>
    !> @details Constructs an ErrorContext object with file, line, and optional prefix
    !>
    !> @param[in] file Name of the source file where error occurred
    !> @param[in] line Line number in the source file
    !> @param[in] prefix Optional prefix message for error identification
    !>
    !> @return ErrorContext object containing error location information
    !*****************************************************************************
    module function create_error_context(file, line, prefix) result(this)
      implicit none
      character(len=*), intent(in) :: file
      integer, intent(in) :: line
      character(len=*), intent(in), optional :: prefix
      type(ErrorContext) :: this
    endfunction create_error_context
    !*****************************************************************************
    !> @brief Throw an error with context information
    !>
    !> @details Outputs an error message with file, line, and prefix
    !>          information, then terminates program
    !>
    !> @param[in] msg Error message to display
    !> @param[in] this ErrorContext object containing location information
    !*****************************************************************************
    module subroutine throw_error_impl(msg, this)
      implicit none
      character(len=*), intent(in) :: msg
      class(ErrorContext), intent(in) :: this
    endsubroutine throw_error_impl
    !*****************************************************************************
    !> @brief Assert that a logical expression is true
    !>
    !> @details Checks if the expression is true, if not, throws an error
    !>          with the provided message and location information
    !>
    !> @param[in] expr Logical expression to evaluate
    !> @param[in] msg Error message to display if assertion fails
    !> @param[in] file Name of the source file where assertion is called
    !> @param[in] line Line number in the source file
    !*****************************************************************************
    module subroutine ASSERT_TRUE(expr, msg, file, line)
      logical, intent(in) :: expr
      character(len=*), intent(in) :: msg
      character(len=*), intent(in) :: file
      character(len=:), allocatable :: validator_msg
      integer, intent(in) :: line
    endsubroutine ASSERT_TRUE
  endinterface ! end interface
contains

  !*****************************************************************************
  !> @brief Assert that two integer values are equal
  !>
  !> @details Compares two integer values, if they are not equal, throws an error
  !>          with the provided message and location information
  !>
  !> @param[in] lhs Left-hand side integer value
  !> @param[in] rhs Right-hand side integer value
  !> @param[in] msg Error message to display if assertion fails
  !> @param[in] file Name of the source file where assertion is called
  !> @param[in] line Line number in the source file
  !*****************************************************************************
  subroutine ASSERT_EQUAL(lhs, rhs, msg, file, line)
    implicit none
    ! Input/Output variables
    integer, intent(in) :: lhs, rhs, line
    character(len=*), intent(in) :: msg, file
    character(len=:), allocatable :: validator_msg
    type(ErrorContext) Error
    integer :: msg_len
    !
    ! 分配足够大的缓冲区用于整数输出
    ! I0格式：对于32位整数，最大长度约为11（包括负号）
    msg_len = 50  ! 足够容纳 "Value X/= expected Y"
    allocate(character(len=msg_len) :: validator_msg)
    write(validator_msg, '("Value ",I0, "/= expected ", I0)') &
      lhs, rhs
    if(lhs /= rhs) then
      Error = ErrorContext(file, line, msg)
      call Error%throw_error(validator_msg)
    endif
  endsubroutine ASSERT_EQUAL
  !*****************************************************************************
  !> @brief Assert that two floating-point values are equal within tolerance
  !>
  !> @details Compares two floating-point values, if the absolute difference
  !>          exceeds EPS tolerance, throws an error with the provided message
  !>          and location information
  !>
  !> @param[in] lhs Left-hand side floating-point value
  !> @param[in] rhs Right-hand side floating-point value
  !> @param[in] msg Error message to display if assertion fails
  !> @param[in] file Name of the source file where assertion is called
  !> @param[in] line Line number in the source file
  !*****************************************************************************
  subroutine ASSERT_FLOAT_EQUAL(lhs, rhs, msg, file, line)
    use Base_config, only: DP, EPS
    implicit none
    ! Input/Output variables
    real(DP), intent(in) :: lhs, rhs
    character(len=*), intent(in) :: msg, file
    integer, intent(in) :: line
    character(len=:), allocatable :: validator_msg
    type(ErrorContext) Error
    integer :: msg_len
    ! 分配足够大的缓冲区用于浮点数输出
    ! G0格式可能产生较长的字符串（科学计数法）
    msg_len = 200  ! 足够容纳 "Value X.XXXXXX/=expected X.XXXXXX (tolerance: X.XXXXXX)"
    allocate(character(len=msg_len) :: validator_msg)
    write(validator_msg, '("Value ", G0, "/=expected ", G0," (tolerance: ",G0,")" )') &
      lhs, rhs, EPS
    if(abs(lhs - rhs) > EPS) then
      Error = ErrorContext(file, line, msg)
      call Error%throw_error(validator_msg)
    endif
  endsubroutine ASSERT_FLOAT_EQUAL
endmodule exception_mod
!*****************************************************************************
!> @brief Elastic constitutive model module
!>
!> @details
!> This module implements elastic constitutive relations for soil mechanics
!> within the UMAT framework. It provides functions for calculating elastic
!> properties, stiffness tensors, stress increments, and yield criteria.
!> The module includes methods for principal stress calculation, shear and
!> bulk moduli, anisotropy evaluation, and elastic stress updates.
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
  !> @brief Elastic operations type
  !>
  !> @details
  !> This type encapsulates elastic constitutive operations for soil mechanics.
  !> It provides methods for calculating elastic properties, stiffness tensors,
  !> stress increments, and yield criteria evaluation. All methods are
  !> nopass procedures that operate on input parameters without requiring
  !> type instance data.
  !>
  !> @author wuwenhao
  !> @date 2025/12/02
  !-----------------------------------------------------------------------------
  type, public :: Elast
  contains
    procedure, public, nopass :: Get_principal => Get_principal_impl
    procedure, public, nopass :: Get_gtheta => Get_gtheta_impl
    procedure, public, nopass :: Get_Fr => Get_Fr_impl
    procedure, public, nopass :: Get_dnorm => Get_dnorm_impl
    procedure, public, nopass :: Get_anisotropy => Get_anisotropy_impl
    procedure, public, nopass :: Get_shear => Get_shear_impl
    procedure, public, nopass :: Get_bulk => Get_bulk_impl
    procedure, public, nopass :: Yield_distance => yield_distance_impl
    procedure, public, nopass :: Get_stiffness => Get_stiffness_impl
    procedure, public, nopass :: calc_dsigma => calc_dsigma_impl
  endtype Elast
  !=============================================================================
  ! Abstract interface definition (implemented in the sub-module)
  !=============================================================================
  interface
    !***************************************************************************
    !> @brief Calculate principal stresses
    !>
    !> @details
    !> Compute the principal stresses (eigenvalues) of a 3x3 symmetric tensor.
    !> The eigenvalues are returned in descending order (σ₁ ≥ σ₂ ≥ σ₃).
    !>
    !> @param[in]  tensor  3x3 symmetric stress or strain tensor
    !>
    !> @return Array of principal values (σ₁, σ₂, σ₃) in descending order
    !***************************************************************************
    module function Get_principal_impl(tensor) result(res)
      real(DP), dimension(3, 3), intent(in) :: tensor
      real(DP), dimension(3) :: res
    endfunction Get_principal_impl
    !***************************************************************************
    !> @brief Calculate g(θ) function for Lode angle dependence
    !>
    !> @details
    !> Compute the g(θ) function that describes the dependence of yield
    !> surface on the Lode angle θ. This function modulates the yield
    !> surface shape in the deviatoric plane according to the stress
    !> state's Lode angle.
    !>
    !> @param[in]  shvars  Shared variables containing stress state
    !>
    !> @return Array of g(θ) values for principal stress directions
    !***************************************************************************

    module function Get_gtheta_impl(shvars) result(gtheta)
      type(Share_var), intent(in) :: shvars
      real(DP), dimension(3) :: gtheta
    endfunction Get_gtheta_impl
    !***************************************************************************
    !> @brief Calculate fabric ratio tensor
    !>
    !> @details
    !> Compute the fabric ratio tensor Fᵣ that characterizes the anisotropic
    !> fabric structure of the soil. This tensor relates the current fabric
    !> state to the reference isotropic configuration and influences the
    !> elastic and plastic behavior.
    !>
    !> @param[in]  shvars  Shared variables containing fabric information
    !>
    !> @return 3x3 fabric ratio tensor
    !***************************************************************************
    module function Get_Fr_impl(shvars) result(res)
      type(Share_var), intent(in) :: shvars
      real(DP), dimension(3, 3) :: res
    endfunction Get_Fr_impl
    !***************************************************************************
    !> @brief Calculate normalized fabric tensor
    !>
    !> @details
    !> Compute the normalized fabric tensor that represents the directional
    !> distribution of soil fabric. This tensor is normalized to have unit
    !> magnitude and characterizes the anisotropy direction without magnitude
    !> information.
    !>
    !> @param[in]  shvars  Shared variables containing fabric tensor
    !>
    !> @return 3x3 normalized fabric tensor
    !***************************************************************************
    module function Get_dnorm_impl(shvars) result(res)
      type(Share_var), intent(in) :: shvars
      real(DP), dimension(3, 3) :: res
    endfunction Get_dnorm_impl
    !***************************************************************************
    !> @brief Calculate anisotropy coefficient
    !>
    !> @details
    !> Compute the anisotropy coefficient that quantifies the degree of
    !> fabric anisotropy in the soil. This scalar value represents the
    !> deviation from isotropic fabric configuration and influences the
    !> directional dependence of mechanical properties.
    !>
    !> @param[in]  shvars  Shared variables containing fabric information
    !>
    !> @return Anisotropy coefficient (scalar)
    !***************************************************************************
    module function Get_anisotropy_impl(shvars) result(val)
      type(Share_var), intent(in) :: shvars
      real(DP) :: val
    endfunction Get_anisotropy_impl
    !***************************************************************************
    !> @brief Calculate shear modulus
    !>
    !> @details
    !> Compute the shear modulus (G) of the soil based on current stress
    !> state and void ratio. The shear modulus represents the material's
    !> resistance to shear deformation and is a key parameter for elastic
    !> stress-strain relationships.
    !>
    !> @param[in]  shvars  Shared variables containing stress state
    !> @param[in]  stvars  State variables containing void ratio
    !>
    !> @return Shear modulus (G)
    !***************************************************************************
    module function Get_shear_impl(shvars, stvars) result(shear)
      type(Share_var), intent(in) :: shvars
      type(State_var), intent(in) :: stvars
      real(DP) :: shear
    endfunction Get_shear_impl

    !***************************************************************************
    !> @brief Calculate bulk modulus
    !>
    !> @details
    !> Compute the bulk modulus (K) of the soil based on current stress
    !> state and void ratio. The bulk modulus represents the material's
    !> resistance to volumetric compression and is essential for calculating
    !> volumetric stress-strain responses.
    !>
    !> @param[in]  shvars  Shared variables containing stress state
    !> @param[in]  stvars  State variables containing void ratio
    !>
    !> @return Bulk modulus (K)
    !***************************************************************************
    module function Get_bulk_impl(shvars, stvars) result(bulk)
      type(Share_var), intent(in) :: shvars
      type(State_var), intent(in) :: stvars
      real(DP) :: bulk
    endfunction Get_bulk_impl
    !***************************************************************************
    !> @brief Calculate distance to yield surface
    !>
    !> @details
    !> Compute the distance (f) from the current stress state to the yield
    !> surface. This scalar value indicates how close the material is to
    !> yielding: f < 0 indicates elastic state, f = 0 indicates yielding,
    !> and f > 0 indicates stress state outside the yield surface.
    !>
    !> @param[in]  shvars  Shared variables containing stress state
    !>
    !> @return Yield function value (f)
    !***************************************************************************
    module function Yield_distance_impl(shvars) result(ftol)
      type(Share_var), intent(in) :: shvars
      real(DP) :: ftol
    endfunction Yield_distance_impl

    !***************************************************************************
    !> @brief Calculate elastic stiffness tensor
    !>
    !> @details
    !> Compute the fourth-order elastic stiffness tensor (Cᵢⱼₖₗ) based on
    !> current stress state and void ratio. This tensor relates stress
    !> increments to strain increments through the constitutive relation
    !> Δσᵢⱼ = Cᵢⱼₖₗ Δεₖₗ. For isotropic elasticity, it reduces to a function
    !> of shear and bulk moduli.
    !>
    !> @param[in]  shvars  Shared variables containing stress state
    !> @param[in]  stvars  State variables containing void ratio
    !>
    !> @return 3x3x3x3 elastic stiffness tensor
    !***************************************************************************
    module function Get_stiffness_impl(shvars, stvars) result(stiffness)
      type(Share_var), intent(in) :: shvars
      type(State_var), intent(in) :: stvars
      real(DP), dimension(3, 3, 3, 3) :: stiffness
    endfunction Get_stiffness_impl
    !***************************************************************************
    !> @brief Calculate stress increment from strain increment
    !>
    !> @details
    !> Compute the elastic stress increment (Δσ) corresponding to a given
    !> strain increment (Δε) using the elastic stiffness tensor. This
    !> implements the constitutive relation Δσᵢⱼ = Cᵢⱼₖₗ Δεₖₗ for elastic
    !> loading/unloading.
    !>
    !> @param[in]  shvars  Shared variables containing stress state
    !> @param[in]  stvars  State variables containing void ratio
    !> @param[in]  depsln  Strain increment tensor (3x3)
    !>
    !> @return Stress increment tensor (3x3)
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
!*******************************************************************************
!> @brief Plasticity module for SANISAND model
!>
!> @details This module implements the plasticity algorithms for the SANISAND
!>          constitutive model, including yield function derivatives, plastic
!>          potential derivatives, dilatancy calculation, fabric evolution,
!>          and plastic modulus computation. It provides the core plastic
!>          correction algorithms used in the UMAT implementation.
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
    procedure, public, nopass :: Get_pfpr => Get_pfpr_impl
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
    !> @brief Calculate partial derivative of yield function with respect to p'
    !>
    !> @details This function computes the partial derivative of the yield
    !>          function with respect to mean effective stress (p'). This
    !>          derivative is essential for computing the plastic multiplier
    !>          and consistent tangent modulus in plasticity algorithms.
    !>
    !> @param[in] shvars   Shared variables (stress tensor, etc.)
    !>
    !> @return res         Partial derivative ∂f/∂p' (3x3 tensor)
    !***************************************************************************
    module function Get_pfpr_impl(shvars) result(res)
      type(Share_var), intent(in) :: shvars
      real(DP), dimension(3, 3) :: res
    endfunction Get_pfpr_impl
    !***************************************************************************
    !> @brief Calculate partial derivative of yield function with respect to stress
    !>
    !> @details This function computes the partial derivative of the yield
    !>          function with respect to the stress tensor (∂f/∂σ). This
    !>          derivative is essential for computing the plastic flow
    !>          direction and consistent tangent modulus in plasticity
    !>          algorithms.
    !>
    !> @param[in] shvars   Shared variables containing current stress tensor
    !>
    !> @return res         Partial derivative ∂f/∂σ (3x3 tensor)
    !***************************************************************************
    module function Get_pfsig_impl(shvars) result(res)
      type(Share_var), intent(in) :: shvars
      real(DP), dimension(3, 3) :: res
    endfunction Get_pfsig_impl
    !***************************************************************************
    !> @brief Calculate partial derivative of plastic potential with respect to stress
    !>
    !> @details This function computes the partial derivative of the plastic
    !>          potential function with respect to the stress tensor (∂g/∂σ).
    !>          This derivative defines the direction of plastic flow in
    !>          stress space and is essential for non-associative plasticity
    !>          models where the plastic potential differs from the yield
    !>          function.
    !>
    !> @param[in] shvars   Shared variables containing current stress tensor
    !> @param[in] stvars   State variables (void ratio, etc.)
    !>
    !> @return res         Partial derivative ∂g/∂σ (3x3 tensor)
    !***************************************************************************
    module function Get_pgsig_impl(shvars, stvars) result(res)
      implicit none
      type(Share_var), intent(in) :: shvars
      type(State_var), intent(in) :: stvars
      real(DP), dimension(3, 3) :: res
    endfunction Get_pgsig_impl
    !***************************************************************************
    !> @brief Calculate state parameter (ψ) for SANISAND model
    !>
    !> @details This function computes the state parameter ψ, which represents
    !>          the difference between current void ratio and critical state
    !>          void ratio at the same mean effective stress. The state
    !>          parameter is a key variable in critical state soil mechanics
    !>          that characterizes the soil's density state relative to the
    !>          critical state line.
    !>
    !> @param[in] shvars   Shared variables (stress tensor, fabric tensor, etc.)
    !> @param[in] stvars   State variables (void ratio, etc.)
    !>
    !> @return res         State parameter ψ = e - e_cs
    !***************************************************************************
    module function Get_psim_impl(shvars, stvars) result(res)
      ! input
      type(Share_var), intent(in) :: shvars
      type(State_var), intent(in) :: stvars
      ! output
      real(DP) :: res
    endfunction Get_psim_impl
    !***************************************************************************
    !> @brief Calculate dilatancy coefficient for SANISAND model
    !>
    !> @details This function computes the dilatancy coefficient D, which
    !>          relates the volumetric plastic strain increment to the
    !>          deviatoric plastic strain increment. The dilatancy coefficient
    !>          determines whether the soil exhibits contractive (D > 0) or
    !>          dilative (D < 0) behavior during plastic deformation.
    !>
    !> @param[in] shvars   Shared variables (stress tensor, fabric tensor, etc.)
    !> @param[in] stvars   State variables (void ratio, etc.)
    !>
    !> @return res         Dilatancy coefficient D
    !***************************************************************************
    module function Get_dilatancy_impl(shvars, stvars) result(res)
      type(Share_var), intent(in) :: shvars
      type(State_var), intent(in) :: stvars
      real(DP) :: res
    endfunction Get_dilatancy_impl
    !***************************************************************************
    !> @brief Calculate fabric evolution for SANISAND model
    !>
    !> @details This subroutine computes the evolution of fabric tensor and
    !>          hardening parameters in the SANISAND model. The fabric tensor
    !>          represents the anisotropic microstructure of the soil, which
    !>          evolves with plastic deformation and affects the material's
    !>          mechanical response.
    !>
    !> @param[in]  shvars   Shared variables (stress tensor, fabric tensor, etc.)
    !> @param[in]  stvars   State variables (void ratio, etc.)
    !> @param[out] Rh       Hardening parameter evolution rate
    !> @param[out] RF       Fabric tensor evolution rate (3x3 tensor)
    !***************************************************************************
    module subroutine Get_evolution_impl(shvars, stvars, Rh, RF)
      type(Share_var), intent(in) :: shvars
      type(State_var), intent(in) :: stvars
      real(DP) :: Rh, RF(3, 3)
    endsubroutine Get_evolution_impl
    !***************************************************************************
    !> @brief Calculate plastic modulus for SANISAND model
    !>
    !> @details This function computes the plastic modulus Dkp, which relates
    !>          the plastic multiplier to the consistency condition in
    !>          plasticity theory. The plastic modulus determines the hardening
    !>          or softening behavior of the material and is essential for
    !>          computing the plastic strain increments.
    !>
    !> @param[in] shvars   Shared variables (stress tensor, fabric tensor, etc.)
    !> @param[in] stvars   State variables (void ratio, etc.)
    !>
    !> @return Dkp         Plastic modulus
    !***************************************************************************
    module function Get_Dkp_impl(shvars, stvars) result(Dkp)
      type(Share_var), intent(in) :: shvars
      type(State_var), intent(in) :: stvars
      real(DP) :: Dkp
    endfunction Get_Dkp_impl
    !***************************************************************************
    !> @brief Perform elastic predictor - plastic corrector step
    !>
    !> @details This subroutine implements the elastic predictor - plastic
    !>          corrector algorithm for the SANISAND model. It computes the
    !>          updated stress state and consistent tangent modulus given an
    !>          incremental strain. The algorithm consists of two steps:
    !>          1. Elastic predictor: Compute trial stress assuming elastic
    !>             behavior.
    !>          2. Plastic corrector: If the trial stress violates the yield
    !>             condition, apply plastic correction to return to the yield
    !>             surface.
    !>
    !> @param[in]  shvars   Shared variables (stress tensor, fabric tensor, etc.)
    !> @param[in]  stvars   State variables (void ratio, etc.)
    !> @param[in]  depsln   Incremental strain tensor (3x3)
    !> @param[out] Rshvars  Updated shared variables after plastic correction
    !> @param[out] dempx    Consistent tangent modulus (3x3x3x3 fourth-order tensor)
    !***************************************************************************
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
!*******************************************************************************
!> @brief Math module for numerical algorithms
!>
!> @details This module provides mathematical utilities including bisection method,
!>          interval checking, and other numerical algorithms used in the UMAT.
!>
!> @author wuwenhao
!> @date 2025/12/11
!*******************************************************************************
module math_mod
  use Base_config
  use Container_mod
  implicit none
  private
  !*****************************************************************************
  !> @brief Abstract interface for functions with parameters
  !>
  !> @details This abstract interface defines the signature for functions that
  !>          take shared variables, state variables, and an amplitude parameter,
  !>          returning a double precision value.
  !>
  !> @param[in] shvars    Shared variables (stress tensor, etc.)
  !> @param[in] stvars    State variables (void ratio, etc.)
  !> @param[in] amplitude Amplitude parameter (e.g., scaling factor)
  !> @return fval         Function value
  !*****************************************************************************
  abstract interface
    function func_with_param(shvars, stvars, depsln) result(fval)
      use Base_config
      use Container_mod
      implicit none
      type(Share_var), intent(in) :: shvars
      type(State_var), intent(in) :: stvars
      real(DP), dimension(3, 3), intent(in) :: depsln
      real(DP) :: fval
    endfunction func_with_param
  endinterface
  !
  type, public :: Math
  contains
    procedure, public, nopass :: Intchc => intchc_impl
    procedure, public, nopass :: Onyield => Onyield_impl
    procedure, public, nopass :: Bisection_impl
    procedure, public, nopass :: drift_shvars_impl
    procedure, public, nopass :: Get_residual_impl
    procedure, public, nopass :: ftol_with_depsln
    procedure, public, nopass :: mean_with_depsln
    procedure, private, nopass :: is_monotonic
    procedure, private, nopass :: flow_direction_impl
    procedure, private, nopass :: radial_direction_impl
  endtype
  !
  interface
    !***************************************************************************
    !> @brief Interval checking implementation
    !>
    !> @details This subroutine performs interval checking to find the appropriate
    !>          scaling factor for the strain increment. It determines the right
    !>          boundary and the scaling factor for plastic correction.
    !>
    !> @param[in]  shvars Shared variables (stress tensor, etc.)
    !> @param[in]  stvars State variables (void ratio, etc.)
    !> @param[out] rbd    Right boundary of the scaling factor (0 <= rbd <= 1)
    !> @param[out] alout  Scaling factor for plastic correction
    !***************************************************************************
    module function intchc_impl(shvars, stvars, depsln, tol) result(alout)
      type(Share_var), intent(in) :: shvars
      type(State_var), intent(in) :: stvars
      real(DP), dimension(3, 3), intent(in) :: depsln
      real(DP), intent(in) :: tol
      real(DP) :: alout
    endfunction intchc_impl
    !***************************************************************************
    !> @brief Bisection method implementation
    !>
    !> @details This function implements the bisection method to find the root of
    !>          a function within a given interval [lbd, rbd]. The function must
    !>          have opposite signs at the boundaries. Monotonicity check can be
    !>          enabled in debug mode.
    !>
    !> @param[in] shvars   Shared variables (stress tensor, etc.)
    !> @param[in] stvars   State variables (void ratio, etc.)
    !> @param[in] func     Function to find root of (conforms to func_with_param)
    !> @param[in] lbd      Left boundary of the interval
    !> @param[in] rbd      Right boundary of the interval
    !> @param[in] condition Target value (usually 0 for root finding)
    !> @return alout       Root found within the interval
    !***************************************************************************
    module function Bisection_impl(shvars, stvars, depsln, func, lbd, rbd, condition, tol) result(alout)
      ! declration
      type(Share_var), intent(in) :: shvars
      type(State_var), intent(in) :: stvars
      real(DP), dimension(3, 3), intent(in) :: depsln
      procedure(func_with_param) :: func
      real(DP), intent(in) :: lbd
      real(DP), intent(in) :: rbd
      real(DP), intent(in) :: condition
      real(DP), intent(in) :: tol
      real(DP) :: alout
    endfunction Bisection_impl
    !***************************************************************************
    !> @brief Calculate residual between shared variable states
    !>
    !> @details This function computes the residual (difference) between three
    !>          shared variable states. It is used in convergence checking and
    !>          iterative algorithms to measure the change between successive
    !>          iterations.
    !>
    !> @param[in] shfor    First shared variable state
    !> @param[in] shsec    Second shared variable state
    !> @param[in] shtmp    Third shared variable state (reference)
    !>
    !> @return residual    Computed residual value
    !***************************************************************************
    module function Get_residual_impl(shfor, shsec, shtmp) result(residual)
      type(Share_var), intent(in) :: shfor
      type(Share_var), intent(in) :: shsec
      type(Share_var), intent(in) :: shtmp
      real(DP) :: residual
    endfunction Get_residual_impl
    !***************************************************************************
    !> @brief Update state variables when on yield surface
    !>
    !> @details This subroutine updates shared and state variables when the
    !>          material is on the yield surface. It performs plastic correction
    !>          and updates the elasticity tensor for consistent tangent modulus.
    !>          The algorithm ensures stress state remains on the yield surface
    !>          while updating hardening parameters and fabric tensors.
    !>
    !> @param[in]  shvars      Input shared variables (stress tensor, etc.)
    !> @param[in]  stvars      Input state variables (void ratio, etc.)
    !> @param[in]  depsln      Strain increment tensor
    !> @param[in]  tol         Tolerance for convergence checking
    !> @param[out] shvar_upd   Updated shared variables
    !> @param[out] stvar_upd   Updated state variables
    !> @param[out] dempx       Updated elasticity tensor (consistent tangent)
    !***************************************************************************
    module subroutine Onyield_impl(shvars, stvars, depsln, tol, shvar_upd, stvar_upd, dempx)
      type(Share_var), intent(in) :: shvars
      type(State_var), intent(in) :: stvars
      real(DP), dimension(3, 3), intent(in) :: depsln
      real(DP), intent(in) :: tol
      type(Share_var), intent(out) :: shvar_upd
      type(State_var), intent(out) :: stvar_upd
      real(DP), dimension(3, 3, 3, 3) :: dempx
    endsubroutine Onyield_impl
    !***************************************************************************
    !> @brief Drift correction for shared variables
    !>
    !> @details This subroutine performs drift correction to ensure stress state
    !>          remains on the yield surface after plastic correction. It adjusts
    !>          shared and state variables to compensate for numerical drift that
    !>          may occur during iterative solution procedures.
    !>
    !> @param[in]  shtmp   Temporary shared variables (after plastic correction)
    !> @param[in]  sttmp   Temporary state variables (after plastic correction)
    !> @param[in]  dempx   Elasticity tensor (consistent tangent)
    !> @param[in]  tol     Tolerance for drift correction
    !> @param[out] shdrt   Drift-corrected shared variables
    !> @param[out] stdrt   Drift-corrected state variables
    !> @param[out] dedrt   Updated elasticity tensor after drift correction
    !***************************************************************************
    module subroutine drift_shvars_impl(shtmp, sttmp, dempx, tol, shdrt, stdrt, dedrt)
      type(Share_var), intent(in) :: shtmp
      type(State_var), intent(in) :: sttmp
      real(DP), dimension(3, 3, 3, 3), intent(in) :: dempx
      real(DP), intent(in) :: tol
      type(Share_var), intent(out) :: shdrt
      type(State_var), intent(out) :: stdrt
      real(DP), dimension(3, 3, 3, 3), intent(out) :: dedrt
    endsubroutine drift_shvars_impl
    !***************************************************************************
    !> @brief Yield distance with strain increment
    !>
    !> @details This function calculates the yield distance after applying a
    !>          scaled strain increment. It computes the stress increment from
    !>          the strain increment using the elasticity tensor, updates the
    !>          stress, and returns the yield distance.
    !>
    !> @param[in] shvars    Shared variables (stress tensor, etc.)
    !> @param[in] stvars    State variables (void ratio, etc.)
    !> @param[in] amplitude Scaling factor for the strain increment
    !> @return ftol         Yield distance after applying scaled strain increment
    !***************************************************************************
    module function ftol_with_depsln(shvars, stvars, depsln) result(ftol)
      type(Share_var), intent(in) :: shvars
      type(State_var), intent(in) :: stvars
      real(DP), dimension(3, 3), intent(in) :: depsln
      real(DP) :: ftol
    endfunction ftol_with_depsln
    !***************************************************************************
    !> @brief Mean stress with strain increment
    !>
    !> @details This function calculates the mean stress after applying a
    !>          scaled strain increment. It computes the stress increment from
    !>          the strain increment using the elasticity tensor, updates the
    !>          stress, and returns the mean stress (trace/3).
    !>
    !> @param[in] shvars    Shared variables (stress tensor, etc.)
    !> @param[in] stvars    State variables (void ratio, etc.)
    !> @param[in] amplitude Scaling factor for the strain increment
    !> @return res          Mean stress after applying scaled strain increment
    !***************************************************************************
    module function mean_with_depsln(shvars, stvars, depsln) result(res)
      type(Share_var), intent(in) :: shvars
      type(State_var), intent(in) :: stvars
      real(DP), dimension(3, 3), intent(in) :: depsln
      real(DP) :: res
    endfunction mean_with_depsln
    !***************************************************************************
    !> @brief Monotonicity check for a function
    !>
    !> @details This function checks whether a given function is monotonic
    !>          (either non-decreasing or non-increasing) within the interval
    !>          [lbd, rbd]. It samples the function at multiple points and
    !>          verifies monotonic behavior.
    !>
    !> @param[in] shvars Shared variables (stress tensor, etc.)
    !> @param[in] stvars State variables (void ratio, etc.)
    !> @param[in] func   Function to check (conforms to func_with_param)
    !> @param[in] lbd    Left boundary of the interval
    !> @param[in] rbd    Right boundary of the interval
    !> @return           .true. if function is monotonic, .false. otherwise
    !***************************************************************************
    module logical function is_monotonic(shvars, stvars, depsln, func, lbd, rbd)
      type(Share_var), intent(in) :: shvars
      type(State_var), intent(in) :: stvars
      real(DP), dimension(3, 3), intent(in) :: depsln
      procedure(func_with_param) :: func
      real(DP), intent(in) :: lbd
      real(DP), intent(in) :: rbd
    endfunction is_monotonic
    !***************************************************************************
    !> @brief Calculate plastic flow direction
    !>
    !> @details This function computes the plastic flow direction tensor based on
    !>          the current stress state and material properties. The flow
    !>          direction defines the direction of plastic strain increment in
    !>          stress space, which is essential for associative or non-associative
    !>          plasticity models.
    !>
    !> @param[in] shvars   Shared variables (stress tensor, etc.)
    !> @param[in] stvars   State variables (void ratio, etc.)
    !>
    !> @return res         Flow direction tensor
    !***************************************************************************
    module function flow_direction_impl(shvars, stvars) result(res)
      type(Share_var), intent(in) :: shvars
      type(State_var), intent(in) :: stvars
      type(Share_var) :: res
    endfunction flow_direction_impl
    !***************************************************************************
    !> @brief Calculate radial direction in stress space
    !>
    !> @details This function computes the radial direction tensor in stress
    !>          space, which points from the origin to the current stress state.
    !>          The radial direction is used in soil mechanics models to define
    !>          fabric evolution and anisotropic hardening directions.
    !>
    !> @param[in] shvars   Shared variables (stress tensor, etc.)
    !> @param[in] stvars   State variables (void ratio, etc.)
    !>
    !> @return res         Radial direction tensor
    !***************************************************************************
    module function radial_direction_impl(shvars, stvars) result(res)
      type(Share_var), intent(in) :: shvars
      type(State_var), intent(in) :: stvars
      type(Share_var) :: res
    endfunction radial_direction_impl
  endinterface

contains
!*******************************************************************************
endmodule math_mod
!*******************************************************************************
!> @brief share_vars_impl
!>
!> @details 模块详细描述
!>
!> @author wuwenhao
!> @date 2025/11/27
!*******************************************************************************
submodule(Container_mod) Container_impl
  use Base_config
  use tensor_opt_mod
  use exception_mod
#include "macro.h"
  implicit none
  type(Torch) torch_
contains
  !=============================================================================
  !
  ! interface Share_var
  !
  !=============================================================================
  module procedure share_construct_param
  real(DP) :: mean
  !-----------------------------------------------------------------------------
  this%harden_ = harden
  this%sigma_(:, :) = sigma(:, :)
  this%fabric_(:, :) = fabric(:, :)
  !
  this%initialized_ = .true.
  !
  mean = torch_%Trace(this%sigma_) / 3.0_DP
  this%is_lowstress = merge(.true., .false., mean <= EPS)
  call this%jugde_nan_inf_impl()
  end procedure share_construct_param
  !*****************************************************************************
  module procedure Share_construct_zero
  this%harden_ = 0.0_DP
  this%sigma_(:, :) = 0.0_DP
  this%fabric_(:, :) = 0.0_DP
  this%initialized_ = .true.
  this%is_lowstress = .true.
  this%is_nan_inf = .false.
  end procedure Share_construct_zero
  !*****************************************************************************
  module procedure get_harden_impl
  CHECK_TRUE(this%initialized_, "container share_vars has not initialized")
  harden = this%harden_
  end procedure get_harden_impl
  !*****************************************************************************
  module procedure get_sigma_impl
  CHECK_TRUE(this%initialized_, "container share_vars has not initialized")
  sigma(:, :) = this%sigma_(:, :)
  end procedure get_sigma_impl
  !*****************************************************************************
  module procedure get_fabric_impl
  CHECK_TRUE(this%initialized_, "container share_vars has not initialized")
  fabric(:, :) = this%fabric_(:, :)
  end procedure get_fabric_impl
  !*****************************************************************************
  module procedure low_impl
  CHECK_TRUE(this%initialized_, "container share_vars has not initialized")
  is_true = this%is_lowstress
  end procedure low_impl
  !*****************************************************************************
  module procedure update_harden_impl
  CHECK_TRUE(this%initialized_, "container share_vars has not initialized")
  this%harden_ = this%harden_ + harden
  call this%jugde_nan_inf_impl()
  end procedure update_harden_impl
  !*****************************************************************************
  module procedure update_sigma_impl
  real(DP) :: mean
  !-----------------------------------------------------------------------------
  CHECK_TRUE(this%initialized_, "container share_vars has not initialized")
  this%sigma_(:, :) = this%sigma_(:, :) + dsigma(:, :)
  mean = torch_%Trace(this%sigma_) / 3.0_DP
  this%is_lowstress = merge(.true., .false., mean <= EPS)
  call this%jugde_nan_inf_impl()
  end procedure update_sigma_impl
  !*****************************************************************************
  module procedure update_fabric_impl
  CHECK_TRUE(this%initialized_, "container share_vars has not initialized")
  this%fabric_(:, :) = this%fabric_(:, :) + fabric(:, :)
  call this%jugde_nan_inf_impl()
  end procedure update_fabric_impl
  !*****************************************************************************
  module procedure update_shvars_impl
  real(DP) :: mean
  !-----------------------------------------------------------------------------
  CHECK_TRUE(this%initialized_, "container share_vars has not initialized")
  this%harden_ = this%harden_ + dharden
  this%sigma_(:, :) = this%sigma_(:, :) + dsigma(:, :)
  this%fabric_(:, :) = this%fabric_(:, :) + dfabric(:, :)
  !
  mean = torch_%Trace(this%sigma_) / 3.0_DP
  this%is_lowstress = merge(.true., .false., mean <= EPS)
  call this%jugde_nan_inf_impl()
  end procedure update_shvars_impl
  !*****************************************************************************
  module procedure changed_harden_impl
  CHECK_TRUE(this%initialized_, "container share_vars has not initialized")
  this%harden_ = harden
  call this%jugde_nan_inf_impl()
  end procedure changed_harden_impl
  !*****************************************************************************
  module procedure changed_sigma_impl
  real(DP) :: mean
  !-----------------------------------------------------------------------------
  CHECK_TRUE(this%initialized_, "container share_vars has not initialized")
  this%sigma_(:, :) = sigma(:, :)
  mean = torch_%Trace(this%sigma_) / 3.0_DP
  this%is_lowstress = merge(.true., .false., mean <= EPS)
  call this%jugde_nan_inf_impl()
  end procedure changed_sigma_impl
  !*****************************************************************************
  module procedure changed_fabric_impl
  CHECK_TRUE(this%initialized_, "container share_vars has not initialized")
  this%fabric_(:, :) = fabric(:, :)
  call this%jugde_nan_inf_impl()
  end procedure changed_fabric_impl
  !*****************************************************************************
  module procedure norm_impl
  res(1) = abs(this%get_harden())
  res(2) = torch_%Norm(this%get_sigma())
  res(3) = torch_%Norm(this%get_fabric())
  end procedure norm_impl
  !*****************************************************************************
  module procedure print_impl
  write(6, *) "Stress Tensor : "
  call torch_%Print(this%get_sigma())
  write(6, *) "Fabric Tensor : "
  call torch_%Print(this%fabric_)
  write(6, '(A12, ES13.6)') "Hardening = ", this%harden_
  end procedure print_impl
  !*****************************************************************************
  module procedure jugde_nan_inf_impl
  logical has_error
  has_error = (this%harden_ /= this%harden_) .or. (abs(this%harden_) > MAX_DATA)
  has_error = has_error .or. any(this%sigma_ /= this%sigma_) &
              .or. any(abs(this%sigma_) > MAX_DATA)
  has_error = has_error .or. any(this%fabric_ /= this%fabric_) &
              .or. any(abs(this%fabric_) > MAX_DATA)
  this%is_nan_inf = has_error
  end procedure jugde_nan_inf_impl
  !*****************************************************************************
  module procedure assign_impl
  CHECK_TRUE(other%initialized_, "container share_vars has not initialized")
  this%harden_ = other%harden_
  this%sigma_(:, :) = other%sigma_(:, :)
  this%fabric_(:, :) = other%fabric_(:, :)
  this%initialized_ = other%initialized_
  this%is_lowstress = other%is_lowstress
  this%is_nan_inf = other%is_nan_inf
  end procedure assign_impl
  !*****************************************************************************
  module procedure binary_add_impl
  real(DP) :: mean
  !-----------------------------------------------------------------------------
  CHECK_TRUE(this%initialized_, "container share_vars has not initialized")
  CHECK_TRUE(other%initialized_, "container share_vars has not initialized")
  res%harden_ = this%harden_ + other%harden_
  res%sigma_ = this%sigma_ + other%sigma_
  res%fabric_ = this%fabric_ + other%fabric_
  res%initialized_ = .true.
  mean = torch_%Trace(res%sigma_) / 3.0_DP
  res%is_lowstress = merge(.true., .false., mean <= EPS)
  call res%jugde_nan_inf_impl()
  end procedure binary_add_impl
  !*****************************************************************************
  module procedure binary_sub_impl
  real(DP) :: mean
  !-----------------------------------------------------------------------------
  CHECK_TRUE(this%initialized_, "container share_vars has not initialized")
  CHECK_TRUE(other%initialized_, "container share_vars has not initialized")
  res%harden_ = this%harden_ - other%harden_
  res%sigma_ = this%sigma_ - other%sigma_
  res%fabric_ = this%fabric_ - other%fabric_
  res%initialized_ = .true.
  mean = torch_%Trace(res%sigma_) / 3.0_DP
  res%is_lowstress = merge(.true., .false., mean <= EPS)
  call res%jugde_nan_inf_impl()
  end procedure binary_sub_impl
  !*****************************************************************************
  module procedure unary_minus_impl
  real(DP) :: mean
  !-----------------------------------------------------------------------------
  CHECK_TRUE(this%initialized_, "container share_vars has not initialized")
  res%harden_ = -this%harden_
  res%sigma_ = -this%sigma_
  res%fabric_ = -this%fabric_
  res%initialized_ = this%initialized_
  mean = torch_%Trace(res%sigma_) / 3.0_DP
  res%is_lowstress = merge(.true., .false., mean <= EPS)
  call res%jugde_nan_inf_impl()
  end procedure unary_minus_impl
  !*****************************************************************************
  module procedure unary_lhs_scalar_impl
  real(DP) :: mean
  !-----------------------------------------------------------------------------
  CHECK_TRUE(this%initialized_, "container share_vars has not initialized")
  res%harden_ = scalar * this%harden_
  res%sigma_ = scalar * this%sigma_
  res%fabric_ = scalar * this%fabric_
  res%initialized_ = .true.
  mean = torch_%Trace(res%sigma_) / 3.0_DP
  res%is_lowstress = merge(.true., .false., mean <= EPS)
  call res%jugde_nan_inf_impl()
  end procedure unary_lhs_scalar_impl
  !*****************************************************************************
  module procedure unary_rhs_scalar_impl
  real(DP) :: mean
  !-----------------------------------------------------------------------------
  CHECK_TRUE(this%initialized_, "container share_vars has not initialized")
  res%harden_ = this%harden_ * scalar
  res%sigma_ = this%sigma_ * scalar
  res%fabric_ = this%fabric_ * scalar
  res%initialized_ = .true.
  mean = torch_%Trace(res%sigma_) / 3.0_DP
  res%is_lowstress = merge(.true., .false., mean <= EPS)
  call res%jugde_nan_inf_impl()
  end procedure unary_rhs_scalar_impl
  !*****************************************************************************
  module procedure unary_div_impl
  real(DP) :: mean, scalar_
  !-----------------------------------------------------------------------------
  CHECK_TRUE(this%initialized_, "container share_vars has not initialized")
  scalar_ = scalar
  if(abs(scalar_) < EPS) scalar_ = sign(scalar_, EPS)
  res%harden_ = this%harden_ / scalar_
  res%sigma_ = this%sigma_ / scalar_
  res%fabric_ = this%fabric_ / scalar_
  res%initialized_ = .true.
  mean = torch_%Trace(res%sigma_) / 3.0_DP
  res%is_lowstress = merge(.true., .false., mean <= EPS)
  call res%jugde_nan_inf_impl()
  end procedure unary_div_impl
  !
  !=============================================================================
  !
  ! interface State_var
  !
  !=============================================================================
  !
  module procedure State_construct_param
  this%voidr_ = voidr
  this%pnewdt_ = pnewdt
  this%initialized_ = .true.
  end procedure State_construct_param
  !*****************************************************************************
  module procedure get_voidr_impl
  CHECK_TRUE(this%initialized_, "container state_vars has not initialized")
  voidr = this%voidr_
  end procedure get_voidr_impl
  !*****************************************************************************
  module procedure get_pnewdt_impl
  CHECK_TRUE(this%initialized_, "container state_vars has not initialized")
  pnewdt = this%pnewdt_
  end procedure get_pnewdt_impl
  !*****************************************************************************
  module procedure update_voidr_impl
  real(DP) :: despv
  !-----------------------------------------------------------------------------
  CHECK_TRUE(this%initialized_, "container state_vars has not initialized")
  despv = torch_%Trace(depsln)
  this%voidr_ = this%voidr_ - (1.0_DP + this%voidr_) * despv
  end procedure update_voidr_impl
  !*****************************************************************************
  module procedure changed_voidr_impl
  CHECK_TRUE(this%initialized_, "container state_vars has not initialized")
  this%voidr_ = voidr
  end procedure changed_voidr_impl
  !*****************************************************************************
  module procedure changed_pnewdt_impl
  CHECK_TRUE(this%initialized_, "container state_vars has not initialized")
  this%pnewdt_ = pnewdt
  end procedure changed_pnewdt_impl
  !*****************************************************************************
  module procedure assign_impl_state
  CHECK_TRUE(other%initialized_, "container state_vars has not initialized")
  this%voidr_ = other%voidr_
  this%pnewdt_ = other%pnewdt_
  this%initialized_ = other%initialized_
  end procedure assign_impl_state
endsubmodule
!*****************************************************************************
!> @brief Implementation of exception module procedures
!>
!> @details Contains the implementation of error handling and assertion
!>          procedures defined in the exception_mod interface
!>
!> @author wuwenhao
!> @date 2025/11/27
!*****************************************************************************
submodule(exception_mod) exception_impl
  implicit none
contains
  !*****************************************************************************
  !> @brief Create an error context object
  !>
  !> @details Implementation of the ErrorContext constructor that stores
  !>          file, line, and optional prefix information for error reporting
  !>
  !> @param[in] file Name of the source file where error occurred
  !> @param[in] line Line number in the source file
  !> @param[in] prefix Optional prefix message for error identification
  !>
  !> @return ErrorContext object containing error location information
  !*****************************************************************************
  module procedure create_error_context
  this%file_ = file
  this%line_ = line
  if(present(prefix)) then
    this%prefix_ = prefix
  else
    this%prefix_ = "Validation failed"
  endif
  end procedure create_error_context
  !*****************************************************************************
  !> @brief Throw an error with context information
  !>
  !> @details Implementation of error throwing procedure that formats
  !>          and outputs an error message with file, line, and prefix
  !>          information, then terminates the program
  !>
  !> @param[in] msg Error message to display
  !> @param[in] this ErrorContext object containing location information
  !*****************************************************************************
  module procedure throw_error_impl
  character(len=:), allocatable :: full_msg
  integer :: msg_len
  !
  ! 计算所需消息长度并分配缓冲区
  msg_len = len(this%prefix_) + len(msg) + len(this%file_) + 30  ! 额外空间用于格式文本和行号
  allocate(character(len=msg_len) :: full_msg)
  !
  ! 格式化错误消息
  write(full_msg, '(A,": ", A, " Error at: ",A, ": ",I0)') &
    this%prefix_, msg, this%file_, this%line_
  ! 在标准环境中使用单元0输出到标准错误
  write(6, '(A)') trim(full_msg)
  call xit
  end procedure throw_error_impl
  !*****************************************************************************
  !> @brief Assert that a logical expression is true
  !>
  !> @details Implementation of assertion procedure that checks if the
  !>          expression is true, if not, throws an error with the
  !>          provided message and location information
  !>
  !> @param[in] expr Logical expression to evaluate
  !> @param[in] msg Error message to display if assertion fails
  !> @param[in] file Name of the source file where assertion is called
  !> @param[in] line Line number in the source file
  !*****************************************************************************
  module procedure ASSERT_TRUE
  character(len=:), allocatable :: validator_msg
  type(ErrorContext) Error
  !
  validator_msg = "Condition evaluated to false"
  if(.not. expr) then
    Error = ErrorContext(file, line, msg)
    call Error%throw_error(validator_msg)
  endif
  end procedure ASSERT_TRUE
endsubmodule
!*****************************************************************************
!> @brief Preprocessing implementation submodule
!>
!> @details Implementation submodule for preprocessing utilities,
!> containing actual implementations of tensor-array conversions,
!> rotation matrix generation, and debugging functions.
!>
!> @author wuwenhao
!> @date 2025/11/27
!*****************************************************************************
submodule(presolve_mod) presolve_impl
  use Base_config, only: DP
  use tensor_opt_mod
  implicit none
  type(Torch) :: torch_
contains
  !**************************************************************************
  !> @brief Convert array to tensor implementation
  !>
  !> @details Convert a 1D array to a 3x3 2D tensor representation.
  !> Supports different array sizes (3, 4, 6) for different tensor
  !> representations (2D plane stress, axisymmetric, full 3D).
  !>
  !> @param[in]  array  Input vector in Voigt notation
  !> @param[in]  scalar Optional scaling factor for shear components
  !>
  !> @return 3x3 tensor representation
  !**************************************************************************
  module procedure convert_array_to_tensor
  real(DP) :: scalar_
  if(present(scalar)) then
    scalar_ = scalar
  else
    scalar_ = 1.0d0
  endif
  select case(size(array))
  case(3)
    tensor(1, 1) = array(1)
    tensor(2, 2) = array(2)
    tensor(1, 2) = array(3) / scalar_
    tensor(2, 1) = array(3) / scalar_
  case(4)
    tensor(1, 1) = array(1)
    tensor(2, 2) = array(2)
    tensor(3, 3) = array(3)
    tensor(1, 2) = array(4) / scalar_
    tensor(2, 1) = array(4) / scalar_
  case(6)
    tensor(1, 1) = array(1)
    tensor(2, 2) = array(2)
    tensor(3, 3) = array(3)
    tensor(1, 2) = array(4) / scalar_
    tensor(2, 1) = array(4) / scalar_
    tensor(1, 3) = array(5) / scalar_
    tensor(3, 1) = array(5) / scalar_
    tensor(2, 3) = array(6) / scalar_
    tensor(3, 2) = array(6) / scalar_
  endselect
  end procedure convert_array_to_tensor
  !**************************************************************************
  !> @brief Convert tensor to array implementation
  !>
  !> @details Convert a 3x3 2D tensor to a 1D array representation.
  !> Supports different output sizes (3, 4, 6) for different tensor
  !> representations. Applies optional scaling to shear components.
  !>
  !> @param[in]  tensor  Input 3x3 tensor
  !> @param[in]  size    Output array size (3, 4, or 6)
  !> @param[in]  scalar  Optional integer scaling parameter
  !>
  !> @return 1D array in Voigt notation
  !**************************************************************************
  module procedure Convert_tensor_to_array
  real(DP) :: scalar_
  if(present(scalar)) then
    scalar_ = scalar
  else
    scalar_ = 1.0_DP
  endif
  select case(size)
  case(3)
    array(1) = tensor(1, 1)
    array(2) = tensor(2, 2)
    array(3) = tensor(1, 2) * scalar_
  case(4)
    array(1) = tensor(1, 1)
    array(2) = tensor(2, 2)
    array(3) = tensor(3, 3)
    array(4) = tensor(1, 2) * scalar_
  case(6)
    array(1) = tensor(1, 1)
    array(2) = tensor(2, 2)
    array(3) = tensor(3, 3)
    array(4) = tensor(1, 2) * scalar_
    array(5) = tensor(1, 3) * scalar_
    array(6) = tensor(2, 3) * scalar_
  endselect
  end procedure Convert_tensor_to_array
  !*****************************************************************************
  !> @brief Convert fourth-order tensor to second-order matrix implementation
  !>
  !> @details Convert a 3x3x3x3 fourth-order tensor to a 2D matrix
  !> using Voigt notation mapping. Applies symmetrization for
  !> minor symmetries in the fourth-order tensor.
  !>
  !> @param[in]  tensor4  Input fourth-order tensor (3x3x3x3)
  !> @param[in]  size     Output matrix size (6 for symmetric tensors)
  !>
  !> @return 2D matrix representation
  !*****************************************************************************
  module procedure Convert_tensor4_to_tensor2
  integer, parameter :: FST(6) = [1, 2, 3, 1, 1, 2]
  integer, parameter :: SCD(6) = [1, 2, 3, 2, 3, 3]
  integer :: i1, i2, j1, j2, i, j
  !
  do j = 1, size
    j1 = FST(j)
    j2 = SCD(j)
    do i = 1, size
      i1 = FST(i)
      i2 = SCD(i)
      tensor2(i, j) = 0.25d0 * (tensor4(i1, i2, j1, j2) + tensor4(i1, i2, j2, j1) + &
                                tensor4(i2, i1, j1, j2) + tensor4(i2, i1, j2, j1))
    enddo
  enddo
  end procedure Convert_tensor4_to_tensor2
  !*****************************************************************************
  !> @brief Generate rotation matrix implementation
  !>
  !> @details Generate a 3x3 rotation matrix using Rodrigues' rotation
  !> formula for given angle and axis. Validates input axis and
  !> normalizes it before computation.
  !>
  !> @param[in]  angle      Rotation angle in radians
  !> @param[in]  axis       Rotation axis vector (3D)
  !>
  !> @return 3x3 rotation matrix
  !*****************************************************************************
  module procedure Get_rotation_matrix
  real(DP), dimension(3) :: uaxis
  real(DP) :: cos, sin, temp, ux, uy, uz
  !
  if(axis(1) == 0.0_DP .and. axis(2) == 0.0_DP .and. axis(3) == 0.0_DP) then
    write(6, *) "Error: Rotation axis cannot be zero vector."
    call exit(1)
  endif
  uaxis = axis / dsqrt(sum(axis**2))
  ux = uaxis(1)
  uy = uaxis(2)
  uz = uaxis(3)
  cos = dcos(angle)
  sin = dsin(angle)
  temp = 1.0_DP - cos
  rot_matrix(1, 1) = cos + ux * ux * temp
  rot_matrix(1, 2) = ux * uy * temp - uz * sin
  rot_matrix(1, 3) = ux * uz * temp + uy * sin
  rot_matrix(2, 1) = uy * ux * temp + uz * sin
  rot_matrix(2, 2) = cos + uy * uy * temp
  rot_matrix(2, 3) = uy * uz * temp - ux * sin
  rot_matrix(3, 1) = uz * ux * temp - uy * sin
  rot_matrix(3, 2) = uz * uy * temp + ux * sin
  rot_matrix(3, 3) = cos + uz * uz * temp
  end procedure Get_rotation_matrix
  !**************************************************************************
  !> @brief Print array implementation
  !>
  !> @details Print the contents of a 1D array to standard output
  !> with formatting for debugging purposes.
  !>
  !> @param[in]  array  Input array to print
  !**************************************************************************
  module procedure print_array
  implicit none
  integer :: i, n

  n = size(array)
  print *, "Array size: ", n
  print *, "Array elements:"
  do i = 1, n
    print *, "  array(", i, ") = ", array(i)
  enddo
  end procedure print_array
  !**************************************************************************
  !> @brief ABAQUS debugging utility implementation
  !>
  !> @details Implementation of debugging function for ABAQUS UMAT
  !> simulations. Checks if current element and integration point
  !> match specified debugging targets, then prints debug information
  !> and optionally pauses for user input on first run.
  !>
  !> @param[in]  noel_num   Number of elements
  !> @param[in]  npt_num    Number of integration points
  !> @param[in]  num        Additional numerical parameter
  !> @param[in]  noel       Current element number
  !> @param[in]  npt        Current integration point number
  !> @param[in]  iteration  Current iteration number
  !> @param[in]  names      Descriptive name for debugging output
  !**************************************************************************
  module procedure abaqus_debug
  logical :: firstrun = .true.
  integer :: tempvar
  if((noel_num == noel) .and. (npt_num == npt) .and. num >= iteration) then
    write(*, *) "debug in ", trim(names)
    if(firstrun) then
      write(*, *) "please input an integer"
      read(*, *) tempvar
    endif
  endif
  end procedure abaqus_debug
endsubmodule
!*******************************************************************************
!> @brief tensor_torch_mod
!>
!> @details Module for tensor operations in continuum mechanics
!>
!> @author wuwenhao
!> @date 2025/11/27
!*******************************************************************************
submodule(tensor_opt_mod) tensor_torch_impl
  use Base_config
  implicit none
  type(Torch) :: torch_
contains
  !*****************************************************************************
  !> @brief Tensor4_ddot_tensor2
  !>
  !> @details Calculate the double dot product of a fourth-order tensor and
  !> a second-order tensor
  !> @param[in]  tensor4  Fourth-order tensor
  !> @param[in]  tensor2  Second-order tensor
  !>
  !> @return Resulting second-order tensor
  !*****************************************************************************
  module procedure Tensor4_ddot_tensor2
  integer :: i, j
  do j = 1, 3
    do i = 1, 3
      res(i, j) = sum(tensor4(i, j, :, :) * tensor2(:, :))
    enddo
  enddo
  end procedure Tensor4_ddot_tensor2
  !*****************************************************************************
  !> @brief Tensor2_ddot_tensor4
  !>
  !> @details Calculate the double dot product of a second-order tensor and
  !> a fourth-order tensor
  !> @param[in]  tensor2  Second-order tensor
  !> @param[in]  tensor4  Fourth-order tensor
  !>
  !> @return Resulting second-order tensor
  !*****************************************************************************
  module procedure Tensor2_ddot_tensor4
  integer :: i, j
  do j = 1, 3
    do i = 1, 3
      res(i, j) = sum(tensor2(:, :) * tensor4(:, :, i, j))
    enddo
  enddo
  end procedure Tensor2_ddot_tensor4
  !*****************************************************************************
  !> @brief Dyadic product implementation
  !>
  !> @details Calculate the dyadic (outer) product of two second-order
  !> tensors, resulting in a fourth-order tensor.
  !>
  !> @param[in]  tensorA  First second-order tensor (3x3)
  !> @param[in]  tensorB  Second second-order tensor (3x3)
  !>
  !> @return Fourth-order tensor (3x3x3x3)
  !*****************************************************************************
  module procedure Tensor2_dyad_tensor2
  integer :: i, j, k, l
  DO l = 1, 3
    DO k = 1, 3
      DO j = 1, 3
        DO i = 1, 3
          res(i, j, k, l) = tensorA(i, j) * tensorB(k, l)
        ENDDO
      ENDDO
    ENDDO
  ENDDO
  end procedure Tensor2_dyad_tensor2
  !*****************************************************************************
  !> @brief Print tensor implementation
  !>
  !> @details Print the components of a 3x3 tensor to standard output
  !> in formatted matrix layout for debugging and visualization.
  !>
  !> @param[in]  tensor  3x3 tensor to print
  !*****************************************************************************
  module procedure Print_impl
  integer :: i, j
  ! declaration
  do i = 1, 3
    write(6, '(A, I1, A)', advance='no') "Row ", i, ": ["
    do j = 1, 3
      write(6, '(F6.2)', advance='no') tensor(i, j)
      if(j < 3) write(6, '(A)', advance='no') ", "
    enddo
    write(6, '(A)') "]"
  enddo
  end procedure Print_impl
  !*****************************************************************************
  !> @brief Trace_Impl
  !>
  !> @details Calculate the trace of a stress tensor
  !>
  !> @param[in]  stress  Stress tensor
  !>
  !> @return Trace of the stress tensor
  !*****************************************************************************
  module procedure Trace_impl
  integer :: i
  !-----------------------------------------------------------------------------
  val = sum([(tensor(i, i), i=1, 3)])
  end procedure Trace_impl
  !*****************************************************************************
  !> @brief Deviatoric_impl
  !>
  !> @details Calculate the deviatoric part of a stress tensor
  !>
  !> @param[in]  stress  Stress tensor
  !>
  !> @return Deviatoric stress tensor
  !*****************************************************************************
  module procedure Deviatoric_impl
  real(DP) :: P
  !-----------------------------------------------------------------------------
  P = torch_%Trace(tensor) / 3.0_DP
  res = tensor - P * DELTA
  end procedure Deviatoric_impl
  !*****************************************************************************
  !> @brief Sec_dev_invar_impl
  !>
  !> @details Calculate the second deviatoric invariant (J2) of a stress tensor
  !>
  !> @param[in]  stress  Stress tensor
  !>
  !> @return Second deviatoric invariant (J2)
  !*****************************************************************************
  module procedure Get_J2_impl
  real(DP), dimension(3, 3) :: S
  !-----------------------------------------------------------------------------
  S = torch_%Deviatoric(tensor)
  val = sum(S**2) / 2.0_DP
  end procedure Get_J2_impl
  !*****************************************************************************
  !> @brief Get_J3_impl
  !>
  !> @details Calculate the third deviatoric invariant (J3) of a stress tensor
  !>
  !> @param[in]  stress  Stress tensor
  !>
  !> @return Third deviatoric invariant (J3)
  !*****************************************************************************
  module procedure Get_J3_impl
  real(DP), dimension(3, 3) :: S, temp
  !-----------------------------------------------------------------------------
  S = torch_%Deviatoric(tensor)
  temp = matmul(S, matmul(S, S))
  val = torch_%Trace(temp) / 3.0_DP
  end procedure Get_J3_impl
  !*****************************************************************************
  !> @brief Ratio_impl
  !>
  !> @details Calculate the stress ratio tensor (deviatoric stress divided by
  !> mean stress)
  !> @param[in]  stress  Stress tensor
  !>
  !> @return Stress ratio tensor
  !*****************************************************************************
  module procedure Ratio_impl
  ! declaration
  real(DP), dimension(3, 3) :: S
  real(DP) :: P
  ! implementation
  P = torch_%Trace(tensor) / 3.0_DP
  P = merge(P, sign(EPS, P), abs(P) >= EPS)
  ! deviatoric stress
  S = torch_%Deviatoric(tensor)
  ! return tensor
  res = S / P
  end procedure Ratio_impl
  !*****************************************************************************
  !> @brief Get_sin3t_impl
  !>
  !> @details Calculate sin(3θ) where θ is the Lode angle
  !>
  !> @param[in]  stress  Stress tensor
  !>
  !> @return sin(3θ) value
  !*****************************************************************************
  module procedure Get_sin3t_impl
  real(DP) :: J2, J3
  !-----------------------------------------------------------------------------
  ! compute J2 and J3
  J2 = torch_%Get_J2(tensor)
  J2 = merge(J2, sign(EPS, J2), abs(J2) >= EPS)
  J3 = torch_%Get_J3(tensor)
  !
  val = -1.5_dp * DSQRT(3.0_dp) * J3 / (J2**1.5_dp)
  val = merge(val, sign(1.0_DP, val), abs(val) <= 1.0_DP)
  end procedure Get_sin3t_impl
  !*****************************************************************************
  !> @brief Shear_impl
  !>
  !> @details Calculate the shear stress (sqrt(J2))
  !>
  !> @param[in]  stress  Stress tensor
  !>
  !> @return Shear stress value
  !*****************************************************************************
  module procedure Shear_impl
  real(DP) :: J2
  !-----------------------------------------------------------------------------
  J2 = torch_%Get_J2(tensor)
  res = dsqrt(3.0_DP * J2)
  end procedure Shear_impl
  !*****************************************************************************
  !> @brief Normalize tensor implementation
  !>
  !> @details Normalize a tensor by dividing by its Frobenius norm,
  !> resulting in a unit tensor with the same direction.
  !> Handles near-zero norms with epsilon protection.
  !>
  !> @param[in]  tensor  Input 3x3 tensor
  !>
  !> @return Normalized unit tensor
  !*****************************************************************************
  module procedure Normalize_impl
  real(DP) :: norm
  !-----------------------------------------------------------------------------
  norm = sum(tensor**2)
  norm = max(dsqrt(norm), eps)
  res(:, :) = tensor(:, :) / norm
  end procedure Normalize_impl
  !*****************************************************************************
  !> @brief Calculate tensor norm implementation
  !>
  !> @details Calculate the Frobenius norm (Euclidean norm) of a 3x3 tensor.
  !> Returns zero for tensors with near-zero components.
  !>
  !> @param[in]  tensor  Input 3x3 tensor
  !>
  !> @return Frobenius norm of the tensor
  !*****************************************************************************
  module procedure Norm_impl
  real(DP) :: temp
  temp = sum(tensor**2)
  res = max(dsqrt(temp), 0.0_DP)
  end procedure Norm_impl
  !*****************************************************************************
  !> @brief Calculate cosine of angle between tensors implementation
  !>
  !> @details Calculate the cosine of the angle between two tensors
  !> using their double dot product and Frobenius norms.
  !> Includes protection against division by zero and ensures
  !> result stays within [-1, 1] range.
  !>
  !> @param[in]  tensorA  First 3x3 tensor
  !> @param[in]  tensorB  Second 3x3 tensor
  !>
  !> @return Cosine of angle between tensors (clamped to [-1, 1])
  !*****************************************************************************
  module procedure Get_cost_impl
  real(DP) :: norm_A, norm_B, dot_product
  !
  norm_A = torch_%norm(tensorA)
  norm_A = max(norm_A, EPS)
  norm_B = torch_%Norm(tensorB)
  norm_B = max(norm_B, EPS)
  dot_product = sum(tensorA * tensorB)
  !
  val = dot_product / (norm_A * norm_B)
  val = merge(val, sign(1.0_DP, val), abs(val) <= 1.0_DP)
  end procedure Get_cost_impl
  !*****************************************************************************
  !> @brief Calculate R_m parameter implementation
  !>
  !> @details Calculate the R_m parameter used in critical state soil
  !> mechanics. Computed as sqrt(3*J2) of the stress ratio tensor.
  !>
  !> @param[in]  tensor  Stress tensor
  !>
  !> @return R_m parameter value
  !*****************************************************************************
  module procedure Get_Rm_impl
  real(DP), dimension(3, 3) :: ratio
  ratio = torch_%Get_ratio(tensor)
  val = dsqrt(3.0_DP * torch_%Get_J2(ratio))
  end procedure Get_Rm_impl
  !*****************************************************************************
  !> @brief Calculate unit deviatoric tensor implementation
  !>
  !> @details Calculate the unit deviatoric tensor by first extracting
  !> the deviatoric part of the stress tensor, then normalizing it.
  !>
  !> @param[in]  tensor  Input stress tensor
  !>
  !> @return Unit deviatoric tensor
  !*****************************************************************************
  module procedure Get_unit_devivator_impl
  real(DP), dimension(3, 3) :: S
  !-----------------------------------------------------------------------------
  S = torch_%Deviatoric(tensor)
  res = torch_%Normalize(S)
  end procedure
!*******************************************************************************
endsubmodule tensor_torch_impl
submodule(elastic_mod) elastic_impl
  use Base_config
  use Material_config
  use tensor_opt_mod
  implicit none
  type(Torch) torch_
  type(Elast) elast_
contains
  !*****************************************************************************
  module procedure Get_principal_impl
  real(DP), dimension(6) :: temp
  !-----------------------------------------------------------------------------
  temp(1) = tensor(1, 1)
  temp(2) = tensor(2, 2)
  temp(3) = tensor(3, 3)
  temp(4) = tensor(1, 2)
  temp(5) = tensor(1, 3)
  temp(6) = tensor(2, 3)
  call sprinc(temp, res, 1, 3, 3)
  end procedure Get_principal_impl
  !*****************************************************************************
  !> @brief 函数简要说明
  !>
  !> @details 函数详细描述
  !>
  !> @param[in]  参数名 输入参数说明
  !> @param[out] 参数名 输出参数说明
  !>
  !> @return 返回值说明
  !*****************************************************************************
  module procedure Get_gtheta_impl
  real(DP) :: gtheta1, sin3t
  !
  sin3t = torch_%Sin3theta(shvars%get_sigma())
  if(abs(sin3t) .lt. EPS) sin3t = sign(EPS, sin3t)
  gtheta1 = (-dsqrt((1.0_DP + PARAM%C**2)**2 + 4.0_DP * PARAM%C * &
                    (1.0_DP - PARAM%C**2) * sin3t) - (1.0_DP + PARAM%C**2)) &
            / 2.0_DP / (1.0_DP - PARAM%C) / sin3t
  gtheta(1) = -PARAM%C * (1.0_DP + PARAM%C) / (1.0_DP - PARAM%C) / sin3t &
              / gtheta1
  IF((gtheta(1)) .GE. 1.0D0) gtheta(1) = 1.0D0
  IF((gtheta(1)) .LE. PARAM%C) gtheta(1) = PARAM%C
  ! Calculate pgtheta_psin3t
  gtheta(2) = (PARAM%C - 1.0_DP) * gtheta(1)**2 &
              / (2.0_DP * (1.0_DP - PARAM%C) &
                 * sin3t * gtheta(1) + (1.0_DP + PARAM%C**2))
  gtheta(2) = merge(gtheta(2), sign(EPS, gtheta(2)), abs(gtheta(2)) >= EPS)
  !
  gtheta(3) = (4.0_DP * (PARAM%C - 1.0_DP) * gtheta(1) * gtheta(2) &
               + 2.0_DP * (PARAM%C - 1.0_DP) * sin3t * gtheta(2)) &
              / (sin3t * gtheta(1) * 2.0_DP * (1.0_DP - PARAM%C) &
                 + (1.0_DP + PARAM%C**2))
  gtheta(3) = merge(gtheta(3), sign(EPS, gtheta(3)), abs(gtheta(3)) >= EPS)
  end procedure Get_gtheta_impl
  !*****************************************************************************
  !> @brief Get_Fr_impl
  !>
  !> @details 函数详细描述
  !>
  !> @param[in]  参数名 输入参数说明
  !> @param[out] 参数名 输出参数说明
  !>
  !> @return 返回值说明
  !*****************************************************************************
  module procedure Get_Fr_impl
  real(DP) :: RM, gtheta(3), sin3t
  real(DP), dimension(3, 3) :: r
  gtheta = elast_%Get_gtheta(shvars)
  sin3t = torch_%Sin3theta(shvars%get_sigma())
  r = torch_%Get_ratio(shvars%get_sigma())
  RM = torch_%Get_Rm(shvars%get_sigma())
  res = 1.5_DP / (RM * gtheta(1))**2 * &
        ((RM * gtheta(1) + 3.0_DP * RM * sin3t * gtheta(2)) * r + &
         9.0_DP * gtheta(2) * matmul(r, r))
  !
  end procedure Get_Fr_impl
  !*****************************************************************************
  module procedure Get_dnorm_impl
  real(DP), dimension(3, 3) :: Fr
  real(DP), dimension(3) :: pris
  logical :: is_isotropic
  !-----------------------------------------------------------------------------
  pris = elast_%Get_principal(shvars%get_sigma())
  is_isotropic = (abs(pris(1) - pris(2)) < EPS) &
                 .and. (abs(pris(2) - pris(3)) <= EPS) &
                 .and. (abs(pris(1) - pris(3)) <= EPS)
  if(is_isotropic) then
    res(:, :) = 0.0_DP
    res(1, 1) = dsqrt(2.0_DP / 3.0_DP)
    res(2, 2) = -dsqrt(2.0_DP / 3.0_DP) / 2.D0
    res(3, 3) = -dsqrt(2.0_DP / 3.0_DP) / 2.D0
  else
    Fr = elast_%Get_Fr(shvars)
    res = torch_%Get_unit_devivator(Fr)
  endif
  end procedure Get_dnorm_impl
  !*****************************************************************************
  module procedure Get_anisotropy_impl
  real(DP), dimension(3, 3) :: dnorm
  !-----------------------------------------------------------------------------
  dnorm = elast_%Get_dnorm(shvars)
  ! Calculate abase
  val = sum(shvars%get_fabric() * dnorm)
  val = merge(val, sign(1.0_DP, val), abs(val) <= 1.0_DP)
  end procedure Get_anisotropy_impl
  !*****************************************************************************
  module procedure Get_shear_impl
  real(DP) :: P
  !-----------------------------------------------------------------------------
  ! mean effective shvars%get_sigma()(:,:)
  P = torch_%Trace(shvars%get_sigma()) / 3.0_DP
  ! shear modulus
  shear = PARAM%G0 * (2.973_DP - stvars%get_voidr())**2 &
          / (1.0_DP + stvars%get_voidr()) * dsqrt(P * PA)
  end procedure Get_shear_impl
  !*****************************************************************************
  module procedure Get_bulk_impl
  real(DP) :: shear
  shear = elast_%Get_shear(shvars, stvars)
  bulk = shear * 2.0_DP * (1.0_DP + PARAM%NU) / 3.0_DP &
         / (1.0_DP - 2.0_DP * PARAM%NU)
  end procedure Get_bulk_impl
  !*****************************************************************************
  !> @brief 函数简要说明
  !>
  !> @details 函数详细描述
  !>
  !> @param[in]  参数名 输入参数说明
  !> @param[out] 参数名 输出参数说明
  !>
  !> @return 返回值说明
  !*****************************************************************************
  module procedure Yield_distance_impl
  real(DP) :: RM, gtheta(3), abase, A
  !-----------------------------------------------------------------------------
  abase = elast_%Get_anisotropy(shvars)
  A = -PARAM%KH * (abase - 1)**2
  RM = torch_%Get_Rm(shvars%get_sigma())
  gtheta = elast_%Get_gtheta(shvars)
  ftol = RM / gtheta(1) - shvars%get_harden() * exp(A)
  end procedure Yield_distance_impl
  !*****************************************************************************
  !> @brief get_stiffness_impl
  !>
  !> @details 函数详细描述
  !>
  !> @param[in] shvars%get_sigma()(:,:) : current shvars%get_sigma()(:,:) tensor(3x3)
  !> @param[in] void_ratio : current void ratio(scalar)
  !> @param[out] stiffness : a stiffness tensor of size 3x3x3x3
  !>
  !> @return 返回值说明
  !*****************************************************************************
  module procedure get_stiffness_impl
  real(DP) :: shear, bulk
  integer :: i, j, k, l
  shear = elast_%Get_shear(shvars, stvars)
  bulk = elast_%Get_bulk(shvars, stvars)
  ! stiffness tensor
  do l = 1, 3
    do k = 1, 3
      do j = 1, 3
        do i = 1, 3
          stiffness(i, j, k, l) = &
            (bulk - 2.0_DP / 3.0_DP * shear) * DELTA(i, j) * DELTA(k, l) + &
            shear * (DELTA(i, k) * DELTA(j, l) + DELTA(i, l) * DELTA(j, k))
        enddo
      enddo
    enddo
  enddo
  end procedure get_stiffness_impl
  !*****************************************************************************
  module procedure calc_dsigma_impl
  real(DP), dimension(3, 3, 3, 3) :: stiff
  !-----------------------------------------------------------------------------
  stiff = get_stiffness_impl(shvars, stvars)
  dsigma = stiff.ddot.depsln
  end procedure calc_dsigma_impl
!*******************************************************************************
endsubmodule elastic_impl
submodule(plastic_mod) plastic_impl
  use Base_config
  use Material_config
  use tensor_opt_mod
  use elastic_mod
  implicit none
  type(Torch) :: torch_
  type(Elast) :: elast_
  type(plast) :: plast_
contains
  !*****************************************************************************
  module procedure Get_pfpr_impl
  real(DP) :: sin3t, gtheta(3), RM, frmag, abase, A, hardg, pfpA
  real(DP), dimension(3, 3) :: Fr, r, dnorm, pApr, r_squared
  real(DP), dimension(3, 3, 3, 3) :: pnpr
  integer :: i, j, k, l
  real(DP), dimension(7) :: scalar
  real(DP), dimension(3, 3, 3, 3, 7) :: tensor4
  !-----------------------------------------------------------------------------
  Fr = elast_%Get_Fr(shvars)
  !
  sin3t = torch_%Sin3theta(shvars%get_sigma())
  gtheta = elast_%Get_gtheta(shvars)
  RM = torch_%Get_Rm(shvars%get_sigma())
  r = torch_%Get_ratio(shvars%get_sigma())
  r_squared = matmul(r, r)
  scalar(1) = (1.0_DP + 3.0_DP * sin3t / gtheta(1) * gtheta(2)) &
              * 3.0_DP / 4.0_DP / RM / gtheta(1)
  scalar(2) = 27.0_DP / 4.0_DP / (RM * gtheta(1))**2 * gtheta(2)
  scalar(3) = +9.0_DP / 4.0_DP / RM**3 / gtheta(1) &
              * (-1.0_DP + 9.0_DP * sin3t / gtheta(1) &
                 * (2.0_DP * sin3t / gtheta(1) * gtheta(2)**2 &
                    - gtheta(2) - sin3t * gtheta(3)))
  scalar(4) = 81.0_DP / 4.0_DP / RM**4 / gtheta(1)**2 &
              * (6.0_DP * sin3t / gtheta(1) * gtheta(2)**2 &
                 - 2.0_DP * gtheta(2) - 3.0_DP * sin3t * gtheta(3))
  scalar(5) = 729.0_DP / 4.0_DP / RM**5 / gtheta(1)**3 &
              * (2.0_DP * gtheta(2)**2 - gtheta(1) * gtheta(3))
  scalar(6) = -27.0_DP * sin3t / 2.0_DP / RM**2 / gtheta(1)**2 &
              * (2.D0 / gtheta(1) * gtheta(2)**2 - gtheta(3))
  scalar(7) = -81.0_DP / 2.0_DP / RM**3 / gtheta(1)**2 &
              * (2.0_DP / gtheta(1) * gtheta(2)**2 - gtheta(3))
  do l = 1, 3
    do k = 1, 3
      do j = 1, 3
        do i = 1, 3
          tensor4(i, j, k, l, 1) = &
            (DELTA(I, K) * DELTA(J, L) + DELTA(I, L) * DELTA(J, K))
          tensor4(i, j, k, l, 2) = &
            (R(I, K) * DELTA(J, L) + R(I, L) * DELTA(J, K) &
             + R(L, J) * DELTA(I, K) + R(K, J) * DELTA(I, L))

        enddo
      enddo
    enddo
  enddo
  tensor4(i, j, k, l, 3) = R(i, j) * R(k, l)
  tensor4(:, :, :, :, 4) = &
    2.0 * R(:, :) .dyad.r_squared
  tensor4(:, :, :, :, 5) = r_squared.dyad.r_squared
  tensor4(:, :, :, :, 6) = DELTA.dyad.R
  tensor4(:, :, :, :, 7) = DELTA(:, :) .dyad.r_squared
  !
  pnpr(:, :, :, :) = sum([(scalar(i) * tensor4(:, :, :, :, i), i=1, 7)])
  frmag = torch_%Norm(Fr)
  dnorm = elast_%Get_dnorm(shvars)
  abase = elast_%Get_anisotropy(shvars)
  pApr(:, :) = ((shvars%get_fabric() - abase * dnorm) / frmag) .ddot.pnpr
  A = -PARAM%KH * (abase - 1)**2
  hardg = RM / gtheta(1) / exp(A)
  pfpA = 2.0_DP * PARAM%KH * hardg * (abase - 1.0_DP) * exp(A)
  res = Fr + pfpA * pApr
  end procedure Get_pfpr_impl
  !*****************************************************************************
  !> @brief get_pfsig_impl
  !>
  !> @details 函数详细描述
  !>
  !> @param[in]  stress :current stress tensor
  !> @param[out] pfsig :Return a yield function corresponding to the partial
  !> derivative of stress
  !>
  !> @return 返回值说明
  !*****************************************************************************
  module procedure get_pfsig_impl
  real(DP), dimension(3, 3, 3, 3) :: prpsig
  real(DP) :: mean
  real(DP), dimension(3, 3) :: pfpr, sigma
  integer :: i, j, k, l
  sigma = shvars%get_sigma()
  mean = torch_%Trace(sigma) / 3.0_dp
  do l = 1, 3
    do k = 1, 3
      do j = 1, 3
        do i = 1, 3
          prpsig(i, j, k, l) = DELTA(i, k) * DELTA(j, l) / mean - &
                               sigma(i, j) * DELTA(k, l) / mean**2 / 3.0_dp
        enddo
      enddo
    enddo
  enddo
  !
  pfpr = plast_%Get_pfpr(shvars)
  res = pfpr.ddot.prpsig
  end procedure get_pfsig_impl
  !*****************************************************************************
  !> @brief Get_pgsig_impl
  !>
  !> @details 函数详细描述
  !>
  !> @param[in]  stress :current stress tensor
  !> @param[out] pfsig :Return a yield function corresponding to the partial
  !> derivative of stress
  !>
  !> @return 返回值说明
  !*****************************************************************************
  module procedure Get_pgsig_impl
  real(DP), dimension(3, 3) :: pgpr, dev_pgpr, norm_dev_pgpr
  real(DP) :: dpla
  !
  pgpr = plast_%Get_pfpr(shvars)
  !
  dpla = plast_%Get_dilatancy(shvars, stvars)
  dev_pgpr = torch_%Deviatoric(pgpr)
  norm_dev_pgpr = torch_%Normalize(dev_pgpr)
  res = norm_dev_pgpr + dsqrt(2.0_DP / 3.0_DP) * dpla * DELTA / 3.0_DP
  end procedure Get_pgsig_impl
  !*****************************************************************************
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
  !*****************************************************************************
  module procedure Get_psim_impl
  real(DP) :: e_c, P, psi, abase
  P = torch_%Trace(shvars%get_sigma()) / 3.0_DP
  abase = elast_%Get_anisotropy(shvars)
  e_c = PARAM%VOIDC - PARAM%LAC * (P / PA)**PARAM%KSI
  psi = stvars%get_voidr() - e_c
  res = psi - PARAM%EA * (abase - 1.0_DP)
  end procedure Get_psim_impl
  !*****************************************************************************
  !> @brief Get_dilatancy_impl
  !>
  !> @details 函数详细描述
  !>
  !> @param[in] sigma : current stress tensor
  !> @param[in] fabric : current fabric tensor
  !> @param[in]  voidr : current void ratio
  !> @param[out] voidc : psim
  !>
  !> @return 返回值说明
  !*****************************************************************************
  module procedure Get_dilatancy_impl
  real(DP) :: psim, M_d, gtheta(3), RM
  !
  RM = torch_%Get_Rm(shvars%get_sigma())
  gtheta = elast_%Get_gtheta(shvars)
  psim = plast_%Get_psim(shvars, stvars)
  M_d = PARAM%MCS * gtheta(1) * exp(PARAM%DM * psim)
  res = PARAM%D1 / (PARAM%MCS * gtheta(1)) &
        * (1.0_DP + RM / (PARAM%MCS * gtheta(1))) * (M_d - RM)
  end procedure Get_dilatancy_impl
  !*****************************************************************************
  !> @brief Get_evolution_impl
  !>
  !> @details 函数详细描述
  !>
  !> @param[in] sigma : current stress tensor
  !> @param[in] fabric : current fabric tensor
  !> @param[in]  voidr : current void ratio
  !> @param[out] voidc : psim
  !>
  !> @return 返回值说明
  !*****************************************************************************
  module procedure Get_evolution_impl
  real(DP) :: psim, gtheta(3), RM, P
  real(DP) :: M_p, abase, G
  real(DP), dimension(3, 3) :: dnorm, Fr
  !-----------------------------------------------------------------------------
  Fr = elast_%Get_Fr(shvars)
  dnorm = elast_%Get_dnorm(shvars)
  ! shear modulus
  G = elast_%Get_shear(shvars, stvars)
  ! an
  abase = elast_%Get_anisotropy(shvars)
  RM = torch_%Get_Rm(shvars%get_sigma())
  gtheta = elast_%Get_gtheta(shvars)
  psim = plast_%Get_psim(shvars, stvars)
  M_p = PARAM%MCS * gtheta(1) * exp(-PARAM%NKP * psim)
  P = torch_%Trace(shvars%get_sigma()) / 3.0_DP
  !
  Rh = G * (1.0_DP - PARAM%CH * stvars%get_voidr()) * (M_p - RM) / (P * RM)
  RF = PARAM%FEVR * (dnorm - shvars%get_fabric())
  end procedure Get_evolution_impl
  !*****************************************************************************
  module procedure Get_Dkp_impl
  real(DP) :: Rh, Rf(3, 3), gtheta(3), RM
  real(DP) :: pfpA, pfph, abase, A, hardg, P
  real(DP), dimension(3, 3) :: Fr, pApF
  !-----------------------------------------------------------------------------
  call plast_%Get_evolution(shvars, stvars, Rh, Rf)
  gtheta = elast_%Get_gtheta(shvars)
  RM = torch_%Get_Rm(shvars%get_sigma())
  abase = elast_%Get_anisotropy(shvars)
  A = -PARAM%KH * (abase - 1)**2
  hardg = RM / gtheta(1) / exp(A)
  pfpA = 2.0_DP * PARAM%KH * hardg * (abase - 1.0_DP) * exp(A)
  Fr = elast_%Get_Fr(shvars)
  pApf = torch_%Get_unit_devivator(Fr)
  pfph = -exp(A)
  P = torch_%Trace(shvars%get_sigma()) / 3.0_DP
  Dkp = -(pfph * Rh + pfpA * sum(pApf * Rf))
  end procedure Get_Dkp_impl
  !*****************************************************************************
  module procedure Elstop_impl
  real(DP), dimension(3, 3) :: pfsig, xm, theta
  real(DP), dimension(3, 3, 3, 3) :: stiff
  real(DP) :: dnmetr, Dkp, frnde, lamda, hdl, Rf(3, 3), Rh
  !-----------------------------------------------------------------------------
  pfsig = plast_%Get_pfsig(shvars)
  xm = plast_%Get_pgsig(shvars, stvars)
  Dkp = plast_%Get_Dkp(shvars, stvars)
  stiff = elast_%Get_stiffness(shvars, stvars)
  dnmetr = sum(pfsig * (stiff.ddot.xm))
  frnde = dnmetr + Dkp
  if(abs(frnde) <= EPS) frnde = sign(frnde, EPS)
  !
  theta = (pfsig.ddot.stiff) / frnde
  !
  lamda = sum(theta * depsln)
  ! hdl = lamda > 0.0_DP ? 1.0_DP : 0.0_DP
  hdl = merge(1.0_DP, 0.0_DP, lamda > 0.0_DP)
  dempx = stiff - hdl * ((stiff.ddot.xm) .dyad.theta)
  !
  call plast_%Get_evolution(shvars, stvars, Rh, Rf)
  Rshvars = Share_var(hdl * lamda * Rh, dempx.ddot.depsln, hdl * lamda * Rf)
  end procedure Elstop_impl
  !*****************************************************************************
  !
!*******************************************************************************
endsubmodule plastic_impl
!*******************************************************************************
!> @brief Math module for numerical algorithms
!>
!> @details This module provides mathematical utilities including bisection method,
!>          interval checking, and other numerical algorithms used in the UMAT.
!>
!> @author wuwenhao
!> @date 2025/11/27
!*******************************************************************************
! #ifdef DEBUG
! #define ENABLE_MONOTONIC_CHECK 1
! #else
! #define ENABLE_MONOTONIC_CHECK 0
! #endif
submodule(math_mod) math_impl
  use exception_mod
  use tensor_opt_mod
  use elastic_mod
  use plastic_mod
#include "macro.h"
  implicit none
  type(Elast) :: elast_
  type(Plast) :: plast_
  type(Math) :: math_
  type(Torch) :: torch_
  !
contains
  !*****************************************************************************
  !> @brief Interval checking implementation
  !>
  !> @details This subroutine performs interval checking to find the appropriate
  !>          scaling factor for the strain increment. It determines the right
  !>          boundary and the scaling factor for plastic correction.
  !>
  !> @param[in]  shvars Shared variables (stress tensor, etc.)
  !> @param[in]  stvars State variables (void ratio, etc.)
  !> @param[out] rbd    Right boundary of the scaling factor (0 <= rbd <= 1)
  !> @param[out] alout  Scaling factor for plastic correction
  !*****************************************************************************
  module procedure intchc_impl
  real(DP) :: mean_etr, mean_cur
  real(DP), dimension(3, 3) :: pfsig, dsigma
  real(DP) :: angle, lbd
  real(DP) :: fright, fleft, ftemp
  real(DP) :: iter, rbd
  !
  mean_cur = torch_%Trace(shvars%get_sigma()) / 3.0_DP
  mean_etr = mean_with_depsln(shvars, stvars, depsln)
  if(mean_etr <= 0.0_DP .and. mean_cur >= 0.0_DP) then
    ! Ensure that there is a solution within the domain.
    rbd = Bisection_impl(shvars, stvars, depsln, mean_with_depsln, &
                         0.0_DP, 1.0_DP, 0.0_DP, tol)
  else
    rbd = 1.0_DP
  endif
  !
  fleft = elast_%Yield_distance(shvars)
  fright = ftol_with_depsln(shvars, stvars, rbd * depsln)
  !
  if(fleft * fright >= 0.0_DP) then
    pfsig(:, :) = plast_%Get_pfsig(shvars)
    dsigma(:, :) = elast_%calc_dsigma(shvars, stvars, depsln)
    angle = torch_%Get_cost(pfsig, dsigma)
    if(angle < 0.0_DP) then
      iter = 0.0_DP
      do while(iter <= 1.0_DP)
        ftemp = ftol_with_depsln(shvars, stvars, iter * depsln)
        if(ftemp <= -tol) then
          lbd = iter
          exit
        else
          iter = iter + 0.01_DP
          if(iter >= 1.0_DP) then
            alout = 0.0_DP
            return
          endif
        endif
      enddo
    else
      alout = 0.0_DP
      return
    endif
  else
    lbd = 0.0_DP
  endif
  alout = Bisection_impl(shvars, stvars, depsln, ftol_with_depsln, &
                         lbd, rbd, 0.0_DP, tol)
  !
  end procedure intchc_impl
  !*****************************************************************************
  !> @brief Bisection method implementation
  !>
  !> @details This function implements the bisection method to find the root of
  !>          a function within a given interval [lbd, rbd]. The function must
  !>          have opposite signs at the boundaries. Monotonicity check can be
  !>          enabled in debug mode.
  !>
  !> @param[in] shvars   Shared variables (stress tensor, etc.)
  !> @param[in] stvars   State variables (void ratio, etc.)
  !> @param[in] func     Function to find root of (conforms to func_with_param)
  !> @param[in] lbd      Left boundary of the interval
  !> @param[in] rbd      Right boundary of the interval
  !> @param[in] condition Target value (usually 0 for root finding)
  !> @return alout       Root found within the interval
  !*****************************************************************************
  module procedure Bisection_impl
  real(DP) :: left, right, mid
  real(DP) :: f_left, f_right, f_mid
  real(DP) :: df_left, df_right, df_mid
  integer :: it, it_max
  !-----------------------------------------------------------------------------
  left = lbd
  right = rbd
  it_max = 100
  !check input variable
  CHECK_TRUE(left >= 0.0_DP .and. right <= 1.0_DP, "left and right should be in [0,1]")
  CHECK_TRUE(left < right, "The interval can not be emptied.")
  !
  f_left = func(shvars, stvars, left * depsln)
  f_right = func(shvars, stvars, right * depsln)
  !
  CHECK_TRUE(f_left * f_right <= 0.0_DP, " Bisection_impl: Thefunction must have different signs at the boundaries.")
  ! monotonic
  ! if(ENABLE_MONOTONIC_CHECK == 1) then
  !   monotonic = is_monotonic(shvars, stvars, depsln, func, left, right)
  !   CHECK_TRUE(monotonic, "func must be monotonic")
  ! endif
  ! iterator
  do it = 1, it_max
    mid = left + (right - left) / 2.0_DP
    f_mid = func(shvars, stvars, mid * depsln)
    df_mid = f_mid - condition
    df_left = f_left - condition
    df_right = f_right - condition
    if(abs(df_mid) <= tol) then
      alout = mid
      return
    endif
    if(df_left * df_mid >= 0.0_DP) then
      left = mid
      f_left = f_mid
    else
      right = mid
      f_right = f_mid
    endif
  enddo
  end procedure Bisection_impl
  !*****************************************************************************
  module procedure Onyield_impl
  type(Share_var) :: shtmp, Defor, Desec, shfor, shdrt
  type(State_var) :: sttmp, stfor, stdrt
  real(DP), dimension(3, 3, 3, 3) :: dempx1, dempx2, dedrt
  integer :: it, nfail
  real(DP) :: dt, t, sstol, rtol, beta, fupd
  logical :: converged
  !-----------------------------------------------------------------------------
  ! initialize variable
  dt = 1.0_DP
  t = 0.0_DP
  dempx = 0.0_DP
  shvar_upd = shvars
  stvar_upd = stvars
  sstol = 1.0D-6
  nfail = 0
  converged = .false.
  !
  do it = 1, 200
    call plast_%Elstop(shvar_upd, stvar_upd, dt * depsln, Defor, dempx1)
    shfor = shvar_upd + Defor
    stfor = stvar_upd
    if(shfor%is_low()) then
      if(dt <= 1.0D-6) then
        exit
      else
        dt = dt / 2.0_DP
        cycle
      endif
    endif
    call plast_%Elstop(shfor, stfor, dt * depsln, Desec, dempx2)
    shtmp = shvar_upd + (Defor + Desec) / 2.0_DP
    dempx = dempx + ((dempx1 + dempx2) / 2.0_DP) * dt
    sttmp = stfor
    call sttmp%update_voidr(dt * depsln)
    if(shtmp%is_low()) then
      if(dt <= 1.0D-6) then
        shvar_upd = shfor
        exit
      else
        dt = dt / 2.0_DP
        cycle
      endif
    endif
    !
    rtol = Get_residual_impl(Defor, Desec, shtmp)
    beta = 0.8 * dsqrt(sstol / rtol)
    !
    if(rtol <= sstol) then
      fupd = elast_%Yield_distance(shtmp)
      ! revise
      call drift_shvars_impl(shtmp, sttmp, dempx, tol, &
                             shdrt, stdrt, dedrt)
      ! update time
      t = t + dt
      ! update variable
      shvar_upd = shdrt
      stvar_upd = stdrt
      dempx = dedrt
      ! exit
      if(abs(1.0_DP - t) <= EPS) then
        converged = .true.
        exit
      endif
      ! the next dt
      select case(nfail)
      case(1)
        dt = min(beta * dt, dt, 1.0D0 - t)
      case(0)
        dt = min(beta * dt, 1.1d0 * dt, 1.0D0 - t)
      endselect
    else
      ! update fail
      nfail = 1
      ! the next dt
      dt = max(beta * dt, 1.0D-3, 0.1 * dt)
    endif
  enddo
  !
  if(.not. converged) then
    ! 迭代失败
    call stvar_upd%changed_pnewdt(0.5d0)
    write(7, *) "too many attempt made for the increment of stress"
    write(7, *) "total time=", t, "current increment time=", dt, &
      "sstol=", sstol, "rtol=", rtol
    return
  endif
  ! 应力太小
  if(shvar_upd%is_low()) then
    !
    return
  endif
  !
  return
  end procedure Onyield_impl
  !*****************************************************************************
  module procedure Get_residual_impl
  type(Share_var) :: sh_dif
  real(DP), dimension(3) :: norm_dif, norm_tmg, vartol
  !-----------------------------------------------------------------------------
  sh_dif = shfor - shsec
  norm_dif = sh_dif%norm()
  norm_tmg = shtmp%norm()
  where(norm_tmg < EPS)
    norm_tmg = EPS
  endwhere
  !
  vartol = norm_dif / 2.0_DP / norm_tmg
  residual = max(maxval(vartol), EPS)
  end procedure Get_residual_impl
  !*****************************************************************************
  !> @brief : drift_shvars_impl
  !
  !> @param[in] shtmp
  !> @param[in] sttmp
  !> @param[in] depsln
  !> @param[out] shdrt
  !> @param[out] stdrt
  !*****************************************************************************
  module procedure drift_shvars_impl
  type(Share_var) :: sh_flow, sh_radial
  real(DP) :: dftol_pre, dftol_upd, ftol
  integer :: it
  logical :: converged
  !-----------------------------------------------------------------------------
  shdrt = shtmp
  stdrt = sttmp
  dedrt = dempx
  converged = .false.
  ftol = elast_%Yield_distance(shdrt)
  if(abs(ftol) <= tol) then
    return
  endif
  ! iterator
  do it = 1, 8
    dftol_pre = elast_%Yield_distance(shdrt)
    sh_flow = flow_direction_impl(shdrt, stdrt)
    dftol_upd = elast_%Yield_distance(sh_flow)
    if(abs(dftol_upd) >= abs(dftol_pre)) then
      ! 径向返回
      sh_radial = radial_direction_impl(shdrt, stdrt)
      shdrt = sh_radial
    else
      shdrt = sh_flow
    endif
    ! update sucessfully
    dftol_upd = elast_%Yield_distance(shdrt)
    if(abs(dftol_upd) <= tol) then
      converged = .true.
      exit
    endif
    !
  enddo
  if(.not. converged) then
    call stdrt%changed_pnewdt(0.5d0)
  endif
  end procedure drift_shvars_impl
  !*****************************************************************************
  !> @brief Yield distance with strain increment
  !>
  !> @details This function calculates the yield distance after applying a
  !>          scaled strain increment. It computes the stress increment from
  !>          the strain increment using the elasticity tensor, updates the
  !>          stress, and returns the yield distance.
  !>
  !> @param[in] shvars    Shared variables (stress tensor, etc.)
  !> @param[in] stvars    State variables (void ratio, etc.)
  !> @param[in] amplitude Scaling factor for the strain increment
  !> @return ftol         Yield distance after applying scaled strain increment
  !*****************************************************************************
  module procedure ftol_with_depsln
  real(DP), dimension(3, 3, 3, 3) :: stiff
  real(DP), dimension(3, 3) :: dsigma
  type(Share_var) :: sh_temp
  stiff = elast_%Get_stiffness(shvars, stvars)
  dsigma(:, :) = stiff.ddot.depsln
  sh_temp = shvars
  call sh_temp%update_sigma(dsigma)
  ftol = elast_%Yield_distance(sh_temp)
  end procedure ftol_with_depsln
  !*****************************************************************************
  !> @brief Mean stress with strain increment
  !>
  !> @details This function calculates the mean stress after applying a
  !>          scaled strain increment. It computes the stress increment from
  !>          the strain increment using the elasticity tensor, updates the
  !>          stress, and returns the mean stress (trace/3).
  !>
  !> @param[in] shvars    Shared variables (stress tensor, etc.)
  !> @param[in] stvars    State variables (void ratio, etc.)
  !> @param[in] amplitude Scaling factor for the strain increment
  !> @return res          Mean stress after applying scaled strain increment
  !*****************************************************************************
  module procedure mean_with_depsln
  real(DP), dimension(3, 3, 3, 3) :: stiff
  real(DP), dimension(3, 3) :: dsigma
  type(Share_var) :: sh_temp
  !
  stiff = elast_%Get_stiffness(shvars, stvars)
  dsigma = stiff.ddot.depsln
  sh_temp = shvars
  call sh_temp%update_sigma(dsigma(:, :))
  res = torch_%Trace(sh_temp%get_sigma()) / 3.0_DP
  end procedure mean_with_depsln
  !*****************************************************************************
  !> @brief Monotonicity check for a function
  !>
  !> @details This function checks whether a given function is monotonic
  !>          (either non-decreasing or non-increasing) within the interval
  !>          [lbd, rbd]. It samples the function at multiple points and
  !>          verifies monotonic behavior.
  !>
  !> @param[in] shvars Shared variables (stress tensor, etc.)
  !> @param[in] stvars State variables (void ratio, etc.)
  !> @param[in] func   Function to check (conforms to func_with_param)
  !> @param[in] lbd    Left boundary of the interval
  !> @param[in] rbd    Right boundary of the interval
  !> @return           .true. if function is monotonic, .false. otherwise
  !*****************************************************************************
  module procedure is_monotonic
  real(DP) :: x1, x2, f1, f2
  integer :: i, n_samples
  logical :: increasing = .true.
  logical :: decreasing = .true.
  n_samples = 10  ! 采样点数
  do i = 1, n_samples
    x1 = lbd + (rbd - lbd) * (i - 1) / real(n_samples, DP)
    x2 = lbd + (rbd - lbd) * i / real(n_samples, DP)

    f1 = func(shvars, stvars, x1 * depsln)
    f2 = func(shvars, stvars, x2 * depsln)
    if(f2 < f1) increasing = .false.
    if(f2 > f1) decreasing = .false.
  enddo
  is_monotonic = increasing .or. decreasing
  end procedure is_monotonic
  !*****************************************************************************
  !> @brief : flow_direction_impl
  !
  !> @param[in] shtmp
  !> @param[in] sttmp
  !> @param[out] res
  !*****************************************************************************
  module procedure flow_direction_impl
  real(DP) :: dftol
  real(DP) :: pfsig(3, 3), xm(3, 3), Dkp, dnmetr, frnde
  real(DP) :: Rh, Rf(3, 3), dlamda, cbxm(3, 3)
  real(DP), dimension(3, 3, 3, 3) :: stiff
  !-----------------------------------------------------------------------------
  dftol = elast_%Yield_distance(shvars)
  stiff = elast_%Get_stiffness(shvars, stvars)
  pfsig = plast_%Get_pfsig(shvars)
  xm = plast_%Get_pgsig(shvars, stvars)
  Dkp = plast_%Get_Dkp(shvars, stvars)
  cbxm = stiff.ddot.xm
  dnmetr = sum(pfsig * (stiff.ddot.xm))
  frnde = dnmetr - Dkp
  if(abs(frnde) <= EPS) frnde = sign(frnde, EPS)
  dlamda = dftol / frnde
  !
  call plast_%Get_evolution(shvars, stvars, Rh, Rf)
  res = shvars
  call res%update_harden(dlamda * rh)
  call res%update_sigma(-cbxm * dlamda)
  call res%update_fabric(dlamda * Rf)
  return
  end procedure flow_direction_impl
  !*****************************************************************************
  !> @brief : flow_direction_impl
  !
  !> @param[in] shtmp
  !> @param[in] sttmp
  !> @param[out] res
  !*****************************************************************************
  module procedure radial_direction_impl
  real(DP) :: dftol, dnme, dlamda
  real(DP), dimension(3, 3) :: pfsig
  !-----------------------------------------------------------------------------
  dftol = elast_%Yield_distance(shvars)
  pfsig = plast_%Get_pfsig(shvars)
  dnme = sum(pfsig**2)
  dlamda = dftol / dnme
  res = shvars
  call res%update_sigma(-dlamda * pfsig)
  return
  end procedure radial_direction_impl
!*******************************************************************************
endsubmodule math_impl
!DIR$ FREEFORM
!*******************************************************************************
!  Project Name
!  Copyright (C) 2025 wuwenhao 617082766@qq.com
!
!  This file is part of Project Name.
!
!  This program is free software; you can redistribute it and/or modify
!  it under the terms of the MIT License version 3 as
!  published by the Free Software Foundation.
!
!  You should have received a copy of the MIT License
!  along with this program. If not, see <https://mit-license.org/>.
!
!  @file     umat.f90
!  @brief    ABAQUS UMAT subroutine for SANISAND constitutive model
!  @details  This file implements the user material subroutine (UMAT) for the
!            SANISAND constitutive model in ABAQUS. The UMAT calculates stress
!            updates and consistent tangent moduli for soil materials under
!            various loading conditions. It incorporates elastic predictor -
!            plastic corrector algorithms, fabric evolution, and state-dependent
!            hardening.
!
!  @author   wuwenhao
!  @email    617082766@qq.com
!  @version  1.0.0.1
!  @date     2025/11/17
!  @license  MIT Massachusetts Institute of Technology (MIT)
!-------------------------------------------------------------------------------
!  Remark         : A state variable array of size NSTATV to be
!  updated by the UMAT,which includes following variables:
!  statev(1) = void_ratio : current void ratio
!  statev(2) = harden : current hardening parameter
!  statev(3) = Fabric_[1] : fabric evlution F11
!  statev(4) = Fabric_[2] : fabric evlution F22
!  statev(5) = Fabric_[3] : fabric evlution F33
!  statev(6) = Fabric_[4] : fabric evlution F12,F21
!  statev(7) = Fabric_[5] : fabric evlution F13,F31
!  statev(8) = Fabric_[6] : fabric evlution F23,F32
!  statev(9) = confining pressure
!  statev(10)= shear stress
!  statev(11)= ratio stress
!  statev(12)= the total of volumetric strain
!  statev(13)= the increment of shear strain
!  statev(14)= ANISOTROPIC VARLABLE
!  statev(15)= the norm of fabric tensor
!  statev(16)= Dkp
!  statev(17)= dilatancy
!  statev(18)= axial stress
!-------------------------------------------------------------------------------
!  Change History :
!  <Date>     | <Version> | <Author>       | <Description>
!  2025/11/17 | 1.0.0.1   | wuwenhao      | Create file
!*******************************************************************************
!> @brief ABAQUS UMAT subroutine for SANISAND constitutive model
!>
!> @details This subroutine implements the user material subroutine (UMAT) for
!>          the SANISAND constitutive model in ABAQUS. It calculates stress
!>          updates and consistent tangent moduli for soil materials under
!>          various loading conditions. The algorithm incorporates elastic
!>          predictor - plastic corrector methodology, fabric evolution, and
!>          state-dependent hardening. The subroutine handles both elastic and
!>          plastic loading paths, including return mapping to the yield surface.
!>
!> @param[in,out] stress    Cauchy stress tensor (ntens)
!> @param[in,out] statev    State variable array (nstatv)
!> @param[out]    ddsdde    Jacobian matrix (ntens x ntens)
!> @param[out]    sse       Specific elastic strain energy
!> @param[out]    spd       Specific plastic dissipation
!> @param[out]    scd       Specific creep dissipation
!> @param[out]    rpl       Volumetric heat generation per unit time
!> @param[out]    ddsddt    Stress variation with temperature (ntens)
!> @param[out]    drplde    Variation of rpl with strain increments (ntens)
!> @param[out]    drpldt    Variation of rpl with temperature
!> @param[in]     stran     Total strains at beginning of increment (ntens)
!> @param[in]     dstran    Strain increments (ntens)
!> @param[in]     time      Time array: time(1) = step time, time(2) = total time
!> @param[in]     dtime     Time increment
!> @param[in]     temp      Temperature at beginning of increment
!> @param[in]     dtemp     Temperature increment
!> @param[in]     predef    Predefined field variables array
!> @param[in]     dpred     Increments of predefined field variables
!> @param[in]     cmname    User-defined material name (CHARACTER*80)
!> @param[in]     ndi       Number of direct stress components
!> @param[in]     nshr      Number of engineering shear stress components
!> @param[in]     ntens     Size of stress/strain array (ndi + nshr)
!> @param[in]     nstatv    Number of state variables
!> @param[in]     props     Material properties array (nprops)
!> @param[in]     nprops    Number of material properties
!> @param[in]     coords    Spatial coordinates of integration point
!> @param[in]     drot      Rotation increment matrix (3x3)
!> @param[in,out] pnewdt    Ratio of suggested new time increment
!> @param[in]     celent    Characteristic element length
!> @param[in]     dfgrd0    Deformation gradient at beginning of increment (3x3)
!> @param[in]     dfgrd1    Deformation gradient at end of increment (3x3)
!> @param[in]     noel      Element number
!> @param[in]     npt       Integration point number
!> @param[in]     layer     Layer number (for composite shells and layered solids)
!> @param[in]     kspt      Section point number within the current layer
!> @param[in]     kstep     Step number
!> @param[in]     kinc      Increment number
!>
!> @author wuwenhao
!> @date 2025/11/17
!*******************************************************************************
subroutine Umat(stress, statev, ddsdde, sse, spd, scd, &
                rpl, ddsddt, drplde, drpldt, &
                stran, dstran, time, dtime, temp, dtemp, predef, dpred, &
                cmname, ndi, nshr, ntens, nstatv, props, nprops, &
                coords, drot, pnewdt, celent, dfgrd0, dfgrd1, &
                noel, npt, layer, kspt, kstep, kinc)
  use Base_config
  use presolve_mod
  use tensor_opt_mod
  use elastic_mod
  use Container_mod
  use plastic_mod
  use math_mod
  ! Variable declarations
  implicit none
  !
  character * 80 cmname
  ! Input/Output variables
  integer :: ndi, nshr, ntens, nstatv, nprops, noel, npt, &
             layer, kspt, kstep, kinc
  real(dp) :: stress(ntens), statev(nstatv), &
              ddsdde(ntens, ntens), ddsddt(ntens), drplde(ntens), &
              stran(ntens), dstran(ntens), time(2), props(nprops), &
              coords(3), drot(3, 3), dfgrd0(3, 3), dfgrd1(3, 3), &
              sse, spd, scd, rpl, drpldt, dtime, temp, dtemp, &
              predef, dpred, celent, pnewdt
  !-----------------------------------------------------------------------------
  type(Torch) :: torch_
  type(Elast) :: elast_
  type(Plast) :: plast_
  type(Math) :: math_
  real(DP), dimension(3, 3) :: deplsn, deplsn_ela, res_depsln
  real(DP), dimension(3, 3) :: dsigma, sigma_final
  type(Share_var) :: Shvars_ini, Shvars_ela, Shvars_final
  type(State_var) :: state_ini, stvar_ela, stvar_final
  real(DP) :: voidr_ini, harden_ini, sigma_ini(3, 3), fabric_ini(3, 3)
  real(DP) :: mean_etr, ftol_etr, alout, ftolr
  real(DP), dimension(3, 3, 3, 3) :: dsigde, dsdeyl, dsdetl
  !
  real(DP) :: mean_final, shear, ratio_stress, dEv, dEq, ANIV, Fmag
  real(DP) :: Dkp, dpla
  INTEGER, SAVE :: NUMBER = 0 ! static variable
  !-----------------------------------------------------------------------------
  ! initialize variable
  ftolr = 1.0D-6
  !
  if(noel == 1 .and. npt == 1) then
    write(6, *) '=============================================================='
    write(6, *) 'noel = ', noel, ' npt = ', npt, ' number = ', NUMBER
    write(6, *) '=============================================================='
    ! call abaqus_debug(1, 1, NUMBER, noel, npt, 4, "Umat")
    NUMBER = NUMBER + 1
  endif
  sigma_ini = -convert_array_to_tensor(stress, 1.0_DP)
  harden_ini = statev(2)
  fabric_ini = convert_array_to_tensor(statev(3:8))
  fabric_ini = matmul(drot, matmul(fabric_ini, transpose(drot)))
  ! create Share_var container
  Shvars_ini = Share_var(harden_ini, sigma_ini, fabric_ini)
  !
  deplsn = -convert_array_to_tensor(dstran, 2.0_DP)
  voidr_ini = statev(1)
  state_ini = State_var(voidr_ini, pnewdt)
  !
  if(Shvars_ini%is_low()) then
    ! too low

  else
    mean_etr = math_%mean_with_depsln(Shvars_ini, state_ini, deplsn)
    ftol_etr = math_%ftol_with_depsln(Shvars_ini, state_ini, deplsn)
    if(ftol_etr <= ftolr .and. mean_etr >= tensno) then
      ! elastic updated
      dsigma(:, :) = elast_%calc_dsigma(Shvars_ini, state_ini, deplsn)
      Shvars_final = Share_var(harden_ini, sigma_ini + dsigma, fabric_ini)
      !
      stvar_final = state_ini
      call stvar_final%update_voidr(deplsn)
      !
      dsdetl = elast_%Get_stiffness(Shvars_ini, state_ini)
      !
    elseif(ftol_etr > ftolr .or. (mean_etr < tensno .and. ftol_etr < ftolr)) then
      !     !
      ! call abaqus_debug(1, 1, NUMBER, noel, npt, 0, "Umat")
      alout = math_%Intchc(Shvars_ini, state_ini, deplsn, ftolr)
      ! update variables
      deplsn_ela(:, :) = alout * deplsn(:, :)
      res_depsln = (1.0_DP - alout) * deplsn
      stvar_ela = state_ini
      call stvar_ela%update_voidr(deplsn_ela)
      dsigma(:, :) = elast_%calc_dsigma(Shvars_ini, state_ini, deplsn_ela)
      Shvars_ela = Shvars_ini
      call Shvars_ela%update_sigma(dsigma)
      dsigde(:, :, :, :) = elast_%Get_stiffness(Shvars_ini, state_ini)
      !
      call math_%Onyield(Shvars_ela, stvar_ela, res_depsln, ftolr, Shvars_final, stvar_final, dsdeyl)
      dsdetl = alout * dsigde + (1.0_DP - alout) * dsdeyl
    endif
  endif
  !-----------------------------------------------------------------------------
  !> update variable
  ! update ddsdde
  ddsdde = Convert_tensor4_to_tensor2(dsdetl, ntens)
  ! update state variables
  stress(:) = -Convert_tensor_to_array(Shvars_final%get_sigma(), ntens)
  statev(1) = stvar_final%get_voidr()
  statev(2) = Shvars_final%get_harden()
  statev(3:8) = Convert_tensor_to_array(Shvars_final%get_fabric(), 6)
  !
  pnewdt = stvar_final%get_pnewdt()
  ! confining pressure
  mean_final = torch_%Trace(Shvars_final%get_sigma()) / 3.0_DP
  statev(9) = mean_final
  ! shear_stress
  shear = torch_%Shear(Shvars_final%get_sigma())
  statev(10) = shear
  ! ratio stress
  ratio_stress = torch_%Get_Rm(Shvars_final%get_sigma())
  statev(11) = ratio_stress
  ! the total of volumetric strain
  dEv = torch_%Trace(deplsn)
  statev(12) = statev(12) + dEv * 100
  ! the increment of shear strain
  dEq = torch_%Shear(deplsn)
  statev(13) = statev(13) + dEq * 100
  ! ANISOTROPIC VARLABLE
  ANIV = elast_%Get_anisotropy(Shvars_final)
  statev(14) = ANIV
  ! the norm of fabric tensor
  Fmag = torch_%Norm(Shvars_final%get_fabric())
  statev(15) = Fmag
  ! Dkp
  Dkp = plast_%Get_Dkp(Shvars_final, stvar_final)
  statev(16) = Dkp
  ! dilatancy
  dpla = plast_%Get_dilatancy(Shvars_final, stvar_final)
  statev(17) = dpla
  ! axial stress
  sigma_final = Shvars_final%get_sigma()
  statev(18) = sigma_final(1, 1)
  ! print
  if(noel == 1 .and. npt == 1) then
    call Shvars_final%print()
11  format(A14, F6.2, 2x, A21, F6.2, /, &
           A15, F6.2, /, &
           A14, F6.2, 2x, A14, F6.2, /, &
           A20, F6.2, 2x, A16, F6.2, /, &
           A6, E15.6, 2x, A13, F10.3)
    write(6, 11) "mean stress = ", mean_final, ' deviatoric stress = ', shear, &
      "ratio stress = ", ratio_stress, &
      "fabric norm = ", Fmag, " anisotropy = ", ANIV, &
      "volumetric strain = ", statev(12), " shear strain = ", statev(13), &
      "Dkp = ", Dkp, " dilatancy = ", dpla
  endif
  !
endsubroutine
!DIR$ FREEFORM
!*****************************************************************************
!  Project Name
!  Copyright (C) 2025 wuwenhao 617082766@qq.com
!
!  This file is part of Project Name.
!
!  This program is free software; you can redistribute it and/or modify
!  it under the terms of the MIT License version 3 as
!  published by the Free Software Foundation.
!
!  You should have received a copy of the MIT License
!  along with this program. If not, see <https://mit-license.org/>.
!
!  @file     sdvini.F90
!  @brief    ABAQUS SDVINI subroutine for state variable initialization
!  @details  This file implements the SDVINI subroutine for initializing state
!            variables in ABAQUS UMAT implementations. It sets initial values
!            for void ratio, hardening parameters, and fabric tensors based on
!            material configuration and spatial coordinates.
!
!  @author   wuwenhao
!  @email    617082766@qq.com
!  @version  1.0.0.1
!  @date     2025/12/08
!  @license  MIT Massachusetts Institute of Technology (MIT)
!---------------------------------------------------------------------------*
!  Remark         : A state variable array of size NSTATV to be
!  updated by the UMAT,which includes following variables:
!  void_ratio = statev(1) : current void ratio
!  harden     = statev(2) : current hardening parameter
!  Fabric_[1] = statev(3) : fabric evlution F11
!  Fabric_[2] = statev(4) : fabric evlution F22
!  Fabric_[3] = statev(5) : fabric evlution F33
!  Fabric_[4] = statev(6) : fabric evlution F12,F21
!  Fabric_[5] = statev(7) : fabric evlution F13,F31
!  Fabric_[6] = statev(8) : fabric evlution F23,F32
!---------------------------------------------------------------------------*
!  Change History :
!  <Date>     | <Version> | <Author>       | <Description>
!  2025/12/08 | 1.0.0.1   | wuwenhao      | Create file
!*****************************************************************************
!> @brief ABAQUS SDVINI subroutine for state variable initialization
!>
!> @details This subroutine initializes state variables for the UMAT
!>          implementation. It sets initial values for void ratio, hardening
!>          parameters, and fabric tensors based on material configuration.
!>          The subroutine also performs validation checks to ensure the
!>          state variable array has sufficient size for all required
!>          variables. Fabric tensors are initialized with anisotropic
!>          orientation if specified.
!>
!> @param[in,out] statev  State variable array (nstatv)
!> @param[in]     coords  Spatial coordinates of integration point (ncrds)
!> @param[in]     nstatv  Number of state variables
!> @param[in]     ncrds   Number of coordinate directions
!> @param[in]     noel    Element number
!> @param[in]     npt     Integration point number
!> @param[in]     layer   Layer number (for composite shells and layered solids)
!> @param[in]     kspt    Section point number within the current layer
!>
!> @author wuwenhao
!> @date 2025/12/08
!*****************************************************************************
SUBROUTINE Sdvini(statev, coords, nstatv, ncrds, noel, npt, layer, kspt)
  use Base_config
  use presolve_mod
  use Material_config
  use exception_mod
#include "macro.h"
  IMPLICIT NONE
  integer, intent(in) :: nstatv, ncrds, noel, npt, layer, kspt
  real(DP), intent(in) :: coords(ncrds)
  real(DP), intent(inout) :: statev(nstatv)
  real(DP) :: void_ini, harden_ini, fabric_ini(3, 3), fabric_rot(3, 3)
  real(DP) :: rot_matrix(3, 3), angle, axis(3), temp
  !
  ! CALL abaqus_debug(1, 1, 0, noel, npt, 0, "Sdvini")
  ! check nstatv
  CHECK_TRUE(nstatv >= 8, "Sdvini: nstatv is less than 8.")
  ! Initialize state variables
  void_ini = 0.76_DP  ! Initial void ratio
  harden_ini = 0.01_DP  ! Initial hardening parameter
  !
  angle = 0.0_DP * PI / 180_DP
  axis = [1.0_DP, 0.0_DP, 0.0_DP]
  rot_matrix(:, :) = Get_rotation_matrix(angle, axis)
  fabric_ini(:, :) = 0.0_DP
  temp = dsqrt(2.0_DP / 3.0_DP) * PARAM%F0
  fabric_ini(:, :) = reshape([temp, 0.0_DP, 0.0_DP, &
                              0.0_DP, -temp / 2.0_DP, 0.0_DP, &
                              0.0_DP, 0.0_DP, -temp / 2.0_DP],[3, 3])
  fabric_rot(:, :) = matmul(rot_matrix, matmul(fabric_ini, transpose(rot_matrix)))
  ! void ratio initial
  statev(1) = void_ini
  ! harden initial
  statev(2) = harden_ini
  ! fabric initial
  statev(3:8) = Convert_tensor_to_array(fabric_rot, 6)
  ! initialize other state variables to zero
  if(nstatv > 8) statev(9:nstatv) = 0.0_DP
  RETURN
ENDSUBROUTINE Sdvini
!*****************************************************************************
!> @brief ABAQUS SIGINI subroutine for initial stress field definition
!>
!> @details This subroutine defines the initial stress field for ABAQUS
!>          analyses. It sets up initial stress conditions based on spatial
!>          coordinates and element/integration point information. The current
!>          implementation provides a simple isotropic compression stress state
!>          with equal stresses in all principal directions.
!>
!> @param[out] sigma    Initial stress tensor (ntens)
!> @param[in]  coords   Spatial coordinates of integration point (ncrds)
!> @param[in]  ntens    Size of stress/strain array
!> @param[in]  ncrds    Number of coordinate directions
!> @param[in]  noel     Element number
!> @param[in]  npt      Integration point number
!> @param[in]  layer    Layer number (for composite shells and layered solids)
!> @param[in]  kspt     Section point number within the current layer
!> @param[in]  lrebar   Rebar layer indicator
!> @param[in]  names    Array of surface names (CHARACTER*80)
!>
!> @author wuwenhao
!> @date 2025/12/10
!*****************************************************************************
subroutine Sigini(sigma, coords, ntens, ncrds, noel, npt, layer, &
                  kspt, lrebar, names)
  use Base_config, only: DP
  implicit none
  integer, intent(in) :: ntens, ncrds, noel, npt, layer, kspt, lrebar
  real(DP), intent(out) :: sigma(ntens)
  real(DP), intent(in) :: coords(ncrds)
  character(len=80), intent(in) :: names(2)
  real(DP) :: sigma_ini
  !
  sigma_ini = 100.0_DP
  sigma(1) = -sigma_ini
  sigma(2) = -sigma_ini
  sigma(3) = -sigma_ini
  sigma(4:ntens) = 0.0_DP
  return
endsubroutine Sigini
