
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
    real(DP) :: KH
    !
    real(DP) :: VOIDC, KSI, LAC, MCS, EA
    real(DP) :: D1, DM
    real(DP) :: CH, NKP
    real(DP) :: FEVR, F0
  endtype param_
  ! initial type
  type(param_), parameter :: PARAM = param_(G0=125.0_DP, NU=0.2_DP, C=0.75_DP, &
                                            KH=0.03_DP, KSI=0.7_DP, LAC=0.05_DP, &
                                            MCS=1.25_DP, EA=0.1_DP, VOIDC=0.94_DP, &
                                            D1=0.1_DP, DM=3.5_DP, &
                                            CH=0.6_DP, NKP=1.1_DP, FEVR=5.7_DP, F0=0.45_DP)
contains

endmodule Material_config
!*****************************************************************************
!> @brief share_vars
!>
!> @details 模块详细描述
!>
!> @author wuwenhao
!> @date 2025/12/07
!*****************************************************************************
module share_vars
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
  contains
    procedure, public, pass(this) :: get_harden => get_harden_impl
    procedure, public, pass(this) :: get_sigma => getsigma_impl
    procedure, public, pass(this) :: update_sigma => update_sigma_impl
    procedure, public, pass(this) :: get_fabric => getfabric_impl
    procedure, public, pass(this) :: update_shvars => update_shvars_impl
    procedure, pass(this) :: assign => assign_impl
    generic, public :: assignment(=) => assign
  endtype Share_var
  interface Share_var
    module procedure :: construct_with_param
  endinterface
contains
  function construct_with_param(harden, sigma, fabric) result(this)
    real(DP) :: harden
    real(DP), dimension(3, 3) :: sigma
    real(DP), dimension(3, 3) :: fabric
    type(Share_var) :: this
    !
    this%harden_ = harden
    this%sigma_ = sigma(:, :)
    this%fabric_ = fabric(:, :)
  endfunction construct_with_param
  !
  function get_harden_impl(this) result(harden)
    class(Share_var), intent(in) :: this
    real(DP) :: harden
    harden = this%harden_
  endfunction
  !
  function getsigma_impl(this) result(sigma)
    class(Share_var), intent(in) :: this
    real(DP), dimension(3, 3) :: sigma
    sigma(:, :) = this%sigma_(:, :)
  endfunction
  !
  subroutine update_sigma_impl(this, dsigma)
    class(Share_var), intent(inout) :: this
    real(DP),dimension(3,3),intent(in) :: dsigma
    this%sigma_(:, :) = this%sigma_(:, :) + dsigma(:, :)
  endsubroutine update_sigma_impl

  function getfabric_impl(this) result(fabric)
    class(Share_var), intent(in) :: this
    real(DP), dimension(3, 3) :: fabric
    fabric(:, :) = this%fabric_(:, :)
  endfunction
  !
  subroutine update_shvars_impl(dharden, dsigma, dfabric, this)
    real(DP), intent(in) :: dharden
    real(DP), dimension(3, 3), intent(in) :: dsigma
    real(DP), dimension(3, 3), intent(in) :: dfabric
    class(Share_var), intent(inout) :: this
    !
    this%harden_ = this%harden_ + dharden
    this%sigma_(:, :) = this%sigma_(:, :) + dsigma(:, :)
    this%fabric_(:, :) = this%fabric_(:, :) + dfabric(:, :)
  endsubroutine update_shvars_impl
  subroutine assign_impl(this, other)
    class(Share_var), intent(inout) :: this
    type(Share_var), intent(in) :: other
    !
    this%harden_ = other%harden_
    this%sigma_(:, :) = other%sigma_(:, :)
    this%fabric_(:, :) = other%fabric_(:, :)
  endsubroutine assign_impl
endmodule share_vars
!*****************************************************************************
!> @brief tensor_opt_mod
!>
!> @details Module for tensor operations in continuum mechanics
!>
!> @author wuwenhao
!> @date 2025/11/27
!*****************************************************************************
module tensor_opt_mod
  ! provide DP is double precision
  use Base_config, only: DP
  implicit none
  public :: Torch, operator(.ddot.)
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
  type :: Torch
  contains
    private
    ! static function method
    procedure, public, nopass :: Print => Print_Impl
    procedure, public, nopass :: Trace => Trace_Impl
    procedure, public, nopass :: Get_J2 => Sec_dev_invar_impl
    procedure, public, nopass :: Get_J3 => Trd_dev_invar_impl
    procedure, public, nopass :: Deviatoric => Deviatoric_impl
    procedure, public, nopass :: Get_ratio => Ratio_impl
    procedure, public, nopass :: Sin3theta => Sin3theta_impl
    procedure, public, nopass :: Shear => Shear_impl
    procedure, public, nopass :: Normalize => normalize_impl
    procedure, public, nopass :: Norm => Norm_impl
    procedure, public, nopass :: eye => eye_impl
  endtype Torch
  !
  interface operator(.ddot.)
    module procedure Tensor4_ddot_tensor2
    module procedure Tensor2_ddot_tensor4
  endinterface
  !======================================================================
  ! Abstract interface definition (implemented in the sub-module)
  !======================================================================
  interface
    !***************************************************************************
    !> @brief Print_Impl
    !>
    !> @details Print a stress tensor
    !>
    !> @param[in]  stress  Stress tensor to print
    !
    !***************************************************************************
    module Subroutine Print_Impl(stress)
      real(DP), intent(in), dimension(3, 3) :: stress
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
    module function Trace_Impl(stress) result(val)
      real(DP), intent(in), dimension(3, 3) :: stress
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
    module function Sec_dev_invar_impl(stress) result(val)
      real(DP), intent(in), dimension(3, 3) :: stress
      real(DP) :: val
    endfunction Sec_dev_invar_impl
    !***************************************************************************
    !> @brief Trd_dev_invar_impl
    !>
    !> @details Calculate the third deviatoric invariant (J3) of a stress tensor
    !>
    !> @param[in]  stress  Stress tensor
    !>
    !> @return Third deviatoric invariant (J3)
    !***************************************************************************
    module function Trd_dev_invar_impl(stress) result(val)
      real(DP), intent(in), dimension(3, 3) :: stress
      real(DP) :: val
    endfunction Trd_dev_invar_impl
    !***************************************************************************
    !> @brief Deviatoric_impl
    !>
    !> @details Calculate the deviatoric part of a stress tensor
    !>
    !> @param[in]  stress  Stress tensor
    !>
    !> @return Deviatoric stress tensor
    !***************************************************************************
    module function Deviatoric_impl(stress) result(tensor)
      real(DP), intent(in), dimension(3, 3) :: stress
      real(DP), dimension(3, 3) :: tensor
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
    module function Ratio_impl(stress) result(tensor)
      real(DP), intent(in), dimension(3, 3) :: stress
      real(DP), dimension(3, 3) :: tensor
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
    module function Sin3theta_impl(stress) result(sin3t)
      real(DP), intent(in), dimension(3, 3) :: stress
      real(DP) :: sin3t
    endfunction Sin3theta_impl
    !***************************************************************************
    !> @brief Shear_impl
    !>
    !> @details Calculate the shear stress (sqrt(J2))
    !>
    !> @param[in]  stress  Stress tensor
    !>
    !> @return Shear stress value
    !***************************************************************************
    module function Shear_impl(stress) result(shear)
      real(DP), dimension(3, 3), intent(in) :: stress
      real(DP) :: shear
    endfunction Shear_impl
    !***************************************************************************
    !> @brief Normalize_impl
    !>
    !> @details Calculate the normalize of the tensor
    !>
    !> @param[in]  a 3x3 size of tensor
    !>
    !> @return Normalized tensor
    !***************************************************************************
    module function Normalize_impl(tensor) result(res)
      real(DP), dimension(3, 3), intent(in) :: tensor
      real(DP), dimension(3, 3) :: res
    endfunction Normalize_impl
    !***************************************************************************
    !> @brief Normalize_impl
    !>
    !> @details Calculate the normalize of the tensor
    !>
    !> @param[in]  a 3x3 size of tensor
    !>
    !> @return the norm of tensor
    !***************************************************************************
    module function Norm_impl(tensor) result(res)
      real(DP), dimension(3, 3), intent(in) :: tensor
      real(DP) :: res
    endfunction Norm_impl
    !***************************************************************************
    !> @brief eye_impl
    !>
    !> @details Calculate the normalize of the tensor
    !>
    !> @param[in]  size: size of tensor
    !>
    !> @return the unit matrix of given size
    !***************************************************************************
    module function eye_impl(size) result(delta)
      integer, intent(in) :: size
      real(DP), dimension(size, size) :: delta
    endfunction eye_impl
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
  endinterface
contains

endmodule tensor_opt_mod
!DIR$ FREEFORM
!*****************************************************************************
!> @brief presolve_mod
!>
!> @details 模块详细描述
!>
!> @author wuwenhao
!> @date 2025/11/17
!*****************************************************************************
module presolve_mod
  use Base_config, only: DP
  implicit none
  public :: Convert_array_to_tensor, Convert_tensor_to_array, Print_array, Get_rotation_matrix
  public :: abaqus_debug
  private
  ! Public interface
  interface
    !**************************************************************************
    !> @brief convert_stress_to_tensor
    !>
    !> @details: this function is used to convert stress into a 3×3 two-dimensional tensor
    !>
    !> @param[in]  array: 输入的向量
    !> @param[out] tensor: 应力张量
    !>
    !> @return 返回值说明
    !**************************************************************************
    module function Convert_array_to_tensor(array, scalar) result(tensor)
      real(DP), intent(in) :: array(:)
      real(DP), intent(in), optional :: scalar
      real(DP), dimension(3, 3) :: tensor
    endfunction Convert_array_to_tensor
    !**************************************************************************
    !> @brief convert_stress_to_tensor
    !>
    !> @details: this function is used to convert a 3×3 two-dimensional tensor into array
    !>
    !> @param[in]  tensor: 输入的向量
    !> @param[in]  size: 向量的大小
    !> @param[out] array: 输出数组
    !>
    !> @return 返回值说明
    !**************************************************************************
    module function Convert_tensor_to_array(tensor, size, scalar) result(array)
      real(DP), dimension(3, 3), intent(in) :: tensor
      integer, intent(in) :: size
      integer, intent(in), optional :: scalar
      real(DP), dimension(size) :: array
    endfunction Convert_tensor_to_array
    module function Get_rotation_matrix(angle, axis) result(rot_matrix)
      real(DP), intent(in) :: angle
      real(DP), intent(in), dimension(3) :: axis
      real(DP), dimension(3, 3) :: rot_matrix
    endfunction Get_rotation_matrix
    ! print the array
    module Subroutine Print_array(array)
      real(DP), intent(in), dimension(:) :: array
    endSubroutine
    ! abaqus_debug
    module subroutine abaqus_debug(noel_num, npt_num, num, noel, npt, iteration, names)
      integer, intent(in) :: noel_num, npt_num, num
      integer, intent(in) :: noel, npt,iteration
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
    module function create_error_context(file, line, prefix) result(this)
      implicit none
      character(len=*), intent(in) :: file
      integer, intent(in) :: line
      character(len=*), intent(in), optional :: prefix
      type(ErrorContext) :: this
    endfunction create_error_context
    ! void throw_error(std::string msg);
    module subroutine throw_error_impl(msg, this)
      implicit none
      character(len=*), intent(in) :: msg
      class(ErrorContext), intent(in) :: this
    endsubroutine throw_error_impl
    !
    module subroutine ASSERT_TRUE(expr, msg, file, line)
      logical, intent(in) :: expr
      character(len=*), intent(in) :: msg
      character(len=*), intent(in) :: file
      character(len=:), allocatable :: validator_msg
      integer, intent(in) :: line
    endsubroutine ASSERT_TRUE
  endinterface ! end interface
contains



  !
  ! void ASSERT_EQUAL(int lhs, int rhs, std::string msg, std::string file, int line)
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
  !> @brief ASSERT_FLOAT_EQUAL
  !>
  !> @details 子程序详细描述
  !>
  !> @param[in]  参数名 输入参数说明
  !> @param[out] 参数名 输出参数说明
  !> @param[in,out] 参数名 输入输出参数说明
  !>
  !> @author wuwenhao
  !> @date 2025/12/01
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
!> @brief elastic_mod
!>
!> @details 模块详细描述
!>
!> @author wuwenhao
!> @date 2025/12/02
!*****************************************************************************
module elastic_mod
  use Base_config, only: DP
  use share_vars
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
    procedure, public, nopass :: Get_pFpr => Get_pFpr_impl
    procedure, public, nopass :: Get_anisotropy => Get_anisotropy_impl
    procedure, public, nopass :: Yield_distance => yield_distance_impl
    procedure, public, nopass :: Get_stiffness => Get_stiffness_impl
    procedure, public, nopass :: Update_voidr => Update_voidr_impl
  endtype Elast
  !======================================================================
  ! Abstract interface definition (implemented in the sub-module)
  !======================================================================
  interface
    module function Yield_distance_impl(shvars) result(ftol)
      type(Share_var) :: shvars
      real(DP) :: ftol
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
    module subroutine Get_gtheta_impl(shvars, gtheta, atheta, sdgth)
      type(Share_var) :: shvars
      real(DP) :: gtheta, atheta, sdgth
    endsubroutine Get_gtheta_impl
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
    module function Get_anisotropy_impl(shvars) result(abase)
      type(Share_var) :: shvars
      real(DP) :: abase
    endfunction Get_anisotropy_impl
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
    module function Get_pFpr_impl(shvars) result(pfratio)
      type(Share_var) :: shvars
      real(DP), dimension(3, 3) :: pfratio
    endfunction Get_pFpr_impl
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
    module function Get_stiffness_impl(shvars, voidr) result(stiffness)
      type(Share_var) :: shvars
      real(DP), intent(in) :: voidr
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
!*******************************************************************************
!> @brief plastic_mod
!>
!> @details 模块详细描述
!>
!> @author wuwenhao
!> @date 2025/12/05
!*******************************************************************************
module plastic_mod
  use Base_config, only: DP
  use share_vars, only: Share_var
  implicit none
  private
  type, public :: plast
  contains
    procedure, public, nopass :: Get_pfsig => Get_pfsig_impl
    procedure, public, nopass :: Get_pgsig => Get_pgsig_impl
    procedure, public, nopass :: Get_psim => Get_psim_impl
    procedure, public, nopass :: Get_dilatancy => Get_dilatancy_impl
    procedure, public, nopass :: Get_evolution => Get_evolution_impl
    procedure, public, nopass :: Get_Dkp => Get_Dkp_impl
    ! procedure, public, nopass :: Get_lamda => Get_lamda_impl
  endtype plast
  !
  interface
    !***************************************************************************
    !> @brief get_pfsig_impl
    !>
    !> @details 函数详细描述
    !>
    !> @param[in]  stress :current stress tensor
    !> @param[out] pfsig :Return a yield function corresponding to the partial
    !> derivative of stress
    !>
    !> @return 返回值说明
    !***************************************************************************
    module function Get_pfsig_impl(shvars) result(pFsig)
      type(Share_var) :: shvars
      real(DP), dimension(3, 3) :: pFsig
    endfunction Get_pfsig_impl
    !***************************************************************************
    !> @brief Get_pgsig_impl
    !>
    !> @details 函数详细描述
    !>
    !> @param[in]  stress :current stress tensor
    !> @param[out] pfsig :Return a yield function corresponding to the partial
    !> derivative of stress
    !>
    !> @return 返回值说明
    !***************************************************************************
    module function Get_pgsig_impl(shvars, voidr) result(xm)
      implicit none
      type(Share_var) :: shvars
      real(DP), intent(in) :: voidr
      real(DP), dimension(3, 3) :: xm
    endfunction Get_pgsig_impl
    !***************************************************************************
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
    !***************************************************************************
    module function Get_psim_impl(shvars, voidr) result(psim)
      ! input
      type(Share_var) :: shvars
      real(DP), intent(in) :: voidr
      ! output
      real(DP) :: psim
    endfunction Get_psim_impl
    !***************************************************************************
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
    !***************************************************************************
    module function Get_dilatancy_impl(shvars, voidr) result(dpla)
      type(Share_var) :: shvars
      real(DP), intent(in) :: voidr
      real(DP) :: dpla
    endfunction Get_dilatancy_impl
    !***************************************************************************
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
    !***************************************************************************
    module subroutine Get_evolution_impl(shvars, voidr, Rh, RF)
      type(Share_var) :: shvars
      real(DP), intent(in) :: voidr
      real(DP) :: Rh, RF(3, 3)
    endsubroutine Get_evolution_impl
  endinterface ! end interface
contains
  function Get_Dkp_impl(shvars, voidr) result(Dkp)
    type(Share_var) :: shvars
    real(DP), intent(in) :: voidr
    real(DP) :: Dkp, RH, RF(3, 3)
    type(plast) plast_
    !
    call plast_%Get_evolution(shvars, voidr, RH, RF)
    Dkp = RH
  endfunction Get_Dkp_impl
endmodule plastic_mod
!*****************************************************************************
!> @brief tensor_opt_impl
!>
!> @details 模块详细描述
!>
!> @author wuwenhao
!> @date 2025/11/27
!*****************************************************************************
submodule(exception_mod) exception_impl
  implicit none
contains
  !*****************************************************************************
  !> @brief create_error_context
  !>
  !> @details 模块详细描述
  !>
  !> @author wuwenhao
  !> @date 2025/12/01
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
  !> @brief create_error_context
  !>
  !> @details 模块详细描述
  !>
  !> @author wuwenhao
  !> @date 2025/12/01
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
  write(0, '(A)') trim(full_msg)
  call exit(1)
  end procedure throw_error_impl
  !*****************************************************************************
  !> @brief ASSERT_TRUE
  !>
  !> @details 子程序详细描述
  !>
  !> @param[in]  参数名 输入参数说明
  !> @param[out] 参数名 输出参数说明
  !> @param[in,out] 参数名 输入输出参数说明
  !>
  !> @author wuwenhao
  !> @date 2025/11/27
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
!> @brief presolve_impl
!>
!> @details 模块详细描述
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
  !> @brief convert_array_to_tensor
  !>
  !> @details: this function is used to convert array into a 3×3 two-dimensional tensor
  !>
  !> @param[in]  array: 输入的应力向量
  !> @param[in]  size: 应力向量的大小
  !> @param[out] tensor: 应力张量
  !>
  !> @return 返回值说明
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
  !> @brief convert_stress_to_tensor
  !>
  !> @details: this function is used to convert a 3×3 two-dimensional tensor into array
  !>
  !> @param[in]  tensor: 输入的向量
  !> @param[in]  size: 向量的大小
  !> @param[out] array: 输出数组
  !>
  !> @return 返回值说明
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
  !**************************************************************************
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
  ! print array
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
  ! abaqus_debug
  module procedure abaqus_debug
  logical :: firstrun = .true.
  integer :: tempvar
  write(*, *) "debug in ", trim(names)
  if((noel_num == noel) .and. (npt_num == npt) .and. num >= iteration) then
    if(firstrun) then
      write(*, *) "please input an integer"
      read(*, *) tempvar
    endif
  endif
  end procedure abaqus_debug
endsubmodule
!*****************************************************************************
!> @brief tensor_torch_mod
!>
!> @details Module for tensor operations in continuum mechanics
!>
!> @author wuwenhao
!> @date 2025/11/27
!*****************************************************************************
submodule(tensor_opt_mod) tensor_torch_impl
  use Base_config, only: DP, TWO, THREE, EPS, DELTA
  implicit none
contains
  !***************************************************************************
  !> @brief Print_Impl
  !>
  !> @details Print a stress tensor
  !>
  !> @param[in]  stress  Stress tensor to print
  !
  !***************************************************************************
  module procedure Print_impl
  implicit none
  integer :: i, j
  ! declaration
  write(*, '(A)') "tensor:"
  do i = 1, 3
    write(*, '(A, I1, A)', advance='no') "Row ", i, ": ["
    do j = 1, 3
      write(*, '(F10.6)', advance='no') stress(i, j)
      if(j < 3) write(*, '(A)', advance='no') ", "
    enddo
    write(*, '(A)') "]"
  enddo
  write(*, *)
  end procedure Print_impl
  !***************************************************************************
  !> @brief Trace_Impl
  !>
  !> @details Calculate the trace of a stress tensor
  !>
  !> @param[in]  stress  Stress tensor
  !>
  !> @return Trace of the stress tensor
  !***************************************************************************
  module procedure Trace_impl
  implicit none
  integer :: i
  val = sum([(stress(i, i), i=1, 3)])
  end procedure Trace_impl
  !***************************************************************************
  !> @brief Sec_dev_invar_impl
  !>
  !> @details Calculate the second deviatoric invariant (J2) of a stress tensor
  !>
  !> @param[in]  stress  Stress tensor
  !>
  !> @return Second deviatoric invariant (J2)
  !***************************************************************************
  module procedure Sec_dev_invar_impl
  implicit none
  type(Torch) :: torch_
  real(DP), dimension(3, 3) :: S
  S = torch_%Deviatoric(stress)
  val = sum(S**2) / two
  end procedure Sec_dev_invar_impl
  !***************************************************************************
  !> @brief Trd_dev_invar_impl
  !>
  !> @details Calculate the third deviatoric invariant (J3) of a stress tensor
  !>
  !> @param[in]  stress  Stress tensor
  !>
  !> @return Third deviatoric invariant (J3)
  !***************************************************************************
  module procedure Trd_dev_invar_impl
  implicit none
  type(Torch) :: torch_
  real(DP), dimension(3, 3) :: S, temp
  ! implementation
  ! deviatoric stress
  S = torch_%Deviatoric(stress)
  temp = matmul(S, matmul(S, S))
  val = torch_%Trace(temp) / three
  end procedure Trd_dev_invar_impl
  !***************************************************************************
  !> @brief Deviatoric_impl
  !>
  !> @details Calculate the deviatoric part of a stress tensor
  !>
  !> @param[in]  stress  Stress tensor
  !>
  !> @return Deviatoric stress tensor
  !***************************************************************************
  module procedure Deviatoric_impl
  implicit none
  type(Torch) :: torch_
  real(DP) :: mean
  ! mean is mean stress which must be larger than zero
  mean = torch_%Trace(stress) / three
  tensor = stress - mean * DELTA
  end procedure Deviatoric_impl
  !***************************************************************************
  !> @brief Ratio_impl
  !>
  !> @details Calculate the stress ratio tensor (deviatoric stress divided by
  !> mean stress)
  !> @param[in]  stress  Stress tensor
  !>
  !> @return Stress ratio tensor
  !***************************************************************************
  module procedure Ratio_impl
  ! declaration
  implicit none
  type(Torch) :: torch_
  real(DP), dimension(3, 3) :: S
  real(DP) :: mean
  ! implementation
  mean = torch_%Trace(stress) / three
  IF(ABS(mean) .LT. eps) mean = SIGN(eps, mean)
  ! deviatoric stress
  S = torch_%Deviatoric(stress)
  ! return tensor
  tensor = S / mean
  end procedure Ratio_impl
  !***************************************************************************
  !> @brief Sin3theta_impl
  !>
  !> @details Calculate sin(3θ) where θ is the Lode angle
  !>
  !> @param[in]  stress  Stress tensor
  !>
  !> @return sin(3θ) value
  !***************************************************************************
  module procedure Sin3theta_impl
  ! declaration
  implicit none
  type(Torch) :: torch_
  real(DP) :: J2, J3
  ! implementation
  ! compute J2 and J3
  J2 = torch_%Get_J2(stress)
  print *, "EPS=", EPS
  IF(ABS(J2) .LT. eps) J2 = SIGN(eps, J2)
  J3 = torch_%Get_J3(stress)
  IF(ABS(J3) .LT. eps) J3 = SIGN(eps, J3)
  sin3t = -1.5_dp * DSQRT(3.0_dp) * J3 / (J2**1.5_dp)
  if(ABS(sin3t) .GT. 1.0_dp) sin3t = SIGN(1.0_dp, sin3t)
  end procedure Sin3theta_impl
  !***************************************************************************
  !> @brief Shear_impl
  !>
  !> @details Calculate the shear stress (sqrt(J2))
  !>
  !> @param[in]  stress  Stress tensor
  !>
  !> @return Shear stress value
  !***************************************************************************
  module procedure Shear_impl
  use Base_config, only: three
  type(Torch) :: torch_
  real(DP) :: J2
  J2 = torch_%Get_J2(stress)
  shear = dsqrt(three * J2)
  end procedure Shear_impl
  !***************************************************************************
  !> @brief Normalize_impl
  !>
  !> @details Calculate the normalize of the tensor
  !>
  !> @param[in]  a 3x3 size of tensor
  !>
  !> @return Normalized tensor
  !***************************************************************************
  module procedure Normalize_impl
  real(DP) :: norm
  norm = sum(tensor**2)
  norm = MAX(DSQRT(norm), EPS)
  res(:, :) = tensor(:, :) / norm
  end procedure Normalize_impl
  !***************************************************************************
  !> @brief Normalize_impl
  !>
  !> @details Calculate the normalize of the tensor
  !>
  !> @param[in]  a 3x3 size of tensor
  !>
  !> @return the norm of tensor
  !***************************************************************************
  module procedure Norm_impl
  real(DP) :: temp
  temp = sum(tensor**2)
  res = max(dsqrt(temp), EPS)
  end procedure Norm_impl
  !***************************************************************************
  !> @brief eye_impl
  !>
  !> @details Calculate the normalize of the tensor
  !>
  !> @param[in]  size: size of tensor
  !>
  !> @return the unit matrix of given size
  !***************************************************************************
  module procedure eye_impl
  integer :: i, j

  do i = 1, size
    do j = 1, size
      if(i == j) then
        delta(i, j) = 1.0_DP
      else
        delta(i, j) = 0.0_DP
      endif
    enddo
  enddo
  end procedure eye_impl
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
  module procedure Tensor4_ddot_tensor2
  implicit none
  integer :: I, J
  DO J = 1, 3
    DO I = 1, 3
      res(I, J) = SUM(TENSOR4(I, J, :, :) * TENSOR2(:, :))
    ENDDO
  ENDDO
  end procedure Tensor4_ddot_tensor2
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
  module procedure Tensor2_ddot_tensor4
  integer :: I, J
  DO J = 1, 3
    DO I = 1, 3
      res(I, J) = SUM(TENSOR2(:, :) * TENSOR4(:, :, I, J))
    ENDDO
  ENDDO
  end procedure Tensor2_ddot_tensor4
  !
endsubmodule tensor_torch_impl
submodule(elastic_mod) elastic_impl
  use Base_config, only: DP, DELTA, EPS, PA
  use Material_config, only: PARAM
  use tensor_opt_mod
  implicit none
  type(Torch) torch_
  type(Elast) elast_
contains
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
                    (1.0_DP - PARAM%C**2) * sin3t) - (1.0_DP + PARAM%C**2)) / 2.0_DP / &
            (1.0_DP - PARAM%C) / sin3t
  gtheta = -PARAM%C * (1.0_DP + PARAM%C) / (1.0_DP - PARAM%C) / sin3t / gtheta1
  IF((gtheta) .GE. 1.0D0) gtheta = 1.0D0
  IF((gtheta) .LE. PARAM%C) gtheta = PARAM%C
  atheta = (PARAM%C - 1.0_DP) * gtheta**2 / (2.0_DP * (1.0_DP - PARAM%C) &
                                             * sin3t * gtheta + (1.0_DP + PARAM%C**2))
  IF(ABS(atheta) .LT. EPS) atheta = SIGN(EPS, atheta)
  sdgth = (4.0_DP * (PARAM%C - 1.0_DP) * gtheta * atheta + 2.0_DP * (PARAM%C - 1.0_DP) &
           * sin3t * atheta) / (2.0_DP * (1.0_DP - PARAM%C) * sin3t * gtheta + &
                                (1.0_DP + PARAM%C**2))
  IF(ABS(sdgth) .LT. EPS) sdgth = SIGN(EPS, sdgth)
  end procedure Get_gtheta_impl
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
  module procedure Get_pFpr_impl
  real(DP) :: rm, gtheta, atheta, sdgth, sin3t
  real(DP), dimension(3, 3) :: ratio
  integer :: i
  call elast_%Get_gtheta(shvars, gtheta, atheta, sdgth)
  sin3t = torch_%Sin3theta(shvars%get_sigma())
  ratio = torch_%Get_ratio(shvars%get_sigma())
  rm = dsqrt(3.0_DP * torch_%Get_J2(shvars%get_sigma()))
  pfratio(:, :) = 1.5_DP / (rm * gtheta)**2 * &
                  ((rm * gtheta + 3.0_DP * rm * sin3t * atheta) * ratio(:, :) + &
                   9.0_DP * atheta * (sum([(ratio(:, i) * ratio(i, :), i=1, 3)])))
  end procedure Get_pFpr_impl
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
  real(DP) :: mean, RM, gtheta, atheta, sdgth
  ! implementation
  ! check the input variable
  mean = torch_%Trace(shvars%get_sigma()) / 3.0_DP
  IF(ABS(mean) .LT. eps) mean = SIGN(eps, mean)
  !
  RM = torch_%Shear(shvars%get_sigma()) / mean
  call elast_%Get_gtheta(shvars, gtheta, atheta, sdgth)
  ftol = RM / gtheta - shvars%get_harden()
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
  real(DP) :: shear, bulk, mean
  integer :: i, j, k, l
  ! mean effective shvars%get_sigma()(:,:)
  mean = torch_%Trace(shvars%get_sigma()) / 3.0_DP
  ! shear modulus
  shear = PARAM%G0 * (2.973_DP - voidr)**2 / (1.0_DP + voidr) * dsqrt(mean * PA)
  ! bulk modulus
  bulk = shear * 2.0_DP * (1.0_DP + PARAM%NU) / 3.0_DP / (1.0_DP - 2.0_DP * PARAM%NU)
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
  module procedure update_voidr_impl
  real(DP) :: despv
  !
  despv = torch_%Trace(dstrain)
  new_voidr = voidr - (1.0_dp + voidr) * despv
  end procedure update_voidr_impl
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
  module procedure Get_anisotropy_impl
  real(DP), dimension(3, 3) :: pFpr, dnorm
  pFpr(:, :) = elast_%Get_pFpr(shvars)
  dnorm(:, :) = torch_%Normalize(torch_%Deviatoric(pFpr(:, :)))
  abase = sum(dnorm(:, :) * shvars%get_fabric())
  abase = max(min(abase, 1.0_DP), -1.0_DP)
  end procedure Get_anisotropy_impl
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
  real(DP), dimension(3, 3, 3, 3) :: prpsigma
  real(DP) :: mean
  real(DP), dimension(3, 3) :: pfratio,sigma
  integer :: i, j, k, l
  sigma = shvars%get_sigma()
  mean = torch_%Trace(shvars%get_sigma()) / 3.0_dp
  do l = 1, 3
    do k = 1, 3
      do j = 1, 3
        do i = 1, 3
          prpsigma(i, j, k, l) = DELTA(i, k) * DELTA(j, l) / mean - &
                                 sigma(i,j) * DELTA(k, l) / mean**2 / 3.0_dp
        enddo
      enddo
    enddo
  enddo
  !
  pfratio(:, :) = elast_%Get_pFpr(shvars)
  pfsig(:, :) = pfratio.ddot.prpsigma
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
  real(DP), dimension(3, 3, 3, 3) :: pnpr
  real(DP), dimension(3, 3) :: R, pfratio, dnorm, pApr, pGpA, pgpr, EP
  real(DP) :: sin3t, gtheta, atheta, sdgth, rm, frmag, abase, A, hardg
  real(DP) :: dpla
  real(DP), dimension(7) :: scalar
  real(DP), dimension(3, 3, 3, 3, 7) :: tensor4

  integer :: i, j, k, l, ip, iq
  !
  sin3t = torch_%Sin3theta(shvars%get_sigma())
  call elast_%Get_gtheta(shvars, gtheta, atheta, sdgth)
  rm = dsqrt(3.0_DP * torch_%Get_J2(shvars%get_sigma()))
  R(:, :) = torch_%Get_ratio(shvars%get_sigma())
  pfratio(:, :) = elast_%Get_pFpr(shvars)
  !
  scalar(1) = 3.0_DP / 4.0_DP / rm / gtheta * (1.0_DP + 3.0_DP * sin3t / gtheta * atheta)
  scalar(2) = 27.0_DP / 4.0_DP / rm**2 / gtheta**2 * atheta
  scalar(3) = +9.0_DP / 4.0_DP / rm**3 / gtheta * (-1.0_DP + 9.0_DP * sin3t / gtheta * (2.0_DP * sin3t / gtheta * atheta**2 - atheta - sin3t * sdgth))
  scalar(4) = 81.0_DP / 4.0_DP / rm**4 / gtheta**2 * (6.0_DP * sin3t / gtheta * atheta**2 - 2.0_DP * atheta - 3.0_DP * sin3t * sdgth)
  scalar(5) = 729.0_DP / 4.0_DP / rm**5 / gtheta**3 * (2.0_DP * atheta**2 - gtheta * sdgth)
  scalar(6) = -27.0_DP * sin3t / 2.0_DP / rm**2 / gtheta**2 * (2.D0 / gtheta * atheta**2 - sdgth)
  scalar(7) = -81.0_DP / 2.0_DP / rm**3 / gtheta**2 * (2.0_DP / gtheta * atheta**2 - sdgth)
  do l = 1, 3
    do k = 1, 3
      do j = 1, 3
        do i = 1, 3
          tensor4(i, j, k, l, 1) = (DELTA(I, K) * DELTA(J, L) + DELTA(I, L) * DELTA(J, K))
          tensor4(i, j, k, l, 2) = (R(I, K) * DELTA(J, L) + R(I, L) * DELTA(J, K) + R(L, J) * DELTA(I, K) + R(K, J) * DELTA(I, L))
          tensor4(i, j, k, l, 3) = R(i, j) * R(k, l)
          tensor4(i, j, k, l, 4) = R(i, j) * sum([(R(k, ip) * R(ip, l), ip=1, 3)]) + sum([(R(i, ip) * R(ip, j), ip=1, 3)])
          tensor4(i, j, k, l, 5) = sum([(R(k, ip) * R(ip, l), ip=1, 3)]) * sum([(R(i, iq) * R(iq, j), iq=1, 3)])
          tensor4(i, j, k, l, 6) = DELTA(i, j) * R(k, l)
          tensor4(i, j, k, l, 7) = DELTA(i, j) * sum([(R(k, ip) * R(ip, l), ip=1, 3)])
        enddo
      enddo
    enddo
  enddo
  pnpr(:, :, :, :) = sum([(scalar(i) * tensor4(:, :, :, :, i), i=1, 7)])
  frmag = torch_%Norm(torch_%Deviatoric(pfratio))
  dnorm(:, :) = torch_%Normalize(torch_%Deviatoric(pfratio))
  abase = elast_%Get_anisotropy(shvars)
  A = -PARAM%KH * (abase - 1)**2
  pApr(:, :) = ((shvars%get_fabric() - abase * dnorm(:, :)) / frmag) .ddot.pnpr(:, :, :, :)
  hardg = rm / gtheta / exp(A)
  pGpA = 2.0_DP * PARAM%KH * hardg * (abase - 1.0_DP) * exp(A)
  pgpr = pfratio(:, :) + pGpA * pApr(:, :)
  dpla = plast_%Get_dilatancy(shvars, voidr)

  ep = torch_%Normalize(torch_%Deviatoric(pgpr))
  xm = ep(:, :) + dsqrt(2.0_DP / 3.0_DP) * dpla * DELTA(:, :) / 3.0_DP
  end procedure Get_pgsig_impl
  !
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
  real(DP) :: e_c, mean, psi, abase
  mean = torch_%Trace(shvars%get_sigma()) / 3.0_DP
  abase = elast_%Get_anisotropy(shvars)
  e_c = PARAM%VOIDC - PARAM%LAC * (mean / PA)**PARAM%KSI
  psi = voidr - e_c
  psim = psi - PARAM%EA * (abase - 1.0_DP)
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
  real(DP) :: psim, M_d, gtheta, atheta, sdgth, Rm
  !
  Rm = dsqrt(3.0_DP * torch_%Get_J2(shvars%get_sigma()))
  call elast_%Get_gtheta(shvars, gtheta, atheta, sdgth)
  psim = plast_%Get_psim(shvars, voidr)
  M_d = PARAM%MCS * gtheta * exp(PARAM%DM * psim)
  dpla = PARAM%D1 / PARAM%MCS / gtheta * (1.0_DP + Rm / PARAM%MCS / gtheta) * (M_d - Rm)
  end procedure Get_dilatancy_impl
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
  module procedure Get_evolution_impl
  real(DP) :: psim, M_d, gtheta, atheta, sdgth, Rm
  real(DP) :: M_p, abase, frmag, shear, mean
  real(DP), dimension(3, 3) :: dnorm, pfratio
  pfratio(:, :) = elast_%Get_pFpr(shvars)
  frmag = torch_%Norm(pfratio)
  dnorm = torch_%Normalize(torch_%Deviatoric(pfratio))
  ! mean effective stress
  mean = torch_%Trace(shvars%get_sigma())
  ! shear modulus
  shear = PARAM%G0 * (2.973_dp - voidr)**2 / (1.0_DP + voidr) / dsqrt(mean * PA)
  ! an
  abase = elast_%Get_anisotropy(shvars)
  Rm = dsqrt(3.0_DP * torch_%Get_J2(shvars%get_sigma()))
  call elast_%Get_gtheta(shvars, gtheta, atheta, sdgth)
  psim = plast_%Get_psim(shvars, voidr)
  M_p = PARAM%MCS * gtheta * exp(-PARAM%NKP * psim)
  Rh = frmag * shear * (1.0_DP - PARAM%CH * voidr) / Rm * (M_p - Rm)
  RF(:,:) = PARAM%FEVR*exp(abase)*(dnorm(:,:) - shvars%get_fabric())
  end procedure Get_evolution_impl
endsubmodule plastic_impl
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
!  @file     sanisand.f90
!  @brief    简要说明
!  @details  详细描述
!
!  @author   wuwenhao
!  @email    617082766@qq.com
!  @version  1.0.0.1
!  @date     2025/11/17
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
!  2025/11/17 | 1.0.0.1   | wuwenhao      | Create file
!*****************************************************************************
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
  use share_vars
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
  real(DP), dimension(3, 3) :: deplsn, fabric
  real(DP), dimension(3, 3) :: sigma, dsigetr, sigetr, sig_upd
  real(DP) :: voidr, voidr_upd, harden
  real(DP) :: ftoltr
  real(DP) :: pmeini, meanetr
  real(DP), dimension(3, 3, 3, 3) :: stiffness
  type(Torch) :: torch_
  type(elast) :: elast_
  type(Share_var) :: Shvars, Shvars_etr
  LOGICAL :: FIRSTRUN = .TRUE.
  INTEGER :: TEMPVAR
  INTEGER, SAVE :: NUMBER = 0
  !------------------------
  call abaqus_debug(1, 1, 0, noel, npt, 0, "Umat")
  sigma(:, :) = -convert_array_to_tensor(stress)
  deplsn(:, :) = -convert_array_to_tensor(dstran, 2.0_DP)
  voidr = statev(1)
  harden = statev(2)
  fabric(:, :) = -convert_array_to_tensor(statev(3:8))
  fabric(:, :) = matmul(drot, matmul(fabric, transpose(drot)))
  Shvars = Share_var(harden, sigma, fabric)
  ! initial mean stress
  pmeini = torch_%Trace(sigma) / 3.0_DP
  !

  if(pmeini < tensno) then

  elseif(pmeini > tensno) then
    stiffness = elast_%get_stiffness(Shvars, voidr)
    dsigetr(:, :) = stiffness(:, :, :, :) .ddot.deplsn(:, :)
    ! calculate the elastic trial stress
    Shvars_etr = Shvars
    call Shvars_etr%update_sigma(dsigetr)
    ftoltr = elast_%Yield_distance(Shvars_etr)
    meanetr = torch_%Trace(Shvars_etr%get_sigma()) / 3.0_dp
    if(ftoltr <= eps .and. meanetr >= tensno) then
      ! elastic updated
    elseif(ftoltr > eps .or. meanetr < tensno) then
    endif
  endif
  ! update state variables
  stress(:) = - Convert_tensor_to_array(Shvars_etr%get_sigma(), ntens)
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
!  @brief    简要说明
!  @details  详细描述
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
SUBROUTINE Sdvini(statev, coords, nstatv, ncrds, noel, npt, layer, kspt)
  use Base_config, only: DP, PI
  use presolve_mod
  use Material_config, only: PARAM
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
  void_ini = 0.6_DP  ! Initial void ratio
  harden_ini = 0.0_DP  ! Initial hardening parameter
  !
  angle = 90.0_DP * PI / 180_DP
  axis = [0.0_DP, 0.0_DP, 1.0_DP]
  rot_matrix(:, :) = Get_rotation_matrix(angle, axis)
  fabric_ini(:, :) = 0.0_DP
  temp = dsqrt(2.0_DP / 3.0_DP) * PARAM%F0
  fabric_ini(:, :) = reshape([ &
                             temp, 0.0_DP, 0.0_DP, &
                             0.0_DP, -temp / 2.0_DP, 0.0_DP, &
                             0.0_DP, 0.0_DP, -temp / 2.0_DP],[3, 3])
  fabric_rot(:, :) = matmul(rot_matrix, matmul(fabric_ini, transpose(rot_matrix)))
  !
  statev(1) = void_ini
  statev(2) = harden_ini
  statev(3:8) = Convert_tensor_to_array(fabric_rot, 6)
  if(nstatv > 8) statev(9:nstatv) = 0.0_DP ! initialize other state variables to zero
  RETURN
END SUBROUTINE Sdvini
subroutine Sigini(sigma, coords, ntens, ncrds, noel, npt, layer, &
                  kspt, lrebar, names)
  use Base_config, only: DP
  implicit none
  integer, intent(in) :: ntens, ncrds, noel, npt, layer, kspt, lrebar
  real(DP), intent(out) :: sigma(ntens)
  real(DP), intent(in) :: coords(ncrds)
  character(len=80), intent(in) :: names(2)
  LOGICAL :: FIRSTRUN = .TRUE.
  INTEGER :: TEMPVAR
  !
  sigma(1) = -100.0_DP
  sigma(2) = -100.0_DP
  sigma(3) = -100.0_DP
  sigma(4:ntens) = 0.0_DP
  return
endsubroutine Sigini
