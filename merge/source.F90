
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
  public :: PA, PI, eps, delta, zero, one, two, three, four
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
  real(dp), parameter :: PA = 101.325d0 ! atmospheric pressure, unit kPa
  real(dp), parameter :: PI = 3.1415927D0 ! circular constant
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
    real(data_t) :: G0, nu_
    real(data_t) :: c
  endtype param

  type(param), parameter :: param_ = param(G0=125, nu=0.2, c=0.75)
contains

endmodule Material_config
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
    character(len=:),allocatable :: full_msg
    integer :: msg_len
    !
    ! 计算所需消息长度并分配缓冲区
    msg_len = len(this%prefix_) + len(msg) + len(this%file_) + 30  ! 额外空间用于格式文本和行号
    allocate(character(len=msg_len) :: full_msg)
    !
    ! 格式化错误消息
    write(full_msg, '(A,": ", A, " Error at: ",A, ": ",I0)') &
      this%prefix_, msg, this%file_, this%line_
    ! 输出错误信息到标准错误
    write(0, '(A)') trim(full_msg)
    error stop 1 ! Fortran 2008+ 终止并输出错误
  end procedure throw_error_impl
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
  use Base_config,only:data_t
  implicit none
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
  !
  implicit none
  real(data_t)::scalar_
  if (present(scalar) ) then
    scalar_ = scalar
  else
    scalar_ = 1.0d0
  end if
  select case(size(array))
  case(3)
    tensor(1, 1) = -array(1)
    tensor(2, 2) = -array(2)
    tensor(1, 2) = -array(3)/scalar_
    tensor(2, 1) = -array(3)/scalar_
  case(4)
    tensor(1, 1) = -array(1)
    tensor(2, 2) = -array(2)
    tensor(3, 3) = -array(3)
    tensor(1, 2) = -array(4)/scalar_
    tensor(2, 1) = -array(4)/scalar_
  case(6)
    tensor(1, 1) = -array(1)
    tensor(2, 2) = -array(2)
    tensor(3, 3) = -array(3)
    tensor(1, 2) = -array(4)/scalar_
    tensor(2, 1) = -array(4)/scalar_
    tensor(1, 3) = -array(5)/scalar_
    tensor(3, 1) = -array(5)/scalar_
    tensor(2, 3) = -array(6)/scalar_
    tensor(3, 2) = -array(6)/scalar_
  endselect
  end procedure convert_array_to_tensor
  !
  module procedure print_impl_
    implicit none
    integer :: i, n

    n = size(array)
    print *, "Array size: ", n
    print *, "Array elements:"
    do i = 1, n
      print *, "  array(", i, ") = ", array(i)
    end do
  end procedure print_impl_
endsubmodule
!*****************************************************************************
!> @brief tensor_opt_impl
!>
!> @details 模块详细描述
!>
!> @author wuwenhao
!> @date 2025/11/27
!*****************************************************************************
submodule(tensor_opt_mod) tensor_opt_impl
  ! 这是tensor_opt 模块的实现子模块
  ! 可以在实现文件的开头使用 use 语句导入所需模块
  ! 类似于C++ using namespace, 导入子模块不会污染命名空间，这是安全的
  use Base_config, only: dp, data_t, index_t, two, three, eps
  implicit none
contains
  !*****************************************************************************
  !> @brief print_impl_
  !>
  !> @details 打印3x3应力张量的内容
  !>
  !> @param[in] stress 应力张量
  !*****************************************************************************
  module procedure print_impl_
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

  end procedure print_impl_
  !*****************************************************************************
  !> @brief trace_impl_
  !>
  !> @details 计算矩阵的迹
  !>
  !> @param[in]  stress 应力张量
  !> @param[out] val 矩阵的迹
  !>
  !> @return 矩阵的迹
  !*****************************************************************************
  module procedure trace_impl_
  implicit none
  integer(index_t) :: i
  val = sum([(stress(i, i), i=1, 3)])
  end procedure trace_impl_
  !*****************************************************************************
  !> @brief sec_invariant_impl_
  !>
  !> @details 计算偏张量
  !>
  !> @param[in]  stress 应力张量
  !> @param[out] val 矩阵的迹
  !>
  !> @return 矩阵的迹
  !*****************************************************************************
  module procedure sec_dev_invar_impl_
  implicit none
  type(Torch) :: opt_
  real(data_t), dimension(3, 3) :: S
  S = opt_%Deviatoric(stress)
  val = sum(S**2) / two
  end procedure sec_dev_invar_impl_
  !*****************************************************************************
  module procedure trd_dev_invar_impl_
  implicit none
  type(Torch) :: opt_
  real(data_t), dimension(3, 3) :: S, temp
  ! implementation
  ! deviatoric stress
  S = opt_%Deviatoric(stress)
  temp = matmul(S, matmul(S, S))
  val = opt_%Trace(temp) / three
  end procedure trd_dev_invar_impl_
  !*****************************************************************************
  module procedure deviatoric_impl_
  implicit none
  real(dp), parameter :: delta1(3, 3) = reshape( &
                         [1.D0, 0.D0, 0.D0, &
                          0.D0, 1.D0, 0.D0, &
                          0.D0, 0.D0, 1.D0],[3, 3])
  type(Torch) :: opt_
  real(data_t) :: mean
  ! mean is mean stress which must be larger than zero
  mean = opt_%Trace(stress) / three
  tensor = stress - mean * delta1
  end procedure deviatoric_impl_
  !*****************************************************************************
  !> @brief ratio_impl_
  !>
  !> @details 计算偏张量
  !>
  !> @param[in]  stress 应力张量
  !> @param[out] val 矩阵的迹
  !>
  !> @return 矩阵的迹
  !*****************************************************************************
  module procedure ratio_impl_
  ! declaration
  implicit none
  type(Torch) :: opt_
  real(data_t), dimension(3, 3) :: S
  real(data_t) :: mean
  ! implementation
  mean = opt_%Trace(stress) / three
  IF(ABS(mean) .LT. eps) mean = SIGN(eps, mean)
  ! deviatoric stress
  S = opt_%Deviatoric(stress)
  ! return tensor
  tensor = S / mean
  end procedure ratio_impl_
  !*****************************************************************************
  module procedure sin3theta_impl_
  ! declaration
  implicit none
  type(Torch) :: opt_
  real(data_t) :: J2, J3
  ! implementation
  ! compute J2 and J3
  J2 = opt_%Get_J2(stress)
  IF(ABS(J2) .LT. eps) J2 = SIGN(eps, J2)
  J3 = opt_%Get_J3(stress)
  IF(ABS(J3) .LT. eps) J3 = SIGN(eps, J3)
  sin3t = -1.5_dp * DSQRT(3.0_dp) * J3 / (J2**1.5_dp)
  if(ABS(sin3t) .GT. 1.0_dp) sin3t = SIGN(1.0_dp, sin3t)
  end procedure sin3theta_impl_
  !*****************************************************************************
  module procedure shear_impl
  use Base_config, only: three
  type(Torch) :: opt_
  real(data_t) :: J2
  J2 = opt_%Get_J2(stress)
  shear = dsqrt(three * J2)
  end procedure shear_impl
  !*****************************************************************************
  module procedure tensor4_ddot_tensor2
  implicit none
  integer :: I, J
  DO J = 1, 3
    DO I = 1, 3
      res(I, J) = SUM(TENSOR4(I, J, :, :) * TENSOR2(:, :))
    ENDDO
  ENDDO
  end procedure tensor4_ddot_tensor2
  !
  module procedure tensor2_ddot_tensor4
  integer :: I, J
  DO J = 1, 3
    DO I = 1, 3
      res(I, J) = SUM( TENSOR2(:, :)* TENSOR4(:, :, I, J))
    ENDDO
  ENDDO
  end procedure tensor2_ddot_tensor4
endsubmodule tensor_opt_impl
submodule(elastic_mod) elastic_impl
  use Base_config
  implicit none

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
  module procedure interpolate_impl
  use Material_config, only: param_
  use tensor_opt_mod
  implicit none
  type(Torch) :: opt_
  real(data_t) :: gtheta1, sin3t
  !
  sin3t = opt_%Sin3theta(stress)
  gtheta1 = (-dsqrt((one + param_%c**2)**2 + 4.0_dp * param_%c * &
                   (one - param_%c**2) * sin3t) - (one + param_%c**2)) / two / &
            (one - param_%c) / sin3t
  gtheta = -param_%c * (one + param_%c) / (one - param_%c) / sin3t / gtheta1
  IF((GTHETA) .GE. 1.0D0) GTHETA = 1.0D0
  IF((GTHETA) .LE. param_%c) GTHETA = param_%c
  end procedure interpolate_impl
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
  module procedure isyield_impl
  use tensor_opt_mod
  type(Torch) :: torch_
  type(elast) :: elast_
  real(data_t) :: mean, RM, gtheta
  ! implementation
  ! check the input variable
  mean = torch_%Trace(stress) / three
  IF(ABS(mean) .LT. eps) mean = SIGN(eps, mean)
  RM = torch_%Shear(stress) / mean
  gtheta = elast_%interpolate(stress)
  distance = RM / gtheta - harden
  end procedure isyield_impl
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
  module procedure get_stiffness_impl
  use Material_config, only: param_
  use tensor_opt_mod
  implicit none
  type(Torch) :: torch_
  real(data_t) :: shear, bulk, mean
  integer :: i, j, k, l
  ! mean effective stress
  mean = torch_%Trace(stress)
  ! shear modulus
  shear = param_%G0*(2.973_dp - void_ratio)**2/(one + void_ratio)/dsqrt(mean*PA)
  ! bulk modulus
  bulk = shear * two * (one + param_%nu_) / three / (one - two * param_%nu_)
  ! stiffness tensor
  do l = 1, 3
    do k = 1, 3
      do j = 1, 3
        do i = 1, 3
          stiffness(i, j, k, l) = &
            (bulk - two / three * shear) * delta(i, j) * delta(k, l) + &
            shear * (delta(i, k) * delta(j, l) + delta(i, l) * delta(j, k))
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
  use tensor_opt_mod
  implicit none
  type(Torch) :: torch_
  real(dp) :: despv
  !
  despv = torch_%Trace(dstrain)
  new_voidr = voidr - (1.0_dp + voidr) * despv
  end procedure update_voidr_impl
endsubmodule elastic_impl
!DIR$ FREEFORM
!*****************************************************************************
!  Project Name
!  Copyright (C) 2025 wuwenhao 617082766@qq.com
!
!  This file is part of Project Name.
!
!  This program is free software; you can redistribute it and/or modify
!  it under the terms of the GNU General Public License version 3 as
!  published by the Free Software Foundation.
!
!  You should have received a copy of the GNU General Public License
!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!
!  @file     sanisand.f90
!  @brief    简要说明
!  @details  详细描述
!
!  @author   wuwenhao
!  @email    617082766@qq.com
!  @version  1.0.0.1
!  @date     2025/11/17
!  @license  MIT General Public License (MIT)
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
  real(data_t), dimension(3, 3) :: deplsn, fabric
  real(dp), dimension(3, 3) :: sigma, dsigetr, sigetr, sig_upd
  real(data_t) :: voidr, voidr_upd,harden
  real(dp) :: ftoltr
  real(data_t) :: pmeini, meanetr
  real(dp), dimension(3, 3, 3, 3) :: stiffness
  type(Torch) :: opt_
  type(elast) :: elast_
  !------------------------
  sigma(:, :) = convert_array_to_tensor(stress)
  deplsn(:, :) = convert_array_to_tensor(dstran, two)
  voidr = statev(1)
  harden = statev(2)
  fabric(:, :) = -convert_array_to_tensor(statev(3:8))
  fabric(:, :) = matmul(drot, matmul(fabric, transpose(drot)))
  ! initial mean stress
  pmeini = opt_%Trace(sigma) / three
  !
  if(pmeini < tensno) then

  elseif(pmeini > tensno) then
    stiffness = elast_%get_stiffness(sigma, voidr)
    dsigetr(:, :) = stiffness(:, :, :, :) .ddot.deplsn(:, :)
    ! calculate the elastic trial stress
    sigetr(:, :) = sigma(:, :) + dsigetr(:, :)
    ftoltr = elast_%isyield(sigetr, harden)
    meanetr = opt_%Trace(sigetr) / 3.0_dp
    if(ftoltr <= eps .and. meanetr >= tensno) then
      ! elastic updated
      sig_upd(:, :) = sigetr(:, :)
      voidr_upd = elast_%update_voidr(voidr, deplsn)
    elseif(ftoltr > eps .or.  meanetr < tensno)then
    end if
  endif
endsubroutine
