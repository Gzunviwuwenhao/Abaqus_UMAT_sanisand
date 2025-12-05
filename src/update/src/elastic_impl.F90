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
