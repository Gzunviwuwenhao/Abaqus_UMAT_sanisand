!*****************************************************************************
!> @brief Sigini
!>
!> @details 子程序详细描述
!>
!> @param[in]  参数名 输入参数说明
!> @param[out] 参数名 输出参数说明
!> @param[in,out] 参数名 输入输出参数说明
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
