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
