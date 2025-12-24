!DIR$ FREEFORM
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
