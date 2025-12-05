
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
  public :: ASSERT_TRUE, ASSERT_EQUAL,ASSERT_FLOAT_EQUAL
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
  endinterface ! end interface
contains

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
  subroutine ASSERT_TRUE(expr, msg, file, line)
    implicit none
    ! Input/Output variables
    logical, intent(in) :: expr
    character(len=*), intent(in) :: msg
    character(len=*), intent(in) :: file
    character(len=:), allocatable :: validator_msg
    integer, intent(in) :: line
    type(ErrorContext) Error
    !
    validator_msg = "Condition evaluated to false"
    if(.not. expr) then
      Error = ErrorContext(file, line, msg)
      call Error%throw_error(validator_msg)
    endif
  endsubroutine ASSERT_TRUE
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