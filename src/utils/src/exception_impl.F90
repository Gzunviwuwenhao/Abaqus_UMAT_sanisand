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
