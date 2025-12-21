@echo off
setlocal enabledelayedexpansion

echo ================================================
echo Merge all the Fortran source files together
echo ================================================
:: 使用从 umat.bat 传递的项目源目录
if not defined PROJECT_SOURCE_DIR (
    echo 错误: PROJECT_SOURCE_DIR 未定义
    exit /b 1
)
set "OUTPUT_FILE=%PROJECT_SOURCE_DIR%\merge\source.F90"
echo 输出文件: %OUTPUT_FILE%

:: 清空输出文件
echo. > "%OUTPUT_FILE%"
:: 添加源文件内容
echo 添加 base_config.F90
type "%PROJECT_SOURCE_DIR%\src\utils\include\base_config.F90" >> "%OUTPUT_FILE%"
echo 添加 material_config.F90
type "%PROJECT_SOURCE_DIR%\src\utils\include\material_config.F90" >> "%OUTPUT_FILE%"
echo 添加 container.F90...
type "%PROJECT_SOURCE_DIR%\src\update\include\container.F90" >> "%OUTPUT_FILE%"
echo 添加 tensor_opt.F90...
type  "%PROJECT_SOURCE_DIR%\src\utils\include\tensor_opt.F90"  >> "%OUTPUT_FILE%"
echo 添加 preprocess.F90...
type  "%PROJECT_SOURCE_DIR%\src\utils\include\preprocess.F90"  >> "%OUTPUT_FILE%"
echo 添加 exception.F90...
type  "%PROJECT_SOURCE_DIR%\src\utils\include\exception.F90"  >> "%OUTPUT_FILE%"
echo 添加 elastic.F90...
type  "%PROJECT_SOURCE_DIR%\src\update\include\elastic.F90"  >> "%OUTPUT_FILE%"
echo 添加 plastic.F90...
type  "%PROJECT_SOURCE_DIR%\src\update\include\plastic.F90"  >> "%OUTPUT_FILE%"
echo 添加 math.F90...
type "%PROJECT_SOURCE_DIR%\src\update\include\math.F90"  >> "%OUTPUT_FILE%"
:: 添加 impl
echo 添加 container_impl.F90...
type "%PROJECT_SOURCE_DIR%\src\update\src\container_impl.F90" >> "%OUTPUT_FILE%"
echo 添加 exception_impl.F90...
type "%PROJECT_SOURCE_DIR%\src\utils\src\exception_impl.F90" >> "%OUTPUT_FILE%"
echo 添加 preprocess_impl.F90...
type "%PROJECT_SOURCE_DIR%\src\utils\src\preprocess_impl.F90" >> "%OUTPUT_FILE%"
echo 添加 tensor_opt_impl.F90...
type  "%PROJECT_SOURCE_DIR%\src\utils\src\tensor_opt_impl.F90"  >> "%OUTPUT_FILE%"
echo 添加 elastic_impl.F90...
type  "%PROJECT_SOURCE_DIR%\src\update\src\elastic_impl.F90"  >> "%OUTPUT_FILE%"
echo 添加 plastic_impl.F90...
type  "%PROJECT_SOURCE_DIR%\src\update\src\plastic_impl.F90"  >> "%OUTPUT_FILE%"
echo 添加 math_impl.F90...
type "%PROJECT_SOURCE_DIR%\src\update\src\math_impl.F90"  >> "%OUTPUT_FILE%"
:: 添加umat.cpp内容
echo 添加 sanisand.f90...
type "%PROJECT_SOURCE_DIR%\src\interface\umat.f90" >> "%OUTPUT_FILE%"
echo 添加 sdvini.F90...
type "%PROJECT_SOURCE_DIR%\src\interface\sdvini.F90" >> "%OUTPUT_FILE%"
echo 添加sigini.F90...
type "%PROJECT_SOURCE_DIR%\src\interface\sigini.F90" >> "%OUTPUT_FILE%"
echo.
echo ================================================
echo fortran源文件合并完成
echo 输出文件: %OUTPUT_FILE%
echo ================================================

endlocal
