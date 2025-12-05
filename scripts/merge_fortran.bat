@echo off
setlocal enabledelayedexpansion

echo ================================================
echo Merge all the Fortran source files together
echo ================================================

set "OUTPUT_FILE=..\..\merge\source.F90"

echo 输出文件: %OUTPUT_FILE%

:: 清空输出文件
echo. > "%OUTPUT_FILE%"
:: 添加源文件内容
echo 添加 base_config.F90
type "..\..\src\utils\include\base_config.F90" >> "%OUTPUT_FILE%"
echo 添加 material_config.F90
type "..\..\src\utils\include\material_config.F90" >> "%OUTPUT_FILE%"
echo 添加 exception_impl.F90...
type "..\..\src\utils\src\exception_impl.F90" >> "%OUTPUT_FILE%"
echo 添加 preprocess_impl.F90...
type "..\..\src\utils\src\preprocess_impl.F90" >> "%OUTPUT_FILE%"
echo 添加 tensor_opt_impl.F90...
type  "..\..\src\utils\src\tensor_opt_impl.F90"  >> "%OUTPUT_FILE%"
echo 添加 elastic.F90...
type  "..\..\src\update\src\elastic_impl.F90"  >> "%OUTPUT_FILE%"
:: 添加umat.cpp内容
echo 添加 sanisand.f90...
type "..\..\src\app\sanisand.f90" >> "%OUTPUT_FILE%"

echo.
echo ================================================
echo fortran源文件合并完成
echo 输出文件: %OUTPUT_FILE%
echo ================================================

endlocal
