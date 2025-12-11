@echo off
setlocal enabledelayedexpansion

echo ================================================
echo Merge all the Fortran source files together
echo ================================================
:: 设置项目根目录和输出文件路径
set "PROJECT_ROOT=F:\vscode\Abaqus_UMAT_sanisand"
set "OUTPUT_FILE=%PROJECT_ROOT%\merge\source.F90"
echo 输出文件: %OUTPUT_FILE%

:: 清空输出文件
echo. > "%OUTPUT_FILE%"
:: 添加源文件内容
echo 添加 base_config.F90
type "%PROJECT_ROOT%\src\utils\include\base_config.F90" >> "%OUTPUT_FILE%"
echo 添加 material_config.F90
type "%PROJECT_ROOT%\src\utils\include\material_config.F90" >> "%OUTPUT_FILE%"
echo 添加 share_vars.F90...
type "%PROJECT_ROOT%\src\update\include\share_vars.F90" >> "%OUTPUT_FILE%"
echo 添加 tensor_opt.F90...
type  "%PROJECT_ROOT%\src\utils\include\tensor_opt.F90"  >> "%OUTPUT_FILE%"
echo 添加 preprocess.F90...
type  "%PROJECT_ROOT%\src\utils\include\preprocess.F90"  >> "%OUTPUT_FILE%"
echo 添加 exception.F90...
type  "%PROJECT_ROOT%\src\utils\include\exception.F90"  >> "%OUTPUT_FILE%"
echo 添加 elastic.F90...
type  "%PROJECT_ROOT%\src\update\include\elastic.F90"  >> "%OUTPUT_FILE%"
echo 添加 plastic.F90...
type  "%PROJECT_ROOT%\src\update\include\plastic.F90"  >> "%OUTPUT_FILE%"

echo 添加 exception_impl.F90...
type "%PROJECT_ROOT%\src\utils\src\exception_impl.F90" >> "%OUTPUT_FILE%"
echo 添加 preprocess_impl.F90...
type "%PROJECT_ROOT%\src\utils\src\preprocess_impl.F90" >> "%OUTPUT_FILE%"
echo 添加 tensor_opt_impl.F90...
type  "%PROJECT_ROOT%\src\utils\src\tensor_opt_impl.F90"  >> "%OUTPUT_FILE%"
echo 添加 elastic.F90...
type  "%PROJECT_ROOT%\src\update\src\elastic_impl.F90"  >> "%OUTPUT_FILE%"
echo 添加 plastic.F90...
type  "%PROJECT_ROOT%\src\update\src\plastic_impl.F90"  >> "%OUTPUT_FILE%"
:: 添加umat.cpp内容
echo 添加 sanisand.f90...
type "%PROJECT_ROOT%\src\interface\umat.f90" >> "%OUTPUT_FILE%"
echo 添加 sdvini.F90...
type "%PROJECT_ROOT%\src\interface\sdvini.F90" >> "%OUTPUT_FILE%"
echo 添加sigini.F90...
type "%PROJECT_ROOT%\src\interface\sigini.F90" >> "%OUTPUT_FILE%"
echo.
echo ================================================
echo fortran源文件合并完成
echo 输出文件: %OUTPUT_FILE%
echo ================================================

endlocal
