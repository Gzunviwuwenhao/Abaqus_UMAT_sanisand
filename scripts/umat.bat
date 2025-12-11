@echo off
setlocal enabledelayedexpansion
rem --- 1) 启动 VS2022 vcvars，以 v140 模拟 VC++14.0 ---
@REM call  "C:\Program Files (x86)\Microsoft Visual Studio 14.0\VC\vcvarsall.bat" x86_amd64
call  "E:\VS2022\VC\Auxiliary\Build\vcvars64.bat" x64
rem --- 2) 启动 Intel 环境（如果需要） ---
call "E:\oneapi2022\compiler\latest\env\vars.bat" intel64 vs2022
@REM call "E:\IntelSWTools\compilers_and_libraries_2016.1.146\windows\bin\ipsxe-comp-vars.bat" intel64 vs2015

rem --- 添加运行时库兼容性修复 ---
set "VSCMD_ARG_HOST_ARCH=x64"
set "VSCMD_ARG_TGT_ARCH=x64"
set "Platform=x64"
rem --- 3) 把项目 include/lib 路径放到环境变量前面，确保编译器能找到你的头与兼容的 VC/SDK 头 ---
set "PROJECT_SOURCE_DIR=F:\vscode\Abaqus_UMAT_sanisand"
set "PROJECT_INCLUDE=%PROJECT_SOURCE_DIR%\build-vs2022\include\Release"
rem --- detect latest Windows Kits 10 include version and set include/lib paths ---
set "WINDOWS_VC_INCLUDE=C:\Program Files (x86)\Microsoft Visual Studio 14.0\VC\include"
set "WINDOWS_VC_LIB=C:\Program Files (x86)\Microsoft Visual Studio 14.0\VC\lib\amd64"
set "ONEAPI_LIB=E:\oneapi2022\compiler\latest\windows\compiler\lib\intel64_win"
set "PROJECT_LIB=%PROJECT_SOURCE_DIR%\build-vs2022\lib\Release"
echo ================================================
echo 环境路径信息
echo ================================================

echo.
echo [基本路径]
echo PROJECT_INCLUDE: %PROJECT_INCLUDE%
echo PROJECT_SOURCE: %PROJECT_SOURCE%
echo PROJECT_LIB: %PROJECT_LIB%
echo WINDOWS_VC_INCLUDE: %WINDOWS_VC_INCLUDE%
echo WINDOWS_VC_LIB: %WINDOWS_VC_LIB%
echo ONEAPI_LIB: %ONEAPI_LIB%
echo.
echo [Windows SDK 检测]
set "WIN_KIT_VER="
for /f "delims=" %%V in ('dir /b /ad "C:\Program Files (x86)\Windows Kits\10\Include" 2^>nul ^| sort /r') do (
	set "WIN_KIT_VER=10.0.22621.0"
	goto :FoundWinKit
)
:FoundWinKit
if defined WIN_KIT_VER (
	echo WIN_KIT_VER: %WIN_KIT_VER%

	set "WIN_KIT_UCRT=C:\Program Files (x86)\Windows Kits\10\Include\%WIN_KIT_VER%\ucrt"
	set "WIN_KIT_SHARED=C:\Program Files (x86)\Windows Kits\10\Include\%WIN_KIT_VER%\shared"
	set "WIN_KIT_UM=C:\Program Files (x86)\Windows Kits\10\Include\%WIN_KIT_VER%\um"
	set "WIN_KIT_LIB_UCRT=C:\Program Files (x86)\Windows Kits\10\Lib\%WIN_KIT_VER%\ucrt\x64"
	set "WIN_KIT_LIB_UM=C:\Program Files (x86)\Windows Kits\10\Lib\%WIN_KIT_VER%\um\x64"

	echo WIN_KIT_UCRT: !WIN_KIT_UCRT!
	echo WIN_KIT_SHARED: !WIN_KIT_SHARED!
	echo WIN_KIT_UM: !WIN_KIT_UM!
	echo WIN_KIT_LIB_UCRT: !WIN_KIT_LIB_UCRT!
	echo WIN_KIT_LIB_UM: !WIN_KIT_LIB_UM!

	rem 构建INCLUDE路径 - 避免多余分号
	set "INCLUDE=!PROJECT_INCLUDE!"
	set "INCLUDE=!INCLUDE!;!WIN_KIT_UCRT!"
	set "INCLUDE=!INCLUDE!;!WIN_KIT_SHARED!"
	set "INCLUDE=!INCLUDE!;!WIN_KIT_UM!"
	set "INCLUDE=!INCLUDE!;!WINDOWS_VC_INCLUDE!"
	rem 构建LIB路径 - 避免多余分号
	set "LIB=!PROJECT_LIB!"
	set "LIB=!LIB!;!WIN_KIT_LIB_UCRT!"
	set "LIB=!LIB!;!WIN_KIT_LIB_UM!"
	set "LIB=!LIB!;!WINDOWS_VC_LIB!"
  set "LIB=!LIB!;!ONEAPI_LIB!"
  set "LIB=!LIB!;!PROJECT_LIB!"
) else (
	echo [回退路径 - 未找到 Windows SDK]
	set "INCLUDE=F:\vscode\umat_cpp_tensor\include;F:\vscode\umat_cpp_tensor\include\utils;C:\Program Files (x86)\Microsoft Visual Studio 14.0\VC\include"
	set "LIB=F:\vscode\umat_cpp_tensor\lib;C:\Program Files (x86)\Microsoft Visual Studio 14.0\VC\lib\amd64"
)
echo.
echo [最终环境变量]
echo INCLUDE: %INCLUDE%
echo LIB: %LIB%

echo.
echo [路径验证]
echo 检查 ucrt 路径是否存在:
if exist "%WIN_KIT_UCRT%" (echo   ✓ 存在) else (echo   ✗ 不存在)
echo 检查 shared 路径是否存在:
if exist "%WIN_KIT_SHARED%" (echo   ✓ 存在) else (echo   ✗ 不存在)
echo 检查 um 路径是否存在:
if exist "%WIN_KIT_UM%" (echo   ✓ 存在) else (echo   ✗ 不存在)
echo 检查 VC include 路径是否存在:
if exist "C:\Program Files (x86)\Microsoft Visual Studio 14.0\VC\include" (echo   ✓ 存在) else (echo   ✗ 不存在)

echo.
echo ================================================
echo 环境设置完成
echo ================================================
rem --- 4) 合并所有源文件 ---
echo.
echo ================================================
echo 合并源文件
echo ================================================
call "F:\vscode\Abaqus_UMAT_sanisand\scripts\merge_fortran.bat"

rem --- 5) 删除旧的编译文件 ---
echo.
echo ================================================
echo 清理旧的编译文件
echo ================================================
if exist "F:\vscode\Abaqus_UMAT_sanisand\source-std.obj" (
    echo 删除 source-std.obj...
    del "F:\vscode\Abaqus_UMAT_sanisand\source-std.obj"
) else (
    echo source-std.obj 不存在，无需删除
)

if exist "F:\vscode\Abaqus_UMAT_sanisand\standardU.dll" (
    echo 删除 standardU.dll...
    del "F:\vscode\Abaqus_UMAT_sanisand\standardU.dll"
) else (
    echo standardU.dll 不存在，无需删除
)

@REM rem --- 6) 调用 Abaqus 的 SMALauncher（使用合并后的文件） ---
@REM rem If no arguments are provided, this wrapper was likely invoked directly.
@REM rem Use a label-based check to avoid batch parser issues with parenthesis in environment variables.
if "%~1"=="" goto :NoArgs

"E:\ABAQUS2025\product\win_b64\code\bin\SMALauncher.exe" %*
goto :EOF

:NoArgs
echo No arguments passed to umat.bat. This wrapper should be invoked by the Abaqus launcher (abq2025).
echo Example: abq2025 make library=umat/umat_merged.cpp
exit /b 0
