@echo off
REM ============================================================================
REM Confluence MCP Server - Installer Launcher
REM ============================================================================
REM Double-click this file to start the GUI installer.
REM Requires Python 3.10 or higher to be installed.
REM ============================================================================

python --version >nul 2>&1
if errorlevel 1 (
    echo.
    echo  ERROR: Python is not installed or not in PATH.
    echo  Please install Python 3.10 or higher from https://www.python.org
    echo.
    pause
    exit /b 1
)

python "%~dp0install.py"

if errorlevel 1 (
    echo.
    echo  Something went wrong. Check the error above.
    echo.
    pause
)
