@echo off
REM Setup script for Confluence MCP Server (Windows)
REM This script creates a virtual environment and installs dependencies

echo ========================================
echo Confluence MCP Server - Setup
echo ========================================

REM Check if Python is available
python --version >nul 2>&1
if errorlevel 1 (
    echo ERROR: Python is not installed or not in PATH
    exit /b 1
)

REM Create virtual environment if it doesn't exist
if not exist "venv" (
    echo Creating virtual environment...
    python -m venv venv
    if errorlevel 1 (
        echo ERROR: Failed to create virtual environment
        exit /b 1
    )
    echo Virtual environment created successfully
) else (
    echo Virtual environment already exists
)

REM Activate virtual environment
echo Activating virtual environment...
call venv\Scripts\activate.bat

REM Upgrade pip
echo Upgrading pip...
python -m pip install --upgrade pip

REM Install runtime dependencies
echo Installing runtime dependencies...
pip install -r requirements.txt
if errorlevel 1 (
    echo ERROR: Failed to install runtime dependencies
    exit /b 1
)

REM Install development dependencies
echo Installing development dependencies...
pip install -r requirements-dev.txt
if errorlevel 1 (
    echo ERROR: Failed to install development dependencies
    exit /b 1
)

echo ========================================
echo Setup complete!
echo ========================================
echo.
echo To activate the virtual environment, run:
echo   venv\Scripts\activate.bat
echo.
echo To run the server:
echo   set CONFLUENCE_BASE_URL=https://central.confluence.automotive.cloud
echo   set CONFLUENCE_PAT=your-personal-access-token
echo   python src/server.py
echo.
