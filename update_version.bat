@echo off
setlocal enabledelayedexpansion

:: Get the commit count from git
for /f %%i in ('git rev-list --count main') do set commitCount=%%i
set /a newCommitVersion=%commitCount% + 1

:: Define the version string
set version=0.4.%newCommitVersion%

:: Update the version file
set versionFilePath=sources\version.inc
echo const> !versionFilePath!
echo   VERSION = '!version!';>> !versionFilePath!

:: Git operations
git add !versionFilePath!
