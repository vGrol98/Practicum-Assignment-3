cd src
ghc --make Main.hs
IF %ERRORLEVEL% EQU 0 (
    Main ..\test-cases\01-test-task01.cs
)

IF %ERRORLEVEL% EQU 0 (
        cd ..\ssm-20150616
        java -jar .\ssm.jar --file C:\Users\Max\source\repos\HaskellC#\test-cases\01-test-task01.ssm
    )