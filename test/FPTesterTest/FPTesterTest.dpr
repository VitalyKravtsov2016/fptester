program FPTesterTest;
uses
  Forms,
  TestFramework,
  GUITestRunner,
  OFDAPI in '..\..\src\fptester\Units\OFDAPI.pas',
  duFiscalPrinter in 'Units\duFiscalPrinter.pas',
  duOFDAPI in 'Units\duOFDAPI.pas',
  DrvFRLib_TLB in '..\..\src\fptester\Units\DrvFRLib_TLB.pas',
  FileUtils in '..\..\src\fptester\Units\FileUtils.pas',
  ComPortPair in '..\..\src\fptester\Units\ComPortPair.pas',
  FrEmuPrinter in '..\..\src\fptester\Units\FrEmuPrinter.pas';

{$R *.RES}

begin
  TGUITestRunner.RunTest(RegisteredTests);
end.
