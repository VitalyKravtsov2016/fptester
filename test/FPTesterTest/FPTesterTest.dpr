program FPTesterTest;
uses
  Forms,
  TestFramework,
  GUITestRunner,
  duOFDAPI in 'Units\duOFDAPI.pas',
  OFDAPI in '..\..\src\fptester\Units\OFDAPI.pas';

{$R *.RES}

begin
  TGUITestRunner.RunTest(RegisteredTests);
end.
