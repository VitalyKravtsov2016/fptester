program fptester;
uses
  DriverTester in 'Units\DriverTester.pas',
  DrvFRLib_TLB in 'Units\DrvFRLib_TLB.pas',
  FileUtils in 'Units\FileUtils.pas';

{$R *.RES}

var
  Tester: TDriverTester;
begin
  Tester := TDriverTester.Create;
  try
    Tester.Run;
  finally
    Tester.Free;
  end;
end.


