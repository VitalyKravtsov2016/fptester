program fptester;

uses
  SysUtils,
  DriverTester in 'Units\DriverTester.pas',
  DrvFRLib_TLB in 'Units\DrvFRLib_TLB.pas',
  FileUtils in 'Units\FileUtils.pas',
  ReceiptTest in 'Units\ReceiptTest.pas',
  DriverTest in 'Units\DriverTest.pas',
  CommandTest in 'Units\CommandTest.pas',
  CashRegistersTest in 'Units\CashRegistersTest.pas',
  FptrTypes in 'Units\FptrTypes.pas',
  BinUtils in 'Units\BinUtils.pas',
  gnugettext in 'Units\gnugettext.pas';

{$R *.RES}

var
  Tester: TDriverTester;
  Options: TTesterOptions;
begin
  Tester := TDriverTester.Create;
  try
    Options.Verbose := FindCmdLineSwitch('VERBOSE', ['-', '/'], False);
    Options.FilesPath := GetModulePath + 'tests\';
    Tester.Options := Options;
    Tester.Run;
  finally
    Tester.Free;
  end;
end.


