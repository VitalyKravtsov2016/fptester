program fptester;

uses
  SysUtils,
  Classes,
  Windows,
  DriverTester in 'Units\DriverTester.pas',
  DrvFRLib_TLB in 'Units\DrvFRLib_TLB.pas',
  FileUtils in 'Units\FileUtils.pas',
  ComPortPair in 'Units\ComPortPair.pas',
  FrEmuPrinter in 'Units\FrEmuPrinter.pas',
  ReceiptTest in 'Units\ReceiptTest.pas',
  DriverTest in 'Units\DriverTest.pas',
  CommandTest in 'Units\CommandTest.pas',
  CashRegistersTest in 'Units\CashRegistersTest.pas',
  TableTest in 'Units\TableTest.pas',
  FptrTypes in 'Units\FptrTypes.pas',
  BinUtils in 'Units\BinUtils.pas',
  gnugettext in 'Units\gnugettext.pas',
  LogFile in 'Units\LogFile.pas',
  OFDAPI in 'Units\OFDAPI.pas';

{$R *.RES}

var
  Tester: TDriverTester;
  Options: TTesterOptions;

procedure ParseOptions(var Options: TTesterOptions);
var
  i: Integer;
  S, Key, Val: string;
  P: Integer;
begin
  Options := Default(TTesterOptions);
  Options.Verbose := FindCmdLineSwitch('VERBOSE', ['-', '/'], True);
  Options.UseEmulator := FindCmdLineSwitch('EMU', ['-', '/'], True);
  Options.FilesPath := GetModulePath + 'tests\';
  Options.EmuComPort := '';
  Options.DriverComNumber := 0;
  Options.FlashPath := '';
  Options.TestsPathSpecified := False;
  for i := 1 to ParamCount do
  begin
    S := ParamStr(i);
    if (Length(S) > 0) and ((S[1] = '-') or (S[1] = '/')) then
      Delete(S, 1, 1);
    P := Pos(':', S);
    if P > 0 then
    begin
      Key := UpperCase(Copy(S, 1, P - 1));
      Val := Copy(S, P + 1, MaxInt);
    end else
    begin
      Key := UpperCase(S);
      Val := '';
    end;
    if Key = 'FLASH' then
      Options.FlashPath := Val
    else if Key = 'COM' then
      Options.DriverComNumber := StrToIntDef(Val, 0)
    else if Key = 'EMUCOM' then
      Options.EmuComPort := Val
    else if Key = 'TESTS' then
    begin
      Options.FilesPath := IncludeTrailingPathDelimiter(Val);
      Options.TestsPathSpecified := True;
    end;
  end;
end;

function NextLogRunNumber(const LogDir: string): Integer;
var
  SR: TSearchRec;
  Count: Integer;
begin
  Count := 0;
  if DirectoryExists(LogDir) and
    (FindFirst(LogDir + 'fptester-*.log', faAnyFile, SR) = 0) then
  try
    repeat
      Inc(Count);
    until FindNext(SR) <> 0;
  finally
    SysUtils.FindClose(SR);
  end;
  Result := Count + 1;
end;

function MakeLogFileName: string;
var
  LogDir: string;
  RunNo: Integer;
begin
  LogDir := GetModulePath + 'logs\';
  ForceDirectories(LogDir);
  RunNo := NextLogRunNumber(LogDir);
  Result := Format('%sfptester-%s_run%.3d.log',
    [LogDir, FormatDateTime('yyyy-mm-dd_hh-nn-ss', Now), RunNo]);
end;

begin
  Logger.FileName := MakeLogFileName;
  Logger.WriteConsole := True;
  Logger.Enabled := True;
  WriteLn('Log: ' + Logger.FileName);

  if FindCmdLineSwitch('?', ['-', '/'], True) or
     FindCmdLineSwitch('HELP', ['-', '/'], True) then
  begin
    WriteLn('fptester [options]');
    WriteLn('  (no flags)     real fiscal printer, last saved driver params');
    WriteLn('  -EMU           QEMU emulator via FrEmu.Printer');
    WriteLn('  -COM:N         driver COM number');
    WriteLn('  -EMUCOM:COMx   emulator UART COM');
    WriteLn('                  For -EMU specify both ports or neither.');
    WriteLn('                  If omitted, a com0com pair is created and saved next to the exe');
    WriteLn('  -FLASH:path    firmware image for the emulator');
    WriteLn('  -TESTS:path    YAML tests directory');
    WriteLn('  -VERBOSE       verbose log');
    WriteLn('  Log file:      Bin\logs\fptester-YYYY-MM-DD_HH-MM-SS_runNNN.log');
    Exit;
  end;

  Tester := TDriverTester.Create;
  try
    ParseOptions(Options);
    if Options.UseEmulator then
      ResolveEmulatorComPair(Options.EmuComPort, Options.DriverComNumber);
    Tester.Options := Options;
    Tester.Run;

    Tester.Context.SetNormalColor;
    WriteLn('Press Enter to exit');
    ReadLn;
  finally
    Tester.Free;
  end;
end.


