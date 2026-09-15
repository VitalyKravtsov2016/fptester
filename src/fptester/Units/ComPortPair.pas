unit ComPortPair;

interface

function NormalizeComPortName(const S: string): string;
function ComPortToNumber(const S: string): Integer;
function ComPortExists(const PortName: string): Boolean;
function ComPortPairIniFile: string;
procedure LoadSavedComPair(out EmuPort: string; out DriverNumber: Integer);
procedure SaveComPair(const EmuPort: string; DriverNumber: Integer);
{ If both ports are already set, they are saved and used as-is.
  If neither is set, the pair from the ini is reused when both COM
  devices still exist, numbers are > 16, and loopback works;
  otherwise a new com0com pair (COM17+) is created and checked. }
procedure ResolveEmulatorComPair(var EmuPort: string; var DriverNumber: Integer);

implementation

uses
  Windows, SysUtils, Classes, Registry, IniFiles,
  FileUtils;

const
  IniSection = 'Emulator';
  IniEmuPort = 'EmuComPort';
  IniDriverCom = 'DriverComNumber';
  SetupcTimeoutMs = 120000;
  MinComNumber = 17; { COM1..COM16 считаем занятыми физическими портами }
  MaxComNumber = 256;

function NormalizeComPortName(const S: string): string;
var
  T: string;
  N: Integer;
begin
  T := UpperCase(Trim(S));
  if T = '' then
  begin
    Result := '';
    Exit;
  end;
  if Copy(T, 1, 3) = 'COM' then
    T := Copy(T, 4, MaxInt);
  N := StrToIntDef(T, 0);
  if N <= 0 then
    Result := ''
  else
    Result := 'COM' + IntToStr(N);
end;

function ComPortToNumber(const S: string): Integer;
var
  Name: string;
begin
  Name := NormalizeComPortName(S);
  if Name = '' then
    Result := 0
  else
    Result := StrToIntDef(Copy(Name, 4, MaxInt), 0);
end;

function ComPortExists(const PortName: string): Boolean;
var
  Name: string;
  Buf: array[0..511] of Char;
begin
  Name := NormalizeComPortName(PortName);
  Result := (Name <> '') and (QueryDosDevice(PChar(Name), Buf, Length(Buf)) <> 0);
end;

function ComPortPairIniFile: string;
begin
  Result := ChangeFileExt(FileUtils.GetModuleFileName, '.ini');
end;

procedure LoadSavedComPair(out EmuPort: string; out DriverNumber: Integer);
var
  Ini: TIniFile;
begin
  EmuPort := '';
  DriverNumber := 0;
  if not FileExists(ComPortPairIniFile) then
    Exit;
  Ini := TIniFile.Create(ComPortPairIniFile);
  try
    EmuPort := NormalizeComPortName(Ini.ReadString(IniSection, IniEmuPort, ''));
    DriverNumber := Ini.ReadInteger(IniSection, IniDriverCom, 0);
  finally
    Ini.Free;
  end;
end;

procedure SaveComPair(const EmuPort: string; DriverNumber: Integer);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(ComPortPairIniFile);
  try
    Ini.WriteString(IniSection, IniEmuPort, NormalizeComPortName(EmuPort));
    Ini.WriteInteger(IniSection, IniDriverCom, DriverNumber);
  finally
    Ini.Free;
  end;
end;

function FileIfExists(const Path: string): string;
begin
  if (Path <> '') and FileExists(Path) then
    Result := Path
  else
    Result := '';
end;

function SetupcInDir(const Dir: string): string;
begin
  Result := FileIfExists(IncludeTrailingPathDelimiter(Dir) + 'setupc.exe');
end;

function UnprefixNtPath(const S: string): string;
begin
  Result := S;
  if Copy(Result, 1, 4) = '\??\' then
    Delete(Result, 1, 4);
  if Copy(Result, 1, 4) = '\\?\' then
    Delete(Result, 1, 4);
end;

function FindSetupcFromService: string;
var
  R: TRegistry;
  Img, Dir: string;
begin
  Result := '';
  R := TRegistry.Create(KEY_READ);
  try
    R.RootKey := HKEY_LOCAL_MACHINE;
    if not R.OpenKeyReadOnly('SYSTEM\CurrentControlSet\Services\com0com') then
      Exit;
    if R.ValueExists('ImagePath') then
      Img := UnprefixNtPath(R.ReadString('ImagePath'));
  finally
    R.Free;
  end;
  if Img = '' then
    Exit;
  Dir := ExcludeTrailingPathDelimiter(ExtractFilePath(Img));
  Result := SetupcInDir(Dir);
  if Result = '' then
    Result := SetupcInDir(ExtractFilePath(Dir));
end;

function FindSetupcFromPath: string;
var
  Buf: array[0..MAX_PATH] of Char;
  FilePart: PChar;
begin
  FillChar(Buf, SizeOf(Buf), 0);
  if SearchPath(nil, 'setupc.exe', nil, MAX_PATH, Buf, FilePart) = 0 then
    Result := ''
  else
    Result := Buf;
end;

function FindSetupc: string;
begin
  Result := SetupcInDir(GetEnvironmentVariable('ProgramFiles(x86)') + '\com0com');
  if Result = '' then
    Result := SetupcInDir(GetEnvironmentVariable('ProgramFiles') + '\com0com');
  if Result = '' then
    Result := SetupcInDir('C:\Program Files (x86)\com0com');
  if Result = '' then
    Result := SetupcInDir('C:\Program Files\com0com');
  if Result = '' then
    Result := FindSetupcFromService;
  if Result = '' then
    Result := FindSetupcFromPath;
end;

function ReadTextFile(const FileName: string): string;
var
  Lines: TStringList;
begin
  Result := '';
  if not FileExists(FileName) then
    Exit;
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(FileName);
    Result := Lines.Text;
  finally
    Lines.Free;
  end;
end;

function RunSetupc(const Args: string; out Output: string): DWORD;
var
  Exe, WorkDir, OutFile, Cmd: string;
  SA: TSecurityAttributes;
  SI: TStartupInfo;
  PI: TProcessInformation;
  HOut: THandle;
  Wait: DWORD;
begin
  Exe := FindSetupc;
  if Exe = '' then
    raise Exception.Create(
      'Не найден setupc.exe (com0com). Установите com0com или задайте порты: -COM:N -EMUCOM:COMx');

  WorkDir := ExcludeTrailingPathDelimiter(ExtractFilePath(Exe));
  OutFile := GetModulePath + Format('setupc-%d.txt', [GetCurrentProcessId]);
  DeleteFile(OutFile);

  FillChar(SA, SizeOf(SA), 0);
  SA.nLength := SizeOf(SA);
  SA.bInheritHandle := True;
  HOut := CreateFile(PChar(OutFile), GENERIC_WRITE, FILE_SHARE_READ, @SA,
    CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
  if HOut = INVALID_HANDLE_VALUE then
    RaiseLastOSError;

  FillChar(SI, SizeOf(SI), 0);
  SI.cb := SizeOf(SI);
  SI.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
  SI.wShowWindow := SW_HIDE;
  SI.hStdInput := GetStdHandle(STD_INPUT_HANDLE);
  SI.hStdOutput := HOut;
  SI.hStdError := HOut;

  Cmd := Format('"%s" %s', [Exe, Args]);
  UniqueString(Cmd);
  FillChar(PI, SizeOf(PI), 0);
  try
    if not CreateProcess(PChar(Exe), PChar(Cmd), nil, nil, True, CREATE_NO_WINDOW,
      nil, PChar(WorkDir), SI, PI) then
      RaiseLastOSError;
  finally
    CloseHandle(HOut);
  end;

  try
    Wait := WaitForSingleObject(PI.hProcess, SetupcTimeoutMs);
    if Wait = WAIT_TIMEOUT then
    begin
      TerminateProcess(PI.hProcess, 1);
      raise Exception.Create('Истекло время ожидания setupc.exe (com0com)');
    end;
    if not GetExitCodeProcess(PI.hProcess, Result) then
      Result := 1;
  finally
    CloseHandle(PI.hThread);
    CloseHandle(PI.hProcess);
  end;

  Output := ReadTextFile(OutFile);
  DeleteFile(OutFile);
end;

function LoadBusyComNames: TStringList;
var
  Output: string;
  Lines: TStringList;
  i: Integer;
  S: string;
begin
  Result := TStringList.Create;
  Result.CaseSensitive := False;
  Result.Sorted := True;
  Result.Duplicates := dupIgnore;
  RunSetupc('busynames COM?*', Output);
  Lines := TStringList.Create;
  try
    Lines.Text := Output;
    for i := 0 to Lines.Count - 1 do
    begin
      S := NormalizeComPortName(Trim(Lines[i]));
      if S <> '' then
        Result.Add(S);
    end;
  finally
    Lines.Free;
  end;
end;

function FindFreeComNumber(StartFrom: Integer; Busy: TStrings): Integer;
var
  Name: string;
begin
  Result := StartFrom;
  if Result < MinComNumber then
    Result := MinComNumber;
  while Result <= MaxComNumber do
  begin
    Name := 'COM' + IntToStr(Result);
    if (Busy.IndexOf(Name) < 0) and not ComPortExists(Name) then
      Exit;
    Inc(Result);
  end;
  raise Exception.Create('Нет свободных номеров COM-портов');
end;

function ExtractLastCncaIndex(const Text: string): Integer;
var
  i, P, V: Integer;
  S: string;
begin
  Result := -1;
  S := UpperCase(Text);
  P := 1;
  while P <= Length(S) do
  begin
    i := Pos('CNCA', Copy(S, P, MaxInt));
    if i = 0 then
      Break;
    i := P + i - 1 + 4;
    if (i > Length(S)) or not CharInSet(S[i], ['0'..'9']) then
    begin
      P := i;
      Continue;
    end;
    V := 0;
    while (i <= Length(S)) and CharInSet(S[i], ['0'..'9']) do
    begin
      V := V * 10 + (Ord(S[i]) - Ord('0'));
      Inc(i);
    end;
    Result := V;
    P := i;
  end;
end;

function OpenComHandle(const PortName: string): THandle;
var
  Path: string;
  DCB: TDCB;
  Timeouts: TCommTimeouts;
begin
  Path := '\\.\' + NormalizeComPortName(PortName);
  Result := Windows.CreateFile(PChar(Path), GENERIC_READ or GENERIC_WRITE, 0, nil,
    OPEN_EXISTING, 0, 0);
  if Result = INVALID_HANDLE_VALUE then
    Exit;
  FillChar(DCB, SizeOf(DCB), 0);
  DCB.DCBlength := SizeOf(DCB);
  if not BuildCommDCB('baud=115200 parity=N data=8 stop=1', DCB) then
  begin
    CloseHandle(Result);
    Result := INVALID_HANDLE_VALUE;
    Exit;
  end;
  if not SetCommState(Result, DCB) then
  begin
    CloseHandle(Result);
    Result := INVALID_HANDLE_VALUE;
    Exit;
  end;
  FillChar(Timeouts, SizeOf(Timeouts), 0);
  Timeouts.ReadIntervalTimeout := 50;
  Timeouts.ReadTotalTimeoutMultiplier := 0;
  Timeouts.ReadTotalTimeoutConstant := 1500;
  Timeouts.WriteTotalTimeoutMultiplier := 0;
  Timeouts.WriteTotalTimeoutConstant := 1500;
  SetCommTimeouts(Result, Timeouts);
  PurgeComm(Result, PURGE_RXCLEAR or PURGE_TXCLEAR or PURGE_RXABORT or PURGE_TXABORT);
end;

function ComWriteAll(H: THandle; const Data: AnsiString): Boolean;
var
  Written: DWORD;
begin
  Result := False;
  if (H = INVALID_HANDLE_VALUE) or (Data = '') then
    Exit;
  Result := WriteFile(H, Data[1], Length(Data), Written, nil) and
    (Written = DWORD(Length(Data)));
  if Result then
    FlushFileBuffers(H);
end;

function ComReadAll(H: THandle; Count: Integer): AnsiString;
var
  Got: DWORD;
begin
  Result := '';
  if (H = INVALID_HANDLE_VALUE) or (Count <= 0) then
    Exit;
  SetLength(Result, Count);
  if not ReadFile(H, Result[1], Count, Got, nil) then
  begin
    Result := '';
    Exit;
  end;
  SetLength(Result, Got);
end;

function ComTransfer(Src, Dst: THandle; const Data: AnsiString): Boolean;
begin
  PurgeComm(Dst, PURGE_RXCLEAR);
  Result := ComWriteAll(Src, Data) and (ComReadAll(Dst, Length(Data)) = Data);
end;

function ComPairLoopbackOk(const PortA, PortB: string): Boolean;
var
  A, B: THandle;
  Marker: AnsiString;
begin
  Result := False;
  A := OpenComHandle(PortA);
  if A = INVALID_HANDLE_VALUE then
    Exit;
  B := OpenComHandle(PortB);
  if B = INVALID_HANDLE_VALUE then
  begin
    CloseHandle(A);
    Exit;
  end;
  try
    Sleep(200);
    Marker := AnsiString('FPTESTER') + AnsiString(IntToHex(Int64(GetTickCount), 8));
    Result := ComTransfer(A, B, Marker) and
      ComTransfer(B, A, Marker + AnsiString('-BA'));
  finally
    CloseHandle(B);
    CloseHandle(A);
  end;
end;

function SavedPairUsable(const EmuPort: string; DriverNumber: Integer): Boolean;
var
  DriverPort: string;
begin
  Result := False;
  if (EmuPort = '') or (DriverNumber < MinComNumber) then
    Exit;
  if ComPortToNumber(EmuPort) < MinComNumber then
    Exit;
  DriverPort := 'COM' + IntToStr(DriverNumber);
  Result := ComPortExists(EmuPort) and ComPortExists(DriverPort) and
    ComPairLoopbackOk(EmuPort, DriverPort);
end;

function WaitForComPort(const PortName: string; TimeoutMs: Integer): Boolean;
var
  Left: Integer;
begin
  Left := TimeoutMs;
  while Left > 0 do
  begin
    if ComPortExists(PortName) then
    begin
      Result := True;
      Exit;
    end;
    Sleep(200);
    Dec(Left, 200);
  end;
  Result := ComPortExists(PortName);
end;

procedure CreateCom0ComPair(out EmuPort: string; out DriverNumber: Integer);
var
  A, B, Attempt, StartFrom, PairIndex: Integer;
  Output: string;
  ExitCode: DWORD;
  DriverPort: string;
  Busy: TStringList;
begin
  Busy := LoadBusyComNames;
  try
    StartFrom := MinComNumber;
    for Attempt := 1 to 20 do
    begin
      A := FindFreeComNumber(StartFrom, Busy);
      B := FindFreeComNumber(A + 1, Busy);
      ExitCode := RunSetupc(Format(
        'install PortName=COM%d,EmuBR=yes PortName=COM%d,EmuBR=yes', [A, B]), Output);
      EmuPort := 'COM' + IntToStr(A);
      DriverPort := 'COM' + IntToStr(B);
      if WaitForComPort(EmuPort, 10000) and WaitForComPort(DriverPort, 10000) and
        ComPairLoopbackOk(EmuPort, DriverPort) then
      begin
        DriverNumber := B;
        Exit;
      end;
      PairIndex := ExtractLastCncaIndex(Output);
      if PairIndex >= 0 then
        RunSetupc(Format('remove %d', [PairIndex]), Output);
      if (ExitCode <> 0) and (PairIndex < 0) then
        raise Exception.CreateFmt(
          'Не удалось создать пару COM-портов (setupc, код %d). ' +
          'Запустите программу от имени администратора.'#13#10'%s',
          [ExitCode, Trim(Output)]);
      Busy.Add(EmuPort);
      Busy.Add(DriverPort);
      StartFrom := B + 1;
    end;
    raise Exception.CreateFmt(
      'Не удалось создать рабочую пару COM-портов через com0com.'#13#10'%s',
      [Trim(Output)]);
  finally
    Busy.Free;
  end;
end;

procedure ResolveEmulatorComPair(var EmuPort: string; var DriverNumber: Integer);
var
  SavedEmu: string;
  SavedDrv: Integer;
  HasEmu, HasDrv: Boolean;
begin
  EmuPort := NormalizeComPortName(EmuPort);
  HasEmu := EmuPort <> '';
  HasDrv := DriverNumber > 0;
  if HasEmu xor HasDrv then
    raise Exception.Create(
      'Для эмулятора задайте оба порта (-COM и -EMUCOM) или не задавайте ни одного');

  if HasEmu and HasDrv then
  begin
    SaveComPair(EmuPort, DriverNumber);
    Exit;
  end;

  LoadSavedComPair(SavedEmu, SavedDrv);
  if SavedPairUsable(SavedEmu, SavedDrv) then
  begin
    EmuPort := SavedEmu;
    DriverNumber := SavedDrv;
    Exit;
  end;

  CreateCom0ComPair(EmuPort, DriverNumber);
  SaveComPair(EmuPort, DriverNumber);
end;

end.
