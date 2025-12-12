unit DriverTester;

interface

uses
  // VCL
  Windows, Classes, SysUtils, ComObj, ActiveX,
  // 3'd
  VSoft.YAML,
  // This
  DrvFRLib_TLB, FileUtils;

(*****************************************************************************

name: "Команда 10h"

tests:
  - name: "Нормальный запрос"
    command: "10 1E 00 00 00"
    response: "10 00"

  - name: "Неверная длина команды"
    command: "10"
    response: "10 00"

*****************************************************************************)

const
  FPTR_MODE_DUMP   			  = $01; // Dump mode
  FPTR_MODE_24NOTOVER		  = $02; // Opened shift, 24 hours not left
  FPTR_MODE_24OVER			  = $03; // Opened shift, 24 hours is over
  FPTR_MODE_CLOSED			  = $04; // Closed shift
  FPTR_MODE_LOCKED			  = $05; // ECR is bloced because of incorrect tax offecer password
  FPTR_MODE_WAITDATE			= $06; // Waiting for date confirm
  FPTR_MODE_POINTPOS			= $07; // Change decimal point position permission
  FPTR_MODE_REC		     	  = $08; // Opened document
  FPTR_MODE_TECH		     	= $09; // Technological reset permission
  FPTR_MODE_TEST			    = $0A; // Test run
  FPTR_MODE_FULLREPORT		= $0B; // Full fiscal report printing
  FPTR_MODE_EKLZREPORT		= $0C; // EJ report printing
  FPTR_MODE_SLP		     	  = $0D; // Opened fiscal slip
  FPTR_MODE_SLPPRINT			= $0E; // Slip printing
  FPTR_MODE_SLPREADY			= $0F; // Fiscal slip is ready

type
  { TTesterOptions }

  TTesterOptions = record
    FilesPath: string;
    Verbose: Boolean;
  end;

  { TDriverTest }

  TDriverTest = record
    name: string;
    command: string;
    response: string;
    resultcode: Integer;
    testtype: string;
    ecrmode_before: Integer;
    ecrmode_after: Integer;
    baudrate_after: Integer;
  end;
  TDriverTests = array of TDriverTest;

  { TDriverTestSuite }

  TDriverTestSuite = record
    name: string;
    tests: TDriverTests;
  end;

  { TTestReport }

  TTestReport = record
    StartTime: TDateTime;
    SuccessCount: Integer;
    TotalCount: Integer;
  end;

  { TTestResult }

  TTestResult = record
    IsSucceeded: Boolean;
    Text: string;
  end;

  { TDriverTester }

  TDriverTester = class
  private
    FDriver: TDrvFR;
    FReport: TTestReport;
    FOptions: TTesterOptions;
    FConsoleInfo: TConsoleScreenBufferInfo;
    function GetDriver: TDrvFR;
    property Driver: TDrvFR read GetDriver;
  public
    procedure ShowVersion;
    procedure ExecuteTestFiles;
    procedure SetErrorColor;
    procedure SetNormalColor;
  private
    procedure TestConnection;
  private
    procedure Check(ResultCode: Integer);
    procedure ExecuteTestFile(const FileName: string);
    procedure ExecuteTestSuite(TestSuite: TDriverTestSuite);
    function ExecuteTest(var Test: TDriverTest): TTestResult;
    procedure WriteReport;
    function ExecuteCommandTest(var Test: TDriverTest): TTestResult;
    procedure ResetEcr;
    procedure SetEcrMode(ecrmode: Integer);
    procedure CheckEcrMode(EcrMode: Integer);
  public
    procedure Run;
  end;

implementation

const
  Separator = '--------------------------------------------------------';

function GetFileNames(FileNames: TStrings; Directory, FileMask: string): Integer;
var
  Status: Integer;
  SearchRec: TSearchRec;
begin
  FileNames.BeginUpdate;
  try
    FileNames.Clear;
    Status := FindFirst(Directory+FileMask, faAnyFile, SearchRec);
    while Status = 0 do
    begin
      if (SearchRec.Attr and faDirectory) = 0 then
        FileNames.Add(Directory + SearchRec.Name);
      Status := FindNext(SearchRec);
    end;
    FindClose(SearchRec);
  finally
    FileNames.EndUpdate;
  end;
  Result := FileNames.Count;
end;

//////////////////////////////////////////////////////////////////////////////
//
// В expected можно символом X указать байты, которые не проверять

function CompareResponse(Response, Expected: string): Boolean;
var
  i: Integer;
begin
  Response := Trim(Response);
  Expected := Trim(Expected);
  Result := Length(Expected) <= Length(Response);
  if not Result then Exit;

  for i := 1 to Length(Expected) do
  begin
    if Expected[i] <> 'X' then
    begin
      Result := Response[i] = Expected[i];
      if not Result then Exit;
    end;
  end;
end;


{ TDriverTester }

function TDriverTester.GetDriver: TDrvFR;
begin
  if FDriver = nil then
  begin
    OleCheck(CoInitialize(nil));
    FDriver := TDrvFR.Create(nil);
  end;
  Result := FDriver;
end;

procedure TDriverTester.Run;
begin
  //FOptions.Verbose := True;
  GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE), FConsoleInfo);
  FOptions.FilesPath := GetModulePath + 'tests\';
  FReport.TotalCount := 0;
  FReport.SuccessCount := 0;
  FReport.StartTime := Now;
  

  ShowVersion;
  try
    TestConnection;
    ExecuteTestFiles;
    WriteReport;
  except
    on E: Exception do
    begin
      SetErrorColor;
      WriteLn('ERROR: ' + E.Message);
    end;
  end;
  WriteLn('Для завершения нажмите любую клавишу');
  ReadLn;
end;

procedure TDriverTester.Check(ResultCode: Integer);
begin
  if ResultCode <> 0 then
    raise Exception.CreateFmt('%d, %s',
    [ResultCode, Driver.ResultCodeDescription]);
end;

procedure TDriverTester.TestConnection;
begin
  Check(Driver.Connect);
  Check(Driver.GetECRStatus);
  Check(Driver.GetDeviceMetrics);

  WriteLn(Separator);
  WriteLn('Успешно подключен к ФР');
  WriteLn(Format('Модель ФР : %d, %s', [Driver.UModel, Driver.UDescription]));
  WriteLn(Format('Режим ФР  : %d, %s', [Driver.ECRMode, Driver.ECRModeDescription]));
  WriteLn(Format('Версия ПО : %s, %d, %s', [Driver.ECRSoftVersion, Driver.ECRBuild, DateToStr(Driver.ECRSoftDate)]));
  WriteLn(Separator);
end;

procedure TDriverTester.SetErrorColor;
begin
  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE),
    FOREGROUND_RED or FOREGROUND_INTENSITY);
end;

procedure TDriverTester.SetNormalColor;
begin
  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE),
    FConsoleInfo.wAttributes);
end;

procedure TDriverTester.ShowVersion;
begin
  WriteLn('Fiscal printer tester 1.0, 2025');
end;

procedure TDriverTester.ExecuteTestFiles;
var
  i: Integer;
  Files: TStrings;
const
  FileMask = '\*.yaml';
begin
  Files := TStringList.Create;
  try
    GetFileNames(Files, FOptions.FilesPath, FileMask);
    WriteLn(Format('Найдено файлов тестов: %d', [Files.Count]));
    for i := 0 to Files.Count-1 do
    begin
      ExecuteTestFile(Files[i]);
    end;
  finally
    Files.Free;
  end;
end;

procedure TDriverTester.ExecuteTestFile(const FileName: string);
var
  S: string;
  i: Integer;
  test: IYAMLValue;
  tests: IYAMLValue;
  value : IYAMLValue;
  Sequence: IYAMLSequence;
  doc: IYAMLDocument;
  TestSuite: TDriverTestSuite;
begin
  doc := TYAML.LoadFromFile(FileName);

  TestSuite.name := doc.Root.GetValue('name').AsString;
  tests := doc.Root.GetValue('tests');
  if (not tests.IsNull)and(tests.IsSequence) then
  begin
    Sequence := tests.AsSequence;
    if Sequence.Count > 0 then
    begin
      SetLength(TestSuite.tests, Sequence.Count);
      for i := 0 to Sequence.Count-1 do
      begin
        test := Sequence.Items[i];
        TestSuite.tests[i].name := test.GetValue('name').AsString;
        TestSuite.tests[i].command := test.GetValue('command').AsString;
        TestSuite.tests[i].response := test.GetValue('response').AsString;

        TestSuite.tests[i].testtype := 'command';
        if test.TryGetValue('testtype', value) then
          TestSuite.tests[i].testtype := value.AsString;

        TestSuite.tests[i].ecrmode_before := 0;
        if test.TryGetValue('ecrmode_before', value) then
          TestSuite.tests[i].ecrmode_before := value.AsInteger;

        TestSuite.tests[i].ecrmode_after := 0;
        if test.TryGetValue('ecrmode_after', value) then
          TestSuite.tests[i].ecrmode_after := value.AsInteger;

        TestSuite.tests[i].baudrate_after := 6;
        if test.TryGetValue('baudrate_after', value) then
          TestSuite.tests[i].baudrate_after := value.AsInteger;

        TestSuite.tests[i].resultcode := 0;
        if test.TryGetValue('resultcode', value) then
          TestSuite.tests[i].resultcode := value.AsInteger;
      end;
    end;
  end;
  ExecuteTestSuite(TestSuite);
end;

procedure TDriverTester.ExecuteTestSuite(TestSuite: TDriverTestSuite);
var
  i: Integer;
  Text: string;
  Count: Integer;
  Test: TDriverTest;
  Result: TTestResult;
begin
  WriteLn(Separator);
  WriteLn(TestSuite.name);
  Count := Length(TestSuite.tests);
  for i := 0 to Count-1 do
  begin
    Test := TestSuite.tests[i];
    Result := ExecuteTest(Test);
    if Result.IsSucceeded then
    begin
      SetNormalColor;
      Inc(FReport.SuccessCount);
      WriteLn('[+] ' + Test.name);
    end else
    begin
      SetErrorColor;
      WriteLn('[-] ' + Test.name + ', ' + Result.Text);
      SetNormalColor;
    end;
    Inc(FReport.TotalCount);
  end;
end;

function TDriverTester.ExecuteTest(var Test: TDriverTest): TTestResult;
begin
  ResetEcr;

  if Test.TestType = 'command' then
  begin
    Result := ExecuteCommandTest(Test);
    Exit;
  end;
  Result.IsSucceeded := False;
  Result.Text := 'Тип теста не поддерживается';
end;

procedure TDriverTester.ResetEcr;
const
  RepCount = 10;
var
  i: Integer;
  Password: Integer;
begin
  for i := 0 to RepCount - 1 do
  begin
    Check(Driver.WaitForPrinting);
    case Driver.ECRMode of
      FPTR_MODE_DUMP:
      begin
        Password := Driver.Password;
        Driver.Password := 0;
        Check(Driver.InterruptDataStream);
        Driver.Password := Password;
      end;
      FPTR_MODE_24NOTOVER,
      FPTR_MODE_24OVER: Check(Driver.PrintReportWithCleaning);
      FPTR_MODE_WAITDATE: Check(Driver.ConfirmDate);
      FPTR_MODE_REC: Check(Driver.CancelCheck);
      FPTR_MODE_TEST: Check(Driver.InterruptTest);
      FPTR_MODE_FULLREPORT,
      FPTR_MODE_EKLZREPORT,
      FPTR_MODE_SLPPRINT: ;
    else
      Exit;
    end;
  end;
end;

procedure TDriverTester.SetEcrMode(ecrmode: Integer);
var
  Password: Integer;
begin
  case ecrmode of
    FPTR_MODE_DUMP:
    begin
      Password := Driver.Password;
      Driver.Password := 0;
      Driver.DeviceCode := 6;
      Driver.DampRequest;
      Driver.Password := Password;
      Check(Driver.ResultCode);
      Check(Driver.WaitForPrinting);
      CheckEcrMode(FPTR_MODE_DUMP);
    end;
    FPTR_MODE_CLOSED:
    begin
      CheckEcrMode(FPTR_MODE_CLOSED);
    end;
    FPTR_MODE_24NOTOVER:
    begin
      Check(Driver.OpenSession);
      Check(Driver.WaitForPrinting);
      CheckEcrMode(FPTR_MODE_24NOTOVER);
    end;
    FPTR_MODE_TEST: 
    begin
      Check(Driver.Test);
      Check(Driver.WaitForPrinting);
      CheckEcrMode(FPTR_MODE_TEST);
    end;
    FPTR_MODE_WAITDATE:
    begin
      Check(Driver.GetECRStatus);
      Check(Driver.SetDate);
      Check(Driver.WaitForPrinting);
      CheckEcrMode(FPTR_MODE_WAITDATE);
    end;
    FPTR_MODE_REC:
    begin
      Check(Driver.OpenCheck);
      Check(Driver.WaitForPrinting);
      CheckEcrMode(FPTR_MODE_REC);
    end;
    FPTR_MODE_TECH,
    FPTR_MODE_24OVER,
    FPTR_MODE_POINTPOS,
    FPTR_MODE_FULLREPORT,
    FPTR_MODE_EKLZREPORT,
    FPTR_MODE_SLP,
    FPTR_MODE_SLPPRINT,
    FPTR_MODE_SLPREADY:
    begin
      raise Exception.CreateFmt('Нельзя перевести ФР в состояние, %d', [ecrmode]);
    end;
  end;
end;

procedure TDriverTester.CheckEcrMode(EcrMode: Integer);
begin
  if EcrMode <> Driver.EcrMode then
    raise Exception.CreateFmt('Другое состояние ФР, %d <> %d', [
    EcrMode, Driver.EcrMode]);
end;

function TDriverTester.ExecuteCommandTest(var Test: TDriverTest): TTestResult;
var
  ResultCode: Integer;
begin
  Result.IsSucceeded := False;

  try
    if Test.ecrmode_before <> 0 then
    begin
      SetEcrMode(Test.ecrmode_before);
    end;

    if FOptions.Verbose then
    begin
      WriteLn('-> ' + Test.Command);
    end;
    Driver.BinaryConversion := BINARY_CONVERSION_HEX;
    Driver.TransferBytes := Test.Command;
    ResultCode := Driver.ExchangeBytes;
    if Test.ResultCode = 0 then
    begin
      Check(ResultCode);
      if FOptions.Verbose then
      begin
        WriteLn('<- ' + Driver.TransferBytes);
      end;

      Result.IsSucceeded := CompareResponse(Driver.TransferBytes, Test.response);
      if (not Result.IsSucceeded)and FOptions.Verbose then
      begin
        SetErrorColor;
        Result.Text := 'ответы не совпадают';
        WriteLn('Ошибка: ответы не совпадают');
        WriteLn(Format('Ожидается ответ : "%s"', [Trim(Test.response)]));
        WriteLn(Format('Получен ответ   : "%s"', [Trim(Driver.TransferBytes)]));
        SetNormalColor;
        Exit;
      end;

      if Test.baudrate_after <> 0 then
      begin
        Driver.Disconnect;
        if (Driver.ConnectionType = 0) then
        begin
          Driver.BaudRate := Test.baudrate_after;
          Check(Driver.GetShortECRStatus);
          Driver.SaveParams;
        end;
      end;

      if Test.ecrmode_after <> 0 then
      begin
        Check(Driver.GetShortECRStatus);
        Result.IsSucceeded := Test.ecrmode_after = Driver.ECRMode;
        if not Result.IsSucceeded then
        begin
          Result.Text := 'не совпадает состояние';
          if FOptions.Verbose then
          begin
            SetErrorColor;
            WriteLn('Ошибка: не совпадает состояние');
            WriteLn(Format('Ожидается состояние : %d.', [Test.ecrmode_after]));
            WriteLn(Format('Получено состояние  : %d.', [Driver.ECRMode]));
            SetNormalColor;
          end;
          Exit;
        end;
      end;
      Result.IsSucceeded := True;
    end else
    begin
      if ResultCode = Test.ResultCode then
      begin
        Result.IsSucceeded := True;
      end else
      begin
        Result.Text := 'код ошибки не совпадает';
        if FOptions.Verbose then
        begin
          SetErrorColor;
          WriteLn('Ошибка: код отшибки не совпадает');
          WriteLn(Format('Ожидается код ошибки: %d.', [Test.ResultCode]));
          WriteLn(Format('Получен код ошибки: %d.', [ResultCode]));
          SetNormalColor;
        end;
      end;
    end;
  except
    on E: Exception do
    begin
      Result.Text := E.Message;
      if FOptions.Verbose then
      begin
        WriteLn('Ошибка: ' + E.Message);
      end;
    end;
  end;
end;

procedure TDriverTester.WriteReport;
var
  SuccessPercents: Integer;
begin
  WriteLn(Separator);
  SuccessPercents := 0;
  if FReport.TotalCount > 0 then
    SuccessPercents := Round(FReport.SuccessCount *100 / FReport.TotalCount);

  WriteLn(Format('Успешно выполнено: %d из %d, %d %%', [
    FReport.SuccessCount, FReport.TotalCount, SuccessPercents]));

  WriteLn(FormatDateTime('Время выполнения: hh:mm:ss', Now-FReport.StartTime));
end;

end.
