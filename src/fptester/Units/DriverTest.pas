unit DriverTest;

interface

uses
  // VCL
  Windows, Classes, SysUtils, ComObj, ActiveX, System.Generics.Collections,
  // Indy
  IdGlobal, IdContext, IdTcpServer,
  // 3'd
  VSoft.YAML,
  // This
  DrvFRLib_TLB, LogFile, FrEmuPrinter, FileUtils;

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

const
  Separator = '--------------------------------------------------------';

  TextServerPort      = 12345; // !!!
  GraphicsServerPort  = 12356;

type
  TCashReg = record
    Number:  Integer;
    Name: string;
    Value: Currency;
  end;
  TCashRegs = array of TCashReg;

  TOperReg = record
    Number:  Integer;
    Name: string;
    Value: Int64;
  end;
  TOperRegs = array of TOperReg;

  TFiscalPrinterState = record
    Name: string;
    CashRegs: TCashRegs;
    OperRegs: TOperRegs;
  end;

  { TTesterOptions }

  TTesterOptions = record
    FilesPath: string;
    Verbose: Boolean;
    UseEmulator: Boolean;
    EmuComPort: string;
    FlashPath: string;
    DriverComNumber: Integer;
    TestsPathSpecified: Boolean;
  end;

  { TTestResult }

  TTestResult = record
    IsSucceeded: Boolean;
    Text: string;
  end;

  { TDriverContext }

  TDriverContext = class
  private
    FDriver: TDrvFR;
    FEmu: TFrEmuPrinter;
    FLines: TStrings;
    FGraphics: TIdBytes;
    FTextServer: TIdTCPServer;
    FGraphicsServer: TIdTCPServer;
    FOptions: TTesterOptions;
    FConsoleInfo: TConsoleScreenBufferInfo;
    FStates: TDictionary<string, TFiscalPrinterState>;
    FCurrentTestName: string;
    FReceiptSeq: Integer;
    procedure DebugVerbose(const s: string);
    procedure ErrorVerbose(const s: string);
    procedure ComparePrintedText(const Expected, Actual: string);
  public
    function GetDriver: TDrvFR;
    function IsEquals(Item1, Item2: TFiscalPrinterState): Boolean;
    procedure ReadCashRegsExRange(var CashRegs: TCashRegs; Min, Max: Integer);
    procedure ReadCashRegsRange(var CashRegs: TCashRegs; Min, Max: Integer);
    procedure ReadOperRegsRange(var OperRegs: TOperRegs; Min, Max: Integer);
    procedure TextServerExecute(AContext: TIdContext);
    procedure GraphicsServerExecute(AContext: TIdContext);
  public
    constructor Create;
    destructor Destroy; override;

    procedure ResetEcr;
    procedure StartTest;
    procedure SetErrorColor;
    procedure SetNormalColor;
    procedure Error(const s: string);
    procedure Debug(const s: string);
    procedure TestConnection;
    procedure SetEcrMode(ecrmode: Integer);
    procedure SaveState(const Name: string);
    procedure Check(ResultCode: Integer);
    procedure CheckEcrMode(EcrMode: Integer);
    procedure CheckEcrMode2(EcrMode: Integer);
    procedure SetPrinterState(const Name: string; const V: TFiscalPrinterState);

    function ReadPrinterState: TFiscalPrinterState;
    function GetPrinterState(const Name: string; var V: TFiscalPrinterState): Boolean;

    procedure CheckDayOperRegsZero;
    procedure CheckDayCashRegsZero;

    procedure CheckReceiptOperRegsZero;
    procedure CheckReceiptCashRegsZero;

    function FNReadLastReceipt: string;

    procedure StopTextServer;
    procedure StartTextServer(Port: Integer);
    procedure CheckTextPrinted(const Text: string);

    procedure StopGraphicsServer;
    procedure StartGraphicsServer(Port: Integer);
    procedure CheckGraphicsPrinted(const FileName: string);

    procedure StartEmulator;
    procedure StopEmulator;
    procedure ConfigureDriver;

    procedure ClearPrintedText;
    function WaitPrintedText: string;
    procedure SaveReceiptSnapshot;
    procedure EnsureShiftOpen;
    procedure PrintSimpleSaleReceipt; overload;
    procedure PrintSimpleSaleReceipt(Department, PaymentIndex: Integer); overload;
    procedure PrintSimpleSaleReceipt(Department, PaymentIndex, TaxIndex: Integer); overload;

    property Driver: TDrvFR read GetDriver;
    property Emu: TFrEmuPrinter read FEmu;
    property Options: TTesterOptions read FOptions write FOptions;
    property CurrentTestName: string read FCurrentTestName write FCurrentTestName;
  end;

  { TDriverTest }

  TDriverTest = class
  private
    FContext: TDriverContext;
    function GetDriver: TDrvFR;
    function GetOptions: TTesterOptions;
  public
    name: string;
    text: string;
    testtype: string;
    ecrmode_before: Integer;
    ecrmode_after: Integer;
    text_printed: string;
    graphics_file_name: string;
  public
    constructor CreateTest(AContext: TDriverContext); virtual;
    function Execute: TTestResult; virtual;
    procedure Load(node: IYAMLValue); virtual;
    procedure Check(ResultCode: Integer);

    property Driver: TDrvFR read GetDriver;
    property Context: TDriverContext read FContext;
    property Options: TTesterOptions read GetOptions;
  end;
  TDriverTests = TObjectList<TDriverTest>;

  { TUnknownTest }

  TUnknownTest = class(TDriverTest)
  public
    UnknownType: string;
    function Execute: TTestResult; override;
  end;

  { TModifiedCashReg }

  TModifiedCashReg = record
    Number:  Integer;
    Name: string;
    Value: Currency;
    Delta: Currency;
  end;
  TModifiedCashRegs = array of TModifiedCashReg;

  { TModifiedOperReg }

  TModifiedOperReg = record
    Number:  Integer;
    Name: string;
    Value: Int64;
    Delta: Int64;
  end;
  TModifiedOperRegs = array of TModifiedOperReg;

implementation

function ReadFileToBytes(const FileName: string): TBytes;
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    SetLength(Result, Stream.Size);
    Stream.Read(Result, 0, Stream.Size);
  finally
    Stream.Free;
  end;
end;

{ TDriverTest }

constructor TDriverTest.CreateTest(AContext: TDriverContext);
begin
  inherited Create;
  FContext := AContext;
end;

function TDriverTest.Execute: TTestResult;
begin

end;

procedure TDriverTest.Check(ResultCode: Integer);
begin
  Context.Check(ResultCode);
end;

function TDriverTest.GetDriver: TDrvFR;
begin
  Result := FContext.Driver;
end;

function TDriverTest.GetOptions: TTesterOptions;
begin
  Result := FContext.Options;
end;

procedure TDriverTest.Load(node: IYAMLValue);
var
  value : IYAMLValue;
begin
  Name := '';
  if node.TryGetValue('name', value) then
    Name := value.AsString;

  ecrmode_before := 0;
  if node.TryGetValue('ecrmode_before', value) then
    ecrmode_before := value.AsInteger;

  ecrmode_after := 0;
  if node.TryGetValue('ecrmode_after', value) then
    ecrmode_after := value.AsInteger;

  if node.TryGetValue('graphics_file_name', value) then
    graphics_file_name := value.AsString;

  text_printed := '';
  if node.TryGetValue('text_printed', value) then
    text_printed := value.AsString;
end;

{ TUnknownTest }

function TUnknownTest.Execute: TTestResult;
begin
  Result.IsSucceeded := False;
  Result.Text := Format('unknown testtype "%s"', [UnknownType]);
end;

{ TDriverContext }

procedure TDriverContext.SetEcrMode(ecrmode: Integer);
var
  Password: Integer;
begin
  if ecrmode = 0 then Exit;

  DebugVerbose('SetEcrMode...');
  Check(Driver.GetShortECRStatus);
  if Driver.ECRMode = ecrmode then
  begin
    DebugVerbose('SetEcrMode: OK');
    Exit;
  end;
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
      if Driver.ECRMode = FPTR_MODE_REC then
        Check(Driver.SysAdminCancelCheck);
      if Driver.ECRMode = FPTR_MODE_24OVER then
      begin
        Check(Driver.PrintReportWithCleaning);
        Check(Driver.WaitForPrinting);
        Check(Driver.GetShortECRStatus);
      end;
      if Driver.ECRMode <> FPTR_MODE_24NOTOVER then
      begin
        Check(Driver.OpenSession);
        Check(Driver.WaitForPrinting);
        Check(Driver.GetShortECRStatus);
      end;
      if Driver.ECRMode = FPTR_MODE_24OVER then
      begin
        Check(Driver.PrintReportWithCleaning);
        Check(Driver.WaitForPrinting);
        Check(Driver.OpenSession);
        Check(Driver.WaitForPrinting);
        Check(Driver.GetShortECRStatus);
      end;
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
  DebugVerbose('SetEcrMode: OK');
end;

constructor TDriverContext.Create;
begin
  inherited Create;
  FStates := TDictionary<string, TFiscalPrinterState>.Create;
  GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE), FConsoleInfo);
  FTextServer := TIdTCPServer.Create(nil);
  FGraphicsServer := TIdTCPServer.Create(nil);
  FLines := TStringList.Create;
end;

destructor TDriverContext.Destroy;
begin
  StopEmulator;
  FDriver.Free;
  FDriver := nil;
  FLines.Free;
  FStates.Free;
  FTextServer.Free;
  FGraphicsServer.Free;
  inherited Destroy;
end;

procedure TDriverContext.DebugVerbose(const s: string);
begin
  if Options.Verbose then
  begin
    Debug(s);
  end;
end;

procedure TDriverContext.ErrorVerbose(const s: string);
begin
  if Options.Verbose then
  begin
    Error(s);
  end;
end;

procedure TDriverContext.Debug(const s: string);
begin
  SetNormalColor;
  Logger.Debug(s);
end;

procedure TDriverContext.Error(const s: string);
begin
  SetErrorColor;
  Logger.Error(s);
  SetNormalColor;
end;

function TDriverContext.GetDriver: TDrvFR;
begin
  if FDriver = nil then
  begin
    OleCheck(CoInitialize(nil));
    FDriver := TDrvFR.Create(nil);
  end;
  Result := FDriver;
end;

function TDriverContext.GetPrinterState(const Name: string;
  var V: TFiscalPrinterState): Boolean;
begin
  Result := FStates.TryGetValue(Name, V);
end;

procedure TDriverContext.SetPrinterState(const Name: string;
  const V: TFiscalPrinterState);
begin
  FStates.AddOrSetValue(Name, V);
end;

procedure TDriverContext.SetErrorColor;
begin
  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE),
    FOREGROUND_RED or FOREGROUND_INTENSITY);
end;

procedure TDriverContext.SetNormalColor;
begin
  SetConsoleTextAttribute(GetStdHandle(STD_OUTPUT_HANDLE),
    FConsoleInfo.wAttributes);
end;

procedure TDriverContext.Check(ResultCode: Integer);
begin
  if ResultCode <> 0 then
    raise Exception.CreateFmt('%d, %s',
    [ResultCode, Driver.ResultCodeDescription]);
end;

procedure TDriverContext.CheckEcrMode(EcrMode: Integer);
begin
  if EcrMode <> Driver.EcrMode then
    raise Exception.CreateFmt('режим ККТ, %d <> %d', [
    EcrMode, Driver.EcrMode]);
end;

procedure TDriverContext.CheckEcrMode2(EcrMode: Integer);
begin
  if ecrmode <> 0 then
  begin
    Check(Driver.GetShortECRStatus);
    if ecrmode <> Driver.ECRMode then
    begin
      ErrorVerbose('Ошибка: не достигнуто состояние');
      ErrorVerbose(Format('Ожидаемое состояние : %d.', [ecrmode]));
      ErrorVerbose(Format('Текущее состояние  : %d.', [Driver.ECRMode]));

      raise Exception.CreateFmt('ожидаемое состояние : %d, текущее: %d', [
        ecrmode, Driver.ECRMode]);
    end;
  end;
end;

function TDriverContext.IsEquals(Item1, Item2: TFiscalPrinterState): Boolean;
var
  i: Integer;
begin
  Result := True;
  // �������� ��������
  for i := 0 to Length(Item1.CashRegs)-1 do
  begin
    Result := Item1.CashRegs[i].Number = Item2.CashRegs[i].Number;
    if not Result then
    begin
      Error('Не совпадают номера денежных регистров');
      Error(Format('Ожидается: %d, получено: %d', [
        Item1.CashRegs[i].Number,
        Item2.CashRegs[i].Number]));
      Exit;
    end;
    Result := Item1.CashRegs[i].Value = Item2.CashRegs[i].Value;
    if not Result then
    begin
      Error('Не совпадают значения денежных регистров');
      Error(Format('Ожидается: %.2f, получено: %.2f', [
        Item1.CashRegs[i].Value,
        Item2.CashRegs[i].Value]));
      Exit;
    end;
  end;
  // ������������ ��������
  for i := 0 to Length(Item1.OperRegs)-1 do
  begin
    Result := Item1.OperRegs[i].Number = Item2.OperRegs[i].Number;
    if not Result then
    begin
      Error('Не совпадают номера операционных регистров');
      Error(Format('Ожидается: %d, получено: %d', [
        Item1.OperRegs[i].Number,
        Item2.OperRegs[i].Number]));
      Exit;
    end;
    Result := Item1.OperRegs[i].Value = Item2.OperRegs[i].Value;
    if not Result then
    begin
      Error(Format('Не совпадают значения операционного регистра %d, %s', [
        Item1.OperRegs[i].Number,
        Item1.OperRegs[i].Name]));

      Error(Format('Ожидается: %d, получено: %d', [
        Item1.OperRegs[i].Value,
        Item2.OperRegs[i].Value]));
      Exit;
    end;
  end;
end;

function TDriverContext.ReadPrinterState: TFiscalPrinterState;
const
  MinCashReg = 0;
  MaxCashReg = 252;
  MinCashReg2 = 4096;
  MaxCashReg2 = 4490;
  MinOperationReg = 0;
  MaxOperationReg = 203;
var
  i: Integer;
  Count: Integer;
  Index: Integer;
begin
  Count := (MaxCashReg - MinCashReg + 1) + (MaxCashReg2 - MinCashReg2 + 1);
  SetLength(Result.CashRegs, Count);
  Index := 0;
  // �������� ��������
  for i := MinCashReg to MaxCashReg do
  begin
    Driver.RegisterNumber := i;
    Check(Driver.GetCashReg);
    Result.CashRegs[Index].Number := i;
    Result.CashRegs[Index].Name := Driver.NameCashReg;
    Result.CashRegs[Index].Value := Driver.ContentsOfCashRegister;
    Inc(Index);
  end;
  // ����������� �������� ��������
  for i := MinCashReg2 to MaxCashReg2 do
  begin
    Driver.RegisterNumber := i;
    Check(Driver.GetCashRegEx);
    Result.CashRegs[Index].Number := i;
    Result.CashRegs[Index].Name := Driver.NameCashRegEx;
    Result.CashRegs[Index].Value := Driver.ContentsOfCashRegister;
    Inc(Index);
  end;
  // ������������ ��������
  Index := 0;
  Count := MaxOperationReg - MinOperationReg + 1;
  SetLength(Result.OperRegs, Count);
  for i := MinOperationReg to MaxOperationReg do
  begin
    Driver.RegisterNumber := i;
    Check(Driver.GetOperationReg);
    Result.OperRegs[Index].Number := i;
    Result.OperRegs[Index].Name := Driver.NameOperationReg;
    Result.OperRegs[Index].Value := Driver.ContentsOfOperationRegister;
    Inc(Index);
  end;
end;

procedure TDriverContext.ConfigureDriver;
begin
  if Options.UseEmulator then
  begin
    if (Options.DriverComNumber <= 0) or (Options.EmuComPort = '') then
      raise Exception.Create('Emulator COM pair is not resolved');
    Driver.ConnectionType := 0;
    Driver.ComNumber := Options.DriverComNumber;
    Driver.BaudRate := 6;
    Debug(Format('Driver: COM%d, 115200 (emulator UART %s)',
      [Driver.ComNumber, Options.EmuComPort]));
  end
  else if Options.DriverComNumber > 0 then
  begin
    Driver.ConnectionType := 0;
    Driver.ComNumber := Options.DriverComNumber;
  end;
end;

procedure TDriverContext.StartEmulator;
begin
  if not Options.UseEmulator then
    Exit;
  if FEmu = nil then
    FEmu := TFrEmuPrinter.Create;
  Debug(Format('Starting FrEmu.Printer, port %s', [Options.EmuComPort]));
  FEmu.Start(Options.EmuComPort, Options.FlashPath);
end;

procedure TDriverContext.StopEmulator;
begin
  FreeAndNil(FEmu);
end;

procedure TDriverContext.TestConnection;
var
  i: Integer;
begin
  if Options.UseEmulator then
  begin
    for i := 1 to 120 do
    begin
      Driver.Connect;
      if Driver.ResultCode = 0 then
        Break;
      Sleep(500);
    end;
  end;
  Check(Driver.Connect);
  Check(Driver.GetECRStatus);
  Check(Driver.GetDeviceMetrics);

  Logger.Debug(Separator);
  Logger.Debug('Текущее состояние с ФР');
  Logger.Debug(Format('Модель ФР : %d, %s', [Driver.UModel, Driver.UDescription]));
  Logger.Debug(Format('Режим ФР  : %d, %s', [Driver.ECRMode, Driver.ECRModeDescription]));
  Logger.Debug(Format('Версия ФР : %s, %d, %s', [Driver.ECRSoftVersion, Driver.ECRBuild, DateToStr(Driver.ECRSoftDate)]));
  Logger.Debug(Separator);
end;


procedure TDriverContext.ResetEcr;
const
  RepCount = 10;
var
  i, j, Rc, SavedPassword: Integer;
begin
  DebugVerbose('ResetEcr...');
  for i := 0 to RepCount - 1 do
  begin
    Rc := 0;
    for j := 1 to 120 do
    begin
      Rc := Driver.WaitForPrinting;
      if (Rc <> 80) and (Rc <> -34) and (Rc <> 30) then
        Break;
      Sleep(250);
    end;
    Check(Rc);
    case Driver.ECRMode of
      FPTR_MODE_DUMP:
      begin
        SavedPassword := Driver.Password;
        Driver.Password := 0;
        Check(Driver.InterruptDataStream);
        Driver.Password := SavedPassword;
      end;
      FPTR_MODE_24NOTOVER,
      FPTR_MODE_24OVER: Check(Driver.PrintReportWithCleaning);
      FPTR_MODE_WAITDATE:
      begin
        // Confirm any date
        Check(Driver.GetECRStatus);
        Check(Driver.ConfirmDate);
        // Set current datetime
        Driver.ECRDate := Now;
        Check(Driver.SetDate);
        Check(Driver.ConfirmDate);
      end;
      FPTR_MODE_REC:
      begin
        Rc := Driver.CancelCheck;
        if (Rc = 80) or (Rc = -34) or (Rc = 30) then
        begin
          Sleep(400);
          Continue;
        end;
        Check(Rc);
      end;
      FPTR_MODE_TEST: Check(Driver.InterruptTest);
      FPTR_MODE_FULLREPORT,
      FPTR_MODE_EKLZREPORT,
      FPTR_MODE_SLPPRINT: ;
    else
      Break;
    end;
  end;
  DebugVerbose('ResetEcr: OK');
end;

procedure TDriverContext.SaveState(const Name: string);
begin
  FStates.Add(Name, ReadPrinterState);
end;

// �������� �������� ����
procedure TDriverContext.CheckReceiptCashRegsZero;
var
  CashReg: TCashReg;
  CashRegs: TCashRegs;
begin
  ReadCashRegsRange(CashRegs, 0, 119);
  ReadCashRegsExRange(CashRegs, 4096, 4143);
  ReadCashRegsExRange(CashRegs, 4192, 4207);
  ReadCashRegsExRange(CashRegs, 4427, 4458);

  for CashReg in CashRegs do
  begin
    if CashReg.Value <> 0 then
    begin
      raise Exception.CreateFmt('Ненулевое значение денежного регистра %d, %s', [
        CashReg.Number, CashReg.Name]);
    end;
  end;
end;

// �������� �������� �����
procedure TDriverContext.CheckDayCashRegsZero;
var
  CashReg: TCashReg;
  CashRegs: TCashRegs;
begin
  ReadCashRegsRange(CashRegs, 121, 240);
  ReadCashRegsRange(CashRegs, 242, 243);
  ReadCashRegsRange(CashRegs, 249, 252);
  ReadCashRegsExRange(CashRegs, 4144, 4191);
  ReadCashRegsExRange(CashRegs, 4208, 4223);
  ReadCashRegsExRange(CashRegs, 4363, 4426);
  ReadCashRegsExRange(CashRegs, 4459, 4490);

  for CashReg in CashRegs do
  begin
    if CashReg.Value <> 0 then
    begin
      raise Exception.CreateFmt('Ненулевое значение денежного регистра %d, %s', [
        CashReg.Number, CashReg.Name]);
    end;
  end;
end;

procedure TDriverContext.ReadCashRegsRange(var CashRegs: TCashRegs;
  Min, Max: Integer);
var
  i: Integer;
  Index: Integer;
begin
  Index := Length(CashRegs);
  SetLength(CashRegs, Length(CashRegs) + (Max - Min + 1));
  for i := Min to Max do
  begin
    Driver.RegisterNumber := i;
    Check(Driver.GetCashReg);
    CashRegs[Index].Number := i;
    CashRegs[Index].Name := Driver.NameCashReg;
    CashRegs[Index].Value := Driver.ContentsOfCashRegister;
    Inc(Index);
  end;
end;

procedure TDriverContext.ReadCashRegsExRange(var CashRegs: TCashRegs;
  Min, Max: Integer);
var
  i: Integer;
  Index: Integer;
begin
  Index := Length(CashRegs);
  SetLength(CashRegs, Length(CashRegs) + (Max - Min + 1));
  for i := Min to Max do
  begin
    Driver.RegisterNumber := i;
    Check(Driver.GetCashRegEx);
    CashRegs[Index].Number := i;
    CashRegs[Index].Name := Driver.NameCashRegEx;
    CashRegs[Index].Value := Driver.ContentsOfCashRegister;
    Inc(Index);
  end;
end;

procedure TDriverContext.CheckReceiptOperRegsZero;
var
  OperReg: TOperReg;
  OperRegs: TOperRegs;
begin
  // ������������ ��������
  ReadOperRegsRange(OperRegs, 0, 71);
  for OperReg in OperRegs do
  begin
    if OperReg.Value <> 0 then
    begin
      raise Exception.CreateFmt('Ненулевое значение операционного регистра %d, %s', [
        OperReg.Number, OperReg.Name]);
    end;
  end;
end;

procedure TDriverContext.CheckDayOperRegsZero;
var
  OperReg: TOperReg;
  OperRegs: TOperRegs;
begin
  // ������������ ��������
  ReadOperRegsRange(OperRegs, 72, 147);
  for OperReg in OperRegs do
  begin
    if OperReg.Value <> 0 then
    begin
      raise Exception.CreateFmt('Ненулевое значение операционного регистра %d, %s', [
        OperReg.Number, OperReg.Name]);
    end;
  end;
end;

procedure TDriverContext.ReadOperRegsRange(var OperRegs: TOperRegs;
  Min, Max: Integer);
var
  i: Integer;
  Index: Integer;
begin
  Index := Length(OperRegs);
  SetLength(OperRegs, Length(OperRegs) + (Max - Min + 1));
  for i := Min to Max do
  begin
    Driver.RegisterNumber := i;
    Check(Driver.GetOperationReg);
    OperRegs[Index].Number := i;
    OperRegs[Index].Name := Driver.NameOperationReg;
    OperRegs[Index].Value := Driver.ContentsOfOperationRegister;
    Inc(Index);
  end;
end;

function TDriverContext.FNReadLastReceipt: string;
begin
  Check(Driver.FNGetStatus);
  Driver.RequestDocumentType := 0;
  Driver.ShowTagNumber := True;
  Check(Driver.FNGetDocumentAsString);
  Result := Driver.StringForPrinting;
end;

procedure TDriverContext.StartTextServer(Port: Integer);
begin
  if Options.UseEmulator then
  begin
    if FEmu <> nil then
      FEmu.ClearText;
    Exit;
  end;
  FLines.Clear;
  FTextServer.DefaultPort := Port;
  FTextServer.OnExecute := TextServerExecute;
  FTextServer.Active := True;
end;

procedure TDriverContext.StartGraphicsServer(Port: Integer);
begin
  if Options.UseEmulator then
    Exit;
  FGraphicsServer.DefaultPort := Port;
  FGraphicsServer.OnExecute := GraphicsServerExecute;
  FGraphicsServer.Active := True;
end;

procedure TDriverContext.TextServerExecute(AContext: TIdContext);
begin
  FLines.Add(AContext.Connection.IOHandler.ReadLn);
end;

procedure TDriverContext.GraphicsServerExecute(AContext: TIdContext);
var
  Count: Integer;
begin
  Count := AContext.Connection.IOHandler.InputBuffer.Size;
  if Count > 0 then
  begin
    AContext.Connection.IOHandler.InputBuffer.ExtractToBytes(FGraphics, Count);
  end;
end;

procedure TDriverContext.StopTextServer;
begin
  if Options.UseEmulator then
    Exit;
  FTextServer.Active := False;
end;

procedure TDriverContext.StopGraphicsServer;
begin
  if Options.UseEmulator then
    Exit;
  FGraphicsServer.Active := False;
end;

procedure TDriverContext.ComparePrintedText(const Expected, Actual: string);
var
  i, Count: Integer;
  ExpLines, ActLines: TStrings;
begin
  if Expected = Actual then
    Exit;

  ExpLines := TStringList.Create;
  ActLines := TStringList.Create;
  try
    ExpLines.Text := Expected;
    ActLines.Text := Actual;
    if ExpLines.Count <> ActLines.Count then
    begin
      Logger.Debug('Line count mismatch.');
      Logger.Debug(Format('Expected lines: %d, got: %d', [
        ExpLines.Count, ActLines.Count]));
    end;
    Count := ExpLines.Count;
    if ActLines.Count < Count then
      Count := ActLines.Count;
    for i := 0 to Count - 1 do
    begin
      if ExpLines[i] <> ActLines[i] then
      begin
        Logger.Debug(Format('Lines differ, index: %d.', [i]));
        Logger.Debug(Format('Expected: "%s", got: "%s"', [
          ExpLines[i], ActLines[i]]));
      end;
    end;
    raise Exception.CreateFmt('Text mismatch. Expected: "%s", got: "%s"',
      [Expected, Actual]);
  finally
    ExpLines.Free;
    ActLines.Free;
  end;
end;

procedure TDriverContext.CheckTextPrinted(const Text: string);
var
  i: Integer;
  Expected, Actual: string;
begin
  if Text = '' then Exit;

  if Options.UseEmulator then
  begin
    if FEmu = nil then
      raise Exception.Create('FR emulator is not started');
    Expected := NormalizePrintedText(Text);
    Actual := '';
    for i := 1 to 25 do
    begin
      Actual := NormalizePrintedText(FEmu.ReadText);
      if Actual = Expected then
      begin
        SaveReceiptSnapshot;
        Exit;
      end;
      Sleep(200);
    end;
    SaveReceiptSnapshot;
    ComparePrintedText(Expected, Actual);
    Exit;
  end;

  ComparePrintedText(Text, FLines.Text);
end;

procedure TDriverContext.StartTest;
begin
  SetLength(FGraphics, 0);
  if Options.UseEmulator then
  begin
    if FEmu <> nil then
      FEmu.ClearText;
    Exit;
  end;
  StartTextServer(TextServerPort);
  StartGraphicsServer(GraphicsServerPort);
end;

procedure TDriverContext.CheckGraphicsPrinted(const FileName: string);
var
  i: Integer;
  Bytes, Actual: TBytes;
begin
  if FileName = '' then Exit;

  Bytes := ReadFileToBytes(FileName);
  if Options.UseEmulator then
  begin
    if FEmu = nil then
      raise Exception.Create('FR emulator is not started');
    Actual := FEmu.ReadImage;
    if Length(Bytes) <> Length(Actual) then
      raise Exception.CreateFmt('Data length differs. Expected %d, got %d',
        [Length(Bytes), Length(Actual)]);
    for i := 0 to Length(Bytes) - 1 do
    begin
      if Bytes[i] <> Actual[i] then
        raise Exception.Create('Data differs');
    end;
    Exit;
  end;

  if Length(Bytes) <> Length(FGraphics) then
    raise Exception.CreateFmt('Data length differs. Expected %d, got %d',
    [Length(Bytes), Length(FGraphics)]);

  for i := 0 to Length(Bytes)-1  do
  begin
    if Bytes[i] <> FGraphics[i] then
      raise Exception.Create('Data differs');
  end;
end;

procedure TDriverContext.ClearPrintedText;
begin
  FLines.Clear;
  if Options.UseEmulator and (FEmu <> nil) then
    FEmu.ClearText;
end;

function SanitizeReceiptName(const S: string): string;
var
  i: Integer;
  C: Char;
begin
  Result := '';
  for i := 1 to Length(S) do
  begin
    C := S[i];
    if CharInSet(C, ['A'..'Z', 'a'..'z', '0'..'9', '-', '_']) or
      ((Ord(C) >= $0410) and (Ord(C) <= $044F)) or (C = #$0401) or (C = #$0451) then
      Result := Result + C
    else
      Result := Result + '_';
  end;
  while (Length(Result) > 0) and (Result[Length(Result)] = '_') do
    Delete(Result, Length(Result), 1);
  if Length(Result) > 80 then
    Result := Copy(Result, 1, 80);
  if Result = '' then
    Result := 'receipt';
end;

procedure TDriverContext.SaveReceiptSnapshot;
var
  Dir, Name, Path: string;
begin
  if not Options.UseEmulator or (FEmu = nil) then
    Exit;
  Dir := GetModulePath + 'logs\receipts\';
  ForceDirectories(Dir);
  Inc(FReceiptSeq);
  Name := Format('%.3d_%s', [FReceiptSeq, SanitizeReceiptName(FCurrentTestName)]);
  Path := Dir + Name + '.bmp';
  try
    FEmu.SaveImage(Path);
    Debug(Format('Receipt image: %s', [Path]));
  except
    on E: Exception do
      Debug(Format('Receipt image not saved: %s', [E.Message]));
  end;
end;

function TDriverContext.WaitPrintedText: string;
var
  i, Stable: Integer;
  Last: string;
begin
  Result := '';
  Last := '';
  Stable := 0;
  if Options.UseEmulator then
  begin
    if FEmu = nil then
      raise Exception.Create('FR emulator is not started');
    for i := 1 to 50 do
    begin
      Result := NormalizePrintedText(FEmu.ReadText);
      if Result = Last then
      begin
        Inc(Stable);
        if (Result <> '') and (Stable >= 3) then
        begin
          SaveReceiptSnapshot;
          Exit;
        end;
      end else
        Stable := 0;
      Last := Result;
      Sleep(200);
    end;
    Result := NormalizePrintedText(FEmu.ReadText);
    if Result <> '' then
      SaveReceiptSnapshot;
    Exit;
  end;
  Sleep(500);
  Result := NormalizePrintedText(FLines.Text);
end;

procedure TDriverContext.EnsureShiftOpen;
begin
  Check(Driver.GetECRStatus);
  case Driver.ECRMode of
    FPTR_MODE_REC:
    begin
      Check(Driver.CancelCheck);
      Check(Driver.WaitForPrinting);
    end;
    FPTR_MODE_CLOSED:
    begin
      Check(Driver.OpenSession);
      Check(Driver.WaitForPrinting);
    end;
    FPTR_MODE_24OVER:
    begin
      Check(Driver.PrintReportWithCleaning);
      Check(Driver.WaitForPrinting);
      Check(Driver.OpenSession);
      Check(Driver.WaitForPrinting);
    end;
  end;
  Check(Driver.GetECRStatus);
  if Driver.ECRMode <> FPTR_MODE_24NOTOVER then
    raise Exception.CreateFmt('Need open shift, ECRMode=%d', [Driver.ECRMode]);
end;

procedure TDriverContext.PrintSimpleSaleReceipt;
begin
  PrintSimpleSaleReceipt(1, 1, 6);
end;

procedure TDriverContext.PrintSimpleSaleReceipt(Department, PaymentIndex: Integer);
begin
  PrintSimpleSaleReceipt(Department, PaymentIndex, 6);
end;

procedure TDriverContext.PrintSimpleSaleReceipt(Department, PaymentIndex, TaxIndex: Integer);
  function HexUInt(Value: Int64; Size: Integer): string;
  var
    i: Integer;
  begin
    Result := '';
    for i := 0 to Size - 1 do
    begin
      if Result <> '' then
        Result := Result + ' ';
      Result := Result + IntToHex(Byte((Value shr (8 * i)) and $FF), 2);
    end;
  end;

  function SendHex(const Hex: string): Integer;
  begin
    Driver.BinaryConversion := BINARY_CONVERSION_HEX;
    Driver.TransferBytes := Hex;
    Result := Driver.ExchangeBytes;
    Debug(Format('raw %s -> %d', [Copy(Hex, 1, 24), Result]));
    Driver.BinaryConversion := BINARY_CONVERSION_NONE;
  end;

  procedure CheckHex(const Hex: string);
  begin
    Check(SendHex(Hex));
  end;

var
  TaxRate, Pay: Int64;
  i, Rc: Integer;
  Sums, Taxes, Item, CloseCmd: string;
begin
  if Department < 1 then
    Department := 1;
  if PaymentIndex < 1 then
    PaymentIndex := 1;
  if PaymentIndex > 16 then
    PaymentIndex := 16;
  if TaxIndex < 1 then
    TaxIndex := 6;

  EnsureShiftOpen;
  Driver.BinaryConversion := BINARY_CONVERSION_NONE;
  Driver.TransferBytes := '';
  Driver.Password := 30;

  Check(Driver.GetShortECRStatus);
  if Driver.ECRMode = FPTR_MODE_REC then
    Driver.SysAdminCancelCheck;

  case TaxIndex of
    1: TaxRate := $01;
    2: TaxRate := $02;
    3: TaxRate := $04;
    4: TaxRate := $08;
    5: TaxRate := $10;
  else
    TaxRate := $00;
  end;

  SendHex('8D 1E 00 00 00 00');

  Item := Format(
    'FF 46 1E 00 00 00 01 %s %s %s %s %s %s 04 01 54 41 42 4C 45 20 54 45 53 54',
    [HexUInt(1000000, 6),
     HexUInt(100, 5),
     HexUInt(100, 5),
     HexUInt($FFFFFFFFFF, 5),
     HexUInt(TaxRate, 1),
     HexUInt(Department, 1)]);
  CheckHex(Item);

  Sums := '';
  for i := 1 to 16 do
  begin
    if i = PaymentIndex then
      Pay := 100
    else
      Pay := 0;
    if Sums <> '' then
      Sums := Sums + ' ';
    Sums := Sums + HexUInt(Pay, 5);
  end;
  Taxes := '';
  for i := 1 to 6 do
  begin
    if Taxes <> '' then
      Taxes := Taxes + ' ';
    Taxes := Taxes + HexUInt(0, 5);
  end;
  CloseCmd := Format('FF 45 1E 00 00 00 %s 00 %s 01', [Sums, Taxes]);
  CheckHex(CloseCmd);
  for i := 1 to 25 do
  begin
    Rc := Driver.WaitForPrinting;
    if Rc <> -34 then
      Break;
    Sleep(150);
  end;
  if (Rc <> 0) and (Rc <> -34) then
    Check(Rc);
end;

end.
