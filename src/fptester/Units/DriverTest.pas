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
  DrvFRLib_TLB, LogFile;

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
    FLines: TStrings;
    FGraphics: TIdBytes;
    FTextServer: TIdTCPServer;
    FGraphicsServer: TIdTCPServer;
    FOptions: TTesterOptions;
    FConsoleInfo: TConsoleScreenBufferInfo;
    FStates: TDictionary<string, TFiscalPrinterState>;

    procedure SetErrorColor;
    procedure SetNormalColor;
    function GetDriver: TDrvFR;
    function IsEquals(Item1, Item2: TFiscalPrinterState): Boolean;
    procedure ReadCashRegsExRange(var CashRegs: TCashRegs; Min, Max: Integer);
    procedure ReadCashRegsRange(var CashRegs: TCashRegs; Min, Max: Integer);
    procedure ReadOperRegsRange(var OperRegs: TOperRegs; Min, Max: Integer);
    procedure TextServerExecute(AContext: TIdContext);
    procedure GraphicsServerExecute(AContext: TIdContext);
    procedure StartGraphicsServer;
    procedure StopGraphicsServer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure ResetEcr;
    procedure StartTest;
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
    procedure CheckGraphicsPrinted(const FileName: string);

    property Driver: TDrvFR read GetDriver;
    property Options: TTesterOptions read FOptions write FOptions;
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
  Name := node.GetValue('name').AsString;

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

{ TDriverContext }

procedure TDriverContext.SetEcrMode(ecrmode: Integer);
var
  Password: Integer;
begin
  if ecrmode = 0 then Exit;

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
  FLines.Free;
  FStates.Free;
  FTextServer.Free;
  FGraphicsServer.Free;
  inherited Destroy;
end;

procedure TDriverContext.Debug(const s: string);
begin
  SetNormalColor;
  WriteLn(s);
end;

procedure TDriverContext.Error(const s: string);
begin
  SetErrorColor;
  WriteLn(s);
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
    raise Exception.CreateFmt('Другое состояние ФР, %d <> %d', [
    EcrMode, Driver.EcrMode]);
end;

procedure TDriverContext.CheckEcrMode2(EcrMode: Integer);
begin
  if ecrmode <> 0 then
  begin
    Check(Driver.GetShortECRStatus);
    if ecrmode <> Driver.ECRMode then
    begin
      if Options.Verbose then
      begin
        Error('Ошибка: не совпадает состояние');
        Error(Format('Ожидается состояние : %d.', [ecrmode]));
        Error(Format('Получено состояние  : %d.', [Driver.ECRMode]));
      end;
      raise Exception.CreateFmt('Ожидается состояние : %d, получено: %d', [
        ecrmode, Driver.ECRMode]);
    end;
  end;
end;

function TDriverContext.IsEquals(Item1, Item2: TFiscalPrinterState): Boolean;
var
  i: Integer;
begin
  Result := True;
  // Денежные регистры
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
  // Операционные регистры
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
  // Денежные регистры
  for i := MinCashReg to MaxCashReg do
  begin
    Driver.RegisterNumber := i;
    Check(Driver.GetCashReg);
    Result.CashRegs[Index].Number := i;
    Result.CashRegs[Index].Name := Driver.NameCashReg;
    Result.CashRegs[Index].Value := Driver.ContentsOfCashRegister;
    Inc(Index);
  end;
  // Расширенные денежные регистры
  for i := MinCashReg2 to MaxCashReg2 do
  begin
    Driver.RegisterNumber := i;
    Check(Driver.GetCashRegEx);
    Result.CashRegs[Index].Number := i;
    Result.CashRegs[Index].Name := Driver.NameCashRegEx;
    Result.CashRegs[Index].Value := Driver.ContentsOfCashRegister;
    Inc(Index);
  end;
  // Операционные регистры
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

procedure TDriverContext.TestConnection;
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


procedure TDriverContext.ResetEcr;
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

procedure TDriverContext.SaveState(const Name: string);
begin
  FStates.Add(Name, ReadPrinterState);
end;

// Денежные регистры чека
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

// Денежные регистры смены
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
  // Операционные регистры
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
  // Операционные регистры
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
  Logger.Debug('FNReadLastReceipt');
  Check(Driver.FNGetStatus);
  Driver.RequestDocumentType := 0;
  Driver.ShowTagNumber := True;
  Check(Driver.FNGetDocumentAsString);
  Result := Driver.StringForPrinting;
  Logger.Debug('FNReadLastReceipt=' + Result);
end;

procedure TDriverContext.StartTextServer(Port: Integer);
begin
  FLines.Clear;
  FTextServer.DefaultPort := Port;
  FTextServer.Active := True;
  FTextServer.OnExecute := TextServerExecute;
end;

procedure TDriverContext.StartGraphicsServer;
begin
  FGraphicsServer.DefaultPort := GraphicsServerPort;
  FGraphicsServer.Active := True;
  FGraphicsServer.OnExecute := GraphicsServerExecute;
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
  FTextServer.Active := False;
end;

procedure TDriverContext.StopGraphicsServer;
begin
  FGraphicsServer.Active := False;
end;

procedure TDriverContext.CheckTextPrinted(const Text: string);
var
  i: Integer;
  ALines: TStrings;
  Line1, Line2: string;
begin
  if Text = '' then Exit;

  if Text <> FLines.Text then
  begin

    ALines := TStringList.Create;
    try
      ALines.Text := Text;
      if FLines.Count <> ALines.Count then
      begin
        Logger.Debug('Количество строк не совпадает.');
        Logger.Debug(Format('Ожидается строк: %d, получено: %d', [
          ALines.Count, FLines.Count]));
      end;
      for i := 0 to FLines.Count-1 do
      begin
        if ALines[i] <> FLines[i] then
        begin
          Logger.Debug(Format('Строки не совпадают, индекс: %d.', [i]));
          Logger.Debug(Format('Ожидается строка: "%s", получена: "%s"', [
            ALines[i], FLines[i]]));
        end;
      end;
      raise Exception.CreateFmt('Текст не совпадает. Ожидается: "%s", получен: "%s"',
        [Text, FLines.Text]);
    finally
      ALines.Free;
    end;
  end;
end;

procedure TDriverContext.StartTest;
begin
  StartGraphicsServer;
  SetLength(FGraphics, 0);
  StartTextServer(TextServerPort);
end;

procedure TDriverContext.CheckGraphicsPrinted(const FileName: string);
var
  i: Integer;
  Bytes: TBytes;
begin
  Bytes := ReadFileToBytes(FileName);
  if Length(Bytes) <> Length(FGraphics) then
    raise Exception.CreateFmt('Длина данных отличается. Ожидается %d, получено %d',
    [Length(Bytes), Length(FGraphics)]);

  for i := 0 to Length(Bytes)-1  do
  begin
    if Bytes[i] <> FGraphics[i] then
      raise Exception.Create('Данные отличаются');
  end;
end;






end.
