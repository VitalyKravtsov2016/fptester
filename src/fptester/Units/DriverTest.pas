unit DriverTest;

interface

uses
  // VCL
  Windows, Classes, SysUtils, ComObj, ActiveX, System.Generics.Collections,
  // 3'd
  VSoft.YAML,
  // This
  DrvFRLib_TLB;

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

type
  TCashRegister = record
    Number:  Integer;
    Name: string;
    Value: Currency;
  end;

  TOperationRegister = record
    Number:  Integer;
    Name: string;
    Value: Int64;
  end;

  TFiscalPrinterState = record
    CashRegisters: array of TCashRegister;
    OperationRegisters: array of TOperationRegister;
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
    FOptions: TTesterOptions;
    FConsoleInfo: TConsoleScreenBufferInfo;
    function GetDriver: TDrvFR;
  public
    constructor Create;
    destructor Destroy; override;

    procedure ResetEcr;
    procedure SetErrorColor;
    procedure SetNormalColor;
    procedure TestConnection;
    procedure SetEcrMode(ecrmode: Integer);
    procedure Check(ResultCode: Integer);
    procedure CheckEcrMode(EcrMode: Integer);
    procedure CheckEcrMode2(EcrMode: Integer);
    function ReadPrinterState: TFiscalPrinterState;

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

function IsEquals(Item1, Item2: TFiscalPrinterState): Boolean;

implementation

function IsEquals(Item1, Item2: TFiscalPrinterState): Boolean;
var
  i: Integer;
begin
  Result := True;
  // Денежные регистры
  for i := 0 to Length(Item1.CashRegisters)-1 do
  begin
    Result := Item1.CashRegisters[i].Number = Item2.CashRegisters[i].Number;
    if not Result then
    begin
      WriteLn('Не совпадают номера денежных регистров');
      WriteLn(Format('Ожидается: %d, получено: %d', [
        Item1.CashRegisters[i].Number,
        Item2.CashRegisters[i].Number]));
      Exit;
    end;
    Result := Item1.CashRegisters[i].Value = Item2.CashRegisters[i].Value;
    if not Result then
    begin
      WriteLn('Не совпадают значения денежных регистров');
      WriteLn(Format('Ожидается: %.2f, получено: %.2f', [
        Item1.CashRegisters[i].Value,
        Item2.CashRegisters[i].Value]));
      Exit;
    end;
  end;
  // Операционные регистры
  for i := 0 to Length(Item1.OperationRegisters)-1 do
  begin
    Result := Item1.OperationRegisters[i].Number = Item2.OperationRegisters[i].Number;
    if not Result then
    begin
      WriteLn('Не совпадают номера операционных регистров');
      WriteLn(Format('Ожидается: %d, получено: %d', [
        Item1.OperationRegisters[i].Number,
        Item2.OperationRegisters[i].Number]));
      Exit;
    end;
    Result := Item1.OperationRegisters[i].Value = Item2.OperationRegisters[i].Value;
    if not Result then
    begin
      WriteLn(Format('Не совпадают значения операционного регистра %d, %s', [
        Item1.OperationRegisters[i].Number,
        Item1.OperationRegisters[i].Name]));

      WriteLn(Format('Ожидается: %d, получено: %d', [
        Item1.OperationRegisters[i].Value,
        Item2.OperationRegisters[i].Value]));
      Exit;
    end;
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
  GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE), FConsoleInfo);
end;

destructor TDriverContext.Destroy;
begin

  inherited;
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
        SetErrorColor;
        WriteLn('Ошибка: не совпадает состояние');
        WriteLn(Format('Ожидается состояние : %d.', [ecrmode]));
        WriteLn(Format('Получено состояние  : %d.', [Driver.ECRMode]));
        SetNormalColor;
      end;
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
  SetLength(Result.CashRegisters, Count);
  Index := 0;
  // Денежные регистры
  for i := MinCashReg to MaxCashReg do
  begin
    Driver.RegisterNumber := i;
    Check(Driver.GetCashReg);
    Result.CashRegisters[Index].Number := i;
    Result.CashRegisters[Index].Name := Driver.NameCashReg;
    Result.CashRegisters[Index].Value := Driver.ContentsOfCashRegister;
    Inc(Index);
  end;
  // Расширенные денежные регистры
  for i := MinCashReg2 to MaxCashReg2 do
  begin
    Driver.RegisterNumber := i;
    Check(Driver.GetCashRegEx);
    Result.CashRegisters[Index].Number := i;
    Result.CashRegisters[Index].Name := Driver.NameCashRegEx;
    Result.CashRegisters[Index].Value := Driver.ContentsOfCashRegister;
    Inc(Index);
  end;
  // Операционные регистры
  Index := 0;
  Count := MaxOperationReg - MinOperationReg + 1;
  SetLength(Result.OperationRegisters, Count);
  for i := MinOperationReg to MaxOperationReg do
  begin
    Driver.RegisterNumber := i;
    Check(Driver.GetOperationReg);
    Result.OperationRegisters[Index].Number := i;
    Result.OperationRegisters[Index].Name := Driver.NameOperationReg;
    Result.OperationRegisters[Index].Value := Driver.ContentsOfOperationRegister;
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


end.
