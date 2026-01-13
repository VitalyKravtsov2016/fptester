unit ReceiptTest;

interface

uses
  // VCL
  Classes, SysUtils, System.Generics.Collections, Math,
  // 3'd
  VSoft.YAML,
  // This
  DriverTest, FptrTypes, LogFile;

type
  TTestCommand = class
  public
    Name: string;
    Text: string;
    procedure Load(node: IYAMLValue); virtual;
    procedure Execute(Context: TDriverContext); virtual; abstract;
  end;
  TTestCommands = TObjectList<TTestCommand>;

  { TOpenReceiptCommand }

  TOpenReceiptCommand = class(TTestCommand)
  public
    CheckType: Integer;
    procedure Load(node: IYAMLValue); override;
    procedure Execute(Context: TDriverContext); override;
  end;

  { TFNOperationCommand }

  TFNOperationCommand = class(TTestCommand)
  public
    CheckType: Integer;
    Quantity: Double;
    Price: Currency;
    Summ1: Currency;
    TaxValue: Currency;
    Tax: Integer;
    Department: Integer;
    PaymentTypeSign: Integer;
    PaymentItemSign: Integer;
    PrintString: string;

    procedure Load(node: IYAMLValue); override;
    procedure Execute(Context: TDriverContext); override;
  end;

  { TFNCloseCheckEx3Command }

  TFNCloseCheckEx3Command = class(TTestCommand)
  public
    Summ1: Currency;
    Summ2: Currency;
    Summ3: Currency;
    Summ4: Currency;
    Summ5: Currency;
    Summ6: Currency;
    Summ7: Currency;
    Summ8: Currency;
    Summ9: Currency;
    Summ10: Currency;
    Summ11: Currency;
    Summ12: Currency;
    Summ13: Currency;
    Summ14: Currency;
    Summ15: Currency;
    Summ16: Currency;
    RoundingSumm: Integer;
    TaxValue1: Currency;
    TaxValue2: Currency;
    TaxValue3: Currency;
    TaxValue4: Currency;
    TaxValue5: Currency;
    TaxValue6: Currency;
    TaxValue7: Currency;
    TaxValue8: Currency;
    TaxValue9: Currency;
    TaxValue10: Currency;
    TaxType: Integer;
    StringForPrinting: string;

    procedure Load(node: IYAMLValue); override;
    procedure Execute(Context: TDriverContext); override;
  end;

  { TSaveStateCommand }

  TSaveStateCommand = class(TTestCommand)
  public
    StateName: string;
    procedure Load(node: IYAMLValue); override;
    procedure Execute(Context: TDriverContext); override;
  end;

  { TCompareStateCommand }

  TCompareStateCommand = class(TTestCommand)
  private
    FOperRegs: TModifiedOperRegs;
    FCashRegs: TModifiedCashRegs;
  public
    StateName: string;
    update_state: Boolean;
    procedure Load(node: IYAMLValue); override;
    procedure Execute(Context: TDriverContext); override;
    procedure CompareOperRegs(Context: TDriverContext; const OperRegs1, OperRegs2: TOperRegs);
    procedure CompareCashRegs(Context: TDriverContext; const CashRegs1, CashRegs2: TCashRegs);

    property OperRegs: TModifiedOperRegs read FOperRegs;
    property CashRegs: TModifiedCashRegs read FCashRegs;
  end;

  // Проверка, что чековые регистры обнулены

  { TCheckZeroReceiptRegsCommand }

  TCheckZeroReceiptRegsCommand = class(TTestCommand)
  public
    procedure Execute(Context: TDriverContext); override;
  end;

  { TFNReadLastReceiptCommand }

  TFNReadLastReceiptCommand = class(TTestCommand)
  public
    ErrorFile: string;
    ReceiptFile: string;
    procedure Load(node: IYAMLValue); override;
    procedure Execute(Context: TDriverContext); override;
  end;


  { TReceiptTest }

  TReceiptTest = class(TDriverTest)
  private
    FCommands: TTestCommands;
  public
    constructor CreateTest(AContext: TDriverContext); override;
    destructor Destroy; override;
    function Execute: TTestResult; override;
    procedure Load(node: IYAMLValue); override;

    property Commands: TTestCommands read FCommands;
  end;

implementation

function ReadFileData(const FileName: string): string;
var
  Buffer: TBytes;
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    SetLength(Buffer, Stream.Size);
    Stream.Read(Buffer, 0, Stream.Size);
    Result := TEncoding.UTF8.GetString(Buffer, 0, Length(Buffer));
  finally
    Stream.Free;
  end;
end;

procedure WriteFileData(const FileName, Data: string);
var
  Buffer: TBytes;
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    Buffer := TEncoding.UTF8.GetBytes(Data);
    Stream.WriteBuffer(Buffer, Length(Buffer));
  finally
    Stream.Free;
  end;
end;


function StrToCheckType(RecType: string): Integer;
begin
  Result := FPTR_RECTYPE_SALE;
  if CompareText(RecType, 'sale') = 0 then Result := FPTR_RECTYPE_SALE;
  if CompareText(RecType, 'buy') = 0 then Result := FPTR_RECTYPE_BUY;
  if CompareText(RecType, 'retbuy') = 0 then Result := FPTR_RECTYPE_RETBUY;
  if CompareText(RecType, 'retsale') = 0 then Result := FPTR_RECTYPE_RETSALE;
end;

{ TReceiptTest }

constructor TReceiptTest.CreateTest(AContext: TDriverContext);
begin
  inherited CreateTest(AContext);
  FCommands := TTestCommands.Create;
end;

destructor TReceiptTest.Destroy;
begin
  FCommands.Free;
  inherited Destroy;
end;

procedure TReceiptTest.Load(node: IYAMLValue);
var
  i: Integer;
  Value: IYAMLValue;
  regnode: IYAMLValue;
  itmnode: IYAMLValue;
  Sequence: IYAMLSequence;
  Operation: string;
  Command: TTestCommand;
begin
  inherited Load(node);
  regnode := node.GetValue('commands');
  if (not regnode.IsNull)and(regnode.IsSequence) then
  begin
    Sequence := regnode.AsSequence;
    if Sequence.Count > 0 then
    begin
      for i := 0 to Sequence.Count-1 do
      begin
        Command := nil;
        itmnode := Sequence.Items[i];
        Operation := itmnode.GetValue('type').AsString;
        if Operation = 'open_receipt' then
        begin
          Command := TOpenReceiptCommand.Create;
        end;
        if Operation = 'close_receipt3' then
        begin
          Command := TFNCloseCheckEx3Command.Create;
        end;
        if Operation = 'fnoperation' then
        begin
          Command := TFNOperationCommand.Create;
        end;
        if Operation = 'save_state' then
        begin
          Command := TSaveStateCommand.Create;
        end;
        if Operation = 'compare_state' then
        begin
          Command := TCompareStateCommand.Create;
        end;
        if Operation = 'check_zero_receipt_regs' then
        begin
          Command := TCheckZeroReceiptRegsCommand.Create;
        end;
        if Operation = 'fn_read_last_receipt' then
        begin
          Command := TFNReadLastReceiptCommand.Create;
        end;


        if command <> nil then
        begin
          Commands.Add(Command);
          Command.Load(itmnode);
        end;
     end;
    end;
  end;
end;

function TReceiptTest.Execute: TTestResult;
var
  Command: TTestCommand;
begin
  Result.Text := '';
  Result.IsSucceeded := True;

  Context.CheckEcrMode(FPTR_MODE_24NOTOVER);
  for Command in Commands do
  begin
    Command.Execute(Context);
  end;
end;

{ TTestCommand }

procedure TTestCommand.Load(node: IYAMLValue);
var
  Value: IYAMLValue;
begin
  if node.TryGetValue('name', Value) then
    name := Value.AsString;
  if node.TryGetValue('text', Value) then
    text := Value.AsString;
end;

{ TOpenReceiptCommand }

procedure TOpenReceiptCommand.Load(node: IYAMLValue);
begin
  inherited Load(node);
  CheckType := node.GetValue('check_type').AsInteger;
end;

procedure TOpenReceiptCommand.Execute(Context: TDriverContext);
begin
  Context.Driver.CheckType := CheckType;
  Context.Check(Context.Driver.OpenCheck);
  Context.Check(Context.Driver.WaitForPrinting);
  Context.CheckEcrMode(FPTR_MODE_REC);
end;

{ TFNOperationCommand }

procedure TFNOperationCommand.Execute(Context: TDriverContext);
begin
  Context.Driver.CheckType := CheckType;
  Context.Driver.Quantity := Quantity;
  Context.Driver.Price := Price;
  Context.Driver.Summ1 := Summ1;
  Context.Driver.TaxValue := TaxValue;
  Context.Driver.Tax1 := Tax;
  Context.Driver.Department := Department;
  Context.Driver.PaymentTypeSign := PaymentTypeSign;
  Context.Driver.PaymentItemSign := PaymentItemSign;
  Context.Driver.StringForPrinting := PrintString;
  Context.Check(Context.Driver.FNOperation);
end;

procedure TFNOperationCommand.Load(node: IYAMLValue);
begin
  inherited Load(node);
  CheckType := node.GetValue('check_type').AsInteger;
  Quantity := node.GetValue('quantity').AsFloat;
  Price := node.GetValue('price').AsFloat;
  Summ1 := node.GetValue('summ1').AsFloat;
  TaxValue := node.GetValue('tax_value').AsFloat;
  Tax := node.GetValue('tax').AsInteger;
  Department := node.GetValue('department').AsInteger;
  PaymentTypeSign := node.GetValue('payment_type_sign').AsInteger;
  PaymentItemSign := node.GetValue('payment_item_sign').AsInteger;
  PrintString := node.GetValue('text').AsString;
end;

{ TSaveStateCommand }

procedure TSaveStateCommand.Load(node: IYAMLValue);
begin
  inherited Load(node);
  StateName := node.GetValue('state_name').AsString;
end;

procedure TSaveStateCommand.Execute(Context: TDriverContext);
begin
  Context.SaveState(StateName);
end;

{ TCompareStateCommand }

procedure TCompareStateCommand.Load(node: IYAMLValue);
var
  i: Integer;
  Value: IYAMLValue;
  regnode: IYAMLValue;
  itmnode: IYAMLValue;
  Sequence: IYAMLSequence;
  Operation: string;
  Command: TTestCommand;
begin
  StateName := node.GetValue('state_name').AsString;
  update_state := False;
  if node.TryGetValue('update_state', Value) then
    update_state := Value.AsBoolean;

  // Операционные регистры
  regnode := node.GetValue('operation_registers');
  if (not regnode.IsNull)and(regnode.IsSequence) then
  begin
    Sequence := regnode.AsSequence;
    if Sequence.Count > 0 then
    begin
      SetLength(FCashRegs, Sequence.Count);
      for i := 0 to Sequence.Count-1 do
      begin
        itmnode := Sequence.Items[i];
        FCashRegs[i].Number := itmnode.GetValue('number').AsInteger;

        if itmnode.TryGetValue('name', Value) then
        FCashRegs[i].Name := Value.AsString;

        FCashRegs[i].Delta := 0;
        if itmnode.TryGetValue('delta', Value) then
          FCashRegs[i].Delta := Value.AsInteger;

        FCashRegs[i].Value := -1;
        if itmnode.TryGetValue('value', Value) then
          FCashRegs[i].Value := Value.AsInteger;
      end;
    end;
  end;
  // Денежные регистры
  regnode := node.GetValue('cash_registers');
  if (not regnode.IsNull)and(regnode.IsSequence) then
  begin
    Sequence := regnode.AsSequence;
    if Sequence.Count > 0 then
    begin
      SetLength(FCashRegs, Sequence.Count);
      for i := 0 to Sequence.Count-1 do
      begin
        itmnode := Sequence.Items[i];
        FCashRegs[i].Number := itmnode.GetValue('number').AsInteger;

        if itmnode.TryGetValue('name', Value) then
        FCashRegs[i].Name := Value.AsString;

        FCashRegs[i].Delta := 0;
        if itmnode.TryGetValue('delta', Value) then
          FCashRegs[i].Delta := Value.AsFloat;

        FCashRegs[i].Value := -1;
        if itmnode.TryGetValue('value', Value) then
          FCashRegs[i].Value := Value.AsFloat;
      end;
    end;
  end;
end;

procedure TCompareStateCommand.Execute(Context: TDriverContext);
var
  State1: TFiscalPrinterState;
  State2: TFiscalPrinterState;
begin
  if not Context.GetPrinterState(StateName, State1) then
    raise Exception.CreateFmt('Состояние "%s" не найдено', [StateName]);

  State2 := Context.ReadPrinterState;
  CompareCashRegs(Context, State1.CashRegs, State2.CashRegs);
  CompareCashRegs(Context, State1.CashRegs, State2.CashRegs);
  if update_state then
  begin
    Context.SetPrinterState(StateName, State2);
  end;
end;

procedure TCompareStateCommand.CompareOperRegs(Context: TDriverContext;
  const OperRegs1, OperRegs2: TOperRegs);

  function FindOperReg(Number: Integer; var R: TModifiedOperReg): Boolean;
  var
    OperReg: TModifiedOperReg;
  begin
    Result := False;
    for OperReg in FOperRegs do
    begin
      Result := OperReg.Number = Number;
      if Result then
      begin
        R := OperReg;
        Break;
      end;
    end;
  end;

var
  i: Integer;
  Count: Integer;
  OperReg1: TOperReg;
  OperReg2: TOperReg;
  OperReg: TModifiedOperReg;
begin
  for i := 0 to Length(OperRegs1)-1 do
  begin
    OperReg1 := OperRegs1[i];
    OperReg2 := OperRegs2[i];
    if OperReg1.Number <> OperReg2.Number then
    begin
      Context.Error('Command name: ' + Name);
      Context.Error('Номера регистров не соответствуют');
      Context.Error(Format('Операционный регистр: %d, %s', [OperReg1.Number, OperReg1.Name]));
      Context.Error(Format('Ожидается номер: %d, получен %d', [
        OperReg1.Number, OperReg2.Number]));

      raise Exception.CreateFmt('Номера регистров не соответствуют, %d <> %d',[
        OperReg1.Number, OperReg2.Number]);
    end;
    if OperReg1.Value <> OperReg2.Value then
    begin
      if not FindOperReg(OperReg1.Number, OperReg) then
      begin
        Context.Error('Command name: ' + Name);
        Context.Error(Format('Операционный регистр: %d, %s', [OperReg1.Number, OperReg1.Name]));
        raise Exception.CreateFmt('Значения регистров не равны, %d <> %d',[
          OperReg1.Value, OperReg2.Value]);
      end else
      begin
        if OperReg.Delta <> 0 then
        begin
          if OperReg2.Value <> (OperReg1.Value + OperReg.Delta) then
          begin
            Context.Error('Command name: ' + Name);
            Context.Error(Format('Операционный регистр: %d, %s', [OperReg1.Number, OperReg1.Name]));
            raise Exception.CreateFmt('Значения регистров не равны, %d <> %d',[
              OperReg2.Value, OperReg1.Value + OperReg.Delta]);
          end;
        end;
        if OperReg.Value <> -1 then
        begin
          if OperReg2.Value <> OperReg.Value then
          begin
            Context.Error('Command name: ' + Name);
            Context.Error(Format('Операционный регистр: %d, %s', [OperReg1.Number, OperReg1.Name]));
            raise Exception.CreateFmt('Значения регистров не равны, %d <> %d',[
              OperReg2.Value, OperReg.Value]);
          end;
        end;
      end;
    end;
  end;
end;

procedure TCompareStateCommand.CompareCashRegs(Context: TDriverContext;
  const CashRegs1, CashRegs2: TCashRegs);

  function FindCashReg(Number: Integer; var R: TModifiedCashReg): Boolean;
  var
    CashReg: TModifiedCashReg;
  begin
    Result := False;
    for CashReg in FCashRegs do
    begin
      Result := CashReg.Number = Number;
      if Result then
      begin
        R := CashReg;
        Break;
      end;
    end;
  end;

var
  i: Integer;
  Count: Integer;
  CashReg1: TCashReg;
  CashReg2: TCashReg;
  CashReg: TModifiedCashReg;
begin
  for i := 0 to Length(CashRegs1)-1 do
  begin
    CashReg1 := CashRegs1[i];
    CashReg2 := CashRegs2[i];
    if CashReg1.Number <> CashReg2.Number then
    begin
      Context.Error('Command name: ' + Name);
      Context.Error('Номера регистров не соответствуют');
      Context.Error(Format('Денежный регистр: %d, %s', [CashReg1.Number, CashReg1.Name]));
      Context.Error(Format('Ожидается номер: %d, получен %d', [
        CashReg1.Number, CashReg2.Number]));

      raise Exception.CreateFmt('Номера регистров не соответствуют, %d <> %d',[
        CashReg1.Number, CashReg2.Number]);
    end;
    if CashReg1.Value <> CashReg2.Value then
    begin
      if not FindCashReg(CashReg1.Number, CashReg) then
      begin
        Context.Error('Command name: ' + Name);
        Context.Error(Format('Денежный регистр: %d, %s', [CashReg1.Number, CashReg1.Name]));
        raise Exception.CreateFmt('Значения регистров не равны, %.6f <> %.6f',[
          CashReg1.Value, CashReg2.Value]);
      end else
      begin
        if CashReg.Delta <> 0 then
        begin
          if CashReg2.Value <> (CashReg1.Value + CashReg.Delta) then
          begin
            Context.Error('Command name: ' + Name);
            Context.Error(Format('Денежный регистр: %d, %s', [CashReg1.Number, CashReg1.Name]));
            raise Exception.CreateFmt('Значения регистров не равны, %.6f <> %.6f',[
              CashReg2.Value, CashReg1.Value + CashReg.Delta]);
          end;
        end;
        if CashReg.Value <> -1 then
        begin
          if CashReg2.Value <> CashReg.Value then
          begin
            Context.Error('Command name: ' + Name);
            Context.Error(Format('Денежный регистр: %d, %s', [CashReg1.Number, CashReg1.Name]));
            raise Exception.CreateFmt('Значения регистров не равны, %.6f <> %.6f',[
              CashReg2.Value, CashReg.Value]);
          end;
        end;
      end;
    end;
  end;
end;

{ TFNCloseCheckEx3Command }

procedure TFNCloseCheckEx3Command.Load(node: IYAMLValue);
begin
  inherited Load(node);
  Summ1 := node.GetValue('summ1').AsFloat;
  Summ2 := node.GetValue('summ2').AsFloat;
  Summ3 := node.GetValue('summ3').AsFloat;
  Summ4 := node.GetValue('summ4').AsFloat;
  Summ5 := node.GetValue('summ5').AsFloat;
  Summ6 := node.GetValue('summ6').AsFloat;
  Summ7 := node.GetValue('summ7').AsFloat;
  Summ8 := node.GetValue('summ8').AsFloat;
  Summ9 := node.GetValue('summ9').AsFloat;
  Summ10 := node.GetValue('summ10').AsFloat;
  Summ11 := node.GetValue('summ11').AsFloat;
  Summ12 := node.GetValue('summ12').AsFloat;
  Summ13 := node.GetValue('summ13').AsFloat;
  Summ14 := node.GetValue('summ14').AsFloat;
  Summ15 := node.GetValue('summ15').AsFloat;
  Summ16 := node.GetValue('summ16').AsFloat;
  RoundingSumm := node.GetValue('round_summ').AsInteger;
  TaxValue1 := node.GetValue('tax_value1').AsFloat;
  TaxValue2 := node.GetValue('tax_value2').AsFloat;
  TaxValue3 := node.GetValue('tax_value3').AsFloat;
  TaxValue4 := node.GetValue('tax_value4').AsFloat;
  TaxValue5 := node.GetValue('tax_value5').AsFloat;
  TaxValue6 := node.GetValue('tax_value6').AsFloat;
  TaxValue7 := node.GetValue('tax_value7').AsFloat;
  TaxValue8 := node.GetValue('tax_value8').AsFloat;
  TaxValue9 := node.GetValue('tax_value9').AsFloat;
  TaxValue10 := node.GetValue('tax_value10').AsFloat;
  TaxType := node.GetValue('tax_type').AsInteger;
  StringForPrinting := node.GetValue('text').AsString;
end;

procedure TFNCloseCheckEx3Command.Execute(Context: TDriverContext);
begin
  with Context do
  begin
    Driver.Summ1 := Summ1;
    Driver.Summ2 := Summ2;
    Driver.Summ3 := Summ3;
    Driver.Summ4 := Summ4;
    Driver.Summ5 := Summ5;
    Driver.Summ6 := Summ6;
    Driver.Summ7 := Summ7;
    Driver.Summ8 := Summ8;
    Driver.Summ9 := Summ9;
    Driver.Summ10 := Summ10;
    Driver.Summ11 := Summ11;
    Driver.Summ12 := Summ12;
    Driver.Summ13 := Summ13;
    Driver.Summ14 := Summ14;
    Driver.Summ15 := Summ15;
    Driver.Summ16 := Summ16;
    Driver.RoundingSumm := RoundingSumm;
    Driver.TaxValue1 := TaxValue1;
    Driver.TaxValue2 := TaxValue2;
    Driver.TaxValue3 := TaxValue3;
    Driver.TaxValue4 := TaxValue4;
    Driver.TaxValue5 := TaxValue5;
    Driver.TaxValue6 := TaxValue6;
    Driver.TaxValue7 := TaxValue7;
    Driver.TaxValue8 := TaxValue8;
    Driver.TaxValue9 := TaxValue9;
    Driver.TaxValue10 := TaxValue10;
    Driver.TaxType := TaxType;
    Driver.StringForPrinting := StringForPrinting;
    Check(Driver.FNCloseCheckEx3);
    Check(Driver.WaitForPrinting);
  end;
end;

{ TCheckZeroReceiptRegsCommand }

procedure TCheckZeroReceiptRegsCommand.Execute(Context: TDriverContext);
begin
  Context.CheckReceiptOperRegsZero;
  Context.CheckReceiptCashRegsZero;
end;

{ TFNReadLastReceiptCommand }

procedure TFNReadLastReceiptCommand.Load(node: IYAMLValue);
begin
  inherited Load(node);
  ReceiptFile := node.GetValue('receipt_file').AsString;
  ErrorFile := node.GetValue('error_file').AsString;
end;

(*
*)

function IsEqualFNDoc(const Doc1, Doc2: string): Boolean;

  function IsIgnoredTag(const Line: string): Boolean;
  const
    TagsToIgnore: array [0..3] of Integer = (1040, 1012, 1077, 1038);
  var
    Tag: Integer;
  begin
    Result := False;
    for Tag in TagsToIgnore do
    begin
      Result := Pos(IntToStr(Tag), Line) <> 0;
      if Result then Break;
    end;
  end;


var
  i: Integer;
  Count: Integer;
  Lines1: TStrings;
  Lines2: TStrings;
begin
  Result := True;
  Lines1 := TStringList.Create;
  Lines2 := TStringList.Create;
  try
    Lines1.Text := Doc1;
    Lines2.Text := Doc2;
    Count := Min(Lines1.Count, Lines2.Count);
    for i := 0 to Count-1 do
    begin
      if IsIgnoredTag(Lines1[i]) then Continue;
      Result := Trim(Lines1[i]) = Trim(Lines2[i]);
      if not Result then Exit;
    end;
  finally
    Lines1.Free;
    Lines2.Free;
  end;
end;

procedure TFNReadLastReceiptCommand.Execute(Context: TDriverContext);
var
  Doc1: string;
  Doc2: string;
begin
  Doc1 := ReadFileData(Context.Options.FilesPath + ReceiptFile);
  Doc2 := Context.FNReadLastReceipt;
  if not IsEqualFNDoc(Doc1, Doc2) then
  begin
    WriteFileData(Context.Options.FilesPath + ErrorFile, Doc2);

    Logger.Debug('Ожидается документ:' + Doc1);
    Logger.Debug('Получен документ:' + Doc2);
    raise Exception.Create('Текст документов отличается');
  end;
end;


end.
