unit TableTest;

interface

uses
  SysUtils, DateUtils, Classes, System.Generics.Collections,
  VSoft.YAML,
  DriverTest, DrvFRLib_TLB, FrEmuPrinter, BinUtils;

type
  { TTableFieldTest }

  TTableFieldTest = class(TDriverTest)
  private
    FTable: Integer;
    FRow: Integer;
    FField: Integer;
    FIsString: Boolean;
    FPad: Integer;
    FValues: TStringList;
    FPatterns: TStringList;
    FRequireReceipt: Boolean;
    function FormatShown(const Value: string): string;
    function ReceiptHasValue(const Receipt, Value: string): Boolean;
    procedure CheckReceiptValue(const Receipt, Value, Step: string);
    procedure PrintReceipt(out Receipt: string);
    procedure CheckLogicalNumber(const Value: string);
  public
    constructor CreateTest(AContext: TDriverContext); override;
    destructor Destroy; override;
    procedure Load(node: IYAMLValue); override;
    function Execute: TTestResult; override;
  end;

  { TAutoZeroCashTest }

  TAutoZeroCashTest = class(TDriverTest)
  private
    FTable: Integer;
    FRow: Integer;
    FField: Integer;
    FRegister: Integer;
    FAmount: Currency;
    FValues: TStringList;
  public
    constructor CreateTest(AContext: TDriverContext); override;
    destructor Destroy; override;
    procedure Load(node: IYAMLValue); override;
    function Execute: TTestResult; override;
  end;

  { TTableFlagTest }

  TTableFlagKind = (tfAdText, tfGrandTotal, tfDrawer, tfCut,
    tfFracQty, tfDrawerLevel, tfDrawerPulse, tfNearEnd, tfTaxMode,
    tfAutoTime);

  TTableFlagTest = class(TDriverTest)
  private
    FKind: TTableFlagKind;
    FTable: Integer;
    FRow: Integer;
    FField: Integer;
    FTimeTable: Integer;
    FValues: TStringList;
    FAdTable: Integer;
    FAdRows: TList<Integer>;
    FAdText: string;
    FCutMark: string;
    FGrandNeedles: TStringList;
    FOffRise: string;
    FPrevKickN: Integer;
    FPrevWidth: Integer;
    FPrevGap: Integer;
    procedure PrepareIdle;
  public
    constructor CreateTest(AContext: TDriverContext); override;
    destructor Destroy; override;
    procedure Load(node: IYAMLValue); override;
    function Execute: TTestResult; override;
    property Kind: TTableFlagKind read FKind write FKind;
  end;

  { TAllTableFieldsTest }

  TAllTableFieldsTest = class(TDriverTest)
  private
    FSkipTables: TList<Integer>;
    FPrintAll: Boolean;
  public
    constructor CreateTest(AContext: TDriverContext); override;
    destructor Destroy; override;
    procedure Load(node: IYAMLValue); override;
    function Execute: TTestResult; override;
  end;

implementation

const
  AdminPwdHex = '1E 00 00 00';
  ErrFieldReadOnly = 122;

type
  TFieldInfo = record
    Name: string;
    IsString: Boolean;
    Size: Integer;
    MinValue: Int64;
    MaxValue: Int64;
    Valid: Boolean;
  end;

  TTableInfo = record
    Name: string;
    FieldCount: Integer;
    RowCount: Integer;
    Valid: Boolean;
  end;

function FoldOcr(const S: string): string;
begin
  Result := S;
  Result := StringReplace(Result, '_', '-', [rfReplaceAll]);
  Result := StringReplace(Result, 'А', 'A', [rfReplaceAll]);
  Result := StringReplace(Result, 'В', 'B', [rfReplaceAll]);
  Result := StringReplace(Result, 'Е', 'E', [rfReplaceAll]);
  Result := StringReplace(Result, 'К', 'K', [rfReplaceAll]);
  Result := StringReplace(Result, 'М', 'M', [rfReplaceAll]);
  Result := StringReplace(Result, 'Н', 'H', [rfReplaceAll]);
  Result := StringReplace(Result, 'О', 'O', [rfReplaceAll]);
  Result := StringReplace(Result, 'Р', 'P', [rfReplaceAll]);
  Result := StringReplace(Result, 'С', 'C', [rfReplaceAll]);
  Result := StringReplace(Result, 'Т', 'T', [rfReplaceAll]);
  Result := StringReplace(Result, 'Х', 'X', [rfReplaceAll]);
  Result := StringReplace(Result, 'а', 'a', [rfReplaceAll]);
  Result := StringReplace(Result, 'е', 'e', [rfReplaceAll]);
  Result := StringReplace(Result, 'о', 'o', [rfReplaceAll]);
  Result := StringReplace(Result, 'р', 'p', [rfReplaceAll]);
  Result := StringReplace(Result, 'с', 'c', [rfReplaceAll]);
  Result := StringReplace(Result, 'х', 'x', [rfReplaceAll]);
end;

function IsDigitChar(C: Char): Boolean;
begin
  Result := (C >= '0') and (C <= '9');
end;

function ContainsTokenEx(const Text, Token: string): Boolean;
var
  Start, i: Integer;
  BeforeOk, AfterOk: Boolean;
begin
  Result := False;
  if Token = '' then
    Exit;
  Start := 1;
  while Start <= Length(Text) do
  begin
    i := Pos(Token, Copy(Text, Start, MaxInt));
    if i = 0 then
      Exit;
    i := Start + i - 1;
    BeforeOk := (i = 1) or not IsDigitChar(Text[i - 1]);
    AfterOk := (i + Length(Token) > Length(Text)) or
      not IsDigitChar(Text[i + Length(Token)]);
    if BeforeOk and AfterOk then
    begin
      Result := True;
      Exit;
    end;
    Start := i + 1;
  end;
end;

function HexByte(V: Byte): string;
begin
  Result := IntToHex(V, 2);
end;

function HexWordLE(V: Word): string;
begin
  Result := Format('%.2X %.2X', [Lo(V), Hi(V)]);
end;

function TrimZ(const S: string): string;
begin
  Result := Trim(StringReplace(S, #0, ' ', [rfReplaceAll]));
end;

function HasBoxDrawing(const S: string): Boolean;
var
  C: Char;
begin
  for C in S do
    if (Ord(C) >= $2500) and (Ord(C) <= $257F) then
      Exit(True);
  Result := False;
end;

function BytesToCodePage(const Data: TBytes; CodePage: Integer): string;
var
  Enc: TEncoding;
begin
  Enc := TEncoding.GetEncoding(CodePage);
  try
    Result := TrimZ(Enc.GetString(Data));
  finally
    Enc.Free;
  end;
end;

{ Table strings on this firmware are WIN1251. CP866 decode turns
  "ПРОДУКТЫ" into box-drawing (╧╨╬─…). Prefer 1251 when that happens. }
function BytesToTableString(const Data: TBytes): string;
var
  S866, S1251: string;
begin
  S866 := BytesToCodePage(Data, 866);
  S1251 := BytesToCodePage(Data, 1251);
  if HasBoxDrawing(S866) and not HasBoxDrawing(S1251) then
    Result := S1251
  else
    Result := S866;
end;

function StringToTableBytes(const S: string; Size: Integer): TBytes;
var
  Enc: TEncoding;
  Raw: TBytes;
  i: Integer;
begin
  Enc := TEncoding.GetEncoding(1251);
  try
    Raw := Enc.GetBytes(S);
  finally
    Enc.Free;
  end;
  SetLength(Result, Size);
  for i := 0 to Size - 1 do
    Result[i] := 0;
  if Length(Raw) > 0 then
  begin
    if Length(Raw) > Size then
      Move(Raw[0], Result[0], Size)
    else
      Move(Raw[0], Result[0], Length(Raw));
  end;
end;

function BytesToIntLE(const Data: TBytes): Int64;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Length(Data) - 1 do
    Result := Result or (Int64(Data[i]) shl (8 * i));
end;

function IntToBytesLE(Value: Int64; Size: Integer): TBytes;
var
  i: Integer;
begin
  SetLength(Result, Size);
  for i := 0 to Size - 1 do
    Result[i] := Byte((Value shr (8 * i)) and $FF);
end;

function ExchangeCmd(Driver: TDrvFR; const Hex: string; out Payload: TBytes): Integer;
var
  Raw: TBytes;
  SavedConv: Integer;
begin
  SavedConv := Driver.BinaryConversion;
  try
    Driver.BinaryConversion := BINARY_CONVERSION_HEX;
    Driver.TransferBytes := Hex;
    Result := Driver.ExchangeBytes;
    Raw := HexToBytes(Driver.TransferBytes);
    if Length(Raw) >= 2 then
      Payload := Copy(Raw, 2, Length(Raw) - 2)
    else
      SetLength(Payload, 0);
  finally
    Driver.BinaryConversion := SavedConv;
  end;
end;

function GetTableInfo(Driver: TDrvFR; Table: Integer; out Info: TTableInfo): Integer;
var
  Payload: TBytes;
  Hex: string;
begin
  Info := Default(TTableInfo);
  Hex := Format('2D %s %s', [AdminPwdHex, HexByte(Table)]);
  Result := ExchangeCmd(Driver, Hex, Payload);
  if Result <> 0 then
    Exit;
  if Length(Payload) < 44 then
  begin
    Result := 51;
    Exit;
  end;
  Info.Name := BytesToTableString(Copy(Payload, 0, 40));
  Info.FieldCount := Payload[40] or (Payload[41] shl 8);
  Info.RowCount := Payload[42] or (Payload[43] shl 8);
  Info.Valid := True;
end;

function GetFieldInfo(Driver: TDrvFR; Table, Field: Integer; out Info: TFieldInfo): Integer;
var
  Payload: TBytes;
  Hex: string;
  MinB, MaxB: TBytes;
begin
  Info := Default(TFieldInfo);
  Hex := Format('2E %s %s %s', [AdminPwdHex, HexByte(Table), HexByte(Field)]);
  Result := ExchangeCmd(Driver, Hex, Payload);
  if Result <> 0 then
    Exit;
  if Length(Payload) < 42 then
  begin
    Result := 51;
    Exit;
  end;
  Info.Name := BytesToTableString(Copy(Payload, 0, 40));
  Info.IsString := Payload[40] <> 0;
  Info.Size := Payload[41];
  if Info.Size <= 0 then
    Info.Size := 1;
  if (not Info.IsString) and (Length(Payload) >= 42 + Info.Size * 2) then
  begin
    MinB := Copy(Payload, 42, Info.Size);
    MaxB := Copy(Payload, 42 + Info.Size, Info.Size);
    Info.MinValue := BytesToIntLE(MinB);
    Info.MaxValue := BytesToIntLE(MaxB);
  end;
  Info.Valid := True;
end;

function ReadFieldRaw(Driver: TDrvFR; Table, Row, Field: Integer;
  out Data: TBytes): Integer;
var
  Hex: string;
begin
  Hex := Format('1F %s %s %s %s',
    [AdminPwdHex, HexByte(Table), HexWordLE(Row), HexByte(Field)]);
  Result := ExchangeCmd(Driver, Hex, Data);
end;

function WriteFieldRaw(Driver: TDrvFR; Table, Row, Field: Integer;
  const Data: TBytes): Integer;
var
  Hex: string;
  Dummy: TBytes;
begin
  Hex := Format('1E %s %s %s %s %s',
    [AdminPwdHex, HexByte(Table), HexWordLE(Row), HexByte(Field), BytesToHex(Data)]);
  Result := ExchangeCmd(Driver, Hex, Dummy);
end;

function ReadFieldValue(Driver: TDrvFR; Table, Row, Field: Integer;
  const Info: TFieldInfo): string;
var
  Data: TBytes;
  Rc: Integer;
begin
  Rc := ReadFieldRaw(Driver, Table, Row, Field, Data);
  if Rc <> 0 then
    raise Exception.CreateFmt('ReadTable %d/%d/%d: %d', [Table, Row, Field, Rc]);
  if Info.IsString then
    Result := BytesToTableString(Data)
  else
    Result := IntToStr(BytesToIntLE(Data));
end;

function WriteFieldValueCode(Driver: TDrvFR; Table, Row, Field: Integer;
  const Info: TFieldInfo; const Value: string): Integer;
var
  Data: TBytes;
begin
  if Info.IsString then
    Data := StringToTableBytes(Value, Info.Size)
  else
    Data := IntToBytesLE(StrToInt64(Value), Info.Size);
  Result := WriteFieldRaw(Driver, Table, Row, Field, Data);
end;

procedure WriteFieldValue(Driver: TDrvFR; Table, Row, Field: Integer;
  const Info: TFieldInfo; const Value: string);
var
  Rc: Integer;
begin
  Rc := WriteFieldValueCode(Driver, Table, Row, Field, Info, Value);
  if Rc <> 0 then
    raise Exception.CreateFmt('WriteTable %d/%d/%d: %d', [Table, Row, Field, Rc]);
end;

function SameFieldValue(const Info: TFieldInfo; const A, B: string): Boolean;
begin
  if Info.IsString then
    Result := A = B
  else
  begin
    if (Trim(A) = '') or (Trim(B) = '') then
      Result := Trim(A) = Trim(B)
    else
      Result := StrToInt64(A) = StrToInt64(B);
  end;
end;

function FieldOnReceipt(Table, Field: Integer): Boolean;
begin
  Result :=
    ((Table = 1) and (Field = 1)) or
    (Table = 4) or
    ((Table = 5) and (Field = 1)) or
    ((Table = 6) and (Field = 2)) or
    ((Table = 7) and (Field = 1)) or
    ((Table = 18) and ((Field = 7) or (Field = 8)));
end;

function SkipFieldName(const FieldName: string): Boolean;
var
  S: string;
begin
  S := AnsiLowerCase(FieldName);
  Result := (Pos('парол', S) > 0) or (Pos('passwd', S) > 0) or
    (Pos('password', S) > 0) or (Pos('ффд', S) > 0) or (Pos('ffd', S) > 0) or
    (Pos('формат фд', S) > 0) or (Pos('обмен', S) > 0);
end;

function PickIntValues(MinV, MaxV: Int64; out A, B: string): Boolean;
begin
  Result := False;
  if MaxV <= MinV then
    Exit;
  if (MinV <= 7) and (MaxV >= 99) then
  begin
    A := '7';
    B := '99';
    Result := True;
    Exit;
  end;
  A := IntToStr(MinV);
  B := IntToStr(MaxV);
  if (MaxV - MinV >= 2) and (MinV + 1 <> MaxV) then
    A := IntToStr(MinV + 1);
  Result := A <> B;
end;

procedure FillTestStrings(Table, Row, Field, Size: Integer; out A, B: string);
begin
  A := Format('A%d_%d_%d', [Table, Row, Field]);
  B := Format('B%d_%d_%d', [Table, Row, Field]);
  if Length(A) > Size then
    A := Copy(A, 1, Size);
  if Length(B) > Size then
    B := Copy(B, 1, Size);
end;

function FallbackFieldInfo(IsString: Boolean): TFieldInfo;
begin
  Result := Default(TFieldInfo);
  Result.IsString := IsString;
  if IsString then
    Result.Size := 40
  else
    Result.Size := 1;
  Result.Valid := True;
end;

{ TTableFieldTest }

constructor TTableFieldTest.CreateTest(AContext: TDriverContext);
begin
  inherited CreateTest(AContext);
  FValues := TStringList.Create;
  FPatterns := TStringList.Create;
  FPad := 0;
  FTable := 1;
  FRow := 1;
  FField := 1;
  FRequireReceipt := True;
end;

destructor TTableFieldTest.Destroy;
begin
  FValues.Free;
  FPatterns.Free;
  inherited Destroy;
end;

procedure TTableFieldTest.Load(node: IYAMLValue);
var
  value: IYAMLValue;
  seq: IYAMLSequence;
  i: Integer;
  kind: string;
begin
  inherited Load(node);
  FTable := node.GetValue('table').AsInteger;
  FRow := node.GetValue('row').AsInteger;
  FField := node.GetValue('field').AsInteger;

  FIsString := False;
  if node.TryGetValue('field_type', value) then
  begin
    kind := LowerCase(value.AsString);
    FIsString := (kind = 'string') or (kind = 'str');
  end;

  FPad := 0;
  if node.TryGetValue('pad', value) then
    FPad := value.AsInteger;

  FRequireReceipt := True;
  if node.TryGetValue('check_receipt', value) then
    FRequireReceipt := value.AsBoolean;

  FValues.Clear;
  if node.TryGetValue('values', value) and value.IsSequence then
  begin
    seq := value.AsSequence;
    for i := 0 to seq.Count - 1 do
      FValues.Add(Trim(seq.Items[i].AsString));
  end;

  FPatterns.Clear;
  if node.TryGetValue('patterns', value) and value.IsSequence then
  begin
    seq := value.AsSequence;
    for i := 0 to seq.Count - 1 do
      FPatterns.Add(seq.Items[i].AsString);
  end;
end;

function TTableFieldTest.FormatShown(const Value: string): string;
var
  n: Integer;
begin
  Result := Value;
  if (not FIsString) and (FPad > Length(Value)) then
  begin
    n := FPad - Length(Value);
    Result := StringOfChar('0', n) + Value;
  end;
end;

function TTableFieldTest.ReceiptHasValue(const Receipt, Value: string): Boolean;
var
  i: Integer;
  Shown, Needle, Norm: string;
begin
  Norm := FoldOcr(NormalizePrintedText(Receipt));
  Shown := FoldOcr(FormatShown(Value));
  if FPatterns.Count = 0 then
  begin
    Result := ContainsTokenEx(Norm, Shown);
    if (not Result) and (Shown <> Value) then
      Result := ContainsTokenEx(Norm, Value);
    Exit;
  end;
  for i := 0 to FPatterns.Count - 1 do
  begin
    Needle := FoldOcr(StringReplace(FPatterns[i], '{value}', FormatShown(Value), [rfReplaceAll]));
    Needle := StringReplace(Needle, '{raw}', FoldOcr(Value), [rfReplaceAll]);
    if (Needle <> '') and (Pos(Needle, Norm) > 0) then
    begin
      Result := True;
      Exit;
    end;
    if Shown <> Value then
    begin
      Needle := StringReplace(FPatterns[i], '{value}', Value, [rfReplaceAll]);
      Needle := StringReplace(Needle, '{raw}', Value, [rfReplaceAll]);
      if (Needle <> '') and (Pos(Needle, Norm) > 0) then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;
  Result := ContainsTokenEx(Norm, Shown);
end;

procedure TTableFieldTest.CheckReceiptValue(const Receipt, Value, Step: string);
begin
  if ReceiptHasValue(Receipt, Value) then
    Exit;
  raise Exception.CreateFmt(
    '%s: value "%s" not found in receipt text (table %d row %d field %d). Receipt:%s%s',
    [Step, FormatShown(Value), FTable, FRow, FField, sLineBreak, Receipt]);
end;

procedure TTableFieldTest.PrintReceipt(out Receipt: string);
var
  Dept, Pay, Tax: Integer;
begin
  Dept := 1;
  Pay := 1;
  Tax := 6;
  if FTable = 7 then
    Dept := FRow;
  if FTable = 5 then
    Pay := FRow;
  if FTable = 6 then
    Tax := FRow;
  Driver.BinaryConversion := BINARY_CONVERSION_NONE;
  Driver.TransferBytes := '';
  Context.ClearPrintedText;
  Context.PrintSimpleSaleReceipt(Dept, Pay, Tax);
  Receipt := Context.WaitPrintedText;
  Context.Debug('Receipt:' + sLineBreak + Receipt);
end;

procedure TTableFieldTest.CheckLogicalNumber(const Value: string);
var
  N, Rc: Integer;
begin
  if (FTable <> 1) or (FField <> 1) then
    Exit;
  N := StrToIntDef(Value, -1);
  if N < 0 then
    Exit;
  Rc := Driver.GetECRStatus;
  Context.Debug(Format('GetECRStatus after write: rc=%d logical=%d',
    [Rc, Driver.LogicalNumber]));
  if Rc <> 0 then
    raise Exception.CreateFmt('GetECRStatus %d, %s',
      [Rc, Driver.ResultCodeDescription]);
  if Driver.LogicalNumber <> N then
    raise Exception.CreateFmt('LogicalNumber=%d, table value=%d',
      [Driver.LogicalNumber, N]);
end;

function TTableFieldTest.Execute: TTestResult;
var
  Orig, ReadBack, Receipt, Prev: string;
  Info: TFieldInfo;
  i, Rc: Integer;
begin
  Result.IsSucceeded := False;
  Result.Text := '';
  try
    if FValues.Count < 2 then
      raise Exception.Create('table_field needs at least two values');

    Rc := GetFieldInfo(Driver, FTable, FField, Info);
    if Rc <> 0 then
      Info := FallbackFieldInfo(FIsString);
    FIsString := Info.IsString;

    Orig := ReadFieldValue(Driver, FTable, FRow, FField, Info);
    Context.Debug(Format('Table %d/%d/%d original=%s',
      [FTable, FRow, FField, Orig]));
    Driver.BinaryConversion := BINARY_CONVERSION_NONE;

    try
      Prev := '';
      for i := 0 to FValues.Count - 1 do
      begin
        if FValues[i] = '' then
          Continue;
        WriteFieldValue(Driver, FTable, FRow, FField, Info, FValues[i]);
        Context.Debug(Format('wrote %s', [FValues[i]]));
        ReadBack := ReadFieldValue(Driver, FTable, FRow, FField, Info);
        Context.Debug(Format('read %s', [ReadBack]));
        if not SameFieldValue(Info, ReadBack, FValues[i]) then
          raise Exception.CreateFmt('WriteTable failed: wrote %s, read %s',
            [FValues[i], ReadBack]);
        Driver.BinaryConversion := BINARY_CONVERSION_NONE;
        Driver.TransferBytes := '';
        CheckLogicalNumber(FValues[i]);

        Context.Debug('print receipt');
        PrintReceipt(Receipt);
        if FRequireReceipt then
          CheckReceiptValue(Receipt, FValues[i], Format('value[%d]', [i]));

        if (Prev <> '') and (Pos(FormatShown(Prev), FormatShown(FValues[i])) = 0) and
          (Pos(FormatShown(FValues[i]), FormatShown(Prev)) = 0) then
        begin
          if FRequireReceipt and ReceiptHasValue(Receipt, Prev) then
            raise Exception.CreateFmt(
              'Old value "%s" is still on the receipt after writing "%s"',
              [Prev, FValues[i]]);
        end;
        Prev := FValues[i];
      end;
    finally
      try
        Driver.SysAdminCancelCheck;
      except
      end;
      try
        WriteFieldValue(Driver, FTable, FRow, FField, Info, Orig);
      except
      end;
      Driver.BinaryConversion := BINARY_CONVERSION_NONE;
    end;

    Result.IsSucceeded := True;
  except
    on E: Exception do
    begin
      Result.IsSucceeded := False;
      Result.Text := E.Message;
    end;
  end;
end;

{ TAllTableFieldsTest }

constructor TAllTableFieldsTest.CreateTest(AContext: TDriverContext);
begin
  inherited CreateTest(AContext);
  FSkipTables := TList<Integer>.Create;
  FPrintAll := False;
end;

destructor TAllTableFieldsTest.Destroy;
begin
  FSkipTables.Free;
  inherited Destroy;
end;

procedure TAllTableFieldsTest.Load(node: IYAMLValue);
var
  value: IYAMLValue;
  seq: IYAMLSequence;
  i: Integer;
begin
  inherited Load(node);
  FSkipTables.Clear;
  FSkipTables.Add(2);
  FSkipTables.Add(3);
  FSkipTables.Add(16);
  if node.TryGetValue('skip_tables', value) and value.IsSequence then
  begin
    seq := value.AsSequence;
    for i := 0 to seq.Count - 1 do
      if not FSkipTables.Contains(seq.Items[i].AsInteger) then
        FSkipTables.Add(seq.Items[i].AsInteger);
  end;
  FPrintAll := False;
  if node.TryGetValue('print_all', value) then
    FPrintAll := value.AsBoolean;
end;

function TAllTableFieldsTest.Execute: TTestResult;
var
  Table, Field, Row: Integer;
  Tbl: TTableInfo;
  Fld: TFieldInfo;
  Rc, Tested, Failed, Skipped: Integer;
  Orig, A, B, ReadBack, Receipt: string;
  Dept, Pay, Tax: Integer;
  RequireRcpt: Boolean;
  FailText: TStringList;

  procedure RestoreOrig;
  begin
    try
      WriteFieldValue(Driver, Table, Row, Field, Fld, Orig);
    except
    end;
  end;

  procedure DoPrint;
  begin
    Dept := 1;
    Pay := 1;
    Tax := 6;
    if Table = 7 then
      Dept := Row;
    if Table = 5 then
      Pay := Row;
    if Table = 6 then
      Tax := Row;
    Context.ClearPrintedText;
    Context.PrintSimpleSaleReceipt(Dept, Pay, Tax);
    Receipt := Context.WaitPrintedText;
  end;

  function ValueOnReceipt(const Value: string): Boolean;
  begin
    Result := ContainsTokenEx(FoldOcr(NormalizePrintedText(Receipt)), FoldOcr(Value));
  end;

begin
  Result.IsSucceeded := False;
  Result.Text := '';
  Tested := 0;
  Failed := 0;
  Skipped := 0;
  FailText := TStringList.Create;
  try
    for Table := 1 to 24 do
    begin
      if FSkipTables.Contains(Table) then
      begin
        Inc(Skipped);
        Continue;
      end;
      Rc := GetTableInfo(Driver, Table, Tbl);
      if (Rc <> 0) or (not Tbl.Valid) or (Tbl.FieldCount <= 0) or (Tbl.RowCount <= 0) then
        Continue;
      Context.Debug(Format('Table %d "%s" fields=%d rows=%d',
        [Table, Tbl.Name, Tbl.FieldCount, Tbl.RowCount]));

      for Field := 1 to Tbl.FieldCount do
      begin
        Rc := GetFieldInfo(Driver, Table, Field, Fld);
        if (Rc <> 0) or (not Fld.Valid) then
        begin
          Inc(Skipped);
          Continue;
        end;
        if SkipFieldName(Fld.Name) then
        begin
          Context.Debug(Format('skip %d/%d "%s"', [Table, Field, Fld.Name]));
          Inc(Skipped);
          Continue;
        end;

        for Row := 1 to Tbl.RowCount do
        begin
          if Fld.IsString then
          begin
            FillTestStrings(Table, Row, Field, Fld.Size, A, B);
            if A = B then
            begin
              Inc(Skipped);
              Continue;
            end;
          end
          else if not PickIntValues(Fld.MinValue, Fld.MaxValue, A, B) then
          begin
            A := '';
            B := '';
          end;

          try
            Orig := ReadFieldValue(Driver, Table, Row, Field, Fld);
          except
            on E: Exception do
            begin
              Inc(Skipped);
              Context.Debug(Format('read skip %d/%d/%d: %s',
                [Table, Row, Field, E.Message]));
              Continue;
            end;
          end;

          RequireRcpt := FieldOnReceipt(Table, Field) or FPrintAll;
          if (A = '') and not RequireRcpt then
          begin
            Inc(Skipped);
            Continue;
          end;

          Inc(Tested);
          try
            if A <> '' then
            begin
              Rc := WriteFieldValueCode(Driver, Table, Row, Field, Fld, A);
              if Rc = ErrFieldReadOnly then
              begin
                if RequireRcpt then
                begin
                  DoPrint;
                  if not ValueOnReceipt(Orig) then
                    raise Exception.CreateFmt('read-only "%s" not on receipt', [Orig]);
                end
                else
                begin
                  Dec(Tested);
                  Inc(Skipped);
                end;
                Continue;
              end;
              if Rc <> 0 then
                raise Exception.CreateFmt('write %s: %d', [A, Rc]);
              ReadBack := ReadFieldValue(Driver, Table, Row, Field, Fld);
              if not SameFieldValue(Fld, ReadBack, A) then
                raise Exception.CreateFmt('wrote %s read %s', [A, ReadBack]);
            end;

            if RequireRcpt then
            begin
              DoPrint;
              if A <> '' then
              begin
                if not ValueOnReceipt(A) then
                  raise Exception.CreateFmt('"%s" not on receipt', [A]);
              end
              else if not ValueOnReceipt(Orig) then
                raise Exception.CreateFmt('"%s" not on receipt', [Orig]);
            end;

            if B <> '' then
            begin
              WriteFieldValue(Driver, Table, Row, Field, Fld, B);
              ReadBack := ReadFieldValue(Driver, Table, Row, Field, Fld);
              if not SameFieldValue(Fld, ReadBack, B) then
                raise Exception.CreateFmt('wrote %s read %s', [B, ReadBack]);
              if RequireRcpt then
              begin
                DoPrint;
                if not ValueOnReceipt(B) then
                  raise Exception.CreateFmt('"%s" not on receipt', [B]);
                if (Pos(A, B) = 0) and (Pos(B, A) = 0) and ValueOnReceipt(A) then
                  raise Exception.CreateFmt('old value "%s" still on receipt', [A]);
              end;
            end;

            Context.Debug(Format('[+] %d/%d/%d %s', [Table, Row, Field, Fld.Name]));
          except
            on E: Exception do
            begin
              Inc(Failed);
              FailText.Add(Format('%d/%d/%d %s: %s',
                [Table, Row, Field, Fld.Name, E.Message]));
              Context.Error(Format('[-] %d/%d/%d %s: %s',
                [Table, Row, Field, Fld.Name, E.Message]));
            end;
          end;
          RestoreOrig;
        end;
      end;
    end;

    Result.Text := Format('tested %d, failed %d, skipped %d', [Tested, Failed, Skipped]);
    Context.Debug(Result.Text);
    Result.IsSucceeded := (Tested > 0) and (Failed = 0);
    if Failed > 0 then
      Result.Text := Result.Text + sLineBreak + FailText.Text;
  finally
    FailText.Free;
  end;
end;

{ TAutoZeroCashTest }

function SameMoney(A, B: Currency): Boolean;
begin
  Result := Abs(A - B) < 0.005;
end;

procedure WaitPrintDone(Driver: TDrvFR);
var
  i, Rc: Integer;
begin
  for i := 1 to 180 do
  begin
    Rc := Driver.WaitForPrinting;
    { 80 — ещё печатает; -34 — пауза TPE после отрезки; 30 иногда
      всплывает как ложный код после 1Eh, пока лента доигрывает. }
    if (Rc <> -34) and (Rc <> 80) and (Rc <> 30) then
    begin
      if Rc <> 0 then
        raise Exception.CreateFmt('WaitForPrinting %d, %s',
          [Rc, Driver.ResultCodeDescription]);
      Exit;
    end;
    Sleep(250);
  end;
end;

constructor TAutoZeroCashTest.CreateTest(AContext: TDriverContext);
begin
  inherited CreateTest(AContext);
  FValues := TStringList.Create;
  FTable := 1;
  FRow := 1;
  FField := 2;
  FRegister := 241;
  FAmount := 10.00;
end;

destructor TAutoZeroCashTest.Destroy;
begin
  FValues.Free;
  inherited Destroy;
end;

procedure TAutoZeroCashTest.Load(node: IYAMLValue);
var
  value: IYAMLValue;
  seq: IYAMLSequence;
  i: Integer;
begin
  inherited Load(node);
  FTable := 1;
  FRow := 1;
  FField := 2;
  if node.TryGetValue('table', value) then
    FTable := value.AsInteger;
  if node.TryGetValue('row', value) then
    FRow := value.AsInteger;
  if node.TryGetValue('field', value) then
    FField := value.AsInteger;
  FRegister := 241;
  if node.TryGetValue('register', value) then
    FRegister := value.AsInteger;
  FAmount := 10.00;
  if node.TryGetValue('amount', value) then
    FAmount := value.AsFloat;
  FValues.Clear;
  if node.TryGetValue('values', value) and value.IsSequence then
  begin
    seq := value.AsSequence;
    for i := 0 to seq.Count - 1 do
      FValues.Add(Trim(seq.Items[i].AsString));
  end;
  if FValues.Count = 0 then
  begin
    FValues.Add('0');
    FValues.Add('1');
  end;
end;

function TAutoZeroCashTest.Execute: TTestResult;
var
  Info: TFieldInfo;
  Orig, Flag: string;
  i, Rc: Integer;
  Dummy: TBytes;
  Before, AfterIn, AfterZ: Currency;
  AutoZero: Boolean;
begin
  Result.IsSucceeded := False;
  Result.Text := '';
  try
    Rc := GetFieldInfo(Driver, FTable, FField, Info);
    if Rc <> 0 then
      Info := FallbackFieldInfo(False);

    Orig := ReadFieldValue(Driver, FTable, FRow, FField, Info);
    Context.Debug(Format('Table %d/%d/%d original=%s register=%d amount=%.2f',
      [FTable, FRow, FField, Orig, FRegister, FAmount]));

    try
      for i := 0 to FValues.Count - 1 do
      begin
        Flag := FValues[i];
        AutoZero := StrToIntDef(Flag, 0) <> 0;
        Context.Debug(Format('flag=%s auto_zero=%s', [Flag, BoolToStr(AutoZero, True)]));

        Check(Driver.GetShortECRStatus);
        if (Driver.ECRMode = FPTR_MODE_24NOTOVER) or
          (Driver.ECRMode = FPTR_MODE_24OVER) then
        begin
          Driver.Password := 30;
          Rc := Driver.PrintReportWithCleaning;
          if (Rc <> 0) and (Rc <> -34) then
            Check(Rc);
          WaitPrintDone(Driver);
        end
        else if Driver.ECRMode = FPTR_MODE_REC then
        begin
          Driver.SysAdminCancelCheck;
          WaitPrintDone(Driver);
        end;

        WriteFieldValue(Driver, FTable, FRow, FField, Info, Flag);
        if not SameFieldValue(Info, ReadFieldValue(Driver, FTable, FRow, FField, Info), Flag) then
          raise Exception.CreateFmt('failed to write flag %s', [Flag]);

        Context.EnsureShiftOpen;
        Driver.Password := 30;
        Driver.BinaryConversion := BINARY_CONVERSION_NONE;
        Driver.RegisterNumber := FRegister;
        Check(Driver.GetCashReg);
        Before := Driver.ContentsOfCashRegister;
        Context.Debug(Format('cash before income: %.2f', [Before]));

        Rc := ExchangeCmd(Driver, Format('50 %s %s',
          [AdminPwdHex, BytesToHex(IntToBytesLE(Round(FAmount * 100), 5))]), Dummy);
        if Rc <> 0 then
          raise Exception.CreateFmt('CashIncome: %d, %s',
            [Rc, Driver.ResultCodeDescription]);
        WaitPrintDone(Driver);

        Driver.BinaryConversion := BINARY_CONVERSION_NONE;
        Driver.RegisterNumber := FRegister;
        Check(Driver.GetCashReg);
        AfterIn := Driver.ContentsOfCashRegister;
        Context.Debug(Format('cash after income: %.2f', [AfterIn]));
        if not SameMoney(AfterIn, Before + FAmount) then
          raise Exception.CreateFmt(
            'after income: expected %.2f, got %.2f (register %d)',
            [Before + FAmount, AfterIn, FRegister]);

        Driver.Password := 30;
        Rc := Driver.PrintReportWithCleaning;
        if (Rc <> 0) and (Rc <> -34) then
          Check(Rc);
        WaitPrintDone(Driver);

        Driver.BinaryConversion := BINARY_CONVERSION_NONE;
        Driver.RegisterNumber := FRegister;
        Check(Driver.GetCashReg);
        AfterZ := Driver.ContentsOfCashRegister;
        Context.Debug(Format('cash after Z-report: %.2f', [AfterZ]));

        if AutoZero then
        begin
          if not SameMoney(AfterZ, 0) then
            raise Exception.CreateFmt(
              'flag=1: cash register %d must be 0 after Z-report, got %.2f',
              [FRegister, AfterZ]);
        end
        else if not SameMoney(AfterZ, AfterIn) then
          raise Exception.CreateFmt(
            'flag=0: cash register %d must stay %.2f after Z-report, got %.2f',
            [FRegister, AfterIn, AfterZ]);
      end;
    finally
      try
        WriteFieldValue(Driver, FTable, FRow, FField, Info, Orig);
      except
      end;
    end;

    Result.IsSucceeded := True;
  except
    on E: Exception do
    begin
      Result.IsSucceeded := False;
      Result.Text := E.Message;
    end;
  end;
end;

{ TTableFlagTest }

function FlagOn(const Flag: string): Boolean;
begin
  Result := StrToIntDef(Flag, 0) <> 0;
end;

function HasFold(const Receipt, Needle: string): Boolean;
begin
  Result := (Needle <> '') and
    (Pos(FoldOcr(Needle), FoldOcr(NormalizePrintedText(Receipt))) > 0);
end;

function ParseRisePort(const Map: string; Port: Integer): Cardinal;
var
  Key, Hex: string;
  P: Integer;
begin
  Result := 0;
  Key := Format('P%d=', [Port]);
  P := Pos(Key, Map);
  if P = 0 then
    Exit;
  Hex := Copy(Map, P + Length(Key), 8);
  Result := Cardinal(StrToIntDef('$' + Trim(Hex), 0));
end;

function RiseExtra(const OffMap, OnMap: string): string;
var
  i: Integer;
  Extra: Cardinal;
begin
  Result := '';
  for i := 0 to 5 do
  begin
    Extra := ParseRisePort(OnMap, i) and not ParseRisePort(OffMap, i);
    if Extra <> 0 then
      Result := Result + Format(' P%d=%08X', [i, Extra]);
  end;
  Result := Trim(Result);
end;

function HexUIntLE(Value: Int64; Size: Integer): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Size - 1 do
  begin
    if Result <> '' then
      Result := Result + ' ';
    Result := Result + HexByte(Byte((Value shr (8 * i)) and $FF));
  end;
end;

function ParseTraceToken(const S, Key: string): string;
var
  P, P2: Integer;
begin
  Result := '';
  P := Pos(Key, S);
  if P = 0 then
    Exit;
  P := P + Length(Key);
  P2 := P;
  while (P2 <= Length(S)) and (S[P2] <> ' ') do
    Inc(P2);
  Result := Copy(S, P, P2 - P);
end;

function FirstCsvInt(const Csv: string): Integer;
var
  P: Integer;
begin
  P := Pos(',', Csv);
  if P = 0 then
    Result := StrToIntDef(Csv, 0)
  else
    Result := StrToIntDef(Copy(Csv, 1, P - 1), 0);
end;

function ParseKickN(const Trace: string): Integer;
begin
  Result := StrToIntDef(ParseTraceToken(Trace, 'n='), 0);
end;

function ParseKickWidth(const Trace: string): Integer;
begin
  Result := FirstCsvInt(ParseTraceToken(Trace, 'w='));
end;

function ParseKickGap(const Trace: string): Integer;
begin
  Result := FirstCsvInt(ParseTraceToken(Trace, 'g='));
end;

function HasFracQty(const Receipt: string): Boolean;
begin
  Result := HasFold(Receipt, '1.000') or HasFold(Receipt, '1,000') or
    HasFold(Receipt, '1.OOO') or HasFold(Receipt, '1.500') or
    HasFold(Receipt, '1,500') or HasFold(Receipt, '1.5OO');
end;

function SaleQtyItem(Driver: TDrvFR; Qty: Int64; const NameHex: string): Integer;
var
  Dummy: TBytes;
  Item: string;
  Sum: Int64;
begin
  Sum := (Qty * 100) div 1000000;
  if Sum < 1 then
    Sum := 100;
  Item := Format(
    'FF 46 %s 01 %s %s %s %s %s %s 04 01 %s',
    [AdminPwdHex,
     HexUIntLE(Qty, 6),
     HexUIntLE(100, 5),
     HexUIntLE(Sum, 5),
     HexUIntLE($FFFFFFFFFF, 5),
     HexUIntLE(0, 1),
     HexUIntLE(1, 1),
     NameHex]);
  Result := ExchangeCmd(Driver, Item, Dummy);
end;

procedure WriteByteField(Driver: TDrvFR; Field: Integer; Value: Integer);
var
  Info: TFieldInfo;
begin
  Info := FallbackFieldInfo(False);
  WriteFieldValue(Driver, 1, 1, Field, Info, IntToStr(Value));
end;

function SaleItemRc(Driver: TDrvFR; TaxRate: Integer; TaxAmount: Int64): Integer;
var
  Dummy: TBytes;
  Item: string;
begin
  Item := Format(
    'FF 46 %s 01 %s %s %s %s %s %s 04 01 54 41 42 4C 45 20 54 45 53 54',
    [AdminPwdHex,
     HexUIntLE(1000000, 6),
     HexUIntLE(100, 5),
     HexUIntLE(100, 5),
     HexUIntLE(TaxAmount, 5),
     HexUIntLE(TaxRate, 1),
     HexUIntLE(1, 1)]);
  Result := ExchangeCmd(Driver, Item, Dummy);
end;

function CloseSaleRc(Driver: TDrvFR; PayKop: Int64; const TaxSums: string): Integer;
var
  Dummy: TBytes;
  Sums, Taxes, Cmd: string;
  i: Integer;
begin
  Sums := HexUIntLE(PayKop, 5);
  for i := 2 to 16 do
    Sums := Sums + ' ' + HexUIntLE(0, 5);
  if TaxSums <> '' then
    Taxes := TaxSums
  else
  begin
    Taxes := HexUIntLE(0, 5);
    for i := 2 to 6 do
      Taxes := Taxes + ' ' + HexUIntLE(0, 5);
  end;
  Cmd := Format('FF 45 %s %s 00 %s 01', [AdminPwdHex, Sums, Taxes]);
  Result := ExchangeCmd(Driver, Cmd, Dummy);
end;

procedure CloseShiftIfOpen(Driver: TDrvFR);
var
  Rc: Integer;
begin
  Driver.Password := 30;
  WaitPrintDone(Driver);
  if Driver.GetShortECRStatus <> 0 then
    Exit;
  if Driver.ECRMode = FPTR_MODE_REC then
  begin
    Driver.SysAdminCancelCheck;
    WaitPrintDone(Driver);
    if Driver.GetShortECRStatus <> 0 then
      Exit;
  end;
  if (Driver.ECRMode = FPTR_MODE_24NOTOVER) or
    (Driver.ECRMode = FPTR_MODE_24OVER) then
  begin
    Rc := Driver.PrintReportWithCleaning;
    if (Rc <> 0) and (Rc <> -34) and (Rc <> 80) then
      raise Exception.CreateFmt('Z-report %d, %s',
        [Rc, Driver.ResultCodeDescription]);
    WaitPrintDone(Driver);
  end;
end;

function FindTimeShiftTable(Driver: TDrvFR): Integer;
var
  t, Rc: Integer;
  Info: TTableInfo;
  Name: string;
begin
  Result := 0;
  for t := 1 to 40 do
  begin
    Rc := GetTableInfo(Driver, t, Info);
    if (Rc <> 0) or not Info.Valid then
      Continue;
    Name := AnsiLowerCase(Info.Name);
    if (Pos('перевод', Name) > 0) and (Pos('времен', Name) > 0) then
    begin
      Result := t;
      Exit;
    end;
  end;
  Rc := GetTableInfo(Driver, 3, Info);
  if (Rc = 0) and Info.Valid and (Info.FieldCount >= 3) and
    (Info.RowCount >= 1) then
    Result := 3;
end;

procedure SetEcrClock(Driver: TDrvFR; DT: TDateTime);
var
  Rc: Integer;
begin
  CloseShiftIfOpen(Driver);
  Driver.Password := 30;
  if Driver.GetShortECRStatus = 0 then
    if Driver.ECRMode = FPTR_MODE_WAITDATE then
    begin
      Rc := Driver.ConfirmDate;
      if (Rc <> 0) and (Rc <> 80) and (Rc <> -34) then
        raise Exception.CreateFmt('ConfirmDate %d, %s',
          [Rc, Driver.ResultCodeDescription]);
    end;
  Driver.ECRDate := DT;
  Rc := Driver.SetDate;
  if Rc <> 0 then
    raise Exception.CreateFmt('SetDate %d, %s',
      [Rc, Driver.ResultCodeDescription]);
  Rc := Driver.ConfirmDate;
  if Rc <> 0 then
    raise Exception.CreateFmt('ConfirmDate %d, %s',
      [Rc, Driver.ResultCodeDescription]);
  Driver.ECRTime := DT;
  Rc := Driver.SetTime;
  if Rc <> 0 then
    raise Exception.CreateFmt('SetTime %d, %s',
      [Rc, Driver.ResultCodeDescription]);
end;

procedure TTableFlagTest.PrepareIdle;
var
  i, Rc: Integer;
begin
  Driver.Password := 30;
  for i := 1 to 90 do
  begin
    try
      WaitPrintDone(Driver);
    except
    end;
    Rc := Driver.GetShortECRStatus;
    if (Rc = 80) or (Rc = -34) or (Rc = 30) then
    begin
      Sleep(500);
      Continue;
    end;
    if Rc <> 0 then
    begin
      Sleep(300);
      Continue;
    end;
    Context.Debug(Format('PrepareIdle mode=%d %s',
      [Driver.ECRMode, Driver.ECRModeDescription]));
    case Driver.ECRMode of
      FPTR_MODE_REC:
      begin
        Rc := Driver.SysAdminCancelCheck;
        if (Rc = 80) or (Rc = -34) or (Rc = 30) then
        begin
          Context.Debug(Format('cancel check rc=%d, wait print', [Rc]));
          Sleep(500);
          Continue;
        end;
        if Rc <> 0 then
          Rc := Driver.CancelCheck;
        Context.Debug(Format('cancel check rc=%d', [Rc]));
        Sleep(400);
      end;
      FPTR_MODE_24NOTOVER, FPTR_MODE_24OVER:
        Exit;
      FPTR_MODE_CLOSED:
        Exit;
    else
      Sleep(250);
    end;
  end;
  if Driver.GetShortECRStatus = 0 then
    if Driver.ECRMode = FPTR_MODE_REC then
      raise Exception.CreateFmt('cannot cancel open check, mode=%d',
        [Driver.ECRMode]);
end;

constructor TTableFlagTest.CreateTest(AContext: TDriverContext);
begin
  inherited CreateTest(AContext);
  FValues := TStringList.Create;
  FAdRows := TList<Integer>.Create;
  FGrandNeedles := TStringList.Create;
  FKind := tfAdText;
  FTable := 1;
  FRow := 1;
  FField := 3;
  FTimeTable := 0;
  FAdTable := 4;
  FAdText := 'ADVTEXT01';
  FCutMark := 'отрезка чека';
  FAdRows.Add(1);
  FGrandNeedles.Add('НЕОБНУЛ');
  FGrandNeedles.Add('НЕОБНУЛЯЕМ');
  FGrandNeedles.Add('НЕ0БНУЛ');
  FGrandNeedles.Add('НЕОБН');
  FGrandNeedles.Add('НЕО6');
  FGrandNeedles.Add('СУММЫ НА НАЧ');
  FGrandNeedles.Add('НСУММ');
  FGrandNeedles.Add('НАКОПЛ');
end;

destructor TTableFlagTest.Destroy;
begin
  FValues.Free;
  FAdRows.Free;
  FGrandNeedles.Free;
  inherited Destroy;
end;

procedure TTableFlagTest.Load(node: IYAMLValue);
var
  value: IYAMLValue;
  seq: IYAMLSequence;
  i: Integer;
begin
  inherited Load(node);
  if node.TryGetValue('table', value) then
    FTable := value.AsInteger;
  if node.TryGetValue('row', value) then
    FRow := value.AsInteger;
  if node.TryGetValue('field', value) then
    FField := value.AsInteger;
  if node.TryGetValue('time_table', value) then
    FTimeTable := value.AsInteger;
  if node.TryGetValue('ad_table', value) then
    FAdTable := value.AsInteger;
  if node.TryGetValue('ad_text', value) then
    FAdText := Trim(value.AsString);
  if node.TryGetValue('cut_mark', value) then
    FCutMark := Trim(value.AsString);
  FValues.Clear;
  if node.TryGetValue('values', value) and value.IsSequence then
  begin
    seq := value.AsSequence;
    for i := 0 to seq.Count - 1 do
      FValues.Add(Trim(seq.Items[i].AsString));
  end;
  if node.TryGetValue('ad_rows', value) and value.IsSequence then
  begin
    FAdRows.Clear;
    seq := value.AsSequence;
    for i := 0 to seq.Count - 1 do
      FAdRows.Add(seq.Items[i].AsInteger);
  end;
  if node.TryGetValue('needles', value) and value.IsSequence then
  begin
    FGrandNeedles.Clear;
    seq := value.AsSequence;
    for i := 0 to seq.Count - 1 do
      FGrandNeedles.Add(Trim(seq.Items[i].AsString));
  end;
  if FValues.Count = 0 then
  begin
    FValues.Add('0');
    if FKind = tfCut then
      FValues.Add('2')
    else
      FValues.Add('1');
  end;
  if FAdRows.Count = 0 then
    FAdRows.Add(1);
end;

function TTableFlagTest.Execute: TTestResult;
var
  Info, AdInfo: TFieldInfo;
  Orig, Flag, Receipt, ReadBack, RiseMap, Extra, Trace: string;
  OrigAd, OrigPerm, OrigDay, OrigMon: TStringList;
  i, r, Rc, Cuts0, Cuts1, KickN, Width, Gap, ItemRc: Integer;
  Needle: string;
  Dummy: TBytes;
  OnFlag, Found, OpenClosed, OpenOpened, HasSeason: Boolean;
  TimeTbl: TTableInfo;
  PermInfo, DayInfo, MonInfo, SeasonInfo: TFieldInfo;
  TimeTableNo, SpringRow, Hour: Integer;
  TargetDate: TDateTime;
  Season, PermNow: string;
begin
  Result.IsSucceeded := False;
  Result.Text := '';
  OrigAd := TStringList.Create;
  OrigPerm := TStringList.Create;
  OrigDay := TStringList.Create;
  OrigMon := TStringList.Create;
  TimeTableNo := 0;
  SpringRow := 1;
  try
    Rc := GetFieldInfo(Driver, FTable, FField, Info);
    if Rc <> 0 then
      Info := FallbackFieldInfo(False);

    Orig := ReadFieldValue(Driver, FTable, FRow, FField, Info);
    Context.Debug(Format('Table %d/%d/%d original=%s kind=%d min=%d max=%d',
      [FTable, FRow, FField, Orig, Ord(FKind), Info.MinValue, Info.MaxValue]));
    FOffRise := '';
    FPrevKickN := -1;
    FPrevWidth := -1;
    FPrevGap := -1;

    AdInfo := FallbackFieldInfo(True);
    if FKind = tfAdText then
    begin
      Rc := GetFieldInfo(Driver, FAdTable, 1, AdInfo);
      if Rc <> 0 then
        AdInfo := FallbackFieldInfo(True);
      for r := 0 to FAdRows.Count - 1 do
        OrigAd.Add(ReadFieldValue(Driver, FAdTable, FAdRows[r], 1, AdInfo));
    end;

    if FKind = tfAutoTime then
    begin
      TimeTableNo := FTimeTable;
      if TimeTableNo <= 0 then
        TimeTableNo := FindTimeShiftTable(Driver);
      if TimeTableNo <= 0 then
        raise Exception.Create('Таблица перевода времени не найдена (2Dh)');
      Rc := GetTableInfo(Driver, TimeTableNo, TimeTbl);
      if (Rc <> 0) or not TimeTbl.Valid then
        raise Exception.CreateFmt('GetTableStruct %d: %d', [TimeTableNo, Rc]);
      Context.Debug(Format('time table %d "%s" fields=%d rows=%d',
        [TimeTableNo, TimeTbl.Name, TimeTbl.FieldCount, TimeTbl.RowCount]));
      Rc := GetFieldInfo(Driver, TimeTableNo, 1, PermInfo);
      if Rc <> 0 then
        PermInfo := FallbackFieldInfo(False);
      Rc := GetFieldInfo(Driver, TimeTableNo, 2, DayInfo);
      if Rc <> 0 then
        DayInfo := FallbackFieldInfo(False);
      Rc := GetFieldInfo(Driver, TimeTableNo, 3, MonInfo);
      if Rc <> 0 then
        MonInfo := FallbackFieldInfo(False);
      HasSeason := GetFieldInfo(Driver, TimeTableNo, 4, SeasonInfo) = 0;
      for r := 1 to TimeTbl.RowCount do
      begin
        OrigPerm.Add(ReadFieldValue(Driver, TimeTableNo, r, 1, PermInfo));
        OrigDay.Add(ReadFieldValue(Driver, TimeTableNo, r, 2, DayInfo));
        OrigMon.Add(ReadFieldValue(Driver, TimeTableNo, r, 3, MonInfo));
        if HasSeason then
        begin
          Season := ReadFieldValue(Driver, TimeTableNo, r, 4, SeasonInfo);
          Context.Debug(Format('time row %d perm=%s %s.%s %s',
            [r, OrigPerm[r - 1], OrigDay[r - 1], OrigMon[r - 1], Season]));
          if (SpringRow = 1) and (Pos('ВЕСНА', AnsiUpperCase(Season)) > 0) then
            SpringRow := r;
        end
        else
          Context.Debug(Format('time row %d perm=%s %s.%s',
            [r, OrigPerm[r - 1], OrigDay[r - 1], OrigMon[r - 1]]));
      end;
      Context.Debug(Format('spring row=%d', [SpringRow]));
    end;

    try
      for i := 0 to FValues.Count - 1 do
      begin
        Flag := FValues[i];
        OnFlag := FlagOn(Flag);
        Context.Debug(Format('flag=%s on=%s', [Flag, BoolToStr(OnFlag, True)]));

        PrepareIdle;
        if FKind = tfAutoTime then
          CloseShiftIfOpen(Driver)
        else
        begin
          WriteFieldValue(Driver, FTable, FRow, FField, Info, Flag);
          ReadBack := ReadFieldValue(Driver, FTable, FRow, FField, Info);
          if not SameFieldValue(Info, ReadBack, Flag) then
            raise Exception.CreateFmt('failed to write flag %s, read %s',
              [Flag, ReadBack]);

          if FKind = tfAdText then
          begin
            for r := 0 to FAdRows.Count - 1 do
              WriteFieldValue(Driver, FAdTable, FAdRows[r], 1, AdInfo, FAdText);
          end;

          Context.EnsureShiftOpen;
          WaitPrintDone(Driver);
        end;

        case FKind of
          tfAdText:
          begin
            Context.ClearPrintedText;
            Context.PrintSimpleSaleReceipt;
            WaitPrintDone(Driver);
            Receipt := Context.WaitPrintedText;
            Context.Debug('Receipt:' + sLineBreak + Receipt);
            Found := HasFold(Receipt, FAdText);
            if OnFlag and not Found then
              raise Exception.CreateFmt(
                'flag=%s: advertising "%s" must be on the receipt',
                [Flag, FAdText]);
            if (not OnFlag) and Found then
              raise Exception.CreateFmt(
                'flag=%s: advertising "%s" must not be on the receipt',
                [Flag, FAdText]);
          end;
          tfGrandTotal:
          begin
            Context.ClearPrintedText;
            Driver.Password := 30;
            Rc := Driver.PrintReportWithCleaning;
            if (Rc <> 0) and (Rc <> -34) then
              Check(Rc);
            WaitPrintDone(Driver);
            for r := 1 to 40 do
            begin
              if Driver.GetShortECRStatus = 0 then
                if Driver.ECRMode = FPTR_MODE_CLOSED then
                  Break;
              Sleep(200);
            end;
            Sleep(800);
            if Options.UseEmulator and (Context.Emu <> nil) then
            begin
              Receipt := NormalizePrintedText(Context.Emu.ReadText);
              Context.SaveReceiptSnapshot;
            end
            else
              Receipt := Context.WaitPrintedText;
            Context.Debug(Format('Z-report len=%d', [Length(Receipt)]));
            if Length(Receipt) > 1200 then
              Context.Debug('Z-report tail:' + sLineBreak +
                Copy(Receipt, Length(Receipt) - 1199, 1200))
            else
              Context.Debug('Z-report:' + sLineBreak + Receipt);
            Found := False;
            for r := 0 to FGrandNeedles.Count - 1 do
            begin
              Needle := FGrandNeedles[r];
              if HasFold(Receipt, Needle) then
              begin
                Found := True;
                Context.Debug('grand total needle: ' + Needle);
                Break;
              end;
            end;
            if OnFlag and not Found then
            begin
              r := Length(Receipt) - 400;
              if r < 1 then
                r := 1;
              raise Exception.CreateFmt(
                'flag on: Z-report must contain необнуляемая сумма (len=%d, tail=%s)',
                [Length(Receipt), Copy(Receipt, r, 400)]);
            end;
            if (not OnFlag) and Found then
              raise Exception.Create(
                'flag off: Z-report must not contain необнуляемая сумма');
          end;
          tfDrawer:
          begin
            if (not Options.UseEmulator) or (Context.Emu = nil) then
              raise Exception.Create('drawer test needs -EMU');
            Context.Emu.GpioRisePull;
            Context.ClearPrintedText;
            Context.PrintSimpleSaleReceipt;
            WaitPrintDone(Driver);
            Sleep(400);
            RiseMap := Context.Emu.GpioRisePull;
            Context.Debug(Format('gpio-rise flag=%s %s', [Flag, RiseMap]));
            if not OnFlag then
              FOffRise := RiseMap
            else
            begin
              Extra := RiseExtra(FOffRise, RiseMap);
              Context.Debug(Format('gpio-rise extra vs flag=0: %s', [Extra]));
              if Extra = '' then
                raise Exception.CreateFmt(
                  'flag=%s: cash drawer GPIO pulse expected after receipt' +
                  sLineBreak + 'flag0=%s' + sLineBreak + 'flag1=%s',
                  [Flag, FOffRise, RiseMap]);
            end;
          end;
          tfCut:
          begin
            if (not Options.UseEmulator) or (Context.Emu = nil) then
              raise Exception.Create('cut test needs -EMU');
            Cuts0 := Context.Emu.CutCount;
            Context.ClearPrintedText;
            Context.PrintSimpleSaleReceipt;
            WaitPrintDone(Driver);
            Sleep(400);
            Cuts1 := Context.Emu.CutCount;
            Receipt := Context.WaitPrintedText;
            Context.Debug(Format('cut-count %d -> %d', [Cuts0, Cuts1]));
            Context.Debug('Receipt:' + sLineBreak + Receipt);
            Found := HasFold(Receipt, FCutMark) or
              (Pos(FCutMark, Receipt) > 0);
            if OnFlag then
            begin
              if Cuts1 <= Cuts0 then
                raise Exception.CreateFmt(
                  'flag=%s: cutter pulse expected, cut-count %d -> %d',
                  [Flag, Cuts0, Cuts1]);
              if not Found then
                raise Exception.CreateFmt(
                  'flag=%s: receipt text must contain "%s"',
                  [Flag, FCutMark]);
            end else
            begin
              if Cuts1 > Cuts0 then
                raise Exception.CreateFmt(
                  'flag=%s: no cut expected, cut-count %d -> %d',
                  [Flag, Cuts0, Cuts1]);
              if Found then
                raise Exception.CreateFmt(
                  'flag=%s: cut mark "%s" must not be in the receipt',
                  [Flag, FCutMark]);
            end;
          end;
          tfFracQty:
          begin
            Context.ClearPrintedText;
            ExchangeCmd(Driver, '8D ' + AdminPwdHex + ' 00', Dummy);
            Check(SaleQtyItem(Driver, 1000000, '51 54 59 31'));
            Check(SaleQtyItem(Driver, 1500000, '51 54 59 31 35'));
            Rc := CloseSaleRc(Driver, 250, '');
            if (Rc <> 0) and (Rc <> 80) and (Rc <> -34) then
              Check(Rc);
            WaitPrintDone(Driver);
            Receipt := Context.WaitPrintedText;
            Context.Debug('Receipt:' + sLineBreak + Receipt);
            Found := HasFracQty(Receipt);
            if OnFlag and not Found then
            begin
              Driver.QuantityFormat := 1;
              Driver.CheckType := 1;
              Driver.Quantity := 1;
              Driver.Price := 1;
              Driver.Summ1 := 1;
              Driver.Tax1 := 0;
              Driver.Department := 1;
              Driver.PaymentTypeSign := 4;
              Driver.PaymentItemSign := 1;
              Driver.StringForPrinting := 'QTY1';
              Rc := Driver.FNOperation;
              if Rc = 0 then
              begin
                Driver.Quantity := 1.5;
                Driver.Summ1 := 1.5;
                Driver.StringForPrinting := 'QTY15';
                Driver.FNOperation;
                Driver.Summ1 := 2.5;
                Driver.CloseCheck;
                WaitPrintDone(Driver);
                Receipt := Context.WaitPrintedText;
                Context.Debug('FNOperation receipt:' + sLineBreak + Receipt);
                Found := HasFracQty(Receipt);
              end;
              if not Found then
              begin
                Context.ClearPrintedText;
                Driver.CheckType := 0;
                Driver.OpenCheck;
                Driver.Quantity := 1;
                Driver.Price := 1;
                Driver.Department := 1;
                Driver.StringForPrinting := 'SALE1';
                Rc := Driver.Sale;
                if Rc = 0 then
                begin
                  Driver.Quantity := 1.5;
                  Driver.StringForPrinting := 'SALE15';
                  Driver.Sale;
                  Driver.Summ1 := 2.5;
                  Driver.CloseCheck;
                  WaitPrintDone(Driver);
                  Receipt := Context.WaitPrintedText;
                  Context.Debug('Sale receipt:' + sLineBreak + Receipt);
                  Found := HasFracQty(Receipt);
                end else
                  Context.Debug(Format('Sale rc=%d %s',
                    [Rc, Driver.ResultCodeDescription]));
              end;
            end;
            if OnFlag and not Found then
              raise Exception.Create(
                'flag=1: quantity must print with 3 decimals (1.000 / 1.500)');
            if (not OnFlag) and Found then
              raise Exception.Create(
                'flag=0: integer/short qty must not be 1.000/1.500');
          end;
          tfDrawerLevel:
          begin
            if (not Options.UseEmulator) or (Context.Emu = nil) then
              raise Exception.Create('drawer level test needs -EMU');
            Context.Emu.DrawerClosed := True;
            Sleep(150);
            Check(Driver.GetECRStatus);
            OpenClosed := Boolean(Driver.IsDrawerOpen);
            Context.Emu.DrawerClosed := False;
            Sleep(150);
            Check(Driver.GetECRStatus);
            OpenOpened := Boolean(Driver.IsDrawerOpen);
            Context.Emu.DrawerClosed := True;
            Context.Debug(Format(
              'flag=%s IsDrawerOpen closed=%s opened=%s',
              [Flag, BoolToStr(OpenClosed, True), BoolToStr(OpenOpened, True)]));
            if OpenClosed = OpenOpened then
              raise Exception.Create(
                'drawer sensor does not follow QOM drawer-closed');
            if OnFlag and (not OpenClosed) then
              raise Exception.Create(
                'flag=1: logical level must invert (closed pin reads open)');
            if (not OnFlag) and OpenClosed then
              raise Exception.Create(
                'flag=0: closed pin must read as closed');
          end;
          tfDrawerPulse:
          begin
            if (not Options.UseEmulator) or (Context.Emu = nil) then
              raise Exception.Create('drawer pulse test needs -EMU');
            WriteByteField(Driver, 6, 1);
            if FField <> 10 then
              WriteByteField(Driver, 10, 10);
            if FField <> 11 then
              WriteByteField(Driver, 11, 1);
            if FField = 11 then
              WriteByteField(Driver, 12, 3)
            else if FField <> 12 then
              WriteByteField(Driver, 12, 1);
            WriteFieldValue(Driver, FTable, FRow, FField, Info, Flag);
            Context.Emu.DrawerKickTrace;
            Driver.DrawerNumber := 0;
            Rc := Driver.OpenDrawer;
            if (Rc <> 0) and (Rc <> 80) and (Rc <> -34) then
              Check(Rc);
            WaitPrintDone(Driver);
            r := 800 + StrToIntDef(Flag, 1) * 30;
            if FField = 11 then
              r := r + 3 * (200 + StrToIntDef(Flag, 1) * 20);
            if FField = 12 then
              r := r + StrToIntDef(Flag, 1) * 200;
            Sleep(r);
            Trace := Context.Emu.DrawerKickTrace;
            if ParseKickN(Trace) < 1 then
            begin
              Sleep(500);
              Context.Emu.DrawerKickTrace;
              Rc := Driver.OpenDrawer;
              WaitPrintDone(Driver);
              Sleep(r);
              Trace := Context.Emu.DrawerKickTrace;
            end;
            KickN := ParseKickN(Trace);
            Width := ParseKickWidth(Trace);
            Gap := ParseKickGap(Trace);
            Context.Debug(Format('drawer-trace flag=%s %s n=%d w=%d g=%d',
              [Flag, Trace, KickN, Width, Gap]));
            if FPrevKickN < 0 then
            begin
              FPrevKickN := KickN;
              FPrevWidth := Width;
              FPrevGap := Gap;
              if KickN < 1 then
                raise Exception.Create('OpenDrawer produced no P0.5 pulse');
            end else
            begin
              case FField of
                10:
                  if Width * 2 < FPrevWidth * 3 then
                    raise Exception.CreateFmt(
                      'pulse width must grow with field 10: %d ms -> %d ms',
                      [FPrevWidth, Width]);
                11:
                  if Gap * 2 < FPrevGap * 3 then
                    raise Exception.CreateFmt(
                      'pulse gap must grow with field 11: %d ms -> %d ms',
                      [FPrevGap, Gap]);
                12:
                  if KickN <= FPrevKickN then
                    raise Exception.CreateFmt(
                      'pulse count must grow with field 12: %d -> %d',
                      [FPrevKickN, KickN]);
              end;
            end;
          end;
          tfNearEnd:
          begin
            if (not Options.UseEmulator) or (Context.Emu = nil) then
              raise Exception.Create('near-end test needs -EMU');
            Context.Emu.RollPresent := False;
            Sleep(250);
            Check(Driver.GetECRStatus);
            Context.Debug(Format(
              'near-end flag=%s ribbon=%s optical=%s adv=%d %s',
              [Flag,
               BoolToStr(Boolean(Driver.ReceiptRibbonIsPresent), True),
               BoolToStr(Boolean(Driver.ReceiptRibbonOpticalSensor), True),
               Driver.ECRAdvancedMode,
               Driver.ECRAdvancedModeDescription]));
            Context.ClearPrintedText;
            try
              Context.PrintSimpleSaleReceipt;
              WaitPrintDone(Driver);
              Found := True;
              Receipt := Context.WaitPrintedText;
              Context.Debug('printed with roll ending:' + sLineBreak + Receipt);
            except
              on E: Exception do
              begin
                Found := False;
                Context.Debug('print error: ' + E.Message);
              end;
            end;
            Context.Emu.RollPresent := True;
            Sleep(200);
            if OnFlag and Found then
              raise Exception.Create(
                'flag=1: ending roll must raise "нет бумаги"');
            if (not OnFlag) and not Found then
              raise Exception.Create(
                'flag=0: ending roll must still print (no paper error)');
          end;
          tfTaxMode:
          begin
            ExchangeCmd(Driver, '8D ' + AdminPwdHex + ' 00', Dummy);
            ItemRc := SaleItemRc(Driver, 1, $FFFFFFFFFF);
            Context.Debug(Format('item tax-on-line rc=%d flag=%s',
              [ItemRc, Flag]));
            if ItemRc <> 0 then
            begin
              if not OnFlag then
                raise Exception.CreateFmt(
                  'flag=0: tax on item must be accepted, rc=%d', [ItemRc]);
              Context.Debug('flag on: tax on item rejected (receipt-level taxes)');
              ItemRc := SaleItemRc(Driver, 0, $FFFFFFFFFF);
              if ItemRc <> 0 then
                Check(ItemRc);
            end;
            Rc := CloseSaleRc(Driver, 100, '');
            if (Rc <> 0) and (Rc <> 80) and (Rc <> -34) then
              Check(Rc);
            WaitPrintDone(Driver);
            Receipt := Context.WaitPrintedText;
            Context.Debug('Receipt:' + sLineBreak + Receipt);
            Found := HasFold(Receipt, 'НДС') or HasFold(Receipt, 'HDC') or
              HasFold(Receipt, '20%');
            if (not OnFlag) and not Found then
              raise Exception.Create(
                'flag=0: item-tax receipt must contain НДС');
            WaitPrintDone(Driver);
          end;
          tfAutoTime:
          begin
            for r := 1 to TimeTbl.RowCount do
              WriteFieldValue(Driver, TimeTableNo, r, 1, PermInfo, '0');
            Check(Driver.GetECRStatus);
            TargetDate := Trunc(Driver.ECRDate) + 1;
            TargetDate := TargetDate + EncodeTime(3, 0, 0, 0);
            WriteFieldValue(Driver, TimeTableNo, SpringRow, 2, DayInfo,
              IntToStr(DayOf(TargetDate)));
            WriteFieldValue(Driver, TimeTableNo, SpringRow, 3, MonInfo,
              IntToStr(MonthOf(TargetDate)));
            WriteFieldValue(Driver, TimeTableNo, SpringRow, 1, PermInfo, '1');
            WriteFieldValue(Driver, FTable, FRow, FField, Info, Flag);
            ReadBack := ReadFieldValue(Driver, FTable, FRow, FField, Info);
            if not SameFieldValue(Info, ReadBack, Flag) then
              raise Exception.CreateFmt('failed to write flag %s, read %s',
                [Flag, ReadBack]);
            Context.Debug(Format('clock -> %s',
              [FormatDateTime('dd.mm.yyyy hh:nn:ss', TargetDate)]));
            SetEcrClock(Driver, TargetDate);
            Check(Driver.GetECRStatus);
            Context.Debug(Format('clock after set %s %s',
              [DateToStr(Driver.ECRDate), TimeToStr(Driver.ECRTime)]));
            Context.ClearPrintedText;
            Context.PrintSimpleSaleReceipt;
            WaitPrintDone(Driver);
            Check(Driver.GetECRStatus);
            Hour := HourOf(Driver.ECRTime);
            PermNow := ReadFieldValue(Driver, TimeTableNo, SpringRow, 1, PermInfo);
            Receipt := '';
            try
              Receipt := Context.WaitPrintedText;
            except
            end;
            Context.Debug(Format(
              'after document hour=%d perm=%s date=%s time=%s',
              [Hour, PermNow, DateToStr(Driver.ECRDate),
               TimeToStr(Driver.ECRTime)]));
            if Receipt <> '' then
              Context.Debug('Receipt:' + sLineBreak + Receipt);
            if OnFlag then
            begin
              if Hour < 4 then
                raise Exception.CreateFmt(
                  'flag=1: firmware must shift 03:00 -> 04:00, hour=%d perm=%s',
                  [Hour, PermNow]);
            end
            else if Hour >= 4 then
              raise Exception.CreateFmt(
                'flag=0: clock must stay at 03:00, hour=%d', [Hour]);
          end;
        end;
      end;
    finally
      try
        WriteFieldValue(Driver, FTable, FRow, FField, Info, Orig);
      except
      end;
      if TimeTableNo > 0 then
      begin
        for r := 1 to OrigPerm.Count do
        try
          WriteFieldValue(Driver, TimeTableNo, r, 2, DayInfo, OrigDay[r - 1]);
          WriteFieldValue(Driver, TimeTableNo, r, 3, MonInfo, OrigMon[r - 1]);
          WriteFieldValue(Driver, TimeTableNo, r, 1, PermInfo, OrigPerm[r - 1]);
        except
        end;
      end;
      if FKind = tfAdText then
      begin
        for r := 0 to OrigAd.Count - 1 do
        try
          WriteFieldValue(Driver, FAdTable, FAdRows[r], 1, AdInfo, OrigAd[r]);
        except
        end;
      end;
    end;

    Result.IsSucceeded := True;
  except
    on E: Exception do
    begin
      Result.IsSucceeded := False;
      Result.Text := E.Message;
    end;
  end;
  OrigAd.Free;
  OrigPerm.Free;
  OrigDay.Free;
  OrigMon.Free;
end;

end.
