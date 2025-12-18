unit ReceiptTest;

interface

uses
  // VCL
  System.Generics.Collections,
  // 3'd
  VSoft.YAML,
  // This
  DriverTest;

type
  TReceiptCommand = class
  end;
  TReceiptCommands = TObjectList<TReceiptCommand>;

  TOpenReceiptCommand = class(TReceiptCommand)
  public
    RecType: string;
  end;

  { TReceiptTest }

  TReceiptTest = class(TDriverTest)
  private
    FCommands: TReceiptCommands;
    FCashRegs: TModifiedCashRegs;
    FOperRegs: TModifiedOperRegs;
  public
    constructor CreateTest(AContext: TDriverContext); override;
    destructor Destroy; override;
    function Execute: TTestResult; override;
    procedure Load(node: IYAMLValue); override;

    property Commands: TReceiptCommands read FCommands;
    property CashRegs: TModifiedCashRegs read FCashRegs;
    property OperRegs: TModifiedOperRegs read FOperRegs;
  end;

implementation

{ TReceiptTest }

constructor TReceiptTest.CreateTest(AContext: TDriverContext);
begin
  inherited CreateTest(AContext);
  FCommands := TReceiptCommands.Create;
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
  Command: TOpenReceiptCommand;
begin
  inherited Load(node);
  // Операционные регистры
  regnode := node.GetValue('operation_registers');
  if (not regnode.IsNull)and(regnode.IsSequence) then
  begin
    Sequence := regnode.AsSequence;
    if Sequence.Count > 0 then
    begin
      SetLength(FOperRegs, Sequence.Count);
      for i := 0 to Sequence.Count-1 do
      begin
        itmnode := Sequence.Items[i];
        FOperRegs[i].Number := itmnode.GetValue('number').AsInteger;

        if itmnode.TryGetValue('name', Value) then
        FOperRegs[i].Name := Value.AsString;

        if itmnode.TryGetValue('delta', Value) then
        FOperRegs[i].Delta := Value.AsInteger;

        if itmnode.TryGetValue('value', Value) then
          FOperRegs[i].Value := Value.AsInteger;
      end;
    end;
  end;

  regnode := node.GetValue('operations');
  if (not regnode.IsNull)and(regnode.IsSequence) then
  begin
    Sequence := regnode.AsSequence;
    if Sequence.Count > 0 then
    begin
      for i := 0 to Sequence.Count-1 do
      begin
        itmnode := Sequence.Items[i];
        Operation := itmnode.GetValue('type').AsString;
        if Operation = 'open_receipt' then
        begin
          Command := TOpenReceiptCommand.Create;
          Command.rectype := itmnode.GetValue('rectype').AsString;
          Commands.Add(Command);
        end;
      end;
    end;
  end;
end;

function TReceiptTest.Execute: TTestResult;
var
  Command: TReceiptCommand;
  StateBefore: TFiscalPrinterState;
  StateAfter: TFiscalPrinterState;
  OpenReceiptCommand: TOpenReceiptCommand;
begin
  Result.Text := '';
  Result.IsSucceeded := True;

  Context.CheckEcrMode(FPTR_MODE_24NOTOVER);
  StateBefore := Context.ReadPrinterState;
  for Command in Commands do
  begin
    if Command is TOpenReceiptCommand then
    begin
      OpenReceiptCommand := Command as TOpenReceiptCommand;

      Driver.CheckType := 0;
      if OpenReceiptCommand.RecType = 'sale' then
        Driver.CheckType := 0;
      Check(Driver.OpenCheck);
      Check(Driver.WaitForPrinting);
      Context.CheckEcrMode(FPTR_MODE_REC);
    end;
  end;
  StateAfter := Context.ReadPrinterState;

  Result.IsSucceeded := IsEquals(StateBefore, StateAfter);
  Result.Text := '';
end;



end.
