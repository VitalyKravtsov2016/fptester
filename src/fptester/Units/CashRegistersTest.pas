unit CashRegistersTest;

interface

uses
  // VCL
  SysUtils, System.Generics.Collections,
  // 3'd
  VSoft.YAML,
  // This
  DriverTest;

type
  { TCashRegisterRec }

  TCashRegisterRec = record
    number: Integer;
    text: string;
  end;

  { TCashRegistersTest }

  TCashRegistersTest = class(TDriverTest)
  public
    expected_value: Currency;
    registers: array of TCashRegisterRec;
    procedure Load(node: IYAMLValue); override;
    function Execute: TTestResult; override;
  end;

implementation

{ TCashRegistersTest }

procedure TCashRegistersTest.Load(node: IYAMLValue);
var
  i: Integer;
  regnode: IYAMLValue;
  itmnode: IYAMLValue;
  Sequence: IYAMLSequence;
begin
  inherited Load(node);
  expected_value := node.GetValue('expected_value').AsFloat;
  regnode := node.GetValue('registers');
  if (not regnode.IsNull)and(regnode.IsSequence) then
  begin
    Sequence := regnode.AsSequence;
    if Sequence.Count > 0 then
    begin
      SetLength(registers, Sequence.Count);
      for i := 0 to Sequence.Count-1 do
      begin
        itmnode := Sequence.Items[i];
        registers[i].number := itmnode.GetValue('number').AsInteger;
        registers[i].text := itmnode.GetValue('text').AsString;
      end;
    end;
  end;
end;

function TCashRegistersTest.Execute: TTestResult;
var
  CashReg: TCashRegisterRec;
begin
  Result.Text := '';
  Result.IsSucceeded := True;

  for CashReg in registers do
  begin
    Driver.RegisterNumber := CashReg.number;
    if Driver.RegisterNumber <= 255 then
      Check(Driver.GetCashReg)
    else
      Check(Driver.GetCashRegEx);

    Result.IsSucceeded := Driver.ContentsOfCashRegister = expected_value;
    Result.Text := Format('Регистр: %d, Ожидается: %.2f, получено: %.2f', [
      CashReg.number, expected_value, Driver.ContentsOfCashRegister]);
    if not Result.IsSucceeded then Break;
  end;
end;


end.
