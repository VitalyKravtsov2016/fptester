unit CommandTest;

interface

uses
  // VCL
  Windows, Classes, SysUtils, System.Generics.Collections,
  // 3'd
  VSoft.YAML,
  // This
  DriverTest, DrvFRLib_TLB;

type
  { TCommandTest }

  TCommandTest = class(TDriverTest)
  public
    command: string;
    response: string;
    resultcode: Integer;
    printed_text: string;
    baudrate_after: Integer;

    function Execute: TTestResult; override;
    procedure Load(node: IYAMLValue); override;
  end;

  { TCommandsTest }

  TCommandsTest = class(TDriverTest)
  private
    FCommands: TStringList;
  public
    constructor CreateTest(AContext: TDriverContext); override;
    destructor Destroy; override;
    function Execute: TTestResult; override;
    procedure Load(node: IYAMLValue); override;
    property Commands: TStringList read FCommands;
  end;

implementation

//////////////////////////////////////////////////////////////////////////////
//
// � expected ����� �������� X ������� �����, ������� �� ���������

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

{ TCommandTest }

procedure TCommandTest.Load(node: IYAMLValue);
var
  value : IYAMLValue;
begin
  inherited Load(node);
  command := '';
  if node.TryGetValue('command', value) then
    command := value.AsString;
  response := '';
  if node.TryGetValue('response', value) then
    response := value.AsString;

  baudrate_after := 6;
  if node.TryGetValue('baudrate_after', value) then
    baudrate_after := value.AsInteger;

  resultcode := 0;
  if node.TryGetValue('resultcode', value) then
    resultcode := value.AsInteger;

  printed_text := '';
  if node.TryGetValue('printed_text', value) then
    printed_text := value.AsString;
end;

function TCommandTest.Execute: TTestResult;
var
  ResCode: Integer;
begin
  Result.IsSucceeded := False;

  try
    if Options.Verbose then
    begin
      Context.Debug('-> ' + Command);
    end;
    Driver.BinaryConversion := BINARY_CONVERSION_HEX;
    Driver.TransferBytes := Command;
    ResCode := Driver.ExchangeBytes;
    if ResCode = 0 then
    begin
      Context.Check(ResCode);
      if Options.Verbose then
      begin
        Context.Debug('<- ' + Driver.TransferBytes);
      end;

      Result.IsSucceeded := CompareResponse(Driver.TransferBytes, response);
      if not Result.IsSucceeded then
      begin
        Result.Text := Format('Ожидается: %s, получено: %s', [
          Trim(response), Trim(Driver.TransferBytes)]);
        if Options.Verbose then
        begin
          Context.Error('Ошибка: ответ не совпадает');
          Context.Error(Format('Ожидаемый ответ : "%s"', [Trim(response)]));
          Context.Error(Format('Получен ответ   : "%s"', [Trim(Driver.TransferBytes)]));
        end;
        Exit;
      end;

      if printed_text <> '' then
        Context.CheckTextPrinted(printed_text);

      if baudrate_after <> Driver.BaudRate then
      begin
        if (Driver.ConnectionType = 0) then
        begin
          Driver.Disconnect;
          Driver.BaudRate := baudrate_after;
          Check(Driver.GetShortECRStatus);
          Driver.SaveParams;
        end;
      end;
    end else
    begin
      if ResCode = ResultCode then
      begin
        Result.IsSucceeded := True;
      end else
      begin
        Result.Text := Format('Ожидается: %d, получено: %d, %s', [
          ResultCode, ResCode, Driver.ResultCodeDescription]);
        if Options.Verbose then
        begin
          Context.Error('Ошибка: код ошибки не совпадает');
          Context.Error(Format('Ожидаемый код ошибки: %d.', [ResultCode]));
          Context.Error(Format('Получен код ошибки: %d.', [ResCode]));
        end;
      end;
    end;
  except
    on E: Exception do
    begin
      Result.Text := E.Message;
      if Options.Verbose then
      begin
        Context.Debug('Ошибка: ' + E.Message);
      end;
    end;
  end;
end;


{ TCommandsTest }

constructor TCommandsTest.CreateTest(AContext: TDriverContext);
begin
  inherited CreateTest(AContext);
  FCommands := TStringList.Create;
end;

destructor TCommandsTest.Destroy;
begin
  FCommands.Free;
  inherited Destroy;
end;

procedure TCommandsTest.Load(node: IYAMLValue);
var
  value: IYAMLValue;
  seq: IYAMLSequence;
  i: Integer;
  Line: string;
begin
  inherited Load(node);
  FCommands.Clear;
  if node.TryGetValue('commands', value) and value.IsSequence then
  begin
    seq := value.AsSequence;
    for i := 0 to seq.Count - 1 do
    begin
      Line := Trim(seq.Items[i].AsString);
      if Line <> '' then
        FCommands.Add(Line);
    end;
  end;
end;

function TCommandsTest.Execute: TTestResult;
var
  i: Integer;
  ResCode: Integer;
  Line: string;
begin
  Result.IsSucceeded := False;
  Result.Text := '';
  if FCommands.Count = 0 then
  begin
    Result.Text := 'commands list is empty';
    Exit;
  end;
  try
    Driver.BinaryConversion := BINARY_CONVERSION_HEX;
    for i := 0 to FCommands.Count - 1 do
    begin
      Line := FCommands[i];
      if Options.Verbose then
        Context.Debug('-> ' + Line);
      Driver.TransferBytes := Line;
      ResCode := Driver.ExchangeBytes;
      if Options.Verbose then
        Context.Debug(Format('<- %d %s', [ResCode, Driver.TransferBytes]));
      if ResCode <> 0 then
      begin
        Result.Text := Format('command "%s": %d, %s',
          [Line, ResCode, Driver.ResultCodeDescription]);
        Exit;
      end;
    end;
    Result.IsSucceeded := True;
  except
    on E: Exception do
      Result.Text := E.Message;
  end;
end;

end.
