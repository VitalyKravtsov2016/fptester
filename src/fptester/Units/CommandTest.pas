unit CommandTest;

interface

uses
  // VCL
  Windows, SysUtils, System.Generics.Collections,
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

implementation

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

{ TCommandTest }

procedure TCommandTest.Load(node: IYAMLValue);
var
  value : IYAMLValue;
begin
  inherited Load(node);
  command := node.GetValue('command').AsString;
  response := node.GetValue('response').AsString;

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
      WriteLn('-> ' + Command);
    end;
    Driver.BinaryConversion := BINARY_CONVERSION_HEX;
    Driver.TransferBytes := Command;
    ResCode := Driver.ExchangeBytes;
    if ResCode = 0 then
    begin
      Context.Check(ResCode);
      if Options.Verbose then
      begin
        WriteLn('<- ' + Driver.TransferBytes);
      end;

      Result.IsSucceeded := CompareResponse(Driver.TransferBytes, response);
      if not Result.IsSucceeded then
      begin
        Result.Text := Format('ожидается: %s, получен: %s', [
          Trim(response), Trim(Driver.TransferBytes)]);
        if Options.Verbose then
        begin
          Context.SetErrorColor;
          WriteLn('Ошибка: ответы не совпадают');
          WriteLn(Format('Ожидается ответ : "%s"', [Trim(response)]));
          WriteLn(Format('Получен ответ   : "%s"', [Trim(Driver.TransferBytes)]));
          Context.SetNormalColor;
        end;
        Exit;
      end;

      (* !!!
      if Test.printed_text <> '' then
      begin
        Result.IsSucceeded := TextServer.Text = Test.printed_text;
        if not Result.IsSucceeded then
        begin
          Result.Text := Format('ожидается: %s, получен: %s', [
            Trim(Test.printed_text), Trim(TextServer.Text)]);
        end;
      end;
      *)

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
        Result.Text := Format('ожидается: %d, получен: %d', [ResultCode, ResCode]);
        if Options.Verbose then
        begin
          Context.SetErrorColor;
          WriteLn('Ошибка: код отшибки не совпадает');
          WriteLn(Format('Ожидается код ошибки: %d.', [ResultCode]));
          WriteLn(Format('Получен код ошибки: %d.', [ResCode]));
          Context.SetNormalColor;
        end;
      end;
    end;
  except
    on E: Exception do
    begin
      Result.Text := E.Message;
      if Options.Verbose then
      begin
        WriteLn('Ошибка: ' + E.Message);
      end;
    end;
  end;
end;


end.
