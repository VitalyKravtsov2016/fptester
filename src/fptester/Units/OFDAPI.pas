unit OFDAPI;

interface

uses
System.SysUtils, System.Classes, System.JSON, System.Net.HttpClient,
System.Net.URLClient, System.NetConsts, System.Net.Mime, System.DateUtils;

type
  TOFDAPI = class
  private
    FBaseURL: string;
    FApiKey: string;
    FHttpClient: THTTPClient;
    FOnError: TProc<string>;
  public
    function GetAuthorizationHeader: TNetHeader;
    function HandleResponse(AResponse: IHTTPResponse): TJSONValue;
    function BuildURL(const AEndpoint: string): string;
  public
    constructor Create(const AApiKey: string;
      const ABaseURL: string = 'https://universal-api.1-ofd.ru/api');
    destructor Destroy; override;

    // Основные методы API
    function GetCashboxes(APage: Integer = 1; ALimit: Integer = 100)
      : TJSONArray;
    function GetCashboxByID(const AID: string): TJSONObject;
    function GetCashboxStatus(const AID: string): TJSONObject;
    function GetReceipt(const AFiscalNumber: string;
      const AFiscalDocument: string): TJSONObject;
    function SendReceipt(const AReceiptData: TJSONObject): TJSONObject;
    function GetOrganizationInfo: TJSONObject;
    function GetDevices: TJSONArray;

    // Утилиты
    procedure SetBaseURL(const AURL: string);
    procedure SetApiKey(const AKey: string);
    function TestConnection: Boolean;

    // События
    property OnError: TProc<string> read FOnError write FOnError;
  end;

implementation

{ TOFDAPI }

constructor TOFDAPI.Create(const AApiKey: string; const ABaseURL: string);
begin
  inherited Create;
  FApiKey := AApiKey;
  FBaseURL := ABaseURL;
  FHttpClient := THTTPClient.Create;
  FHttpClient.ContentType := 'application/json';
  FHttpClient.Accept := 'application/json';
  FHttpClient.ConnectionTimeout := 30000; // 30 секунд
  FHttpClient.ResponseTimeout := 30000;
end;

destructor TOFDAPI.Destroy;
begin
  FHttpClient.Free;
  inherited;
end;

function TOFDAPI.GetAuthorizationHeader: TNetHeader;
begin
  // В зависимости от формата авторизации в API
  // Вариант 1: Bearer Token
  Result := TNetHeader.Create('Authorization', FApiKey);

  // Или вариант 2: X-API-Key
  // Result := TNetHeader.Create('X-API-Key', FApiKey);
end;

function TOFDAPI.BuildURL(const AEndpoint: string): string;
begin
  Result := FBaseURL;
  if not Result.EndsWith('/') then
    Result := Result + '/';

  if AEndpoint.StartsWith('/') then
    Result := Result + AEndpoint.Substring(1)
  else
    Result := Result + AEndpoint;
end;

function TOFDAPI.HandleResponse(AResponse: IHTTPResponse): TJSONValue;
var
  LContent: string;
  LJSON: TJSONValue;
begin
  Result := nil;

  if not Assigned(AResponse) then
  begin
    if Assigned(FOnError) then
      FOnError('Нет ответа от сервера');
    Exit;
  end;

  LContent := AResponse.ContentAsString;

  try
    if (AResponse.StatusCode >= 200) and (AResponse.StatusCode < 300) then
    begin
      if LContent.Trim.IsEmpty then
        Result := TJSONObject.Create
      else
        Result := TJSONObject.ParseJSONValue(LContent);
    end
    else
    begin
      if Assigned(FOnError) then
        FOnError(Format('Ошибка %d: %s', [AResponse.StatusCode, LContent]));

      // Пытаемся распарсить ошибку
      try
        LJSON := TJSONObject.ParseJSONValue(LContent);
        if Assigned(LJSON) then
        begin
          if Assigned(FOnError) then
          begin
            if LJSON.TryGetValue<string>('message', LContent) then
              FOnError(LContent)
            else if LJSON.TryGetValue<string>('error', LContent) then
              FOnError(LContent);
          end;
          LJSON.Free;
        end;
      except
        // Игнорируем ошибки парсинга JSON ошибки
      end;
    end;
  except
    on E: Exception do
    begin
      if Assigned(FOnError) then
        FOnError('Ошибка обработки ответа: ' + E.Message);
    end;
  end;
end;

function TOFDAPI.GetCashboxes(APage, ALimit: Integer): TJSONArray;
var
  LResponse: IHTTPResponse;
  LURL: string;
  LJSON: TJSONValue;
begin
  Result := nil;

  LURL := BuildURL('v2/kkms') + Format('?page=%d&limit=%d', [APage, ALimit]);

  try
    FHttpClient.CustomHeaders['Authorization'] := 'Bearer ' + FApiKey;
    LResponse := FHttpClient.Get(LURL);

    LJSON := HandleResponse(LResponse);
    if Assigned(LJSON) then
    begin
      if LJSON is TJSONArray then
        Result := TJSONArray(LJSON)
      else if (LJSON is TJSONObject) and TJSONObject(LJSON)
        .TryGetValue<TJSONArray>('data', Result) then
      begin
        // data найден в объекте
      end
      else if (LJSON is TJSONObject) and TJSONObject(LJSON)
        .TryGetValue<TJSONArray>('cashboxes', Result) then
      begin
        // cashboxes найден в объекте
      end
      else
      begin
        LJSON.Free;
        if Assigned(FOnError) then
          FOnError('Неверный формат ответа для списка касс');
      end;
    end;
  except
    on E: Exception do
    begin
      if Assigned(FOnError) then
        FOnError('Ошибка при получении списка касс: ' + E.Message);
    end;
  end;
end;

function TOFDAPI.GetCashboxByID(const AID: string): TJSONObject;
var
  LResponse: IHTTPResponse;
  LURL: string;
  LJSON: TJSONValue;
begin
  Result := nil;

  if AID.IsEmpty then
  begin
    if Assigned(FOnError) then
      FOnError('Не указан ID кассы');
    Exit;
  end;

  LURL := BuildURL('v2/kkms/' + AID);

  try
    FHttpClient.CustomHeaders['Authorization'] := 'Bearer ' + FApiKey;
    LResponse := FHttpClient.Get(LURL);

    LJSON := HandleResponse(LResponse);
    if Assigned(LJSON) and (LJSON is TJSONObject) then
      Result := TJSONObject(LJSON)
    else if Assigned(LJSON) then
      LJSON.Free;
  except
    on E: Exception do
    begin
      if Assigned(FOnError) then
        FOnError('Ошибка при получении информации о кассе: ' + E.Message);
    end;
  end;
end;

function TOFDAPI.GetReceipt(const AFiscalNumber, AFiscalDocument: string)
  : TJSONObject;
var
  LResponse: IHTTPResponse;
  LURL: string;
  LJSON: TJSONValue;
begin
  Result := nil;

  if AFiscalNumber.IsEmpty or AFiscalDocument.IsEmpty then
  begin
    if Assigned(FOnError) then
      FOnError('Не указаны фискальные данные');
    Exit;
  end;

  LURL := BuildURL('v2/receipts') + Format('?fn=%s&fd=%s',
    [AFiscalNumber, AFiscalDocument]);

  try
    FHttpClient.CustomHeaders['Authorization'] := 'Bearer ' + FApiKey;
    LResponse := FHttpClient.Get(LURL);

    LJSON := HandleResponse(LResponse);
    if Assigned(LJSON) and (LJSON is TJSONObject) then
      Result := TJSONObject(LJSON)
    else if Assigned(LJSON) then
      LJSON.Free;
  except
    on E: Exception do
    begin
      if Assigned(FOnError) then
        FOnError('Ошибка при получении чека: ' + E.Message);
    end;
  end;
end;

function TOFDAPI.SendReceipt(const AReceiptData: TJSONObject): TJSONObject;
var
  LResponse: IHTTPResponse;
  LURL: string;
  LStream: TStringStream;
  LJSON: TJSONValue;
begin
  Result := nil;

  if not Assigned(AReceiptData) then
  begin
    if Assigned(FOnError) then
      FOnError('Нет данных чека для отправки');
    Exit;
  end;

  LURL := BuildURL('v2/receipts');
  LStream := TStringStream.Create(AReceiptData.ToJSON, TEncoding.UTF8);

  try
    try
      FHttpClient.CustomHeaders['Authorization'] := 'Bearer ' + FApiKey;
      FHttpClient.ContentType := 'application/json';
      LResponse := FHttpClient.Post(LURL, LStream);

      LJSON := HandleResponse(LResponse);
      if Assigned(LJSON) and (LJSON is TJSONObject) then
        Result := TJSONObject(LJSON)
      else if Assigned(LJSON) then
        LJSON.Free;
    except
      on E: Exception do
      begin
        if Assigned(FOnError) then
          FOnError('Ошибка при отправке чека: ' + E.Message);
      end;
    end;
  finally
    LStream.Free;
  end;
end;

function TOFDAPI.GetCashboxStatus(const AID: string): TJSONObject;
var
  LResponse: IHTTPResponse;
  LURL: string;
  LJSON: TJSONValue;
begin
  Result := nil;

  if AID.IsEmpty then
  begin
    if Assigned(FOnError) then
      FOnError('Не указан ID кассы');
    Exit;
  end;

  LURL := BuildURL('v2/kkms/' + AID + '/status');

  try
    FHttpClient.CustomHeaders['Authorization'] := 'Bearer ' + FApiKey;
    LResponse := FHttpClient.Get(LURL);

    LJSON := HandleResponse(LResponse);
    if Assigned(LJSON) and (LJSON is TJSONObject) then
      Result := TJSONObject(LJSON)
    else if Assigned(LJSON) then
      LJSON.Free;
  except
    on E: Exception do
    begin
      if Assigned(FOnError) then
        FOnError('Ошибка при получении статуса кассы: ' + E.Message);
    end;
  end;
end;

function TOFDAPI.GetOrganizationInfo: TJSONObject;
var
  LResponse: IHTTPResponse;
  LURL: string;
  LJSON: TJSONValue;
begin
  Result := nil;

  LURL := BuildURL('v2/organization');

  try
    FHttpClient.CustomHeaders['Authorization'] := 'Bearer ' + FApiKey;
    LResponse := FHttpClient.Get(LURL);

    LJSON := HandleResponse(LResponse);
    if Assigned(LJSON) and (LJSON is TJSONObject) then
      Result := TJSONObject(LJSON)
    else if Assigned(LJSON) then
      LJSON.Free;
  except
    on E: Exception do
    begin
      if Assigned(FOnError) then
        FOnError('Ошибка при получении информации об организации: ' +
          E.Message);
    end;
  end;
end;

function TOFDAPI.GetDevices: TJSONArray;
var
  LResponse: IHTTPResponse;
  LURL: string;
  LJSON: TJSONValue;
begin
  Result := nil;

  LURL := BuildURL('v2/devices');

  try
    FHttpClient.CustomHeaders['Authorization'] := 'Bearer ' + FApiKey;
    LResponse := FHttpClient.Get(LURL);

    LJSON := HandleResponse(LResponse);
    if Assigned(LJSON) then
    begin
      if LJSON is TJSONArray then
        Result := TJSONArray(LJSON)
      else if (LJSON is TJSONObject) and TJSONObject(LJSON)
        .TryGetValue<TJSONArray>('devices', Result) then
      begin
        // devices найден
      end
      else
      begin
        LJSON.Free;
        if Assigned(FOnError) then
          FOnError('Неверный формат ответа для списка устройств');
      end;
    end;
  except
    on E: Exception do
    begin
      if Assigned(FOnError) then
        FOnError('Ошибка при получении списка устройств: ' + E.Message);
    end;
  end;
end;

procedure TOFDAPI.SetBaseURL(const AURL: string);
begin
  FBaseURL := AURL;
end;

procedure TOFDAPI.SetApiKey(const AKey: string);
begin
  FApiKey := AKey;
end;

function TOFDAPI.TestConnection: Boolean;
var
  LResponse: IHTTPResponse;
  LURL: string;
begin
  LURL := BuildURL('v2/ping'); // или другой эндпоинт для проверки

  try
    FHttpClient.CustomHeaders['Authorization'] := 'Bearer ' + FApiKey;
    LResponse := FHttpClient.Get(LURL);
    Result := (LResponse.StatusCode = 200) or (LResponse.StatusCode = 204);
  except
    Result := False;
  end;
end;

end.
