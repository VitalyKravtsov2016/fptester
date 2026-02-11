unit OFDApiAuth;

interface

uses
  // VCL
  System.SysUtils, System.Classes, System.JSON, System.Net.HttpClient,
  System.Net.URLClient, System.Net.HttpClientComponent,
  // This
  OFDApiException;

type
  TOFDAuthManager = class
  private
    FApiKey: string;
    FToken: string;
    FTokenExpireTime: TDateTime;
    FBaseUrl: string;
    FHttpClient: TNetHTTPClient;
    
    function IsTokenValid: Boolean;
  public
    constructor Create(const ABaseUrl, AApiKey: string);
    destructor Destroy; override;
    
    function GetToken: string;
    procedure RefreshToken;
    function GetAuthHeader: string;
    
    property ApiKey: string read FApiKey write FApiKey;
    property Token: string read FToken;
    property BaseUrl: string read FBaseUrl write FBaseUrl;
  end;

implementation

uses
  System.DateUtils;

constructor TOFDAuthManager.Create(const ABaseUrl, AApiKey: string);
begin
  inherited Create;
  FBaseUrl := ABaseUrl;
  FApiKey := AApiKey;
  FToken := '';
  FTokenExpireTime := 0;
  
  FHttpClient := TNetHTTPClient.Create(nil);
  FHttpClient.ContentType := 'application/json';
  FHttpClient.Accept := 'application/json';
end;

destructor TOFDAuthManager.Destroy;
begin
  FHttpClient.Free;
  inherited;
end;

function TOFDAuthManager.IsTokenValid: Boolean;
begin
  // Токен валиден, если не истек (с запасом 5 минут)
  Result := (FToken <> '') and (Now < IncMinute(FTokenExpireTime, -5));
end;

procedure TOFDAuthManager.RefreshToken;
var
  Request: TJSONObject;
  Response: IHTTPResponse;
  ResponseJson: TJSONObject;
  TokenValue: string;
begin
  // Формируем запрос
  Request := TJSONObject.Create;
  try
    Request.AddPair('apiKey', FApiKey);

    // Отправляем POST /api/auth
    Response := FHttpClient.Post(
      FBaseUrl + '/api/rent/v2/auth',
      TStringStream.Create(Request.ToString, TEncoding.UTF8),
      nil,
      [TNetHeader.Create('Content-Type', 'application/json')]
    );

    // Проверяем статус
    if Response.StatusCode <> 200 then
      TOFDApiErrorHandler.HandleHttpError(Response.StatusCode, Response.ContentAsString);

    // Парсим ответ
    ResponseJson := TJSONObject.ParseJSONValue(Response.ContentAsString) as TJSONObject;
    try
      TokenValue := ResponseJson.GetValue<string>('token', '');

      if TokenValue = '' then
        raise EInvalidApiKeyException.Create('Не удалось получить токен');

      FToken := TokenValue;
      // JWT токены обычно живут 1 час
      FTokenExpireTime := IncHour(Now, 1);

    finally
      ResponseJson.Free;
    end;

  finally
    Request.Free;
  end;
end;

function TOFDAuthManager.GetToken: string;
begin
  if not IsTokenValid then
    RefreshToken;
  
  Result := FToken;
end;

function TOFDAuthManager.GetAuthHeader: string;
begin
  Result := GetToken;
end;

end.