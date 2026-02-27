unit OFDApiClient;

interface

uses
  // VCL
  System.SysUtils, System.Classes, System.JSON, System.Generics.Collections,
  System.Net.HttpClient, System.Net.URLClient, System.DateUtils,
  System.Net.HttpClientComponent,
  // This
  OFDApiTypes, OFDApiModels, OFDApiAuth, OFDApiException,
  // Добавленные модули
  OFDApiConfig, OFDApiLogger, OFDApiConsts;

type
  { Параметры запроса документов }
  
  TDocumentQueryParams = record
    KkmRegId: string;
    FsFactoryNumber: string;
    ShiftNum: Integer;
    FromDate: TDateTime;
    ToDate: TDateTime;
    FromInsDate: TDateTime;
    ToInsDate: TDateTime;
    TransactionTypes: TArray<TTransactionType>;
    IrkktStatuses: TArray<TFnsFlcStatus>;
    
    procedure Clear;
    function HasShiftNum: Boolean;
    function HasDateRange: Boolean;
    function HasInsDateRange: Boolean;
    function ToQueryString: string;
  end;

  { TOFDApiClient - Главный клиент API }
  
  TOFDApiClient = class
  private
    FAuthManager: TOFDAuthManager;
    FHttpClient: TNetHTTPClient;
    FBaseUrl: string;
    
    function ExecuteRequest(const Method, Endpoint: string;
      const Body: string = ''): IHTTPResponse;
    function Get(const Endpoint: string): IHTTPResponse;
    function Post(const Endpoint, Body: string): IHTTPResponse;
    
    procedure CheckResponse(const Response: IHTTPResponse);
    function BuildUrl(const Path: string; const Params: TArray<TPair<string, string>>): string;
    
  public
    constructor Create(const ABaseUrl, AApiKey: string);
    destructor Destroy; override;
    
    // 2. Авторизация
    procedure Authorize;
    
    // 3. Получение списка организаций
    function GetOrganisations(Page: Integer = 1; PageSize: Integer = 10): TOrganisationList;
    
    // 4. Получение списка разрешений
    function GetPermissions(const OrganisationKey: string): TPermissions;
    
    // 5. Получение списка торговых точек
    function GetRetailPlaces(const OrganisationKey: string): TRetailPlaceList;
    
    // 6. Получение выручки по торговой точке
    function GetRevenue(const OrganisationKey: string;
      const RetailPlaces: TArray<Integer>;
      const DateFrom, DateTo: TDateTime): TJSONObject;
    
    // 7. Получение списка касс
    function GetKkms(const OrganisationKey: string;
      RetailPlaceId: Integer = 0;
      const MonitoringFilter: string = ''): TObjectList<TKkm>;
    
    // 8. Получение списка смен по кассе
    function GetShifts(const OrganisationKey: string;
      const Params: TDocumentQueryParams): TObjectList<TShift>;
    
    // 9. Получение списка открытых смен
    function GetOpenShifts(const OrganisationKey: string;
      const RegId: string;
      RetailPlaceId: Integer;
      const DateFrom, DateTo: TDateTime): TObjectList<TShift>;
    
    // 11. Получение списка документов
    function GetDocuments(const OrganisationKey: string;
      const Params: TDocumentQueryParams): TObjectList<TDocument>;
    
    // 12. Получение документов по виртуальной кассе
    function GetVirtualDocuments(const OrganisationKey: string;
      VirtualKkmId: Integer;
      const Params: TDocumentQueryParams): TObjectList<TDocument>;
    
    // 13. Получение документов по доп. реквизитам
    function GetDocumentsByUserProperty(const OrganisationKey: string;
      const UserPropertyKey, UserPropertyValue: string;
      const FromDate, ToDate: TDateTime): TObjectList<TDocument>;
    
    property BaseUrl: string read FBaseUrl write FBaseUrl;
    property AuthManager: TOFDAuthManager read FAuthManager;
  end;

// Константы capabilities (оставлены для обратной совместимости)
const
  CAP_NDS_INFO = 'universal-api.ndsInfo';
  CAP_OPERATOR_INN = 'universal-api.operatorInnInfo';
  CAP_SHIFTS = 'universal-api.shifts';
  CAP_TRANSACTIONS = 'universal-api.transactions';
  CAP_NOMENCLATURE = 'universal-api.nomenclature';
  CAP_BUYER_INFO = 'universal-api.buyerInfo';
  CAP_REVENUE = 'universal-api.revenue';
  CAP_USER_PROPERTY_SEARCH = 'universal-api.user-property-search';
  CAP_VIRTUAL_TRANSACTIONS = 'universal-api.virtual.transactions';
  CAP_VIRTUAL_NOMENCLATURE = 'universal-api.virtual.nomenclature';

implementation

uses
  System.NetEncoding;

{ TDocumentQueryParams }

procedure TDocumentQueryParams.Clear;
begin
  KkmRegId := '';
  FsFactoryNumber := '';
  ShiftNum := 0;
  FromDate := 0;
  ToDate := 0;
  FromInsDate := 0;
  ToInsDate := 0;
  SetLength(TransactionTypes, 0);
  SetLength(IrkktStatuses, 0);
end;

function TDocumentQueryParams.HasShiftNum: Boolean;
begin
  Result := ShiftNum > 0;
end;

function TDocumentQueryParams.HasDateRange: Boolean;
begin
  Result := (FromDate > 0) and (ToDate > 0);
end;

function TDocumentQueryParams.HasInsDateRange: Boolean;
begin
  Result := (FromInsDate > 0) and (ToInsDate > 0);
end;

function TDocumentQueryParams.ToQueryString: string;
var
  Params: TStringList;
  TType: TTransactionType;
  Status: TFnsFlcStatus;
begin
  Params := TStringList.Create;
  try
    if KkmRegId <> '' then
      Params.Add('kkmRegId=' + TNetEncoding.URL.Encode(KkmRegId));

    if FsFactoryNumber <> '' then
      Params.Add('fsFactoryNumber=' + TNetEncoding.URL.Encode(FsFactoryNumber));

    if HasShiftNum then
      Params.Add('shiftNum=' + IntToStr(ShiftNum));

    if HasDateRange then
    begin
      Params.Add('dateFrom=' + TNetEncoding.URL.Encode(DateTimeToISO8601(FromDate)));
      Params.Add('dateTo=' + TNetEncoding.URL.Encode(DateTimeToISO8601(ToDate)));
    end;

    if HasInsDateRange then
    begin
      Params.Add('fromInsDate=' + TNetEncoding.URL.Encode(DateTimeToISO8601(FromInsDate)));
      Params.Add('toInsDate=' + TNetEncoding.URL.Encode(DateTimeToISO8601(ToInsDate)));
    end;

    for TType in TransactionTypes do
      Params.Add('transactionTypes=' + TransactionTypeToString(TType));
    
    for Status in IrkktStatuses do
      Params.Add('irkktStatus=' + FnsFlcStatusToString(Status));
    
    Result := Params.DelimitedText;
    Result := StringReplace(Result, ',', '&', [rfReplaceAll]);
  finally
    Params.Free;
  end;
end;

{ TOFDApiClient }

constructor TOFDApiClient.Create(const ABaseUrl, AApiKey: string);
var
  ConfigTimeout: Integer;
begin
  inherited Create;
  
  // Используем переданные параметры, но если они пусты, пытаемся взять из конфигурации
  if (ABaseUrl = '') and Assigned(GlobalConfig) then
    FBaseUrl := GlobalConfig.BaseUrl
  else
    FBaseUrl := ABaseUrl;
    
  FAuthManager := TOFDAuthManager.Create(FBaseUrl, AApiKey);
  
  FHttpClient := TNetHTTPClient.Create(nil);
  FHttpClient.ContentType := CONTENT_TYPE_JSON;
  FHttpClient.Accept := CONTENT_TYPE_JSON;
  
  // Устанавливаем таймауты: сначала из конфига, иначе константа по умолчанию
  ConfigTimeout := DEFAULT_TIMEOUT;
  if Assigned(GlobalConfig) then
    ConfigTimeout := GlobalConfig.Timeout;
    
  FHttpClient.ConnectionTimeout := ConfigTimeout;
  FHttpClient.ResponseTimeout := ConfigTimeout;
  
  Logger.Info('TOFDApiClient created. BaseUrl: %s', [FBaseUrl]);
end;

destructor TOFDApiClient.Destroy;
begin
  Logger.Info('TOFDApiClient destroyed');
  FAuthManager.Free;
  FHttpClient.Free;
  inherited;
end;

procedure TOFDApiClient.Authorize;
begin
  Logger.Debug('Authorize called');
  FAuthManager.RefreshToken;
  Logger.Info('Authorization completed');
end;

function TOFDApiClient.ExecuteRequest(const Method, Endpoint: string;
  const Body: string = ''): IHTTPResponse;
var
  Headers: TNetHeaders;
  Stream: TStringStream;
  FullUrl: string;
begin
  FullUrl := FBaseUrl + Endpoint;
  Logger.Debug('HTTP %s: %s', [Method, FullUrl]);
  if Body <> '' then
    Logger.Debug('Request body: %s', [Body]);
  
  // Добавляем заголовок авторизации
  SetLength(Headers, 2);
  Headers[0] := TNetHeader.Create(HEADER_AUTHORIZATION, FAuthManager.GetAuthHeader);
  Headers[1] := TNetHeader.Create(HEADER_ACCEPT, CONTENT_TYPE_JSON);
  
  if Method = HTTP_GET then
  begin
    Result := FHttpClient.Get(FullUrl, nil, Headers);
  end
  else if Method = HTTP_POST then
  begin
    Stream := TStringStream.Create(Body, TEncoding.UTF8);
    try
      SetLength(Headers, 3);
      Headers[2] := TNetHeader.Create(HEADER_CONTENT_TYPE, CONTENT_TYPE_JSON);
      Result := FHttpClient.Post(FullUrl, Stream, nil, Headers);
    finally
      Stream.Free;
    end;
  end
  else
    raise Exception.CreateFmt('Неподдерживаемый HTTP метод: %s', [Method]);
  
  Logger.Debug('Response status: %d', [Result.StatusCode]);
  Logger.Debug(Result.ContentAsString());
  CheckResponse(Result);
end;

function TOFDApiClient.Get(const Endpoint: string): IHTTPResponse;
begin
  Result := ExecuteRequest(HTTP_GET, Endpoint);
end;

function TOFDApiClient.Post(const Endpoint, Body: string): IHTTPResponse;
begin
  Result := ExecuteRequest(HTTP_POST, Endpoint, Body);
end;

procedure TOFDApiClient.CheckResponse(const Response: IHTTPResponse);
begin
  if Response.StatusCode >= 400 then
  begin
    Logger.Error('HTTP error %d: %s', [Response.StatusCode, Response.ContentAsString]);
    TOFDApiErrorHandler.HandleHttpError(Response.StatusCode, Response.ContentAsString);
  end;
end;

function TOFDApiClient.BuildUrl(const Path: string;
  const Params: TArray<TPair<string, string>>): string;
var
  QueryString: TStringBuilder;
  Param: TPair<string, string>;
  First: Boolean;
begin
  Result := Path;
  
  if Length(Params) > 0 then
  begin
    QueryString := TStringBuilder.Create;
    try
      First := True;
      for Param in Params do
      begin
        if not First then
          QueryString.Append('&')
        else
          QueryString.Append('?');
        
        QueryString.Append(TNetEncoding.URL.Encode(Param.Key));
        QueryString.Append('=');
        QueryString.Append(TNetEncoding.URL.Encode(Param.Value));
        
        First := False;
      end;
      
      Result := Result + QueryString.ToString;
    finally
      QueryString.Free;
    end;
  end;
end;

function TOFDApiClient.GetOrganisations(Page: Integer = 1;
  PageSize: Integer = 10): TOrganisationList;
var
  Response: IHTTPResponse;
  JsonObj: TJSONObject;
  Endpoint: string;
begin
  Logger.Info('GetOrganisations: page=%d, pageSize=%d', [Page, PageSize]);
  
  Endpoint := Format(API_ORGANISATIONS_LIST, [Page, PageSize]);
  
  Response := Get(Endpoint);
  
  Result := TOrganisationList.Create;
  try
    JsonObj := TJSONObject.ParseJSONValue(Response.ContentAsString) as TJSONObject;
    try
      Result.FromJson(JsonObj);
      Logger.Debug('Organisations loaded: %d items, total: %d', 
        [Result.Page.Count, Result.TotalCount]);
    finally
      JsonObj.Free;
    end;
  except
    Result.Free;
    Logger.Error('Failed to parse organisations response');
    raise;
  end;
end;

function TOFDApiClient.GetPermissions(const OrganisationKey: string): TPermissions;
var
  Response: IHTTPResponse;
  JsonObj: TJSONObject;
  Endpoint: string;
begin
  Logger.Info('GetPermissions: organisationKey=%s', [OrganisationKey]);
  
  Endpoint := Format(API_PERMISSIONS, [TNetEncoding.URL.Encode(OrganisationKey)]);
  
  Response := Get(Endpoint);
  
  Result := TPermissions.Create;
  try
    JsonObj := TJSONObject.ParseJSONValue(Response.ContentAsString) as TJSONObject;
    try
      Result.FromJson(JsonObj);
      Logger.Debug('Permissions loaded, capabilities: %s', 
        [string.Join(', ', Result.Capabilities)]);
    finally
      JsonObj.Free;
    end;
  except
    Result.Free;
    Logger.Error('Failed to parse permissions response');
    raise;
  end;
end;

function TOFDApiClient.GetRetailPlaces(const OrganisationKey: string): TRetailPlaceList;
var
  Response: IHTTPResponse;
  JsonObj: TJSONObject;
  Endpoint: string;
begin
  Logger.Info('GetRetailPlaces: organisationKey=%s', [OrganisationKey]);
  
  Endpoint := Format(API_RETAIL_PLACES, [TNetEncoding.URL.Encode(OrganisationKey)]);
  
  Response := Get(Endpoint);
  
  Result := TRetailPlaceList.Create;
  try
    JsonObj := TJSONObject.ParseJSONValue(Response.ContentAsString) as TJSONObject;
    try
      Result.FromJson(JsonObj);
      Logger.Debug('Retail places loaded: %d items', [Result.RetailPlaces.Count]);
    finally
      JsonObj.Free;
    end;
  except
    Result.Free;
    Logger.Error('Failed to parse retail places response');
    raise;
  end;
end;

function TOFDApiClient.GetRevenue(const OrganisationKey: string;
  const RetailPlaces: TArray<Integer>;
  const DateFrom, DateTo: TDateTime): TJSONObject;
var
  Response: IHTTPResponse;
  Endpoint: string;
  QueryParams: TStringBuilder;
  PlaceId: Integer;
begin
  Logger.Info('GetRevenue: organisationKey=%s, dateFrom=%s, dateTo=%s',
    [OrganisationKey, DateTimeToISO8601(DateFrom), DateTimeToISO8601(DateTo)]);
  
  QueryParams := TStringBuilder.Create;
  try
    // Добавляем торговые точки
    for PlaceId in RetailPlaces do
    begin
      if QueryParams.Length > 0 then
        QueryParams.Append('&');
      QueryParams.AppendFormat('retailPlaces=%d', [PlaceId]);
    end;
    
    // Добавляем даты
    QueryParams.AppendFormat('&dateFrom=%s', [
      TNetEncoding.URL.Encode(DateTimeToISO8601(DateFrom))
    ]);
    QueryParams.AppendFormat('&dateTo=%s', [
      TNetEncoding.URL.Encode(DateTimeToISO8601(DateTo))
    ]);
    
    Endpoint := Format(API_REVENUE, [
      TNetEncoding.URL.Encode(OrganisationKey),
      QueryParams.ToString
    ]);
  finally
    QueryParams.Free;
  end;
  
  Response := Get(Endpoint);
  Result := TJSONObject.ParseJSONValue(Response.ContentAsString) as TJSONObject;
  
  Logger.Debug('Revenue data received, size: %d bytes', [Length(Response.ContentAsString)]);
end;

function TOFDApiClient.GetKkms(const OrganisationKey: string;
  RetailPlaceId: Integer = 0;
  const MonitoringFilter: string = ''): TObjectList<TKkm>;
var
  Response: IHTTPResponse;
  JsonObj: TJSONObject;
  KkmsArray: TJSONArray;
  Endpoint: string;
  QueryParams: TStringList;
  I: Integer;
  Kkm: TKkm;
begin
  Logger.Info('GetKkms: organisationKey=%s, retailPlaceId=%d, filter=%s',
    [OrganisationKey, RetailPlaceId, MonitoringFilter]);
  
  QueryParams := TStringList.Create;
  try
    if RetailPlaceId > 0 then
      QueryParams.Add('retailPlaceId=' + IntToStr(RetailPlaceId));
    
    if MonitoringFilter <> '' then
      QueryParams.Add('monitoringFilter=' + TNetEncoding.URL.Encode(MonitoringFilter));
    
    Endpoint := Format(API_KKMS, [
      TNetEncoding.URL.Encode(OrganisationKey)
    ]);
    
    if QueryParams.Count > 0 then
      Endpoint := Endpoint + '?' + QueryParams.DelimitedText.Replace(',', '&');
  finally
    QueryParams.Free;
  end;
  
  Response := Get(Endpoint);
  
  Result := TObjectList<TKkm>.Create(True);
  try
    JsonObj := TJSONObject.ParseJSONValue(Response.ContentAsString) as TJSONObject;
    try
      KkmsArray := JsonObj.GetValue<TJSONArray>('kkms');
      if Assigned(KkmsArray) then
      begin
        for I := 0 to KkmsArray.Count - 1 do
        begin
          Kkm := TKkm.Create;
          Kkm.FromJson(KkmsArray.Items[I] as TJSONObject);
          Result.Add(Kkm);
        end;
      end;
      Logger.Debug('KKMs loaded: %d items', [Result.Count]);
    finally
      JsonObj.Free;
    end;
  except
    Result.Free;
    Logger.Error('Failed to parse KKMs response');
    raise;
  end;
end;

function TOFDApiClient.GetShifts(const OrganisationKey: string;
  const Params: TDocumentQueryParams): TObjectList<TShift>;
var
  Response: IHTTPResponse;
  JsonObj: TJSONObject;
  ShiftsArray: TJSONArray;
  Endpoint: string;
  I: Integer;
  Shift: TShift;
begin
  Logger.Info('GetShifts: organisationKey=%s, kkmRegId=%s',
    [OrganisationKey, Params.KkmRegId]);
  
  Endpoint := Format(API_SHIFTS, [
    TNetEncoding.URL.Encode(OrganisationKey),
    Params.ToQueryString
  ]);
  
  Response := Get(Endpoint);
  
  Result := TObjectList<TShift>.Create(True);
  try
    JsonObj := TJSONObject.ParseJSONValue(Response.ContentAsString) as TJSONObject;
    try
      ShiftsArray := JsonObj.GetValue<TJSONArray>('kkmShifts');
      if Assigned(ShiftsArray) then
      begin
        for I := 0 to ShiftsArray.Count - 1 do
        begin
          Shift := TShift.Create;
          Shift.FromJson(ShiftsArray.Items[I] as TJSONObject);
          Result.Add(Shift);
        end;
      end;
      Logger.Debug('Shifts loaded: %d items', [Result.Count]);
    finally
      JsonObj.Free;
    end;
  except
    Result.Free;
    Logger.Error('Failed to parse shifts response');
    raise;
  end;
end;

function TOFDApiClient.GetOpenShifts(const OrganisationKey: string;
  const RegId: string; RetailPlaceId: Integer;
  const DateFrom, DateTo: TDateTime): TObjectList<TShift>;
var
  Response: IHTTPResponse;
  JsonObj: TJSONObject;
  ShiftsArray: TJSONArray;
  Endpoint: string;
  QueryParams: TStringList;
  I: Integer;
  Shift: TShift;
begin
  Logger.Info('GetOpenShifts: organisationKey=%s, regId=%s, retailPlaceId=%d',
    [OrganisationKey, RegId, RetailPlaceId]);
  
  QueryParams := TStringList.Create;
  try
    QueryParams.Add('regId=' + TNetEncoding.URL.Encode(RegId));
    QueryParams.Add('retailPlaceId=' + IntToStr(RetailPlaceId));
    QueryParams.Add('dateFrom=' + TNetEncoding.URL.Encode(DateTimeToISO8601(DateFrom)));
    QueryParams.Add('dateTo=' + TNetEncoding.URL.Encode(DateTimeToISO8601(DateTo)));
    
    Endpoint := Format(API_OPEN_SHIFTS, [
      TNetEncoding.URL.Encode(OrganisationKey),
      QueryParams.DelimitedText.Replace(',', '&')
    ]);
  finally
    QueryParams.Free;
  end;
  
  Response := Get(Endpoint);
  
  Result := TObjectList<TShift>.Create(True);
  try
    JsonObj := TJSONObject.ParseJSONValue(Response.ContentAsString) as TJSONObject;
    try
      ShiftsArray := JsonObj.GetValue<TJSONArray>('kkmShifts');
      if Assigned(ShiftsArray) then
      begin
        for I := 0 to ShiftsArray.Count - 1 do
        begin
          Shift := TShift.Create;
          Shift.FromJson(ShiftsArray.Items[I] as TJSONObject);
          Result.Add(Shift);
        end;
      end;
      Logger.Debug('Open shifts loaded: %d items', [Result.Count]);
    finally
      JsonObj.Free;
    end;
  except
    Result.Free;
    Logger.Error('Failed to parse open shifts response');
    raise;
  end;
end;

function TOFDApiClient.GetDocuments(const OrganisationKey: string;
  const Params: TDocumentQueryParams): TObjectList<TDocument>;
var
  Response: IHTTPResponse;
  JsonObj: TJSONObject;
  DocsArray: TJSONArray;
  Endpoint: string;
  I: Integer;
  Doc: TDocument;
begin
  Logger.Info('GetDocuments: organisationKey=%s, kkmRegId=%s',
    [OrganisationKey, Params.KkmRegId]);
  
  // Валидация параметров
  if not (Params.HasShiftNum or Params.HasDateRange or Params.HasInsDateRange) then
  begin
    Logger.Error('GetDocuments: missing required parameters');
    raise EBadRequestException.Create(
      'Необходимо указать shiftNum или fromDate/toDate или fromInsDate/toInsDate');
  end;
  
  if Params.HasDateRange then
  begin
    if DaysBetween(Params.FromDate, Params.ToDate) > 30 then
    begin
      Logger.Error('GetDocuments: date range exceeds 30 days');
      raise EBadRequestException.Create('Максимальный период запроса: 30 дней');
    end;
  end;
  
  Endpoint := Format(API_DOCUMENTS, [
    TNetEncoding.URL.Encode(OrganisationKey),
    Params.ToQueryString
  ]);
  
  Response := Get(Endpoint);
  
  Result := TObjectList<TDocument>.Create(True);
  try
    JsonObj := TJSONObject.ParseJSONValue(Response.ContentAsString) as TJSONObject;
    try
      DocsArray := JsonObj.GetValue<TJSONArray>('documents');
      if Assigned(DocsArray) then
      begin
        for I := 0 to DocsArray.Count - 1 do
        begin
          Doc := TDocument.Create;
          Doc.FromJson(DocsArray.Items[I] as TJSONObject);
          Result.Add(Doc);
        end;
      end;
      Logger.Debug('Documents loaded: %d items', [Result.Count]);
    finally
      JsonObj.Free;
    end;
  except
    Result.Free;
    Logger.Error('Failed to parse documents response');
    raise;
  end;
end;

function TOFDApiClient.GetVirtualDocuments(const OrganisationKey: string;
  VirtualKkmId: Integer; const Params: TDocumentQueryParams): TObjectList<TDocument>;
var
  Response: IHTTPResponse;
  JsonObj: TJSONObject;
  DocsArray: TJSONArray;
  Endpoint: string;
  QueryString: string;
  I: Integer;
  Doc: TDocument;
begin
  Logger.Info('GetVirtualDocuments: organisationKey=%s, virtualKkmId=%d',
    [OrganisationKey, VirtualKkmId]);
  
  QueryString := Params.ToQueryString;
  if QueryString <> '' then
    QueryString := '&' + QueryString;
  
  Endpoint := Format(API_VIRTUAL_DOCUMENTS, [
    TNetEncoding.URL.Encode(OrganisationKey),
    VirtualKkmId,
    QueryString
  ]);
  
  Response := Get(Endpoint);
  
  Result := TObjectList<TDocument>.Create(True);
  try
    JsonObj := TJSONObject.ParseJSONValue(Response.ContentAsString) as TJSONObject;
    try
      DocsArray := JsonObj.GetValue<TJSONArray>('documents');
      if Assigned(DocsArray) then
      begin
        for I := 0 to DocsArray.Count - 1 do
        begin
          Doc := TDocument.Create;
          Doc.FromJson(DocsArray.Items[I] as TJSONObject);
          Result.Add(Doc);
        end;
      end;
      Logger.Debug('Virtual documents loaded: %d items', [Result.Count]);
    finally
      JsonObj.Free;
    end;
  except
    Result.Free;
    Logger.Error('Failed to parse virtual documents response');
    raise;
  end;
end;

function TOFDApiClient.GetDocumentsByUserProperty(const OrganisationKey: string;
  const UserPropertyKey, UserPropertyValue: string;
  const FromDate, ToDate: TDateTime): TObjectList<TDocument>;
var
  Response: IHTTPResponse;
  JsonObj: TJSONObject;
  DocsArray: TJSONArray;
  Endpoint: string;
  QueryParams: TStringList;
  I: Integer;
  Doc: TDocument;
begin
  Logger.Info('GetDocumentsByUserProperty: organisationKey=%s, key=%s',
    [OrganisationKey, UserPropertyKey]);
  
  QueryParams := TStringList.Create;
  try
    QueryParams.Add('userPropertyKey=' + TNetEncoding.URL.Encode(UserPropertyKey));
    QueryParams.Add('userPropertyValue=' + TNetEncoding.URL.Encode(UserPropertyValue));
    QueryParams.Add('fromDate=' + TNetEncoding.URL.Encode(DateTimeToISO8601(FromDate)));
    QueryParams.Add('toDate=' + TNetEncoding.URL.Encode(DateTimeToISO8601(ToDate)));
    
    Endpoint := Format(API_DOCUMENTS_BY_PROPERTY, [
      TNetEncoding.URL.Encode(OrganisationKey),
      QueryParams.DelimitedText.Replace(',', '&')
    ]);
  finally
    QueryParams.Free;
  end;
  
  Response := Get(Endpoint);
  
  Result := TObjectList<TDocument>.Create(True);
  try
    JsonObj := TJSONObject.ParseJSONValue(Response.ContentAsString) as TJSONObject;
    try
      DocsArray := JsonObj.GetValue<TJSONArray>('documents');
      if Assigned(DocsArray) then
      begin
        for I := 0 to DocsArray.Count - 1 do
        begin
          Doc := TDocument.Create;
          Doc.FromJson(DocsArray.Items[I] as TJSONObject);
          Result.Add(Doc);
        end;
      end;
      Logger.Debug('Documents by property loaded: %d items', [Result.Count]);
    finally
      JsonObj.Free;
    end;
  except
    Result.Free;
    Logger.Error('Failed to parse documents by property response');
    raise;
  end;
end;

end.