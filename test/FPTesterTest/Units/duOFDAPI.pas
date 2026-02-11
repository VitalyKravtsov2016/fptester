unit duOFDAPI;

interface

uses
  // VCL
  System.SysUtils, System.Classes, System.JSON, System.Net.HttpClient,
  System.Net.URLClient, System.NetConsts, System.Net.Mime, System.DateUtils,
  // DUnit
  TestFramework,
  // This
  OFDAPI;

type

  { TFirmwareUpdaterTest }

  TFirmwareUpdaterTest = class(TTestCase)
  private
    Server: TOFDAPI;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAuth;
  end;

implementation

const
  BaseUrl = 'https://universal-api.1-ofd.ru/api';
  ApiKey =
    'b5afd315f62dc08e0166a3b2c5df13f9f96ccfa9a435f5d16c505be755ac' +
    '516cb4fe9cd5c5bd4f8848a6d9f304f39cb901ac439435c65795e1cb47d34602f0cd';

{ TFirmwareUpdaterTest }

procedure TFirmwareUpdaterTest.SetUp;
begin
  Server := TOFDAPI.Create(ApiKey, BaseURL);
end;

procedure TFirmwareUpdaterTest.TearDown;
begin
  Server.Free;
end;

(*

curl -X 'POST' \
  'https://universal-api.1-ofd-test.ru/api/rent/v2/auth' \
  -H 'accept: */*' \
  -H 'Content-Type: application/json' \
  -d '{
  "apiKey": "b5afd315f62dc08e0166a3b2c5df13f9f96ccfa9a435f5d16c505be755ac516cb4fe9cd5c5bd4f8848a6d9f304f39cb901ac439435c65795e1cb47d34602f0cd"
}'

*)

procedure TFirmwareUpdaterTest.TestAuth;
var
  URL: string;
  Json: TJsonObject;
  Client: THTTPClient;
  Stream: TStringStream;
  Response: IHTTPResponse;
  JsonValue: TJsonValue;
  TokenValue: TJsonValue;
  ResponseStream: TStringStream;
const
  BaseURL = 'https://universal-api.1-ofd-test.ru/';
  apiKey = 'b5afd315f62dc08e0166a3b2c5df13f9f96ccfa9a435f5d16c505be755ac516cb4fe9cd5c5bd4f8848a6d9f304f39cb901ac439435c65795e1cb47d34602f0cd';
begin
  URL := BaseURL + 'api/rent/v2/auth';

  Client := THTTPClient.Create;
  Client.ContentType := 'application/json';
  Client.Accept := 'application/json';
  Client.ConnectionTimeout := 30000; // 30 секунд
  Client.ResponseTimeout := 30000;

  Json := TJsonObject.Create;
  Json.AddPair('apiKey', apiKey);

  Stream := TStringStream.Create(Json.ToJSON, TEncoding.UTF8);
  ResponseStream := TStringStream.Create;
  Response := Client.Post(URL, Stream, ResponseStream);
  CheckEquals(200, Response.StatusCode, 'Response.StatusCode');

  JsonValue := TJSONObject.ParseJSONValue(ResponseStream.DataString);
  TokenValue := JsonValue.FindValue('token');
  CheckNotNull(TokenValue, 'TokenValue is null');
  CheckNotEquals('', TokenValue.Value, 'TokenValue.Value is empty');

  Json.Free;
  Client.Free;
  Stream.Free;
  ResponseStream.Free;
end;

(*
GET /api/rent/v3/organisations?page=1&pageSize=10 HTTP/1.1
Accept: application/json
Authorization: Bearer
eyJhbGciOiJIUzUxMiJ9.eyJpZCI6IjQyIiwiaWF0IjoxNzY4OTEzOTM0LCJleHAiOjE3Njg5MTQ1MzR9.Pfq8
HTVh1ATRpCgFaoAiwsB88YXzAaIVmvTwuNBCAAUXS0Qj-rg7fdZF1mXPBbkHESqRI8gWIUAD6KZfT5PcAw
Host: universal-api-host
*)

initialization
  RegisterTest('', TFirmwareUpdaterTest.Suite);
end.
