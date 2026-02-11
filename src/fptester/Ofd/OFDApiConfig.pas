unit OFDApiConfig;

interface

uses
  System.SysUtils, System.IniFiles, System.IOUtils, System.Classes;

type
  TOFDLogLevel = (DEBUG, INFO, WARN, ERROR);

  TOFDApiConfig = class
  private
    FIniFile: TIniFile;
    FConfigPath: string;
    
    // API секция
    FBaseUrl: string;
    FApiKey: string;
    FTimeout: Integer;
    
    // Cache секция
    FCacheEnabled: Boolean;
    FTokenCacheDuration: Integer;
    
    // Logging секция
    FLoggingEnabled: Boolean;
    FLogLevel: TOFDLogLevel;
    FLogFile: string;
    
    procedure LoadFromIni;
    procedure SaveToIni;
    function StringToLogLevel(const Value: string): TOFDLogLevel;
    function LogLevelToString(Level: TOFDLogLevel): string;
  public
    constructor Create(const AConfigPath: string = '');
    destructor Destroy; override;
    
    // Загрузка и сохранение
    procedure Load;
    procedure Save;
    
    // Свойства API
    property BaseUrl: string read FBaseUrl write FBaseUrl;
    property ApiKey: string read FApiKey write FApiKey;
    property Timeout: Integer read FTimeout write FTimeout;
    
    // Свойства Cache
    property CacheEnabled: Boolean read FCacheEnabled write FCacheEnabled;
    property TokenCacheDuration: Integer read FTokenCacheDuration write FTokenCacheDuration;
    
    // Свойства Logging
    property LoggingEnabled: Boolean read FLoggingEnabled write FLoggingEnabled;
    property LogLevel: TOFDLogLevel read FLogLevel write FLogLevel;
    property LogFile: string read FLogFile write FLogFile;
    property ConfigPath: string read FConfigPath;
  end;

  // Глобальный экземпляр конфигурации
var
  GlobalConfig: TOFDApiConfig = nil;

implementation

uses
  OFDApiConsts;

const
  SECTION_API = 'API';
  SECTION_CACHE = 'Cache';
  SECTION_LOGGING = 'Logging';
  
  KEY_BASE_URL = 'BaseUrl';
  KEY_API_KEY = 'ApiKey';
  KEY_TIMEOUT = 'Timeout';
  KEY_CACHE_ENABLED = 'Enabled';
  KEY_TOKEN_CACHE_DURATION = 'TokenCacheDuration';
  KEY_LOGGING_ENABLED = 'Enabled';
  KEY_LOG_LEVEL = 'LogLevel';
  KEY_LOG_FILE = 'LogFile';

{ TOFDApiConfig }

constructor TOFDApiConfig.Create(const AConfigPath: string = '');
begin
  inherited Create;
  
  if AConfigPath = '' then
    FConfigPath := TPath.Combine(ExtractFilePath(ParamStr(0)), 'OFDApiConfig.ini')
  else
    FConfigPath := AConfigPath;
    
  FIniFile := TIniFile.Create(FConfigPath);
  
  // Значения по умолчанию
  FBaseUrl := 'https://universal-api-host';
  FApiKey := '';
  FTimeout := 60000;
  FCacheEnabled := True;
  FTokenCacheDuration := 3600;
  FLoggingEnabled := True;
  FLogLevel := TOFDLogLevel.DEBUG;
  FLogFile := 'ofd_api.log';
end;

destructor TOFDApiConfig.Destroy;
begin
  FIniFile.Free;
  inherited;
end;

function TOFDApiConfig.StringToLogLevel(const Value: string): TOFDLogLevel;
var
  UpperValue: string;
begin
  UpperValue := UpperCase(Value);
  if UpperValue = 'DEBUG' then
    Result := TOFDLogLevel.DEBUG
  else if UpperValue = 'INFO' then
    Result := TOFDLogLevel.INFO
  else if UpperValue = 'WARN' then
    Result := TOFDLogLevel.WARN
  else if UpperValue = 'ERROR' then
    Result := TOFDLogLevel.ERROR
  else
    Result := TOFDLogLevel.DEBUG; // По умолчанию
end;

function TOFDApiConfig.LogLevelToString(Level: TOFDLogLevel): string;
begin
  case Level of
    TOFDLogLevel.DEBUG: Result := 'DEBUG';
    TOFDLogLevel.INFO:  Result := 'INFO';
    TOFDLogLevel.WARN:  Result := 'WARN';
    TOFDLogLevel.ERROR: Result := 'ERROR';
  else
    Result := 'DEBUG';
  end;
end;

procedure TOFDApiConfig.LoadFromIni;
begin
  // API секция
  FBaseUrl := FIniFile.ReadString(SECTION_API, KEY_BASE_URL, FBaseUrl);
  FApiKey := FIniFile.ReadString(SECTION_API, KEY_API_KEY, FApiKey);
  FTimeout := FIniFile.ReadInteger(SECTION_API, KEY_TIMEOUT, FTimeout);
  
  // Cache секция
  FCacheEnabled := FIniFile.ReadBool(SECTION_CACHE, KEY_CACHE_ENABLED, FCacheEnabled);
  FTokenCacheDuration := FIniFile.ReadInteger(SECTION_CACHE, KEY_TOKEN_CACHE_DURATION, FTokenCacheDuration);
  
  // Logging секция
  FLoggingEnabled := FIniFile.ReadBool(SECTION_LOGGING, KEY_LOGGING_ENABLED, FLoggingEnabled);
  FLogLevel := StringToLogLevel(FIniFile.ReadString(SECTION_LOGGING, KEY_LOG_LEVEL, LogLevelToString(FLogLevel)));
  FLogFile := FIniFile.ReadString(SECTION_LOGGING, KEY_LOG_FILE, FLogFile);
end;

procedure TOFDApiConfig.SaveToIni;
begin
  // API секция
  FIniFile.WriteString(SECTION_API, KEY_BASE_URL, FBaseUrl);
  FIniFile.WriteString(SECTION_API, KEY_API_KEY, FApiKey);
  FIniFile.WriteInteger(SECTION_API, KEY_TIMEOUT, FTimeout);
  
  // Cache секция
  FIniFile.WriteBool(SECTION_CACHE, KEY_CACHE_ENABLED, FCacheEnabled);
  FIniFile.WriteInteger(SECTION_CACHE, KEY_TOKEN_CACHE_DURATION, FTokenCacheDuration);
  
  // Logging секция
  FIniFile.WriteBool(SECTION_LOGGING, KEY_LOGGING_ENABLED, FLoggingEnabled);
  FIniFile.WriteString(SECTION_LOGGING, KEY_LOG_LEVEL, LogLevelToString(FLogLevel));
  FIniFile.WriteString(SECTION_LOGGING, KEY_LOG_FILE, FLogFile);
  
  FIniFile.UpdateFile;
end;

procedure TOFDApiConfig.Load;
begin
  if TFile.Exists(FConfigPath) then
    LoadFromIni
  else
    SaveToIni; // Создаем файл с значениями по умолчанию
end;

procedure TOFDApiConfig.Save;
begin
  SaveToIni;
end;

initialization
  GlobalConfig := TOFDApiConfig.Create;
  GlobalConfig.Load;

finalization
  GlobalConfig.Free;

end.