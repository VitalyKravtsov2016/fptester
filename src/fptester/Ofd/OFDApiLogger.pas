unit OFDApiLogger;

interface

uses
  System.SysUtils, System.Classes, System.IOUtils,
  OFDApiConfig;

type
  TOFDLogger = class
  private
    FLogFile: string;
    FEnabled: Boolean;
    FCurrentLogLevel: TOFDLogLevel;
    FLogStream: TStreamWriter;
    FLock: TObject;

    procedure WriteLog(const Level: TOFDLogLevel; const Msg: string);
    function GetLogFileName: string;
    procedure RotateLogIfNeeded;
    function GetLogLevelName(Level: TOFDLogLevel): string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Debug(const Msg: string); overload;
    procedure Debug(const Fmt: string; const Args: array of const); overload;

    procedure Info(const Msg: string); overload;
    procedure Info(const Fmt: string; const Args: array of const); overload;

    procedure Warn(const Msg: string); overload;
    procedure Warn(const Fmt: string; const Args: array of const); overload;

    procedure Error(const Msg: string); overload;
    procedure Error(const Fmt: string; const Args: array of const); overload;
    procedure Error(const E: Exception; const Msg: string = ''); overload;

    procedure Configure(const ALogFile: string; AEnabled: Boolean; ALogLevel: TOFDLogLevel);
  end;

var
  Logger: TOFDLogger = nil;

implementation

constructor TOFDLogger.Create;
begin
  inherited;
  FLock := TObject.Create;
  FEnabled := False;
  FCurrentLogLevel := TOFDLogLevel.DEBUG;

  if Assigned(GlobalConfig) then
  begin
    Configure(
      GlobalConfig.LogFile,
      GlobalConfig.LoggingEnabled,
      GlobalConfig.LogLevel
    );
  end;
end;

destructor TOFDLogger.Destroy;
begin
  if Assigned(FLogStream) then
  begin
    FLogStream.Flush;
    FLogStream.Free;
  end;
  FLock.Free;
  inherited;
end;

procedure TOFDLogger.Configure(const ALogFile: string; AEnabled: Boolean; ALogLevel: TOFDLogLevel);
begin
  TMonitor.Enter(FLock);
  try
    FEnabled := AEnabled;
    FCurrentLogLevel := ALogLevel;

    if AEnabled then
    begin
      FLogFile := ALogFile;
      if FLogFile = '' then
        FLogFile := TPath.Combine(ExtractFilePath(ParamStr(0)), 'ofd_api.log');

      RotateLogIfNeeded;

      if Assigned(FLogStream) then
        FLogStream.Free;

      FLogStream := TStreamWriter.Create(TFileStream.Create(GetLogFileName, fmCreate or fmShareDenyWrite));
      FLogStream.AutoFlush := True;
    end;
  finally
    TMonitor.Exit(FLock);
  end;
end;

function TOFDLogger.GetLogFileName: string;
begin
  Result := FLogFile;
end;

procedure TOFDLogger.RotateLogIfNeeded;
const
  MAX_LOG_SIZE = 10 * 1024 * 1024; // 10 MB
var
  LogFileName: string;
  FileSize: Int64;
  I: Integer;
  SearchRec: TSearchRec;
begin
  LogFileName := GetLogFileName;
  if FileExists(LogFileName) then
  begin
    FileSize := 0;
    if FindFirst(LogFileName, faAnyFile, SearchRec) = 0 then
    try
      FileSize := SearchRec.Size;
    finally
      FindClose(SearchRec);
    end;

    if FileSize > MAX_LOG_SIZE then
    begin
      // Ротация логов
      for I := 9 downto 1 do
      begin
        if FileExists(Format('%s.%d', [LogFileName, I])) then
          TFile.Move(Format('%s.%d', [LogFileName, I]), Format('%s.%d', [LogFileName, I + 1]));
      end;
      if FileExists(LogFileName) then
        TFile.Move(LogFileName, Format('%s.%d', [LogFileName, 1]));
    end;
  end;
end;

function TOFDLogger.GetLogLevelName(Level: TOFDLogLevel): string;
begin
  case Level of
    TOFDLogLevel.DEBUG: Result := 'DEBUG';
    TOFDLogLevel.INFO:  Result := 'INFO';
    TOFDLogLevel.WARN:  Result := 'WARN';
    TOFDLogLevel.ERROR: Result := 'ERROR';
  else
    Result := 'UNKNOWN';
  end;
end;

procedure TOFDLogger.WriteLog(const Level: TOFDLogLevel; const Msg: string);
var
  LogMsg: string;
begin
  if not FEnabled then
    Exit;

  if Level < FCurrentLogLevel then
    Exit;

  TMonitor.Enter(FLock);
  try
    if Assigned(FLogStream) then
    begin
      LogMsg := Format('%s [%s] %s', [
        FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now),
        GetLogLevelName(Level),
        Msg
      ]);

      FLogStream.WriteLine(LogMsg);
      FLogStream.Flush;
    end;
  finally
    TMonitor.Exit(FLock);
  end;
end;

procedure TOFDLogger.Debug(const Msg: string);
begin
  WriteLog(TOFDLogLevel.DEBUG, Msg);
end;

procedure TOFDLogger.Debug(const Fmt: string; const Args: array of const);
begin
  WriteLog(TOFDLogLevel.DEBUG, Format(Fmt, Args));
end;

procedure TOFDLogger.Info(const Msg: string);
begin
  WriteLog(TOFDLogLevel.INFO, Msg);
end;

procedure TOFDLogger.Info(const Fmt: string; const Args: array of const);
begin
  WriteLog(TOFDLogLevel.INFO, Format(Fmt, Args));
end;

procedure TOFDLogger.Warn(const Msg: string);
begin
  WriteLog(TOFDLogLevel.WARN, Msg);
end;

procedure TOFDLogger.Warn(const Fmt: string; const Args: array of const);
begin
  WriteLog(TOFDLogLevel.WARN, Format(Fmt, Args));
end;

procedure TOFDLogger.Error(const Msg: string);
begin
  WriteLog(TOFDLogLevel.ERROR, Msg);
end;

procedure TOFDLogger.Error(const Fmt: string; const Args: array of const);
begin
  WriteLog(TOFDLogLevel.ERROR, Format(Fmt, Args));
end;

procedure TOFDLogger.Error(const E: Exception; const Msg: string = '');
var
  FullMsg: string;
begin
  if Msg <> '' then
    FullMsg := Format('%s: %s', [Msg, E.Message])
  else
    FullMsg := E.Message;

  WriteLog(TOFDLogLevel.ERROR, FullMsg);
end;

initialization
  Logger := TOFDLogger.Create;

finalization
  Logger.Free;

end.
