unit OFDApiException;

interface

uses
  SysUtils;

type
  EOFDApiException = class(Exception);
  
  // HTTP ошибки
  EBadRequestException = class(EOFDApiException);      // 400
  EUnauthorizedException = class(EOFDApiException);    // 401
  ENotFoundException = class(EOFDApiException);        // 404
  EConflictException = class(EOFDApiException);        // 409
  EInternalServerException = class(EOFDApiException);  // 500
  EServiceUnavailableException = class(EOFDApiException); // 503
  
  // Бизнес-логика
  EInvalidApiKeyException = class(EOFDApiException);
  ETokenExpiredException = class(EOFDApiException);
  EPermissionDeniedException = class(EOFDApiException);

  TOFDApiErrorHandler = class
  public
    class procedure HandleHttpError(StatusCode: Integer; const ResponseBody: string);
    class function GetErrorMessage(StatusCode: Integer): string;
  end;

implementation

class procedure TOFDApiErrorHandler.HandleHttpError(
  StatusCode: Integer; const ResponseBody: string);
var
  ErrorMsg: string;
begin
  ErrorMsg := Format('HTTP %d: %s', [StatusCode, GetErrorMessage(StatusCode)]);
  
  if ResponseBody <> '' then
    ErrorMsg := ErrorMsg + #13#10 + ResponseBody;
  
  case StatusCode of
    400: raise EBadRequestException.Create(ErrorMsg);
    401: raise EUnauthorizedException.Create(ErrorMsg);
    404: raise ENotFoundException.Create(ErrorMsg);
    409: raise EConflictException.Create(ErrorMsg);
    500: raise EInternalServerException.Create(ErrorMsg);
    503: raise EServiceUnavailableException.Create(ErrorMsg);
  else
    raise EOFDApiException.Create(ErrorMsg);
  end;
end;

class function TOFDApiErrorHandler.GetErrorMessage(StatusCode: Integer): string;
begin
  case StatusCode of
    200: Result := 'OK';
    201: Result := 'Создано';
    204: Result := 'Пусто';
    400: Result := 'Неверные данные';
    401: Result := 'Не авторизован';
    404: Result := 'Не найдено';
    409: Result := 'Конфликт';
    500: Result := 'Внутренняя ошибка сервера';
    503: Result := 'Сервер перегружен';
  else
    Result := 'Неизвестная ошибка';
  end;
end;

end.