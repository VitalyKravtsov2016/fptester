unit OFDApiTypes;

interface

uses
  System.SysUtils, System.DateUtils;

type
  // Типы организационных ключей
  TOrganisationKeyKind = (
    okkInnKpp  // INN.KPP
  );

  // Типы транзакций
  TTransactionType = (
    ttTicket,              // Кассовый чек
    ttCloseShift,          // Отчет о закрытии смены
    ttOpenShift,           // Отчёт об открытии смены
    ttReceiptCorrection,   // Кассовый чек коррекции
    ttBso,                 // Бланк строгой отчетности
    ttBsoCorrection,       // БСО коррекции
    ttFiscalReportCorrection, // Отчет об изменениях параметров регистрации
    ttCloseArchive         // Закрытие архива
  );

  // Статусы документов ФЛК
  TFnsFlcStatus = (
    ffsOk,       // Отправлен в ФНС
    ffsWarning,  // Отправлен в мягкий карантин
    ffsError,    // Отправлен в жесткий карантин
    ffsWaiting   // Ожидается отправка
  );

  // Тип операции
  TOperationType = (
    otIncome = 1,        // Приход
    otIncomeReturn = 2,  // Возврат прихода
    otOutcome = 3,       // Расход
    otOutcomeReturn = 4  // Возврат расхода
  );

  // Способ расчета
  TCalculationTypeSign = (
    ctsPrepayment100 = 1,  // Предоплата 100%
    ctsPrepayment = 2,     // Предоплата
    ctsAdvance = 3,        // Аванс
    ctsFullPayment = 4,    // Полный расчет
    ctsPartialCredit = 5,  // Частичный расчет и кредит
    ctsCredit = 6,         // Передача в кредит
    ctsCreditPayment = 7   // Оплата кредита
  );

  // Ставки НДС
  TNdsRate = (
    nrNone = 0,     // Без НДС
    nrRate0 = 1,    // 0%
    nrRate10 = 2,   // 10%
    nrRate20 = 6,   // 20%
    nrCalc10 = 3,   // Расч. 10/110
    nrCalc20 = 4    // Расч. 20/120
  );

// Функции преобразования
function TransactionTypeToString(AType: TTransactionType): string;
function StringToTransactionType(const AStr: string): TTransactionType;
function FnsFlcStatusToString(AStatus: TFnsFlcStatus): string;
function StringToFnsFlcStatus(const AStr: string): TFnsFlcStatus;

// Работа с датами
function DateTimeToISO8601(ADateTime: TDateTime): string;
function ISO8601ToDateTime(const AISO: string): TDateTime;
function UnixTimeToDateTime(UnixTime: Int64): TDateTime;
function DateTimeToUnixTime(ADateTime: TDateTime): Int64;

implementation

function TransactionTypeToString(AType: TTransactionType): string;
begin
  case AType of
    ttTicket: Result := 'TICKET';
    ttCloseShift: Result := 'CLOSE_SHIFT';
    ttOpenShift: Result := 'OPEN_SHIFT';
    ttReceiptCorrection: Result := 'RECEIPT_CORRECTION';
    ttBso: Result := 'BSO';
    ttBsoCorrection: Result := 'BSO_CORRECTION';
    ttFiscalReportCorrection: Result := 'FISCAL_REPORT_CORRECTION';
    ttCloseArchive: Result := 'CLOSE_ARCHIVE';
  end;
end;

function StringToTransactionType(const AStr: string): TTransactionType;
begin
  if AStr = 'TICKET' then Result := ttTicket
  else if AStr = 'CLOSE_SHIFT' then Result := ttCloseShift
  else if AStr = 'OPEN_SHIFT' then Result := ttOpenShift
  else if AStr = 'RECEIPT_CORRECTION' then Result := ttReceiptCorrection
  else if AStr = 'BSO' then Result := ttBso
  else if AStr = 'BSO_CORRECTION' then Result := ttBsoCorrection
  else if AStr = 'FISCAL_REPORT_CORRECTION' then Result := ttFiscalReportCorrection
  else if AStr = 'CLOSE_ARCHIVE' then Result := ttCloseArchive
  else raise Exception.CreateFmt('Неизвестный тип транзакции: %s', [AStr]);
end;

function FnsFlcStatusToString(AStatus: TFnsFlcStatus): string;
begin
  case AStatus of
    ffsOk: Result := 'OK';
    ffsWarning: Result := 'WARNING';
    ffsError: Result := 'ERROR';
    ffsWaiting: Result := 'WAITING';
  end;
end;

function StringToFnsFlcStatus(const AStr: string): TFnsFlcStatus;
begin
  if AStr = 'OK' then Result := ffsOk
  else if AStr = 'WARNING' then Result := ffsWarning
  else if AStr = 'ERROR' then Result := ffsError
  else if AStr = 'WAITING' then Result := ffsWaiting
  else raise Exception.CreateFmt('Неизвестный статус ФЛК: %s', [AStr]);
end;

function DateTimeToISO8601(ADateTime: TDateTime): string;
begin
  Result := FormatDateTime('yyyy-mm-dd"T"hh:nn:ss.zzz', ADateTime);
end;

function ISO8601ToDateTime(const AISO: string): TDateTime;
begin
  Result := ISO8601ToDate(AISO, False);
end;

function UnixTimeToDateTime(UnixTime: Int64): TDateTime;
begin
  Result := UnixToDateTime(UnixTime div 1000);
end;

function DateTimeToUnixTime(ADateTime: TDateTime): Int64;
begin
  Result := DateTimeToUnix(ADateTime) * 1000;
end;

end.