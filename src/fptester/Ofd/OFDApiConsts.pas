unit OFDApiConsts;

interface

const
  // ----- Версия API -----
  API_VERSION = '1.0';
  
  // ----- HTTP методы -----
  HTTP_GET = 'GET';
  HTTP_POST = 'POST';
  HTTP_PUT = 'PUT';
  HTTP_DELETE = 'DELETE';
  
  // ----- Заголовки HTTP -----
  HEADER_CONTENT_TYPE = 'Content-Type';
  HEADER_AUTHORIZATION = 'Authorization';
  HEADER_ACCEPT = 'Accept';
  HEADER_X_API_KEY = 'X-Api-Key';
  
  // ----- Типы контента -----
  CONTENT_TYPE_JSON = 'application/json';
  
  // ----- Коды ответов HTTP -----
  // Успех
  HTTP_STATUS_OK = 200;
  HTTP_STATUS_CREATED = 201;
  HTTP_STATUS_NO_CONTENT = 204;
  // Ошибки клиента
  HTTP_STATUS_BAD_REQUEST = 400;
  HTTP_STATUS_UNAUTHORIZED = 401;
  HTTP_STATUS_NOT_FOUND = 404;
  HTTP_STATUS_CONFLICT = 409;
  // Системные ошибки
  HTTP_STATUS_INTERNAL_ERROR = 500;
  HTTP_STATUS_SERVICE_UNAVAILABLE = 503;
  
  // ----- Коды ошибок приложения -----
  ERROR_SUCCESS = 0;
  ERROR_UNKNOWN = -1;
  ERROR_NETWORK = -2;
  ERROR_TIMEOUT = -3;
  ERROR_UNAUTHORIZED = -4;
  ERROR_NOT_FOUND = -5;
  ERROR_SERVER = -6;
  ERROR_VALIDATION = -7;
  
  // ----- Форматы даты/времени -----
  ISO8601_DATETIME_FORMAT = 'yyyy-mm-dd"T"hh:nn:ss.zzz';
  ISO8601_DATE_FORMAT = 'yyyy-mm-dd';
  ISO8601_DATETIME_WITH_Z_FORMAT = 'yyyy-mm-dd"T"hh:nn:ss.zzz"Z"';
  
  // ----- Значения по умолчанию -----
  DEFAULT_TIMEOUT = 60000;
  DEFAULT_PAGE_SIZE = 20;
  DEFAULT_TOKEN_CACHE_DURATION = 3600; // 1 час
  DEFAULT_MAX_DAYS_RANGE = 30; // Максимальный период запроса 30 дней
  DEFAULT_PAGE = 1;
  
  // ----- Эндпоинты API (v2/v3) -----
  
  // Авторизация (v1)
  API_AUTH = '/api/auth';
  
  // Организации (v3)
  API_ORGANISATIONS_LIST = '/api/rent/v3/organisations?page=%d&pageSize=%d';
  
  // Разрешения (v2)
  API_PERMISSIONS = '/api/rent/v2/%s/permissions';
  
  // Торговые точки (v2)
  API_RETAIL_PLACES = '/api/rent/v2/organisations/%s/retailPlaces';
  
  // Выручка (v2)
  API_REVENUE = '/api/rent/v2/organisations/%s/revenue?%s';
  
  // Кассы (v2)
  API_KKMS = '/api/rent/v2/organisations/%s/kkms';
  
  // Смены (v2)
  API_SHIFTS = '/api/rent/v2/organisations/%s/shifts?%s';
  API_OPEN_SHIFTS = '/api/rent/v2/organisations/%s/shifts/open?%s';
  API_USER_PROPERTY_SHIFTS = '/api/rent/v2/organisations/%s/userPropertyShifts?%s';
  
  // Документы (v2)
  API_DOCUMENTS = '/api/rent/v2/organisations/%s/documents?%s';
  API_VIRTUAL_DOCUMENTS = '/api/rent/v2/organisations/%s/virtual/documents?virtualKkmId=%d%s';
  API_DOCUMENTS_BY_PROPERTY = '/api/rent/v2/organisations/%s/documents/property?%s';
  
  // -----------------------------------------------------------------
  // ----- Параметры запросов и возможные значения из документации -----
  // -----------------------------------------------------------------
  
  // ----- Типы транзакций (transactionTypes) -----
  // Возможные значения параметра transactionTypes:
  TRANSACTION_TYPE_TICKET = 'TICKET';                 // Кассовый чек
  TRANSACTION_TYPE_CLOSE_SHIFT = 'CLOSE_SHIFT';       // Отчет о закрытии смены
  TRANSACTION_TYPE_OPEN_SHIFT = 'OPEN_SHIFT';         // Отчёт об открытии смены
  TRANSACTION_TYPE_RECEIPT_CORRECTION = 'RECEIPT_CORRECTION'; // Кассовый чек коррекции
  TRANSACTION_TYPE_BSO = 'BSO';                       // Бланк строгой отчетности
  TRANSACTION_TYPE_BSO_CORRECTION = 'BSO_CORRECTION'; // Бланк строгой отчетности коррекции
  TRANSACTION_TYPE_FISCAL_REPORT_CORRECTION = 'FISCAL_REPORT_CORRECTION'; // Отчет об изменениях параметров регистрации
  TRANSACTION_TYPE_CLOSE_ARCHIVE = 'CLOSE_ARCHIVE';   // Закрытие архива
  
  // ----- Статусы документов ФЛК в ФНС (irkktStatus) -----
  // Возможные значения параметра irkktStatus:
  FNS_STATUS_OK = 'OK';           // Отправлен в ФНС
  FNS_STATUS_WARNING = 'WARNING'; // Отправлен в мягкий карантин
  FNS_STATUS_ERROR = 'ERROR';     // Отправлен в жесткий карантин
  FNS_STATUS_WAITING = 'WAITING'; // Ожидается отправка
  
  // ----- Типы операций (operationType) -----
  // Возможные значения поля operationType:
  OPERATION_TYPE_SELL = 1;        // Приход
  OPERATION_TYPE_SELL_RETURN = 2; // Возврат прихода
  OPERATION_TYPE_BUY = 3;         // Расход
  OPERATION_TYPE_BUY_RETURN = 4;  // Возврат расхода
  
  // ----- Признаки способа расчета (calculationTypeSign) -----
  // Возможные значения поля calculationTypeSign:
  CALC_TYPE_PREPAYMENT_100 = 1;  // Предоплата 100%
  CALC_TYPE_PREPAYMENT = 2;      // Предоплата
  CALC_TYPE_ADVANCE = 3;         // Аванс
  CALC_TYPE_FULL_PAYMENT = 4;    // Полный расчет
  CALC_TYPE_PARTIAL_AND_CREDIT = 5; // Частичный расчет и кредит
  CALC_TYPE_TRANSFER_TO_CREDIT = 6; // Передача в кредит
  CALC_TYPE_CREDIT_PAYMENT = 7;  // Оплата кредита
  
  // ----- Ставки НДС (ndsRate) -----
  NDS_RATE_20 = 20;
  NDS_RATE_10 = 10;
  NDS_RATE_0 = 0;
  NDS_RATE_NO = 1;              // Без НДС
  NDS_RATE_CALCULATED_20 = 6;   // Расчетная 20/120
  NDS_RATE_CALCULATED_10 = 5;   // Расчетная 10/110
  NDS_RATE_CALCULATED_7 = 4;    // Расчетная 7/107
  NDS_RATE_CALCULATED_5 = 3;    // Расчетная 5/105
  
  // -----------------------------------------------------------------
  // ----- Фильтры мониторинга касс (monitoringFilter) -----
  // -----------------------------------------------------------------
  
  // Статусы онлайн
  MONITOR_FILTER_ONLINE = 'status.online';
  MONITOR_FILTER_OFFLINE_24 = 'status.offline24';
  MONITOR_FILTER_OFFLINE_48 = 'status.offline48';
  MONITOR_FILTER_OFFLINE_DAYS = 'status.offlineDays';
  MONITOR_FILTER_EXPIRED_24 = 'status.expired24';
  MONITOR_FILTER_HAS_FLC_ERRORS = 'status.hasFlcErrors';
  
  // Статусы смен
  MONITOR_FILTER_SHIFT_OPEN = 'shift.open';
  MONITOR_FILTER_SHIFT_CLOSED = 'shift.closed';
  MONITOR_FILTER_SHIFT_ABSENT = 'shift.absent';
  MONITOR_FILTER_SHIFT_EXPIRED_24 = 'shift.expired24';
  MONITOR_FILTER_SHIFT_UNSENT_NOTIFICATIONS = 'shift.unsentNotifications';
  
  // Статусы тарификации
  MONITOR_FILTER_BILLING_WITHOUT_TARIFF = 'billing.withoutTariff';
  MONITOR_FILTER_BILLING_STOPPED = 'billing.stopped';
  MONITOR_FILTER_BILLING_IS_GRACE_PERIOD = 'billing.isGracePeriod';
  MONITOR_FILTER_BILLING_EXPIRES_3_DAYS = 'billing.expires3days';
  MONITOR_FILTER_BILLING_EXPIRES_7_DAYS = 'billing.expires7days';
  MONITOR_FILTER_BILLING_EXPIRES_30_DAYS = 'billing.expires30days';
  MONITOR_FILTER_BILLING_EXPIRES_DAYS = 'billing.expiresDays';
  
  // Статусы памяти ФН
  MONITOR_FILTER_FD_MEMORY_FULL = 'fdMemory.memoryFull';
  MONITOR_FILTER_FD_MEMORY_70_PERCENT = 'fdMemory.memory70Percent';
  MONITOR_FILTER_FD_MEMORY_90_PERCENT = 'fdMemory.memory90Percent';
  
  // Статусы регистрации
  MONITOR_FILTER_REG_MEMORY_FULL = 'reg.memoryFull';
  MONITOR_FILTER_REG_MEMORY_70_PERCENT = 'reg.memory70Percent';
  MONITOR_FILTER_REG_MEMORY_90_PERCENT = 'reg.memory90Percent';
  MONITOR_FILTER_FDREG_FD_MEMORY_EXCEEDED = 'fdreg.fdMemoryExceededSign';
  MONITOR_FILTER_FDREG_FD_REPLACE_REQUIRED = 'fdreg.fdReplaceRequiredSign';
  MONITOR_FILTER_FDREG_FD_EXHAUSTION = 'fdreg.fdExhaustionSign';
  
  // Статусы маркировки
  MONITOR_FILTER_MARKING_VERIFICATION_FAILED = 'marking.verificationFailed';
  MONITOR_FILTER_MARKING_NEGATIVE_RESULT = 'marking.negativeResult';
  
  // -----------------------------------------------------------------
  // ----- Capabilities (права доступа) -----
  // -----------------------------------------------------------------
  
  CAP_NDS_INFO = 'universal-api.ndsInfo';                       // Право на получение данных НДС
  CAP_OPERATOR_INN = 'universal-api.operatorInnInfo';           // Право на получение данных ИНН оператора
  CAP_SHIFTS = 'universal-api.shifts';                          // Право на получение данных по смене
  CAP_TRANSACTIONS = 'universal-api.transactions';              // Право на получение данных по чекам
  CAP_NOMENCLATURE = 'universal-api.nomenclature';             // Право на получение данных по номенклатуре
  CAP_BUYER_INFO = 'universal-api.buyerInfo';                  // Право на получение данных о покупателе
  CAP_REVENUE = 'universal-api.revenue';                       // Право на получение данных о выручке
  CAP_USER_PROPERTY_SEARCH = 'universal-api.user-property-search'; // Право на получение транзакций по реквизитам key(1085) и value(1086)
  CAP_VIRTUAL_TRANSACTIONS = 'universal-api.virtual.transactions'; // Право на получение данных по чекам виртуальных касс
  CAP_VIRTUAL_NOMENCLATURE = 'universal-api.virtual.nomenclature'; // Право на получение данных по номенклатуре виртуальных касс
  
  // -----------------------------------------------------------------
  // ----- Типы ключей организаций (organisationKeyKind) -----
  // -----------------------------------------------------------------
  
  ORG_KEY_KIND_INN_KPP = 'INN.KPP';
  
  // -----------------------------------------------------------------
  // ----- Системы налогообложения (taxationType) -----
  // -----------------------------------------------------------------
  
  TAXATION_TYPE_OSN = 'ОСН';    // Общая система налогообложения
  TAXATION_TYPE_USN = 'УСН';    // Упрощенная система налогообложения
  TAXATION_TYPE_ESN = 'ЕСН';    // Единый сельскохозяйственный налог
  TAXATION_TYPE_ENVD = 'ЕНВД';  // Единый налог на вмененный доход
  TAXATION_TYPE_PSN = 'ПСН';    // Патентная система налогообложения
  
  // -----------------------------------------------------------------
  // ----- Теги ФФД (для документации) -----
  // -----------------------------------------------------------------
  
  TAG_USER_PROPERTY_KEY = 1085;   // Ключ дополнительного реквизита пользователя
  TAG_USER_PROPERTY_VALUE = 1086; // Значение дополнительного реквизита пользователя
  TAG_INDUSTRY_REQUISITE = 1260;  // Отраслевой реквизит предмета расчета
  TAG_FOIV_ID = 1262;            // Идентификатор ФОИВ
  TAG_DOCUMENT_DATE = 1263;      // Дата документа основания
  TAG_DOCUMENT_NUMBER = 1264;    // Номер документа основания
  TAG_INDUSTRY_VALUE = 1265;     // Значение отраслевого реквизита
  TAG_AGENT_BY_PRODUCT = 1222;   // Признак агента по предмету расчета
  TAG_PROVIDER_INN = 1226;       // ИНН поставщика
  TAG_PROVIDER_DATA = 1224;      // Данные поставщика
  TAG_PROVIDER_PHONE = 1171;     // Телефон поставщика
  TAG_PROVIDER_NAME = 1225;      // Наименование поставщика
  TAG_AGENT_DATA = 1223;         // Данные агента
  
implementation

end.