unit OFDApiModels;

interface

uses
  // VCL
  System.SysUtils, System.Generics.Collections, System.JSON,
  // This
  OFDApiTypes;

type
  // Базовый класс для всех моделей
  TOFDApiModel = class
  public
    function ToJson: TJSONObject; virtual; abstract;
    procedure FromJson(const JsonObj: TJSONObject); virtual; abstract;
  end;

  { TOrganisation - Организация }

  TOrganisation = class(TOFDApiModel)
  private
    FOrganisationKeyKind: string;
    FOrganisationKey: string;
    FStartContractDate: TDateTime;
    FEndContractDate: TDateTime;
    FInn: string;
    FKpp: string;
    FOrganizationName: string;
  public
    procedure FromJson(const JsonObj: TJSONObject); override;
    function ToJson: TJSONObject; override;

    property OrganisationKeyKind: string read FOrganisationKeyKind write FOrganisationKeyKind;
    property OrganisationKey: string read FOrganisationKey write FOrganisationKey;
    property StartContractDate: TDateTime read FStartContractDate write FStartContractDate;
    property EndContractDate: TDateTime read FEndContractDate write FEndContractDate;
    property Inn: string read FInn write FInn;
    property Kpp: string read FKpp write FKpp;
    property OrganizationName: string read FOrganizationName write FOrganizationName;
  end;

  { TOrganisationList - Список организаций }

  TOrganisationList = class(TOFDApiModel)
  private
    FPage: TObjectList<TOrganisation>;
    FTotalCount: Integer;
    FPageNumber: Integer;
    FPageSize: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure FromJson(const JsonObj: TJSONObject); override;
    function ToJson: TJSONObject; override;

    property Page: TObjectList<TOrganisation> read FPage;
    property TotalCount: Integer read FTotalCount write FTotalCount;
    property PageNumber: Integer read FPageNumber write FPageNumber;
    property PageSize: Integer read FPageSize write FPageSize;
  end;

  { TPermissions - Разрешения }

  TPermissions = class(TOFDApiModel)
  private
    FAccessToAllKkms: Boolean;
    FGlobalInterval: string;
    FCapabilities: TArray<string>;
    FRetailPlaceTimePermissions: TDictionary<string, string>;
    FKkmTimePermissions: TDictionary<string, string>;
    FVirtualKkmsPermissions: TDictionary<string, string>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure FromJson(const JsonObj: TJSONObject); override;
    function ToJson: TJSONObject; override;
    function HasCapability(const Capability: string): Boolean;

    property AccessToAllKkms: Boolean read FAccessToAllKkms write FAccessToAllKkms;
    property GlobalInterval: string read FGlobalInterval write FGlobalInterval;
    property RetailPlaceTimePermissions: TDictionary<string, string> read FRetailPlaceTimePermissions;
    property KkmTimePermissions: TDictionary<string, string> read FKkmTimePermissions;
    property VirtualKkmsPermissions: TDictionary<string, string> read FVirtualKkmsPermissions;
    property Capabilities: TArray<string> read FCapabilities write FCapabilities;
  end;

  { TRetailPlace - Торговая точка }

  TRetailPlace = class(TOFDApiModel)
  private
    FRetailPlaceId: Integer;
    FAddress: string;
    FTitle: string;
  public
    function ToJson: TJSONObject; override;
    procedure FromJson(const JsonObj: TJSONObject); override;

    property RetailPlaceId: Integer read FRetailPlaceId write FRetailPlaceId;
    property Address: string read FAddress write FAddress;
    property Title: string read FTitle write FTitle;
  end;

  { TRetailPlaceList - Список торговых точек }

  TRetailPlaceList = class(TOFDApiModel)
  private
    FInn: string;
    FKpp: string;
    FRetailPlaces: TObjectList<TRetailPlace>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure FromJson(const JsonObj: TJSONObject); override;
    function ToJson: TJSONObject; override;

    property Inn: string read FInn write FInn;
    property Kpp: string read FKpp write FKpp;
    property RetailPlaces: TObjectList<TRetailPlace> read FRetailPlaces;
  end;

  { TFiscalDrive - Фискальный накопитель }

  TFiscalDrive = class(TOFDApiModel)
  private
    FFsFactoryNumber: string;
    FActivationDate: TDateTime;
    FExpireDate: TDateTime;
    FCloseArchiveDate: TDateTime;
  public
    procedure FromJson(const JsonObj: TJSONObject); override;
    function ToJson: TJSONObject; override;

    property FsFactoryNumber: string read FFsFactoryNumber write FFsFactoryNumber;
    property ActivationDate: TDateTime read FActivationDate write FActivationDate;
    property ExpireDate: TDateTime read FExpireDate write FExpireDate;
    property CloseArchiveDate: TDateTime read FCloseArchiveDate write FCloseArchiveDate;
  end;

  { TKkm - Касса }

  TKkm = class(TOFDApiModel)
  private
    FRetailPlaceId: Integer;
    FAddress: string;
    FTitle: string;
    FKkmRegId: string;
    FKkmFactoryNumber: string;
    FFsFactoryNumber: string;
    FFiscalDrives: TObjectList<TFiscalDrive>;
    FKkmInternalName: string;
    FKkmAddress: string;
    FOnline: Boolean;
    FLastErrorDate: TDateTime;
    FVirtualKkmId: Integer;
    FVirtualKkmName: string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure FromJson(const JsonObj: TJSONObject); override;
    function ToJson: TJSONObject; override;

    property RetailPlaceId: Integer read FRetailPlaceId write FRetailPlaceId;
    property Address: string read FAddress write FAddress;
    property Title: string read FTitle write FTitle;
    property KkmRegId: string read FKkmRegId write FKkmRegId;
    property KkmFactoryNumber: string read FKkmFactoryNumber write FKkmFactoryNumber;
    property FsFactoryNumber: string read FFsFactoryNumber write FFsFactoryNumber;
    property FiscalDrives: TObjectList<TFiscalDrive> read FFiscalDrives;
    property KkmInternalName: string read FKkmInternalName write FKkmInternalName;
    property KkmAddress: string read FKkmAddress write FKkmAddress;
    property Online: Boolean read FOnline write FOnline;
    property LastErrorDate: TDateTime read FLastErrorDate write FLastErrorDate;
    property VirtualKkmId: Integer read FVirtualKkmId write FVirtualKkmId;
    property VirtualKkmName: string read FVirtualKkmName write FVirtualKkmName;
  end;

  { TOperationCounters - Счетчики операций }

  TOperationCounters = class(TOFDApiModel)
  private
    FQuantity: Integer;
    FCashSum: Currency;
    FEcashSum: Currency;
    FPrepaymentSum: Currency;
    FPostpaymentSum: Currency;
    FCounterSubmissionSum: Currency;
    FResultSum: Currency;
    FNds20: Currency;
    FNds10: Currency;
    FNdsCalculated20: Currency;
    FNdsCalculated10: Currency;
    FNds0: Currency;
    FNdsNo: Currency;
  public
    procedure FromJson(const JsonObj: TJSONObject); override;
    function ToJson: TJSONObject; override;

    property Quantity: Integer read FQuantity write FQuantity;
    property CashSum: Currency read FCashSum write FCashSum;
    property EcashSum: Currency read FEcashSum write FEcashSum;
    property PrepaymentSum: Currency read FPrepaymentSum write FPrepaymentSum;
    property PostpaymentSum: Currency read FPostpaymentSum write FPostpaymentSum;
    property CounterSubmissionSum: Currency read FCounterSubmissionSum write FCounterSubmissionSum;
    property ResultSum: Currency read FResultSum write FResultSum;
    property Nds20: Currency read FNds20 write FNds20;
    property Nds10: Currency read FNds10 write FNds10;
    property NdsCalculated20: Currency read FNdsCalculated20 write FNdsCalculated20;
    property NdsCalculated10: Currency read FNdsCalculated10 write FNdsCalculated10;
    property Nds0: Currency read FNds0 write FNds0;
    property NdsNo: Currency read FNdsNo write FNdsNo;
  end;

  { TShiftCounters - Счетчики смены }

  TShiftCounters = class(TOFDApiModel)
  private
    FQuantity: Integer;
    FSellsCounters: TOperationCounters;
    FSellReturnsCounters: TOperationCounters;
    FBuyesCounters: TOperationCounters;
    FBuyReturnsCounters: TOperationCounters;
  public
    constructor Create;
    destructor Destroy; override;

    procedure FromJson(const JsonObj: TJSONObject); override;
    function ToJson: TJSONObject; override;

    property Quantity: Integer read FQuantity write FQuantity;
    property SellsCounters: TOperationCounters read FSellsCounters;
    property SellReturnsCounters: TOperationCounters read FSellReturnsCounters;
    property BuyesCounters: TOperationCounters read FBuyesCounters;
    property BuyReturnsCounters: TOperationCounters read FBuyReturnsCounters;
  end;

  { TShift - Смена }

  TShift = class(TOFDApiModel)
  private
    FShiftNumber: Integer;
    FDateTimeOpen: TDateTime;
    FDateTimeClose: TDateTime;
    FIncomeSum: Currency;
    FCashSum: Currency;
    FECashSum: Currency;
    FReturnSum: Currency;
    FReturnCashSum: Currency;
    FReturnECashSum: Currency;
    FReturnOutcomeSum: Currency;
    FOutcomeSum: Currency;
    FNds10: Currency;
    FNds20: Currency;
    FKkmName: string;
    FKkmAddress: string;
    FKkmNumber: string;
    FFsNumber: string;
    FKkmRegId: string;
    FKkmSalesPoint: string;
    FShiftCounters: TShiftCounters;
  public
    constructor Create;
    destructor Destroy; override;

    procedure FromJson(const JsonObj: TJSONObject); override;
    function ToJson: TJSONObject; override;

    property ShiftNumber: Integer read FShiftNumber write FShiftNumber;
    property DateTimeOpen: TDateTime read FDateTimeOpen write FDateTimeOpen;
    property DateTimeClose: TDateTime read FDateTimeClose write FDateTimeClose;
    property IncomeSum: Currency read FIncomeSum write FIncomeSum;
    property CashSum: Currency read FCashSum write FCashSum;
    property ECashSum: Currency read FECashSum write FECashSum;
    property ReturnSum: Currency read FReturnSum write FReturnSum;
    property ReturnCashSum: Currency read FReturnCashSum write FReturnCashSum;
    property ReturnECashSum: Currency read FReturnECashSum write FReturnECashSum;
    property ReturnOutcomeSum: Currency read FReturnOutcomeSum write FReturnOutcomeSum;
    property OutcomeSum: Currency read FOutcomeSum write FOutcomeSum;
    property Nds10: Currency read FNds10 write FNds10;
    property Nds20: Currency read FNds20 write FNds20;
    property KkmName: string read FKkmName write FKkmName;
    property KkmAddress: string read FKkmAddress write FKkmAddress;
    property KkmNumber: string read FKkmNumber write FKkmNumber;
    property FsNumber: string read FFsNumber write FFsNumber;
    property KkmRegId: string read FKkmRegId write FKkmRegId;
    property KkmSalesPoint: string read FKkmSalesPoint write FKkmSalesPoint;
    property ShiftCounters: TShiftCounters read FShiftCounters;
  end;

  { TTicketItem - Позиция в чеке }

  TTicketItem = class(TOFDApiModel)
  private
    FQuantity: Double;
    FName: string;
    FSum: Currency;
    FPrice: Currency;
    FNdsRate: Integer;
    FNdsSum: Currency;
    FCalculationTypeSign: Integer;
    FCalculationSubjectSign: Integer;
  public
    procedure FromJson(const JsonObj: TJSONObject); override;
    function ToJson: TJSONObject; override;

    property Quantity: Double read FQuantity write FQuantity;
    property Name: string read FName write FName;
    property Sum: Currency read FSum write FSum;
    property Price: Currency read FPrice write FPrice;
    property NdsRate: Integer read FNdsRate write FNdsRate;
    property NdsSum: Currency read FNdsSum write FNdsSum;
    property CalculationTypeSign: Integer read FCalculationTypeSign write FCalculationTypeSign;
    property CalculationSubjectSign: Integer read FCalculationSubjectSign write FCalculationSubjectSign;
  end;

  { TTicket - Кассовый чек }

  TTicket = class(TOFDApiModel)
  private
    FRequestNumber: Integer;
    FOperationType: Integer;
    FTaxationType: string;
    FTotalSum: Currency;
    FCashTotalSum: Currency;
    FEcashTotalSum: Currency;
    FRetailPlace: string;
    FBuyerPhoneOrAddress: string;
    FItems: TObjectList<TTicketItem>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure FromJson(const JsonObj: TJSONObject); override;
    function ToJson: TJSONObject; override;

    property RequestNumber: Integer read FRequestNumber write FRequestNumber;
    property OperationType: Integer read FOperationType write FOperationType;
    property TaxationType: string read FTaxationType write FTaxationType;
    property TotalSum: Currency read FTotalSum write FTotalSum;
    property CashTotalSum: Currency read FCashTotalSum write FCashTotalSum;
    property EcashTotalSum: Currency read FEcashTotalSum write FEcashTotalSum;
    property RetailPlace: string read FRetailPlace write FRetailPlace;
    property BuyerPhoneOrAddress: string read FBuyerPhoneOrAddress write FBuyerPhoneOrAddress;
    property Items: TObjectList<TTicketItem> read FItems;
  end;

  { TDocument - Фискальный документ }

  TDocument = class(TOFDApiModel)
  private
    FTransactionDate: TDateTime;
    FFiscalSign: string;
    FInsertedAt: TDateTime;
    FTransactionType: string;
    FKkmRegId: string;
    FFiscalDriveNumber: string;
    FFiscalDocumentNumber: Integer;
    FShiftNum: Integer;
    FFnsFlcStatus: string;
    FFnsStatus: Integer;
    FFnsDescription: string;
    FFnsConfirmation: string;
    FTicket: TTicket;
  public
    constructor Create;
    destructor Destroy; override;

    procedure FromJson(const JsonObj: TJSONObject); override;
    function ToJson: TJSONObject; override;

    property TransactionDate: TDateTime read FTransactionDate write FTransactionDate;
    property FiscalSign: string read FFiscalSign write FFiscalSign;
    property InsertedAt: TDateTime read FInsertedAt write FInsertedAt;
    property TransactionType: string read FTransactionType write FTransactionType;
    property KkmRegId: string read FKkmRegId write FKkmRegId;
    property FiscalDriveNumber: string read FFiscalDriveNumber write FFiscalDriveNumber;
    property FiscalDocumentNumber: Integer read FFiscalDocumentNumber write FFiscalDocumentNumber;
    property ShiftNum: Integer read FShiftNum write FShiftNum;
    property FnsFlcStatus: string read FFnsFlcStatus write FFnsFlcStatus;
    property FnsStatus: Integer read FFnsStatus write FFnsStatus;
    property FnsDescription: string read FFnsDescription write FFnsDescription;
    property FnsConfirmation: string read FFnsConfirmation write FFnsConfirmation;
    property Ticket: TTicket read FTicket;
  end;

implementation

{ TOrganisation }

procedure TOrganisation.FromJson(const JsonObj: TJSONObject);
begin
  FOrganisationKeyKind := JsonObj.GetValue<string>('organisationKeyKind', '');
  FOrganisationKey := JsonObj.GetValue<string>('organisationKey', '');
  FStartContractDate := ISO8601ToDateTime(JsonObj.GetValue<string>('startContractDate', ''));
  FEndContractDate := ISO8601ToDateTime(JsonObj.GetValue<string>('endContractDate', ''));
  FInn := JsonObj.GetValue<string>('inn', '');
  FKpp := JsonObj.GetValue<string>('kpp', '');
  FOrganizationName := JsonObj.GetValue<string>('organizationName', '');
end;

function TOrganisation.ToJson: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('organisationKeyKind', FOrganisationKeyKind);
  Result.AddPair('organisationKey', FOrganisationKey);
  Result.AddPair('startContractDate', DateTimeToISO8601(FStartContractDate));
  Result.AddPair('endContractDate', DateTimeToISO8601(FEndContractDate));
  Result.AddPair('inn', FInn);
  Result.AddPair('kpp', FKpp);
  Result.AddPair('organizationName', FOrganizationName);
end;

{ TOrganisationList }

constructor TOrganisationList.Create;
begin
  inherited;
  FPage := TObjectList<TOrganisation>.Create(True);
end;

destructor TOrganisationList.Destroy;
begin
  FPage.Free;
  inherited;
end;

procedure TOrganisationList.FromJson(const JsonObj: TJSONObject);
var
  PageArray: TJSONArray;
  I: Integer;
  Org: TOrganisation;
begin
  FPage.Clear;

  PageArray := JsonObj.FindValue('page') as TJSONArray;
  if Assigned(PageArray) then
  begin
    for I := 0 to PageArray.Count - 1 do
    begin
      Org := TOrganisation.Create;
      Org.FromJson(PageArray.Items[I] as TJSONObject);
      FPage.Add(Org);
    end;
  end;

  FTotalCount := JsonObj.GetValue<Integer>('totalCount', 0);
  FPageNumber := JsonObj.GetValue<Integer>('pageNumber', 1);
  FPageSize := JsonObj.GetValue<Integer>('pageSize', 10);
end;

function TOrganisationList.ToJson: TJSONObject;
var
  PageArray: TJSONArray;
  Org: TOrganisation;
begin
  Result := TJSONObject.Create;

  PageArray := TJSONArray.Create;
  for Org in FPage do
    PageArray.AddElement(Org.ToJson);

  Result.AddPair('page', PageArray);
  Result.AddPair('totalCount', TJSONNumber.Create(FTotalCount));
  Result.AddPair('pageNumber', TJSONNumber.Create(FPageNumber));
  Result.AddPair('pageSize', TJSONNumber.Create(FPageSize));
end;

{ TPermissions }

constructor TPermissions.Create;
begin
  inherited;
  FRetailPlaceTimePermissions := TDictionary<string, string>.Create;
  FKkmTimePermissions := TDictionary<string, string>.Create;
  FVirtualKkmsPermissions := TDictionary<string, string>.Create;
end;

destructor TPermissions.Destroy;
begin
  FRetailPlaceTimePermissions.Free;
  FKkmTimePermissions.Free;
  FVirtualKkmsPermissions.Free;
  inherited;
end;

procedure TPermissions.FromJson(const JsonObj: TJSONObject);

  procedure ParsePermissionsDict(const JsonFieldName: string; Dict: TDictionary<string, string>);
  var
    PermissionsObj: TJSONObject;
    Pair: TJSONPair;
  begin
    Dict.Clear;
    PermissionsObj := JsonObj.FindValue(JsonFieldName) as TJSONObject;
    if Assigned(PermissionsObj) then
    begin
      for Pair in PermissionsObj do
      begin
        Dict.Add(Pair.JsonString.Value, Pair.JsonValue.Value);
      end;
    end;
  end;

var
  AllKkmsObj: TJSONObject;
  CapabilitiesStr: string;
begin
  FAccessToAllKkms := JsonObj.GetValue<Boolean>('accessToAllKkms', False);

  AllKkmsObj := JsonObj.FindValue('allKkmsPermissions') as TJSONObject;
  if Assigned(AllKkmsObj) then
    FGlobalInterval := AllKkmsObj.GetValue<string>('globalInterval', '');

  // Парсинг capabilities
  CapabilitiesStr := JsonObj.GetValue<string>('capabilities', '');
  if CapabilitiesStr <> '' then
    FCapabilities := CapabilitiesStr.Split([' | '])
  else
    SetLength(FCapabilities, 0);

  // Парсинг словарей разрешений
  ParsePermissionsDict('retailPlaceTimePermissions', FRetailPlaceTimePermissions);
  ParsePermissionsDict('kkmTimePermissions', FKkmTimePermissions);
  ParsePermissionsDict('virtualKkmsPermissions', FVirtualKkmsPermissions);
end;

function TPermissions.ToJson: TJSONObject;
var
  AllKkmsObj, RetailPlaceObj, KkmObj, VirtualKkmObj: TJSONObject;
  Pair: TPair<string, string>;
begin
  Result := TJSONObject.Create;
  Result.AddPair('accessToAllKkms', TJSONBool.Create(FAccessToAllKkms));

  if FGlobalInterval <> '' then
  begin
    AllKkmsObj := TJSONObject.Create;
    AllKkmsObj.AddPair('globalInterval', FGlobalInterval);
    Result.AddPair('allKkmsPermissions', AllKkmsObj);
  end;

  Result.AddPair('capabilities', string.Join(' | ', FCapabilities));

  // Сериализация retailPlaceTimePermissions
  if FRetailPlaceTimePermissions.Count > 0 then
  begin
    RetailPlaceObj := TJSONObject.Create;
    for Pair in FRetailPlaceTimePermissions do
      RetailPlaceObj.AddPair(Pair.Key, Pair.Value);
    Result.AddPair('retailPlaceTimePermissions', RetailPlaceObj);
  end;

  // Сериализация kkmTimePermissions
  if FKkmTimePermissions.Count > 0 then
  begin
    KkmObj := TJSONObject.Create;
    for Pair in FKkmTimePermissions do
      KkmObj.AddPair(Pair.Key, Pair.Value);
    Result.AddPair('kkmTimePermissions', KkmObj);
  end;

  // Сериализация virtualKkmsPermissions
  if FVirtualKkmsPermissions.Count > 0 then
  begin
    VirtualKkmObj := TJSONObject.Create;
    for Pair in FVirtualKkmsPermissions do
      VirtualKkmObj.AddPair(Pair.Key, Pair.Value);
    Result.AddPair('virtualKkmsPermissions', VirtualKkmObj);
  end;
end;

function TPermissions.HasCapability(const Capability: string): Boolean;
var
  Cap: string;
begin
  Result := False;
  for Cap in FCapabilities do
  begin
    if Cap = Capability then
    begin
      Result := True;
      Break;
    end;
  end;
end;

{ TRetailPlace }

procedure TRetailPlace.FromJson(const JsonObj: TJSONObject);
begin
  FRetailPlaceId := JsonObj.GetValue<Integer>('retailPlaceId', 0);
  FAddress := JsonObj.GetValue<string>('address', '');
  FTitle := JsonObj.GetValue<string>('title', '');
end;

function TRetailPlace.ToJson: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('retailPlaceId', TJSONNumber.Create(FRetailPlaceId));
  Result.AddPair('address', FAddress);
  Result.AddPair('title', FTitle);
end;

{ TRetailPlaceList }

constructor TRetailPlaceList.Create;
begin
  inherited;
  FRetailPlaces := TObjectList<TRetailPlace>.Create(True);
end;

destructor TRetailPlaceList.Destroy;
begin
  FRetailPlaces.Free;
  inherited;
end;

procedure TRetailPlaceList.FromJson(const JsonObj: TJSONObject);
var
  RetailPlacesArray: TJSONArray;
  I: Integer;
  RetailPlace: TRetailPlace;
begin
  FInn := JsonObj.GetValue<string>('inn', '');
  FKpp := JsonObj.GetValue<string>('kpp', '');

  FRetailPlaces.Clear;
  RetailPlacesArray := JsonObj.FindValue('retailPlaces') as TJSONArray;
  if Assigned(RetailPlacesArray) then
  begin
    for I := 0 to RetailPlacesArray.Count - 1 do
    begin
      RetailPlace := TRetailPlace.Create;
      RetailPlace.FromJson(RetailPlacesArray.Items[I] as TJSONObject);
      FRetailPlaces.Add(RetailPlace);
    end;
  end;
end;

function TRetailPlaceList.ToJson: TJSONObject;
var
  RetailPlacesArray: TJSONArray;
  RetailPlace: TRetailPlace;
begin
  Result := TJSONObject.Create;
  Result.AddPair('inn', FInn);
  Result.AddPair('kpp', FKpp);

  RetailPlacesArray := TJSONArray.Create;
  for RetailPlace in FRetailPlaces do
    RetailPlacesArray.AddElement(RetailPlace.ToJson);

  Result.AddPair('retailPlaces', RetailPlacesArray);
end;

{ TFiscalDrive }

procedure TFiscalDrive.FromJson(const JsonObj: TJSONObject);
begin
  FFsFactoryNumber := JsonObj.GetValue<string>('fsFactoryNumber', '');
  FActivationDate := ISO8601ToDateTime(JsonObj.GetValue<string>('activationDate', ''));
  FExpireDate := ISO8601ToDateTime(JsonObj.GetValue<string>('expireDate', ''));
  FCloseArchiveDate := ISO8601ToDateTime(JsonObj.GetValue<string>('closeArchiveDate', ''));
end;

function TFiscalDrive.ToJson: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('fsFactoryNumber', FFsFactoryNumber);
  Result.AddPair('activationDate', DateTimeToISO8601(FActivationDate));
  Result.AddPair('expireDate', DateTimeToISO8601(FExpireDate));
  Result.AddPair('closeArchiveDate', DateTimeToISO8601(FCloseArchiveDate));
end;

{ TKkm }

constructor TKkm.Create;
begin
  inherited;
  FFiscalDrives := TObjectList<TFiscalDrive>.Create(True);
end;

destructor TKkm.Destroy;
begin
  FFiscalDrives.Free;
  inherited;
end;

procedure TKkm.FromJson(const JsonObj: TJSONObject);
var
  FiscalDrivesArray: TJSONArray;
  I: Integer;
  FiscalDrive: TFiscalDrive;
begin
  FRetailPlaceId := JsonObj.GetValue<Integer>('retailPlaceId', 0);
  FAddress := JsonObj.GetValue<string>('address', '');
  FTitle := JsonObj.GetValue<string>('title', '');
  FKkmRegId := JsonObj.GetValue<string>('kkmRegId', '');
  FKkmFactoryNumber := JsonObj.GetValue<string>('kkmFactoryNumber', '');
  FFsFactoryNumber := JsonObj.GetValue<string>('fsFactoryNumber', '');
  FKkmInternalName := JsonObj.GetValue<string>('kkmInternalName', '');
  FKkmAddress := JsonObj.GetValue<string>('kkmAddress', '');
  FOnline := JsonObj.GetValue<Boolean>('online', False);
  FLastErrorDate := ISO8601ToDateTime(JsonObj.GetValue<string>('lastErrorDate', ''));
  FVirtualKkmId := JsonObj.GetValue<Integer>('virtualKkmId', 0);
  FVirtualKkmName := JsonObj.GetValue<string>('virtualKkmName', '');

  FFiscalDrives.Clear;
  FiscalDrivesArray := JsonObj.FindValue('fiscalDrives') as TJSONArray;
  if Assigned(FiscalDrivesArray) then
  begin
    for I := 0 to FiscalDrivesArray.Count - 1 do
    begin
      FiscalDrive := TFiscalDrive.Create;
      FiscalDrive.FromJson(FiscalDrivesArray.Items[I] as TJSONObject);
      FFiscalDrives.Add(FiscalDrive);
    end;
  end;
end;

function TKkm.ToJson: TJSONObject;
var
  FiscalDrivesArray: TJSONArray;
  FiscalDrive: TFiscalDrive;
begin
  Result := TJSONObject.Create;
  Result.AddPair('retailPlaceId', TJSONNumber.Create(FRetailPlaceId));
  Result.AddPair('address', FAddress);
  Result.AddPair('title', FTitle);
  Result.AddPair('kkmRegId', FKkmRegId);
  Result.AddPair('kkmFactoryNumber', FKkmFactoryNumber);
  Result.AddPair('fsFactoryNumber', FFsFactoryNumber);
  Result.AddPair('kkmInternalName', FKkmInternalName);
  Result.AddPair('kkmAddress', FKkmAddress);
  Result.AddPair('online', TJSONBool.Create(FOnline));
  Result.AddPair('lastErrorDate', DateTimeToISO8601(FLastErrorDate));
  Result.AddPair('virtualKkmId', TJSONNumber.Create(FVirtualKkmId));
  Result.AddPair('virtualKkmName', FVirtualKkmName);

  FiscalDrivesArray := TJSONArray.Create;
  for FiscalDrive in FFiscalDrives do
    FiscalDrivesArray.AddElement(FiscalDrive.ToJson);

  Result.AddPair('fiscalDrives', FiscalDrivesArray);
end;

{ TOperationCounters }

procedure TOperationCounters.FromJson(const JsonObj: TJSONObject);
begin
  FQuantity := JsonObj.GetValue<Integer>('quantity', 0);
  FCashSum := JsonObj.GetValue<Currency>('cashSum', 0);
  FEcashSum := JsonObj.GetValue<Currency>('ecashSum', 0);
  FPrepaymentSum := JsonObj.GetValue<Currency>('prepaymentSum', 0);
  FPostpaymentSum := JsonObj.GetValue<Currency>('postpaymentSum', 0);
  FCounterSubmissionSum := JsonObj.GetValue<Currency>('counterSubmissionSum', 0);
  FResultSum := JsonObj.GetValue<Currency>('resultSum', 0);
  FNds20 := JsonObj.GetValue<Currency>('nds20', 0);
  FNds10 := JsonObj.GetValue<Currency>('nds10', 0);
  FNdsCalculated20 := JsonObj.GetValue<Currency>('ndsCalculated20', 0);
  FNdsCalculated10 := JsonObj.GetValue<Currency>('ndsCalculated10', 0);
  FNds0 := JsonObj.GetValue<Currency>('nds0', 0);
  FNdsNo := JsonObj.GetValue<Currency>('ndsNo', 0);
end;

function TOperationCounters.ToJson: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('quantity', TJSONNumber.Create(FQuantity));
  Result.AddPair('cashSum', TJSONNumber.Create(FCashSum));
  Result.AddPair('ecashSum', TJSONNumber.Create(FEcashSum));
  Result.AddPair('prepaymentSum', TJSONNumber.Create(FPrepaymentSum));
  Result.AddPair('postpaymentSum', TJSONNumber.Create(FPostpaymentSum));
  Result.AddPair('counterSubmissionSum', TJSONNumber.Create(FCounterSubmissionSum));
  Result.AddPair('resultSum', TJSONNumber.Create(FResultSum));
  Result.AddPair('nds20', TJSONNumber.Create(FNds20));
  Result.AddPair('nds10', TJSONNumber.Create(FNds10));
  Result.AddPair('ndsCalculated20', TJSONNumber.Create(FNdsCalculated20));
  Result.AddPair('ndsCalculated10', TJSONNumber.Create(FNdsCalculated10));
  Result.AddPair('nds0', TJSONNumber.Create(FNds0));
  Result.AddPair('ndsNo', TJSONNumber.Create(FNdsNo));
end;

{ TShiftCounters }

constructor TShiftCounters.Create;
begin
  inherited;
  FSellsCounters := TOperationCounters.Create;
  FSellReturnsCounters := TOperationCounters.Create;
  FBuyesCounters := TOperationCounters.Create;
  FBuyReturnsCounters := TOperationCounters.Create;
end;

destructor TShiftCounters.Destroy;
begin
  FSellsCounters.Free;
  FSellReturnsCounters.Free;
  FBuyesCounters.Free;
  FBuyReturnsCounters.Free;
  inherited;
end;

procedure TShiftCounters.FromJson(const JsonObj: TJSONObject);
var
  CountersObj: TJSONObject;
begin
  FQuantity := JsonObj.GetValue<Integer>('quantity', 0);

  CountersObj := JsonObj.FindValue('sellsCounters') as TJSONObject;
  if Assigned(CountersObj) then
    FSellsCounters.FromJson(CountersObj);

  CountersObj := JsonObj.FindValue('sellReturnsCounters') as TJSONObject;
  if Assigned(CountersObj) then
    FSellReturnsCounters.FromJson(CountersObj);

  CountersObj := JsonObj.FindValue('buyesCounters') as TJSONObject;
  if Assigned(CountersObj) then
    FBuyesCounters.FromJson(CountersObj);

  CountersObj := JsonObj.FindValue('buyReturnsCounters') as TJSONObject;
  if Assigned(CountersObj) then
    FBuyReturnsCounters.FromJson(CountersObj);
end;

function TShiftCounters.ToJson: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('quantity', TJSONNumber.Create(FQuantity));
  Result.AddPair('sellsCounters', FSellsCounters.ToJson);
  Result.AddPair('sellReturnsCounters', FSellReturnsCounters.ToJson);
  Result.AddPair('buyesCounters', FBuyesCounters.ToJson);
  Result.AddPair('buyReturnsCounters', FBuyReturnsCounters.ToJson);
end;

{ TShift }

constructor TShift.Create;
begin
  inherited;
  FShiftCounters := TShiftCounters.Create;
end;

destructor TShift.Destroy;
begin
  FShiftCounters.Free;
  inherited;
end;

procedure TShift.FromJson(const JsonObj: TJSONObject);
var
  ShiftCountersObj: TJSONObject;
begin
  FShiftNumber := JsonObj.GetValue<Integer>('shiftNumber', 0);
  FDateTimeOpen := ISO8601ToDateTime(JsonObj.GetValue<string>('dateTimeOpen', ''));
  FDateTimeClose := ISO8601ToDateTime(JsonObj.GetValue<string>('dateTimeClose', ''));
  FIncomeSum := JsonObj.GetValue<Currency>('incomeSum', 0);
  FCashSum := JsonObj.GetValue<Currency>('cashSum', 0);
  FECashSum := JsonObj.GetValue<Currency>('ecashSum', 0);
  FReturnSum := JsonObj.GetValue<Currency>('returnSum', 0);
  FReturnCashSum := JsonObj.GetValue<Currency>('returnCashSum', 0);
  FReturnECashSum := JsonObj.GetValue<Currency>('returnECashSum', 0);
  FReturnOutcomeSum := JsonObj.GetValue<Currency>('returnOutcomeSum', 0);
  FOutcomeSum := JsonObj.GetValue<Currency>('outcomeSum', 0);
  FNds10 := JsonObj.GetValue<Currency>('nds10', 0);
  FNds20 := JsonObj.GetValue<Currency>('nds20', 0);
  FKkmName := JsonObj.GetValue<string>('kkmName', '');
  FKkmAddress := JsonObj.GetValue<string>('kkmAddress', '');
  FKkmNumber := JsonObj.GetValue<string>('kkmNumber', '');
  FFsNumber := JsonObj.GetValue<string>('fsNumber', '');
  FKkmRegId := JsonObj.GetValue<string>('kkmRegId', '');
  FKkmSalesPoint := JsonObj.GetValue<string>('kkmSalesPoint', '');

  ShiftCountersObj := JsonObj.FindValue('shiftCounters') as TJSONObject;
  if Assigned(ShiftCountersObj) then
    FShiftCounters.FromJson(ShiftCountersObj);
end;

function TShift.ToJson: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('shiftNumber', TJSONNumber.Create(FShiftNumber));
  Result.AddPair('dateTimeOpen', DateTimeToISO8601(FDateTimeOpen));
  Result.AddPair('dateTimeClose', DateTimeToISO8601(FDateTimeClose));
  Result.AddPair('incomeSum', TJSONNumber.Create(FIncomeSum));
  Result.AddPair('cashSum', TJSONNumber.Create(FCashSum));
  Result.AddPair('ecashSum', TJSONNumber.Create(FECashSum));
  Result.AddPair('returnSum', TJSONNumber.Create(FReturnSum));
  Result.AddPair('returnCashSum', TJSONNumber.Create(FReturnCashSum));
  Result.AddPair('returnECashSum', TJSONNumber.Create(FReturnECashSum));
  Result.AddPair('returnOutcomeSum', TJSONNumber.Create(FReturnOutcomeSum));
  Result.AddPair('outcomeSum', TJSONNumber.Create(FOutcomeSum));
  Result.AddPair('nds10', TJSONNumber.Create(FNds10));
  Result.AddPair('nds20', TJSONNumber.Create(FNds20));
  Result.AddPair('kkmName', FKkmName);
  Result.AddPair('kkmAddress', FKkmAddress);
  Result.AddPair('kkmNumber', FKkmNumber);
  Result.AddPair('fsNumber', FFsNumber);
  Result.AddPair('kkmRegId', FKkmRegId);
  Result.AddPair('kkmSalesPoint', FKkmSalesPoint);
  Result.AddPair('shiftCounters', FShiftCounters.ToJson);
end;

{ TTicketItem }

procedure TTicketItem.FromJson(const JsonObj: TJSONObject);
begin
  FQuantity := JsonObj.GetValue<Double>('quantity', 0);
  FName := JsonObj.GetValue<string>('name', '');
  FSum := JsonObj.GetValue<Currency>('sum', 0);
  FPrice := JsonObj.GetValue<Currency>('price', 0);
  FNdsRate := JsonObj.GetValue<Integer>('ndsRate', 0);
  FNdsSum := JsonObj.GetValue<Currency>('ndsSum', 0);
  FCalculationTypeSign := JsonObj.GetValue<Integer>('calculationTypeSign', 0);
  FCalculationSubjectSign := JsonObj.GetValue<Integer>('calculationSubjectSign', 0);
end;

function TTicketItem.ToJson: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('quantity', TJSONNumber.Create(FQuantity));
  Result.AddPair('name', FName);
  Result.AddPair('sum', TJSONNumber.Create(FSum));
  Result.AddPair('price', TJSONNumber.Create(FPrice));
  Result.AddPair('ndsRate', TJSONNumber.Create(FNdsRate));
  Result.AddPair('ndsSum', TJSONNumber.Create(FNdsSum));
  Result.AddPair('calculationTypeSign', TJSONNumber.Create(FCalculationTypeSign));
  Result.AddPair('calculationSubjectSign', TJSONNumber.Create(FCalculationSubjectSign));
end;

{ TTicket }

constructor TTicket.Create;
begin
  inherited;
  FItems := TObjectList<TTicketItem>.Create(True);
end;

destructor TTicket.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TTicket.FromJson(const JsonObj: TJSONObject);
var
  ItemsArray: TJSONArray;
  I: Integer;
  Item: TTicketItem;
begin
  FRequestNumber := JsonObj.GetValue<Integer>('requestNumber', 0);
  FOperationType := JsonObj.GetValue<Integer>('operationType', 0);
  FTaxationType := JsonObj.GetValue<string>('taxationType', '');
  FTotalSum := JsonObj.GetValue<Currency>('totalSum', 0);
  FCashTotalSum := JsonObj.GetValue<Currency>('cashTotalSum', 0);
  FEcashTotalSum := JsonObj.GetValue<Currency>('ecashTotalSum', 0);
  FRetailPlace := JsonObj.GetValue<string>('retailPlace', '');
  FBuyerPhoneOrAddress := JsonObj.GetValue<string>('buyerPhoneOrAddress', '');

  FItems.Clear;
  ItemsArray := JsonObj.FindValue('items') as TJSONArray;
  if Assigned(ItemsArray) then
  begin
    for I := 0 to ItemsArray.Count - 1 do
    begin
      Item := TTicketItem.Create;
      Item.FromJson(ItemsArray.Items[I] as TJSONObject);
      FItems.Add(Item);
    end;
  end;
end;

function TTicket.ToJson: TJSONObject;
var
  ItemsArray: TJSONArray;
  Item: TTicketItem;
begin
  Result := TJSONObject.Create;
  Result.AddPair('requestNumber', TJSONNumber.Create(FRequestNumber));
  Result.AddPair('operationType', TJSONNumber.Create(FOperationType));
  Result.AddPair('taxationType', FTaxationType);
  Result.AddPair('totalSum', TJSONNumber.Create(FTotalSum));
  Result.AddPair('cashTotalSum', TJSONNumber.Create(FCashTotalSum));
  Result.AddPair('ecashTotalSum', TJSONNumber.Create(FEcashTotalSum));
  Result.AddPair('retailPlace', FRetailPlace);
  Result.AddPair('buyerPhoneOrAddress', FBuyerPhoneOrAddress);

  ItemsArray := TJSONArray.Create;
  for Item in FItems do
    ItemsArray.AddElement(Item.ToJson);

  Result.AddPair('items', ItemsArray);
end;

{ TDocument }

constructor TDocument.Create;
begin
  inherited;
  FTicket := TTicket.Create;
end;

destructor TDocument.Destroy;
begin
  FTicket.Free;
  inherited;
end;

procedure TDocument.FromJson(const JsonObj: TJSONObject);
var
  TicketObj: TJSONObject;
begin
  FTransactionDate := ISO8601ToDateTime(JsonObj.GetValue<string>('transactionDate', ''));
  FFiscalSign := JsonObj.GetValue<string>('fiscalSign', '');
  FInsertedAt := ISO8601ToDateTime(JsonObj.GetValue<string>('insertedAt', ''));
  FTransactionType := JsonObj.GetValue<string>('transactionType', '');
  FKkmRegId := JsonObj.GetValue<string>('kkmRegId', '');
  FFiscalDriveNumber := JsonObj.GetValue<string>('fiscalDriveNumber', '');
  FFiscalDocumentNumber := JsonObj.GetValue<Integer>('fiscalDocumentNumber', 0);
  FShiftNum := JsonObj.GetValue<Integer>('shiftNum', 0);
  FFnsFlcStatus := JsonObj.GetValue<string>('fnsFlcStatus', '');
  FFnsStatus := JsonObj.GetValue<Integer>('fnsStatus', 0);
  FFnsDescription := JsonObj.GetValue<string>('fnsDescription', '');
  FFnsConfirmation := JsonObj.GetValue<string>('fnsConfirmation', '');

  TicketObj := JsonObj.FindValue('ticket') as TJSONObject;
  if Assigned(TicketObj) then
    FTicket.FromJson(TicketObj);
end;

function TDocument.ToJson: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('transactionDate', DateTimeToISO8601(FTransactionDate));
  Result.AddPair('fiscalSign', FFiscalSign);
  Result.AddPair('insertedAt', DateTimeToISO8601(FInsertedAt));
  Result.AddPair('transactionType', FTransactionType);
  Result.AddPair('kkmRegId', FKkmRegId);
  Result.AddPair('fiscalDriveNumber', FFiscalDriveNumber);
  Result.AddPair('fiscalDocumentNumber', TJSONNumber.Create(FFiscalDocumentNumber));
  Result.AddPair('shiftNum', TJSONNumber.Create(FShiftNum));
  Result.AddPair('fnsFlcStatus', FFnsFlcStatus);
  Result.AddPair('fnsStatus', TJSONNumber.Create(FFnsStatus));
  Result.AddPair('fnsDescription', FFnsDescription);
  Result.AddPair('fnsConfirmation', FFnsConfirmation);
  Result.AddPair('ticket', FTicket.ToJson);
end;

end.
