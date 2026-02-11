program OFDApiExample;

{$APPTYPE CONSOLE}

uses
  // VCL
  DateUtils,
  System.SysUtils,
  System.Generics.Collections,
  // This
  OFDApiClient,
  OFDApiModels,
  OFDApiTypes,
  OFDApiException;

const
  BASE_URL = 'https://universal-api.1-ofd-test.ru';
  API_KEY = 'b5afd315f62dc08e0166a3b2c5df13f9f96ccfa9a435f5d16c505be755ac516cb4fe9cd5c5bd4f8848a6d9f304f39cb901ac439435c65795e1cb47d34602f0cd';

var
  Client: TOFDApiClient;
  Organisations: TOrganisationList;
  Permissions: TPermissions;
  RetailPlaces: TRetailPlaceList;
  Shifts: TObjectList<TShift>;
  Documents: TObjectList<TDocument>;
  Params: TDocumentQueryParams;
  Org: TOrganisation;
  Place: TRetailPlace;
  Shift: TShift;
  Doc: TDocument;

begin
  try
    // Создаем клиент
    Client := TOFDApiClient.Create(BASE_URL, API_KEY);
    try
      // Авторизация
      WriteLn('Авторизация...');
      Client.Authorize;
      WriteLn('Авторизация успешна');
      WriteLn;

      // Получаем список организаций
      WriteLn('Получение списка организаций...');
      Organisations := Client.GetOrganisations(1, 10);
      try
        WriteLn(Format('Найдено организаций: %d', [Organisations.TotalCount]));

        for Org in Organisations.Page do
        begin
          WriteLn(Format('  %s - %s (ИНН: %s, КПП: %s)', [
            Org.OrganisationKey,
            Org.OrganizationName,
            Org.Inn,
            Org.Kpp
          ]));

          // Получаем разрешения для первой организации
          if Organisations.Page.IndexOf(Org) = 0 then
          begin
            WriteLn('  Проверка разрешений...');
            Permissions := Client.GetPermissions(Org.OrganisationKey);
            try
              WriteLn(Format('    Доступ ко всем кассам: %s', [
                BoolToStr(Permissions.AccessToAllKkms, True)
              ]));

              if Permissions.HasCapability(CAP_TRANSACTIONS) then
                WriteLn('    ✓ Доступ к транзакциям');
              if Permissions.HasCapability(CAP_SHIFTS) then
                WriteLn('    ✓ Доступ к сменам');
              if Permissions.HasCapability(CAP_REVENUE) then
                WriteLn('    ✓ Доступ к выручке');
            finally
              Permissions.Free;
            end;
            
            // Получаем торговые точки
            WriteLn('  Получение торговых точек...');
            RetailPlaces := Client.GetRetailPlaces(Org.OrganisationKey);
            try
              WriteLn(Format('    Торговых точек: %d', [RetailPlaces.RetailPlaces.Count]));
              
              for Place in RetailPlaces.RetailPlaces do
              begin
                WriteLn(Format('      [%d] %s - %s', [
                  Place.RetailPlaceId,
                  Place.Title,
                  Place.Address
                ]));
              end;
            finally
              RetailPlaces.Free;
            end;
            
            // Получаем смены за последние дни
            WriteLn('  Получение смен...');
            Params.Clear;
            Params.KkmRegId := '0000000000044665'; // Укажите РНМ кассы
            Params.FromDate := IncDay(Now, -1);
            Params.ToDate := Now;

            Shifts := Client.GetShifts(Org.OrganisationKey, Params);
            try
              WriteLn(Format('    Смен найдено: %d', [Shifts.Count]));

              for Shift in Shifts do
              begin
                WriteLn(Format('      Смена №%d, Касса: %s, Закрыта: %s', [
                  Shift.ShiftNumber,
                  Shift.KkmRegId,
                  DateToStr(Shift.DateTimeClose)
                ]));
                WriteLn(Format('        Выручка: %.2f руб.', [Shift.IncomeSum]));
              end;
            finally
              Shifts.Free;
            end;
            
            // Получаем документы
            WriteLn('  Получение документов...');
            Params.Clear;
            Params.KkmRegId := '0000000000044665'; // Укажите РНМ кассы
            Params.ShiftNum := 3;  // Номер смены
            Params.FromDate := IncMinute(Now, -5);
            Params.ToDate := Now;
            Params.FsFactoryNumber := '9999078902019097';
            SetLength(Params.TransactionTypes, 1);
            Params.TransactionTypes[0] := ttTicket;

            Documents := Client.GetDocuments(Org.OrganisationKey, Params);
            try
              WriteLn(Format('    Документов найдено: %d', [Documents.Count]));
              
              for Doc in Documents do
              begin
                WriteLn(Format('      Документ №%d, ФП: %s', [
                  Doc.FiscalDocumentNumber,
                  Doc.FiscalSign
                ]));
                WriteLn(Format('        Тип: %s', [Doc.TransactionType]));
                WriteLn(Format('        Статус: %s', [Doc.FnsFlcStatus]));
                
                if Assigned(Doc.Ticket) then
                begin
                  WriteLn(Format('        Сумма: %.2f руб.', [Doc.Ticket.TotalSum]));
                  WriteLn(Format('        Позиций: %d', [Doc.Ticket.Items.Count]));
                end;
              end;
            finally
              Documents.Free;
            end;
          end;
        end;
      finally
        Organisations.Free;
      end;
      
    finally
      Client.Free;
    end;
    
    WriteLn;
    WriteLn('Готово!');
    
  except
    on E: Exception do
    begin
      WriteLn('ОШИБКА: ' + E.ClassName + ': ' + E.Message);
      ExitCode := 1;
    end;
  end;
  
  ReadLn;
end.