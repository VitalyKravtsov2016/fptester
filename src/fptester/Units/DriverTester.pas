unit DriverTester;

interface

uses
  // VCL
  Windows, Classes, SysUtils, ComObj, ActiveX, System.Generics.Collections,
  // 3'd
  VSoft.YAML,
  // This
  DrvFRLib_TLB, FileUtils, DriverTest, CommandTest, ReceiptTest,
  CashRegistersTest, TableTest, LogFile;

type
  { TTestReport }

  TTestReport = record
    StartTime: TDateTime;
    SuccessCount: Integer;
    TotalCount: Integer;
  end;

  { TDriverTestSuite }

  TDriverTestSuite = class
  private
    FName: string;
    FText: string;
    FTests: TDriverTests;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(const FileName: string; Context: TDriverContext);

    property Name: string read FName;
    property Text: string read FText;
    property Tests: TDriverTests read FTests;
  end;

  { TDriverTester }

  TDriverTester = class
  private
    FReport: TTestReport;
    FContext: TDriverContext;

    procedure ShowVersion;
    procedure WriteReport;
    procedure ExecuteTestFiles;
    procedure ExecuteTestFile(const FileName: string);
    procedure ExecuteTestSuite(TestSuite: TDriverTestSuite);
    function GetOptions: TTesterOptions;
    procedure SetOptions(const Value: TTesterOptions);
    function GetContext: TDriverContext;
    procedure ExecuteTest(Test: TDriverTest);
  public
    destructor Destroy; override;
    procedure Run;
    property Context: TDriverContext read GetContext;
    property Options: TTesterOptions read GetOptions write SetOptions;
  end;

implementation

function GetFileNames(FileNames: TStrings; Directory, FileMask: string): Integer;
var
  Status: Integer;
  SearchRec: TSearchRec;
begin
  FileNames.BeginUpdate;
  try
    FileNames.Clear;
    Status := FindFirst(Directory+FileMask, faAnyFile, SearchRec);
    while Status = 0 do
    begin
      if (SearchRec.Attr and faDirectory) = 0 then
        FileNames.Add(Directory + SearchRec.Name);
      Status := FindNext(SearchRec);
    end;
    FindClose(SearchRec);
  finally
    FileNames.EndUpdate;
  end;
  Result := FileNames.Count;
end;

{ TDriverTestSuite }

constructor TDriverTestSuite.Create;
begin
  FTests := TDriverTests.Create;
end;

destructor TDriverTestSuite.Destroy;
begin
  FTests.Free;
  inherited Destroy;
end;

procedure TDriverTestSuite.LoadFromFile(const FileName: string;
  Context: TDriverContext);
var
  i: Integer;
  tests: IYAMLValue;
  testNode: IYAMLValue;
  value : IYAMLValue;
  Sequence: IYAMLSequence;
  doc: IYAMLDocument;
  testType: string;
  Test: TDriverTest;
  Unknown: TUnknownTest;
  Loaded: Integer;
  Skipped: Integer;
begin
  Loaded := 0;
  Skipped := 0;
  doc := TYAML.LoadFromFile(FileName);

  FName := ExtractFileName(FileName);
  if doc.Root.TryGetValue('name', value) then
    FName := value.AsString;
  FText := '';
  if doc.Root.TryGetValue('text', value) then
    FText := value.AsString
  else if doc.Root.TryGetValue('description', value) then
    FText := value.AsString;

  tests := nil;
  if doc.Root.TryGetValue('tests', value) then
    tests := value;
  if (tests <> nil) and (not tests.IsNull) and tests.IsSequence then
  begin
    Sequence := tests.AsSequence;
    for i := 0 to Sequence.Count-1 do
    begin
      testNode := Sequence.Items[i];
      Test := nil;
      testtype := 'command';
      if testNode.TryGetValue('testtype', value) then
        testtype := LowerCase(Trim(value.AsString));

      try
        if (testtype = 'command') or (testtype = '') then
        begin
          Test := TCommandTest.CreateTest(Context);
        end
        else if testtype = 'commands' then
        begin
          Test := TCommandsTest.CreateTest(Context);
        end
        else if testtype = 'read_cash_registers' then
        begin
          Test := TCashRegistersTest.CreateTest(Context);
        end
        else if (testtype = 'receipt_test') or (testtype = 'receipt') then
        begin
          Test := TReceiptTest.CreateTest(Context);
        end
        else if (testtype = 'table_field') or (testtype = 'table') then
        begin
          Test := TTableFieldTest.CreateTest(Context);
        end
        else if (testtype = 'all_table_fields') or (testtype = 'all_tables') then
        begin
          Test := TAllTableFieldsTest.CreateTest(Context);
        end
        else if (testtype = 'table_cash_zero') or (testtype = 'auto_zero_cash') then
        begin
          Test := TAutoZeroCashTest.CreateTest(Context);
        end
        else if (testtype = 'table_ad_text') or (testtype = 'ad_text') then
        begin
          Test := TTableFlagTest.CreateTest(Context);
          TTableFlagTest(Test).Kind := tfAdText;
        end
        else if (testtype = 'table_z_grand_total') or (testtype = 'grand_total') then
        begin
          Test := TTableFlagTest.CreateTest(Context);
          TTableFlagTest(Test).Kind := tfGrandTotal;
        end
        else if (testtype = 'table_drawer') or (testtype = 'drawer') then
        begin
          Test := TTableFlagTest.CreateTest(Context);
          TTableFlagTest(Test).Kind := tfDrawer;
        end
        else if (testtype = 'table_cut') or (testtype = 'cut') then
        begin
          Test := TTableFlagTest.CreateTest(Context);
          TTableFlagTest(Test).Kind := tfCut;
        end
        else if (testtype = 'table_frac_qty') or (testtype = 'frac_qty') then
        begin
          Test := TTableFlagTest.CreateTest(Context);
          TTableFlagTest(Test).Kind := tfFracQty;
        end
        else if (testtype = 'table_drawer_level') or (testtype = 'drawer_level') then
        begin
          Test := TTableFlagTest.CreateTest(Context);
          TTableFlagTest(Test).Kind := tfDrawerLevel;
        end
        else if (testtype = 'table_drawer_pulse') or (testtype = 'drawer_pulse') then
        begin
          Test := TTableFlagTest.CreateTest(Context);
          TTableFlagTest(Test).Kind := tfDrawerPulse;
        end
        else if (testtype = 'table_near_end') or (testtype = 'near_end') then
        begin
          Test := TTableFlagTest.CreateTest(Context);
          TTableFlagTest(Test).Kind := tfNearEnd;
        end
        else if (testtype = 'table_tax_mode') or (testtype = 'tax_mode') then
        begin
          Test := TTableFlagTest.CreateTest(Context);
          TTableFlagTest(Test).Kind := tfTaxMode;
        end
        else if (testtype = 'table_auto_time') or (testtype = 'auto_time') then
        begin
          Test := TTableFlagTest.CreateTest(Context);
          TTableFlagTest(Test).Kind := tfAutoTime;
        end
        else
        begin
          Unknown := TUnknownTest.CreateTest(Context);
          Unknown.UnknownType := testtype;
          Test := Unknown;
        end;

        Test.Load(testNode);
        if Test.name = '' then
          Test.name := Format('%s #%d', [ExtractFileName(FileName), i + 1]);
        FTests.Add(Test);
        Test := nil;
        Inc(Loaded);
      except
        on E: Exception do
        begin
          Test.Free;
          Inc(Skipped);
          Logger.Error(Format('Load error %s test %d: %s',
            [ExtractFileName(FileName), i + 1, E.Message]));
          Unknown := TUnknownTest.CreateTest(Context);
          Unknown.UnknownType := testtype;
          Unknown.name := Format('%s #%d: %s',
            [ExtractFileName(FileName), i + 1, E.Message]);
          FTests.Add(Unknown);
        end;
      end;
    end;
  end;
  Logger.Debug(Format('  %s: %d tests', [ExtractFileName(FileName), Loaded]));
  if Skipped > 0 then
    Logger.Error(Format('  %s: %d tests failed to parse',
      [ExtractFileName(FileName), Skipped]));
end;

{ TDriverTester }

destructor TDriverTester.Destroy;
begin
  if FContext <> nil then
  begin
    FContext.StopEmulator;
    FContext.Free;
    FContext := nil;
  end;
  inherited Destroy;
end;

procedure TDriverTester.Run;
begin
  FReport.TotalCount := 0;
  FReport.SuccessCount := 0;
  FReport.StartTime := Now;

  ShowVersion;
  if Options.UseEmulator then
    Logger.Debug('Mode: QEMU emulator (FrEmu.Printer)')
  else
    Logger.Debug('Mode: real fiscal printer');
  try
    try
      Context.StartEmulator;
      Context.ConfigureDriver;
      Context.TestConnection;
      ExecuteTestFiles;
      WriteReport;
    except
      on E: Exception do
      begin
        Context.Error(E.Message);
      end;
    end;
  finally
    Context.StopEmulator;
  end;
end;

procedure TDriverTester.SetOptions(const Value: TTesterOptions);
begin
  Context.Options := Value;
end;

procedure TDriverTester.ShowVersion;
begin
  Logger.Debug('Fiscal printer tester 1.0, 2026');
end;

procedure AddYamlFromDir(Files: TStrings; const Dir: string);
var
  i, j: Integer;
  Found: TStringList;
  FullName, ShortName: string;
  Exists: Boolean;
begin
  if (Dir = '') or not DirectoryExists(ExcludeTrailingPathDelimiter(Dir)) then
    Exit;
  Found := TStringList.Create;
  try
    GetFileNames(Found, IncludeTrailingPathDelimiter(Dir), '*.yaml');
    for i := 0 to Found.Count - 1 do
    begin
      FullName := Found[i];
      ShortName := ExtractFileName(FullName);
      Exists := False;
      for j := 0 to Files.Count - 1 do
      begin
        if SameText(ExtractFileName(Files[j]), ShortName) then
        begin
          Exists := True;
          Break;
        end;
      end;
      if not Exists then
        Files.Add(FullName);
    end;
  finally
    Found.Free;
  end;
end;

procedure TDriverTester.ExecuteTestFiles;
var
  i: Integer;
  Files: TStrings;
begin
  Files := TStringList.Create;
  try
    AddYamlFromDir(Files, Options.FilesPath);
    if not Options.TestsPathSpecified then
    begin
      AddYamlFromDir(Files, GetModulePath + 'data\');
      AddYamlFromDir(Files, GetModulePath + 'tests\');
      AddYamlFromDir(Files, ExpandFileName(GetModulePath + '..\..\res\tests\'));
      AddYamlFromDir(Files, ExpandFileName(GetModulePath + '..\res\tests\'));
    end;
    TStringList(Files).Sort;
    Logger.Debug(Format('Test files: %d', [Files.Count]));
    if Files.Count = 0 then
      Logger.Error('No YAML tests found in ' + Options.FilesPath);
    for i := 0 to Files.Count - 1 do
      ExecuteTestFile(Files[i]);
  finally
    Files.Free;
  end;
end;

procedure TDriverTester.ExecuteTestFile(const FileName: string);
var
  TestSuite: TDriverTestSuite;
begin
  TestSuite := TDriverTestSuite.Create;
  try
    try
      TestSuite.LoadFromFile(FileName, Context);
    except
      on E: Exception do
      begin
        Context.Error(Format('File %s: %s', [ExtractFileName(FileName), E.Message]));
        Exit;
      end;
    end;
    if TestSuite.Tests.Count = 0 then
      Logger.Error('No tests loaded from ' + ExtractFileName(FileName));
    ExecuteTestSuite(TestSuite);
  finally
    TestSuite.Free;
  end;
end;

procedure TDriverTester.ExecuteTestSuite(TestSuite: TDriverTestSuite);
var
  Test: TDriverTest;
begin
  try
    Context.ResetEcr;
  except
    on E: Exception do
      Context.Error('ResetEcr: ' + E.Message);
  end;
  Logger.Debug(Separator);
  Logger.Debug(TestSuite.name);
  for Test in TestSuite.tests do
  begin
    ExecuteTest(Test);
    Inc(FReport.TotalCount);
  end;
end;

procedure TDriverTester.ExecuteTest(Test: TDriverTest);
var
  Result: TTestResult;
begin
  try
    Context.CurrentTestName := Test.name;
    Context.StartTest;
    Context.SetEcrMode(Test.ecrmode_before);
    Result := Test.Execute;
    if Result.IsSucceeded then
    begin
      Context.CheckEcrMode2(Test.ecrmode_after);
      if Test.text_printed <> '' then
        Context.CheckTextPrinted(Test.text_printed);
      if Test.graphics_file_name <> '' then
        Context.CheckGraphicsPrinted(Test.graphics_file_name);

      Inc(FReport.SuccessCount);
      Context.Debug('[+] ' + Test.name);
    end else
    begin
      Context.Error('[-] ' + Test.name + ', ' + Result.Text);
    end;
  except
    on E: Exception do
    begin
      Context.Error('[-] ' + Test.name + ', ' + E.Message);
    end;
  end;
end;

function TDriverTester.GetContext: TDriverContext;
begin
  if FContext = nil then
    FContext := TDriverContext.Create;
  Result := FContext;
end;

function TDriverTester.GetOptions: TTesterOptions;
begin
  Result := Context.Options;
end;

procedure TDriverTester.WriteReport;
var
  SuccessPercents: Integer;
begin
  Logger.Debug(Separator);
  SuccessPercents := 0;
  if FReport.TotalCount > 0 then
    SuccessPercents := Round(FReport.SuccessCount *100 / FReport.TotalCount);

  Logger.Debug(Format('Успешно выполнено: %d из %d, %d %%', [
    FReport.SuccessCount, FReport.TotalCount, SuccessPercents]));

  Logger.Debug(FormatDateTime('Время выполнения: hh:mm:ss', Now-FReport.StartTime));
end;

end.
