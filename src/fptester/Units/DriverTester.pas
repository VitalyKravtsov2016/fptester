unit DriverTester;

interface

uses
  // VCL
  Windows, Classes, SysUtils, ComObj, ActiveX, System.Generics.Collections,
  // 3'd
  VSoft.YAML,
  // This
  DrvFRLib_TLB, FileUtils, DriverTest, CommandTest, ReceiptTest,
  CashRegistersTest;

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
begin
  doc := TYAML.LoadFromFile(FileName);

  FName := doc.Root.GetValue('name').AsString;
  FText := doc.Root.GetValue('text').AsString;
  tests := doc.Root.GetValue('tests');
  if (not tests.IsNull)and(tests.IsSequence) then
  begin
    Sequence := tests.AsSequence;
    if Sequence.Count > 0 then
    begin
      for i := 0 to Sequence.Count-1 do
      begin
        testNode := Sequence.Items[i];

        testtype := 'command';
        if testNode.TryGetValue('testtype', value) then
          testtype := value.AsString;

        if testtype = 'command' then
        begin
          Test := TCommandTest.CreateTest(Context);
          Test.Load(testNode);
          FTests.Add(Test);
        end;

        (*
        if testtype = 'read_cash_register' then
        begin
          //Test := TCashRegisterTest.Create;
          Test.Load(testNode);
          FTests.Add(Test);
        end;
        *)

        if testtype = 'read_cash_registers' then
        begin
          Test := TCashRegistersTest.CreateTest(Context);
          Test.Load(testNode);
          FTests.Add(Test);
        end;

        if testtype = 'receipt_test' then
        begin
          Test := TReceiptTest.CreateTest(Context);
          Test.Load(testNode);
          FTests.Add(Test);
        end;
      end;
    end;
  end;
end;

{ TDriverTester }

procedure TDriverTester.Run;
begin
  FReport.TotalCount := 0;
  FReport.SuccessCount := 0;
  FReport.StartTime := Now;


  ShowVersion;
  try
    Context.TestConnection;
    ExecuteTestFiles;
    WriteReport;
  except
    on E: Exception do
    begin
      Context.Error(E.Message);
    end;
  end;
  WriteLn('Для завершения нажмите любую клавишу');
  ReadLn;
end;

procedure TDriverTester.SetOptions(const Value: TTesterOptions);
begin
  Context.Options := Value;
end;

procedure TDriverTester.ShowVersion;
begin
  WriteLn('Fiscal printer tester 1.0, 2025');
end;

procedure TDriverTester.ExecuteTestFiles;
var
  i: Integer;
  Files: TStrings;
const
  FileMask = '\*.yaml';
begin
  Files := TStringList.Create;
  try
    GetFileNames(Files, Options.FilesPath, FileMask);
    WriteLn(Format('Найдено файлов тестов: %d', [Files.Count]));
    for i := 0 to Files.Count-1 do
    begin
      ExecuteTestFile(Files[i]);
    end;
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
    TestSuite.LoadFromFile(FileName, Context);
    ExecuteTestSuite(TestSuite);
  finally
    TestSuite.Free;
  end;
end;

procedure TDriverTester.ExecuteTestSuite(TestSuite: TDriverTestSuite);
var
  Test: TDriverTest;
begin
  WriteLn(Separator);
  WriteLn(TestSuite.name);
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
    Context.StartTest;
    Context.ResetEcr;
    Context.SetEcrMode(Test.ecrmode_before);
    Result := Test.Execute;
    if Result.IsSucceeded then
    begin
      Context.CheckEcrMode2(Test.ecrmode_after);
      Context.CheckTextPrinted(Test.text_printed);

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
  WriteLn(Separator);
  SuccessPercents := 0;
  if FReport.TotalCount > 0 then
    SuccessPercents := Round(FReport.SuccessCount *100 / FReport.TotalCount);

  WriteLn(Format('Успешно выполнено: %d из %d, %d %%', [
    FReport.SuccessCount, FReport.TotalCount, SuccessPercents]));

  WriteLn(FormatDateTime('Время выполнения: hh:mm:ss', Now-FReport.StartTime));
end;

end.
