unit duFiscalPrinter;

interface

uses
  // VCL
  System.SysUtils, System.Classes, ComObj, ActiveX, DateUtils,
  // DUnit
  TestFramework,
  // This
  DrvFRLib_TLB;

const
  FPTR_MODE_DUMP   			  = $01; // Dump mode
  FPTR_MODE_24NOTOVER		  = $02; // Opened shift, 24 hours not left
  FPTR_MODE_24OVER			  = $03; // Opened shift, 24 hours is over
  FPTR_MODE_CLOSED			  = $04; // Closed shift
  FPTR_MODE_LOCKED			  = $05; // ECR is bloced because of incorrect tax offecer password
  FPTR_MODE_WAITDATE			= $06; // Waiting for date confirm
  FPTR_MODE_POINTPOS			= $07; // Change decimal point position permission
  FPTR_MODE_REC		     	  = $08; // Opened document
  FPTR_MODE_TECH		     	= $09; // Technological reset permission
  FPTR_MODE_TEST			    = $0A; // Test run
  FPTR_MODE_FULLREPORT		= $0B; // Full fiscal report printing
  FPTR_MODE_EKLZREPORT		= $0C; // EJ report printing
  FPTR_MODE_SLP		     	  = $0D; // Opened fiscal slip
  FPTR_MODE_SLPPRINT			= $0E; // Slip printing
  FPTR_MODE_SLPREADY			= $0F; // Fiscal slip is ready

type

  { TFiscalPrinterTest }

  TFiscalPrinterTest = class(TTestCase)
  private
    Driver: TDrvFR;
    procedure Check(ResultCode: Integer);
    procedure ResetEcr;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestConfirmDate;
  end;

implementation


{ TFiscalPrinterTest }

procedure TFiscalPrinterTest.Check(ResultCode: Integer);
begin
  if ResultCode <> 0 then
    raise Exception.CreateFmt('%d, %s',
    [ResultCode, Driver.ResultCodeDescription]);
end;

procedure TFiscalPrinterTest.ResetEcr;
const
  RepCount = 10;
var
  i: Integer;
  Password: Integer;
begin
  for i := 0 to RepCount - 1 do
  begin
    Check(Driver.WaitForPrinting);
    case Driver.ECRMode of
      FPTR_MODE_DUMP:
      begin
        Password := Driver.Password;
        Driver.Password := 0;
        Check(Driver.InterruptDataStream);
        Driver.Password := Password;
      end;
      FPTR_MODE_24NOTOVER,
      FPTR_MODE_24OVER: Check(Driver.PrintReportWithCleaning);
      FPTR_MODE_WAITDATE:
      begin
        // Confirm any date
        Check(Driver.GetECRStatus);
        Check(Driver.ConfirmDate);
        // Set current datetime
        Driver.ECRDate := Now;
        Check(Driver.SetDate);
        Check(Driver.ConfirmDate);
      end;
      FPTR_MODE_REC: Check(Driver.CancelCheck);
      FPTR_MODE_TEST: Check(Driver.InterruptTest);
      FPTR_MODE_FULLREPORT,
      FPTR_MODE_EKLZREPORT,
      FPTR_MODE_SLPPRINT: ;
    else
      Break;
    end;
  end;
end;

procedure TFiscalPrinterTest.SetUp;
begin
  OleCheck(CoInitialize(nil));
  Driver := TDrvFR.Create(nil);
end;

procedure TFiscalPrinterTest.TearDown;
begin
  Driver.Free;
end;

procedure TFiscalPrinterTest.TestConfirmDate;
var
  Date: TDateTime;
begin
  ResetEcr;

  (*
  Date := Now;

  Driver.ECRDate := EncodeDate(2026, 2, 12);
  Check(Driver.SetDate
  *)
end;

initialization
  RegisterTest('', TFiscalPrinterTest.Suite);
end.
