unit FrEmuPrinter;

interface

uses
  SysUtils, Classes, Variants, ComObj, ActiveX;

const
  FrEmuProgID = 'FrEmu.Printer';
  FrEmuCLSID  = '{A3F8C2E1-7B4D-4E9A-9C21-8F6D1B0A4E77}';

type
  { TFrEmuPrinter }

  TFrEmuPrinter = class
  private
    FObj: OleVariant;
    FStarted: Boolean;
    procedure EnsureConnected;
  public
    destructor Destroy; override;
    procedure Connect;
    procedure Start(const ComPort: string = ''; const FlashPath: string = '');
    procedure Stop;
    function Running: Boolean;
    function ReadText: string;
    procedure ClearText;
    function ReadImage: TBytes;
    procedure SaveImage(const FileName: string);
    function CutCount: Integer;
    function DrawerKickCount: Integer;
    function GpioRisePull: string;
    function DrawerKickTrace: string;
    function GetRollPresent: Boolean;
    procedure SetRollPresent(Value: Boolean);
    function GetDrawerClosed: Boolean;
    procedure SetDrawerClosed(Value: Boolean);
    property Started: Boolean read FStarted;
    property RollPresent: Boolean read GetRollPresent write SetRollPresent;
    property DrawerClosed: Boolean read GetDrawerClosed write SetDrawerClosed;
  end;

function NormalizePrintedText(const S: string): string;

implementation

function NormalizePrintedText(const S: string): string;
begin
  Result := StringReplace(S, #13#10, #10, [rfReplaceAll]);
  Result := StringReplace(Result, #13, #10, [rfReplaceAll]);
  Result := Trim(Result);
end;

{ TFrEmuPrinter }

destructor TFrEmuPrinter.Destroy;
begin
  Stop;
  inherited Destroy;
end;

function Connected(const V: OleVariant): Boolean;
begin
  Result := not VarIsEmpty(V) and not VarIsNull(V);
end;

procedure TFrEmuPrinter.Connect;
var
  Unk: IUnknown;
  Disp: IDispatch;
begin
  if Connected(FObj) then
    Exit;
  try
    FObj := CreateOleObject(FrEmuProgID);
  except
    Unk := CreateComObject(StringToGUID(FrEmuCLSID));
    if not Supports(Unk, IDispatch, Disp) then
      raise Exception.Create('FrEmu.Printer does not support IDispatch');
    FObj := Disp;
  end;
end;

procedure TFrEmuPrinter.EnsureConnected;
begin
  if not Connected(FObj) then
    raise Exception.Create('FR emulator is not created (FrEmu.Printer)');
end;

procedure TFrEmuPrinter.Start(const ComPort, FlashPath: string);
begin
  Connect;
  if ComPort <> '' then
    FObj.ComPort := ComPort;
  if FlashPath <> '' then
    FObj.FlashPath := FlashPath;
  FObj.Start;
  FStarted := True;
end;

procedure TFrEmuPrinter.Stop;
begin
  if not FStarted then
    Exit;
  try
    FObj.Stop;
  except
  end;
  FStarted := False;
  FObj := Unassigned;
end;

function TFrEmuPrinter.Running: Boolean;
begin
  Result := False;
  if not Connected(FObj) then
    Exit;
  Result := Boolean(FObj.Running);
end;

function TFrEmuPrinter.ReadText: string;
begin
  EnsureConnected;
  Result := VarToStr(FObj.ReadText);
end;

procedure TFrEmuPrinter.ClearText;
begin
  EnsureConnected;
  FObj.ClearText;
end;

function TFrEmuPrinter.ReadImage: TBytes;
var
  V: OleVariant;
  i, LoBound, HiBound: Integer;
begin
  EnsureConnected;
  V := FObj.ReadImage;
  if not VarIsArray(V) then
  begin
    SetLength(Result, 0);
    Exit;
  end;
  LoBound := VarArrayLowBound(V, 1);
  HiBound := VarArrayHighBound(V, 1);
  SetLength(Result, HiBound - LoBound + 1);
  for i := LoBound to HiBound do
    Result[i - LoBound] := Byte(V[i]);
end;

procedure TFrEmuPrinter.SaveImage(const FileName: string);
begin
  EnsureConnected;
  FObj.SaveImage(FileName);
end;

function TFrEmuPrinter.CutCount: Integer;
begin
  EnsureConnected;
  Result := Integer(FObj.CutCount);
end;

function TFrEmuPrinter.DrawerKickCount: Integer;
begin
  EnsureConnected;
  Result := Integer(FObj.DrawerKickCount);
end;

function TFrEmuPrinter.GpioRisePull: string;
begin
  EnsureConnected;
  Result := Trim(VarToStr(FObj.GpioRisePull));
end;

function TFrEmuPrinter.DrawerKickTrace: string;
begin
  EnsureConnected;
  Result := Trim(VarToStr(FObj.DrawerKickTrace));
end;

function TFrEmuPrinter.GetRollPresent: Boolean;
begin
  EnsureConnected;
  Result := Boolean(FObj.RollPresent);
end;

procedure TFrEmuPrinter.SetRollPresent(Value: Boolean);
begin
  EnsureConnected;
  FObj.RollPresent := Value;
end;

function TFrEmuPrinter.GetDrawerClosed: Boolean;
begin
  EnsureConnected;
  Result := Boolean(FObj.DrawerClosed);
end;

procedure TFrEmuPrinter.SetDrawerClosed(Value: Boolean);
begin
  EnsureConnected;
  FObj.DrawerClosed := Value;
end;

end.
