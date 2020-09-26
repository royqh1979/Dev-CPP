unit U_DCValueInteger;

interface

uses
  U_DCValue;

type
  {
    Class storing integer value
  }
  TDCValueInteger = class(TDCValue)
  private
    FValue : integer;
    function GetInteger: integer; override;
    function GetObject: TObject; override;
    function GetString: string; override;
  public
    constructor Create(AValue : integer);
  end;

implementation

uses SysUtils, U_DCExceptions;

{ TDCValueInteger }

constructor TDCValueInteger.Create(AValue: integer);
begin
  FValue:=AValue;
end;

function TDCValueInteger.GetInteger: integer;
begin
  result:=FValue;
end;

function TDCValueInteger.GetObject: TObject;
begin
  raise EDCValueIncorrectFormat.Create('Integer', 'TObject');
end;

function TDCValueInteger.GetString: string;
begin
  result:=IntToStr(FValue);
end;

end.
