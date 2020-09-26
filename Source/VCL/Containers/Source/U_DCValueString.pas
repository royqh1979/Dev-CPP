unit U_DCValueString;

interface

uses
  U_DCValue;

type
  {
    Class storing string value
  }
  TDCValueString = class(TDCValue)
  private
    FValue : string;
    function GetInteger: integer; override;
    function GetObject: TObject; override;
    function GetString: string; override;
  public
    constructor Create(const AValue : string);
  end;

implementation

uses U_DCExceptions;

{ TDCValueString }

constructor TDCValueString.Create(const AValue: string);
begin
  FValue:=AValue;
end;

function TDCValueString.GetInteger: integer;
begin
  raise EDCValueIncorrectFormat.Create('String', 'Integer');
end;

function TDCValueString.GetObject: TObject;
begin
  raise EDCValueIncorrectFormat.Create('String', 'TObject');
end;

function TDCValueString.GetString: string;
begin
  result:=FValue;
end;

end.
