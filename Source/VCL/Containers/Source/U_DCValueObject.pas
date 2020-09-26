unit U_DCValueObject;

interface

uses
  U_DCValue;

type
  {
    Class storing object value
  }
  TDCValueObject = class(TDCValue)
  private
    FValue : TObject;
    function GetInteger: integer; override;
    function GetObject: TObject; override;
    function GetString: string; override;
  public
    constructor Create(AValue : TObject);
  end;

implementation

uses U_DCExceptions;

{ TDCValueObject }

constructor TDCValueObject.Create(AValue: TObject);
begin
  FValue:=AValue;
end;

function TDCValueObject.GetInteger: integer;
begin
  raise EDCValueIncorrectFormat.Create('TObject', 'Integer');
end;

function TDCValueObject.GetObject: TObject;
begin
  result:=FValue;
end;

function TDCValueObject.GetString: string;
begin
  raise EDCValueIncorrectFormat.Create('TObject', 'String');
end;

end.
