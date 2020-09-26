unit U_DCValue;

interface

type           
  {
    Base class for value stored in container
  }
  TDCValue = class
  protected
    function GetInteger: integer; virtual; abstract;
    function GetObject: TObject; virtual; abstract;
    function GetString: string; virtual; abstract;
  public
    property AsInteger : integer read GetInteger;
    property AsString : string read GetString;
    property AsObject : TObject read GetObject;
  end;

implementation

end.
 