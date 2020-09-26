unit U_DCHashBase;

interface

type
  {
    Base class for hash generators
  }
  TDCHashBase = class
  public
    function Hash(const s : string) : integer; overload; virtual; abstract;
    function Hash(const i : integer) : integer; overload; virtual; abstract;
  end;

implementation

end.
 