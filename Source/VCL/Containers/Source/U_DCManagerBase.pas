unit U_DCManagerBase;

interface

uses
  Classes, U_DCTreeKeyValue;

type
  {
    Base class for managers of object stored in RBTree
  }
  TDCManagerBase = class
  protected
    function GetItem(AIndex : integer) : PDCTreeKeyValue; virtual; abstract;
    function GetCount : integer; virtual; abstract;
    function CheckIsEmpty : boolean; virtual; abstract;
  public
    property Items[AIndex : integer] : PDCTreeKeyValue read GetItem; default;
    property Count : integer read GetCount;
    property IsEmpty : boolean read CheckIsEmpty;

    procedure Clear; virtual; abstract;

    function CreateObject(AKey, AValue : integer) : PDCTreeKeyValue; overload; virtual; abstract;
    function CreateObject(AKey : integer; const AValue : string) : PDCTreeKeyValue; overload; virtual; abstract;
    function CreateObject(AKey : integer; AValue : TObject) : PDCTreeKeyValue; overload; virtual; abstract;

    procedure DeleteObject(AObjPtr : PDCTreeKeyValue); virtual; abstract;

    function IndexOf(AObjPtr : PDCTreeKeyValue) : integer; virtual; abstract;
  end;

implementation

end.

