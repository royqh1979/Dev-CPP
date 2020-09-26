unit U_DCManagerList;

interface

uses
  Classes, U_DCManagerBase, U_DCTreeKeyValue, U_DCValue;

type
  {
    RBTree objects manager.
    Internally objects are stored in list.
  }
  TDCManagerList = class(TDCManagerBase)
  private
    FObjects : TList;

  protected
    function GetItem(AIndex : integer) : PDCTreeKeyValue; override;
    function GetCount : integer; override;
    function CheckIsEmpty : boolean; override;

    function CreateKV(AKey : integer; AValue : TDCValue) : PDCTreeKeyValue;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear; override;

    function CreateObject(AKey, AValue : integer) : PDCTreeKeyValue; override;
    function CreateObject(AKey : integer; const AValue : string) : PDCTreeKeyValue; override;
    function CreateObject(AKey : integer; AValue : TObject) : PDCTreeKeyValue; override;

    procedure DeleteObject(AObjPtr : PDCTreeKeyValue); override;

    function IndexOf(AObjPtr : PDCTreeKeyValue) : integer; override;
  end;

implementation

uses
  U_DCValueInteger, U_DCValueObject, U_DCValueString;

 { TDCManager }

constructor TDCManagerList.Create;
begin
  FObjects:=TList.Create;
end;

destructor TDCManagerList.Destroy;
begin
  Clear;
  FObjects.Free;
  inherited;
end;

procedure TDCManagerList.Clear;
var
  i : integer;
begin
  for i:=0 to FObjects.Count - 1 do
    FreeDCTreeKeyValue(GetItem(i));
  FObjects.Clear;
end;

function TDCManagerList.GetItem(AIndex: integer): PDCTreeKeyValue;
begin
  Result:=PDCTreeKeyValue(FObjects[AIndex]);
end;

function TDCManagerList.GetCount : integer;
begin
  Result:=FObjects.Count;
end;

function TDCManagerList.CheckIsEmpty : boolean;
begin
  Result:=(FObjects.Count = 0);
end; 

function TDCManagerList.CreateKV(AKey : integer; AValue : TDCValue) : PDCTreeKeyValue;
begin
  Result:=CreateDCTreeKeyValue(AKey, AValue);
  FObjects.Add(Result);
end;

function TDCManagerList.CreateObject(AKey, AValue : integer) : PDCTreeKeyValue;
begin
  Result:=CreateKV(AKey, TDCValueInteger.Create(AValue));
end;

function TDCManagerList.CreateObject(AKey : integer; const AValue : string) : PDCTreeKeyValue;
begin
  Result:=CreateKV(AKey, TDCValueString.Create(AValue));
end;

function TDCManagerList.CreateObject(AKey : integer; AValue : TObject) : PDCTreeKeyValue;
var
  val : TDCValue;
begin
  val:=nil;
  if AValue <> nil then
    val:=TDCValueObject.Create(AValue);
  Result:=CreateKV(AKey, val);
end;

procedure TDCManagerList.DeleteObject(AObjPtr : PDCTreeKeyValue);
begin
  FObjects.Remove(AObjPtr);
  FreeDCTreeKeyValue(AObjPtr);
end;

function TDCManagerList.IndexOf(AObjPtr: PDCTreeKeyValue): integer;
begin
  result:=FObjects.IndexOf(AObjPtr);
end;

end.
