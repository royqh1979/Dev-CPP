unit U_RBTreeTestDataObjects;

interface

uses
  U_RBTreeTestTypes, Classes;

type
  TRBTreeTestDataObjects = class
  private
    FDataObjects : TList;
    function GetCount: integer;
    function GetItem(AIndex: integer): TRBTreeData;
  public
    property Count : integer read GetCount;
    property Items[AIndex : integer] : TRBTreeData read GetItem; default;

    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    function CreateDataObject(const AKey, AVal : integer) : TRBTreeData;
  end;

implementation

{ TRBTreeTestDataObjects }

constructor TRBTreeTestDataObjects.Create;
begin
  FDataObjects:=TList.Create;
end;

destructor TRBTreeTestDataObjects.Destroy;
begin
  Clear;
  FDataObjects.Free;
  inherited;
end;

procedure TRBTreeTestDataObjects.Clear;
var
  i : integer;
begin
  for i:=0 to FDataObjects.Count - 1 do
    TRBTreeData(FDataObjects[i]).Free;
  FDataObjects.Clear;
end;

function TRBTreeTestDataObjects.CreateDataObject(const AKey,
  AVal: integer): TRBTreeData;
begin
  result:=TRBTreeData.Create;
  result.Key:=AKey;
  result.Val:=AVal;
  FDataObjects.Add(result);
end;

function TRBTreeTestDataObjects.GetCount: integer;
begin
  result:=FDataObjects.Count;
end;

function TRBTreeTestDataObjects.GetItem(AIndex: integer): TRBTreeData;
begin
  result:=TRBTreeData(FDataObjects[AIndex]);
end;

end.
 