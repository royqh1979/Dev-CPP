unit U_DCTree;

interface

uses
  Classes, RBTree, RBTreeTypes, U_DCTreeKeyValue;

type
  {
    Class encapsulating tree handling for containers
  }
  TDCTree = class
  private
    FRBTree : TRBTree;

    function CheckIsEmpty : boolean;
    function GetCount : Integer;
    function GetCountNode(node : TRBNodeP) : Integer;
  public
    property IsEmpty : boolean read CheckIsEmpty;
    property Count : integer read GetCount;

    constructor Create;
    destructor Destroy; override;

    function Add(AObject : PDCTreeKeyValue) : TRBNodeP;

    function Find(AObject : PDCTreeKeyValue) : TRBNodeP; overload;
    function Find(AHashedKey : integer) : TRBNodeP; overload;

    procedure Delete(AObject : TRBNodeP);

    procedure Clear;

    procedure ShowTree(AStrings : TStrings);
  end;

implementation

uses U_DCTreeVisualization;

constructor TDCTree.Create;
begin
  inherited Create;
  FRBTree:=TRBTree.Create(CompareDCTreeKeyValue);
end;

destructor TDCTree.Destroy;
begin
  Clear;
  FRBTree.Free;
  inherited;
end;

procedure TDCTree.Clear;
begin
  FRBTree.Clear;
end;

function TDCTree.CheckIsEmpty : boolean;
begin
  Result:=GetCount = 0;
end;

function TDCTree.GetCount : Integer;
begin
  if (FRBTree.Root = nil) then
    Result:=0
  else
    Result:=GetCountNode(FRBTree.Root);
end;

function TDCTree.GetCountNode(node : TRBNodeP) : Integer;
begin
  Result:=1;
  if not (node^.left = nil) then
    Result:=Result + GetCountNode(node^.left);
  if not (node^.right = nil) then
    Result:=Result + GetCountNode(node^.right);
end;

function TDCTree.Add(AObject : PDCTreeKeyValue) : TRBNodeP;
begin
  Result:=FRBTree.Add(AObject);
end;

function TDCTree.Find(AObject : PDCTreeKeyValue) : TRBNodeP;
begin
  Result:=FRBTree.Find(AObject);
end;

function TDCTree.Find(AHashedKey: integer): TRBNodeP;
var
  kv : TDCTreeKeyValue;
begin
  kv.Key:=AHashedKey;
  kv.Value:=nil;
  result:=Find(@kv);
end;

procedure TDCTree.Delete(AObject : TRBNodeP);
begin
  FRBTree.Delete(AObject);
end;

procedure TDCTree.ShowTree(AStrings: TStrings);
begin
  with TDCTreeVisualization.Create do
  try
    ShowTree(AStrings, FRBTree);
  finally
    Free;
  end;
end;

end.

