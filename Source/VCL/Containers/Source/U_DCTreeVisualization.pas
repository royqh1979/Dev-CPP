unit U_DCTreeVisualization;

interface

uses
  Classes, RBTreeTypes, rbtree, U_DCTreeKeyValue;

type
  {
    Additional class for Red-Black tree visualization
  }
  TDCTreeVisualization = class
  private
    function RBColorToStr(c : TRBColor) : string;
    function CreateLine(ALevel : integer; ASide : string; PNode : TRBNodeP) : string;
    procedure ProcessNode(AStrings : TStrings; ALevel : integer; ASide : string; PNode : TRBNodeP);
  public
    procedure ShowTree(AStrings : TStrings; ARBTree : TRBTree);
  end;

implementation

uses
  SysUtils;

{ TDCTreeVisualization }

function TDCTreeVisualization.CreateLine(ALevel: integer; ASide : string;
  PNode: TRBNodeP): string;
var
  i : integer;
  s : string;
begin
  s:='';
  for i:=1 to ALevel do s:=s + '=';
  result:=format('%s%s (%s) %d', [s, ASide, RBColorToStr(PNode^.color), PDCTreeKeyValue(PNode^.k).Key]);
end;

procedure TDCTreeVisualization.ProcessNode(AStrings: TStrings;
  ALevel: integer; ASide: string; PNode: TRBNodeP);
begin
  AStrings.Add(CreateLine(ALevel, ASide, PNode));
  if PNode^.left <> nil then
    ProcessNode(AStrings, ALevel + 1, '-', PNode^.left);
  if PNode^.right <> nil then
    ProcessNode(AStrings, ALevel + 1, '+', PNode^.right);
end;

function TDCTreeVisualization.RBColorToStr(c: TRBColor): string;
begin
  if c = clRed then result:='R' else result:='B';
end;

procedure TDCTreeVisualization.ShowTree(AStrings: TStrings;
  ARBTree: TRBTree);
begin
  ProcessNode(AStrings, 0, '0', ARBTree.Root);
end;

end.
