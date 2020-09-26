unit RBTreeTraverse;

interface

uses
  RBTreeTypes;

{
  Oryginalny opis:
  The functions RBInc and RBDec can be used to "walk" through the tree:
  given a TRBNodeP x, RBInc returns the TRBNodeP with the smallest key that
  is larger than x, RBDec returns the TRBNodeP with the largest key that is
  smaller than x.

  Po modyfikacji: RBInc(tree.Last) and RBDec(tree.First) zwracaja nil.
}
procedure RBInc(var x : TRBNodeP);
procedure RBDec(var x : TRBNodeP);

implementation

{ Pre: x <> last }
procedure RBInc(var x : TRBNodeP);
var
  y : TRBNodeP;
begin
  if (x^.right <> nil) then
  begin
    x:=x^.right;
    while (x^.left <> nil) do
      x:=x^.left;
  end
  else begin
    y:=x^.parent;
    while (y <> nil) and (x = y^.right) do
    begin
      x:=y;
      y:=y^.parent;
    end;
    if (x^.right <> y) then
      x:=y;
  end
end;

{ Pre: x <> first }
procedure RBDec(var x : TRBNodeP);
var
  y : TRBNodeP;
begin
  if (x^.left <> nil) then
  begin
    y:=x^.left;
    while (y^.right <> nil) do
      y:=y^.right;
    x:=y;
  end
  else begin
    y:=x^.parent;
    while (y <> nil) and (x = y^.left) do
    begin
      x:=y;
      y:=y^.parent;
    end;
    x:=y;
  end
end;

end.
