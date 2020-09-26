unit U_RBTreeTestTypes;

interface

type
  TRBTreeData = class
  public
    Key, Val : integer;
  end;

function CompareRBTreeData(i1, i2 : Pointer) : Integer;

implementation

function CompareRBTreeData(i1, i2 : Pointer) : Integer;
begin
  result:=TRBTreeData(i2).Key - TRBTreeData(i1).Key;
end;

end.

