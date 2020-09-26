unit RBTreeTypes;

interface

type
  TRBColor = (clRed, clBlack);

  TRBNodeP = ^TRBNode;
  TRBNode = record
    k : Pointer;
    left, right, parent : TRBNodeP;
    color : TRBColor;
  end;

implementation

end.
