unit rbtree;

interface

uses
  Classes, RBTreeTypes;

type
  TRBTree = class
  private
    FRoot : TRBNodeP;
    FLeftMost : TRBNodeP;
    FRightMost : TRBNodeP;
    FCompareFunc : TListSortCompare;

    procedure RotateLeft(var x : TRBNodeP);
    procedure RotateRight(var x : TRBNodeP);
    function Minimum(var x : TRBNodeP) : TRBNodeP;
    function Maximum(var x : TRBNodeP) : TRBNodeP;

    procedure FastErase(x : TRBNodeP);
  public
    property Root : TRBNodeP read FRoot;
    property First : TRBNodeP read FLeftMost;
    property Last : TRBNodeP read FRightMost;

    constructor Create(ACompareFunc : TListSortCompare);
    destructor Destroy; override;

    procedure Clear;

    function Find(key : Pointer) : TRBNodeP;
    function Add(key : Pointer) : TRBNodeP;
    procedure Delete(z : TRBNodeP);
  end;

implementation

constructor TRBTree.Create(ACompareFunc : TListSortCompare);
begin
  inherited Create;
  FCompareFunc:=ACompareFunc;
  FRoot:=nil;
  FLeftMost:=nil;
  FRightMost:=nil;
end;

destructor TRBTree.Destroy;
begin
  Clear();
  inherited Destroy;
end;

procedure TRBTree.FastErase(x : TRBNodeP);
begin
  if (x^.left <> nil) then FastErase(x^.left);
  if (x^.right <> nil) then FastErase(x^.right);
  dispose(x);
end;

procedure TRBTree.Clear;
begin
  if (FRoot <> nil) then
    FastErase(FRoot);
  FRoot:=nil;
  FLeftMost:=nil;
  FRightMost:=nil;
end;

function TRBTree.Find(key : Pointer) : TRBNodeP;
var
  cmp : integer;
begin
  Result:=FRoot;
  while (Result <> nil) do
  begin
    cmp:=FCompareFunc(Result^.k, key);
    if cmp < 0 then
      Result:=Result^.right
    else if cmp > 0 then
      Result:=Result^.left
    else
      break;
  end;
end;

procedure TRBTree.RotateLeft(var x : TRBNodeP);
var
  y : TRBNodeP;
begin
  y:=x^.right;
  x^.right:=y^.left;
  if (y^.left <> nil) then
    y^.left^.parent:=x;
  y^.parent:=x^.parent;
  if (x = FRoot) then
    FRoot:=y
  else if (x = x^.parent^.left) then
    x^.parent^.left:=y
  else
    x^.parent^.right:=y;
  y^.left:=x;
  x^.parent:=y;
end;

procedure TRBTree.RotateRight(var x : TRBNodeP);
var
  y : TRBNodeP;
begin
  y:=x^.left;
  x^.left:=y^.right;
  if (y^.right <> nil) then
    y^.right^.parent:=x;
  y^.parent:=x^.parent;
  if (x = FRoot) then
    FRoot:=y
  else if (x = x^.parent^.right) then
    x^.parent^.right:=y
  else
    x^.parent^.left:=y;
  y^.right:=x;
  x^.parent:=y;
end;

function TRBTree.Minimum(var x : TRBNodeP) : TRBNodeP;
begin
  Result:=x;
  while (Result^.left <> nil) do
    Result:=Result^.left;
end;

function TRBTree.Maximum(var x : TRBNodeP) : TRBNodeP;
begin
  Result:=x;
  while (Result^.right <> nil) do
    Result:=Result^.right;
end;

function TRBTree.Add(key : Pointer) : TRBNodeP;
var
  x, y, z, zpp : TRBNodeP;
  cmp : Integer;
begin
  Result:=nil;
  if key = nil then exit;

  z:=New(TRBNodeP);
  { Initialize fields in new node z }
  z^.k:=key;
  z^.left:=nil;
  z^.right:=nil;
  z^.color:=clRed;

  Result:=z;

  { Maintain leftmost and rightmost nodes }
  if ((FLeftMost = nil) or (FCompareFunc(key, FLeftMost^.k) < 0)) then
    FLeftMost:=z;
  if ((FRightMost = nil) or (FCompareFunc(FRightMost^.k, key) < 0)) then
    FRightMost:=z;

  { Insert node z }
  y:=nil;
  x:=FRoot;
  while (x <> nil) do
  begin
    y:=x;
    cmp:=FCompareFunc(key, x^.k);
    if (cmp < 0) then
      x:=x^.left
    else if (cmp > 0) then
      x:=x^.right
    else begin
      { Value already exists in tree. }
//      Result:=x;
      Result:=nil;
      dispose(z);
      exit;
    end;
  end;
  z^.parent:=y;
  if (y = nil) then
    FRoot:=z
  else if (FCompareFunc(key, y^.k) < 0) then
    y^.left:=z
  else
    y^.right:=z;

  { Rebalance tree }
  while ((z <> FRoot) and (z^.parent^.color = clRed)) do
  begin
    zpp:=z^.parent^.parent;
    if (z^.parent = zpp^.left) then
    begin
      y:=zpp^.right;
      if ((y <> nil) and (y^.color = clRed)) then
      begin
        z^.parent^.color:=clBlack;
        y^.color:=clBlack;
        zpp^.color:=clRed;
        z:=zpp;
      end
      else begin
        if (z = z^.parent^.right) then
        begin
          z:=z^.parent;
          rotateLeft(z);
        end;
        z^.parent^.color:=clBlack;
        zpp^.color:=clRed;
        rotateRight(zpp);
      end;
    end
    else begin
      y:=zpp^.left;
      if ((y <> nil) and (y^.color = clRed)) then
      begin
        z^.parent^.color:=clBlack;
        y^.color:=clBlack;
        zpp^.color:=clRed;
        z:=zpp;
      end
      else begin
        if (z = z^.parent^.left) then
        begin
          z:=z^.parent;
          rotateRight(z);
        end;
        z^.parent^.color:=clBlack;
        zpp^.color:=clRed;
        rotateLeft(zpp);
      end;
    end;
  end;
  FRoot^.color:=clBlack;
end;

procedure TRBTree.Delete(z : TRBNodeP);
var
  w, x, y, x_parent : TRBNodeP;
  tmpcol : TRBColor;
begin
  if z = nil then exit;

  y:=z;
  x:=nil;
  x_parent:=nil;

  if (y^.left = nil) then { z has at most one non-null child. y = z. }
    x:=y^.right { x might be null. }
  else begin
    if (y^.right = nil) then { z has exactly one non-null child. y = z. }
      x:=y^.left { x is not null. }
    else begin
      { z has two non-null children.  Set y to }
      y:=y^.right; {   z's successor.  x might be null. }
      while (y^.left <> nil) do
        y:=y^.left;
      x:=y^.right;
    end;
  end;

  if (y <> z) then
  begin
    { "copy y's sattelite data into z" }
    { relink y in place of z.  y is z's successor }
    z^.left^.parent:=y;
    y^.left:=z^.left;
    if (y <> z^.right) then
    begin
      x_parent:=y^.parent;
      if (x <> nil) then
        x^.parent:=y^.parent;
      y^.parent^.left:=x; { y must be a child of left }
      y^.right:=z^.right;
      z^.right^.parent:=y;
    end
    else
      x_parent:=y;
    if (FRoot = z) then
      FRoot:=y
    else if (z^.parent^.left = z) then
      z^.parent^.left:=y
    else
      z^.parent^.right:=y;
    y^.parent:=z^.parent;
    tmpcol:=y^.color;
    y^.color:=z^.color;
    z^.color:=tmpcol;
    y:=z;
    { y now points to node to be actually deleted }
  end
  else begin { y = z }
    x_parent:=y^.parent;
    if (x <> nil) then
      x^.parent:=y^.parent;
    if (FRoot = z) then
      FRoot:=x
    else begin
      if (z^.parent^.left = z) then
        z^.parent^.left:=x
      else
        z^.parent^.right:=x;
    end;
    if (FLeftMost = z) then
    begin
      if (z^.right = nil) then
        FLeftMost:=z^.parent { z^.left must be null also }
      else
        FLeftMost:=minimum(x);
    end;
    if (FRightMost = z) then
    begin
      if (z^.left = nil) then { z^.right must be null also }
        FRightMost:=z^.parent
      else { x == z^.left }
        FRightMost:=maximum(x);
    end;
  end;

  { Rebalance tree }
  if (y^.color = clBlack) then
  begin
    while ((x <> FRoot) and ((x = nil) or (x^.color = clBlack))) do
    begin
      if (x = x_parent^.left) then
      begin
        w:=x_parent^.right;
        if (w^.color = clRed) then
        begin
          w^.color:=clBlack;
          x_parent^.color:=clRed;
          rotateLeft(x_parent);
          w:=x_parent^.right;
        end;
        if (((w^.left = nil) or (w^.left^.color = clBlack))
          and ((w^.right = nil) or (w^.right^.color = clBlack))) then
        begin
          w^.color:=clRed;
          x:=x_parent;
          x_parent:=x_parent^.parent;
        end
        else begin
          if ((w^.right = nil) or (w^.right^.color = clBlack)) then
          begin
            w^.left^.color:=clBlack;
            w^.color:=clRed;
            rotateRight(w);
            w:=x_parent^.right;
          end;
          w^.color:=x_parent^.color;
          x_parent^.color:=clBlack;
          if (w^.right <> nil) then
            w^.right^.color:=clBlack;
          rotateLeft(x_parent);
          x:=FRoot; { break; }
        end
      end
      else begin
        { same as above, with right <^. left. }
        w:=x_parent^.left;
        if (w^.color = clRed) then
        begin
          w^.color:=clBlack;
          x_parent^.color:=clRed;
          rotateRight(x_parent);
          w:=x_parent^.left;
        end;
        if (((w^.right = nil) or (w^.right^.color = clBlack))
          and ((w^.left = nil) or (w^.left^.color = clBlack))) then
        begin
          w^.color:=clRed;
          x:=x_parent;
          x_parent:=x_parent^.parent;
        end
        else begin
          if ((w^.left = nil) or (w^.left^.color = clBlack)) then
          begin
            w^.right^.color:=clBlack;
            w^.color:=clRed;
            rotateLeft(w);
            w:=x_parent^.left;
          end;
          w^.color:=x_parent^.color;
          x_parent^.color:=clBlack;
          if (w^.left <> nil) then
            w^.left^.color:=clBlack;
          rotateRight(x_parent);
          x:=FRoot; { break; }
        end;
      end;
    end;
    if (x <> nil) then
      x^.color:=clBlack;
  end;
  dispose(y);
end;

end.

