unit TestsRBTree;

interface

uses
  TestFrameWork, RBTree, RBTreeTypes,
  U_RBTreeTestTypes, U_RBTreeTestDataObjects;

type
  TRBTreeTests = class(TTestCase)
  private
    TestObj : TRBTree;
    FObjs : TRBTreeTestDataObjects;

    procedure CheckFoundObject(AObj : TRBTreeData; AFound : TRBNodeP);
    procedure CheckFindAllAddedObjects;
    procedure CheckRBTreeStructure; //verifies correctness of RB tree structure
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure CreatedEmpty;
    procedure AddOneObject_HasOneObject;
    procedure AddOneObject_CanBeFound;
    procedure AddOneObject_CanBeDeleted;
    procedure AddTwoObjects_HasTwoObjects;
    procedure AddTwoObjects_CanBeFound;
    procedure AddTwoObjects_CanBeDeleted;
    procedure AddManyObjects_CanBeFound;
    procedure AddTenObjects_CanBeDeleted;

    procedure AddNil_DoesntAdd;
    procedure AddSameKeyTwice_DoesntAddSecondTime;
    procedure DeleteNil_DoesntDelete;
  end;

implementation

{ TRBTreeTests }

procedure TRBTreeTests.SetUp;
begin
  FObjs:=TRBTreeTestDataObjects.Create;
  TestObj:=TRBTree.Create(CompareRBTreeData);
end;

procedure TRBTreeTests.TearDown;
begin
  TestObj.Free;
  FObjs.Free;
end;

procedure TRBTreeTests.CheckFoundObject(AObj : TRBTreeData;
  AFound : TRBNodeP);
begin
  CheckTrue(AFound <> nil, 'found object is not nil');
  CheckEquals(AObj.Key, TRBTreeData(AFound.k).Key, 'wrong Key');
  CheckEquals(AObj.Val, TRBTreeData(AFound.k).Val, 'wrong Val');
end;

procedure TRBTreeTests.CheckFindAllAddedObjects;
var
  i : integer;
  n : TRBNodeP;
begin
  for i:=0 to FObjs.Count - 1 do
  begin
    n:=TestObj.Find(FObjs[i]);
    CheckFoundObject(FObjs[i], n);
  end;
end;

procedure TRBTreeTests.CheckRBTreeStructure;
var
  blacklength, currblacklength : integer;

  procedure CheckStruct(ANode : TRBNodeP);
  begin
    if ANode = nil then //leaf - length of black path to leaf
    begin
      if blacklength = -1 then
        blacklength:=currblacklength
      else
        CheckEquals(blacklength, currblacklength, 'Black path length differs');
      exit;
    end;
    if ANode.color = clRed then //za czerwonym tylko czarne
    begin
      if ANode.left <> nil then CheckTrue(ANode.left.color = clBlack, 'Red node after red');
      if ANode.right <> nil then CheckTrue(ANode.right.color = clBlack, 'Red node after red');
    end;

    if ANode.color = clBlack then inc(currblacklength);
    CheckStruct(ANode.left);
    CheckStruct(ANode.right);
    if ANode.color = clBlack then dec(currblacklength);
  end;
begin
  {
    Red-black tree properties verification:
    - root is black
    - red node can only have black subnodes
    - every path from root to leaf has same number of black nodes
  }
  if TestObj.Root = nil then exit;
  CheckTrue(TestObj.Root.color = clBlack, 'Root is not black');
  blacklength:=-1;
  currblacklength:=1;
  CheckStruct(TestObj.Root);
end;

procedure TRBTreeTests.CreatedEmpty;
begin
  CheckTrue(TestObj.Root = nil);
  CheckTrue(TestObj.First = nil);
  CheckTrue(TestObj.Last = nil);
end;

procedure TRBTreeTests.AddOneObject_HasOneObject;
var
  n : TRBNodeP;
begin
  n:=TestObj.Add(FObjs.CreateDataObject(1, 2));
  CheckTrue(n <> nil, 'Added nil object');
  CheckTrue(TestObj.Root = n);
  CheckTrue(TestObj.First = n);
  CheckTrue(TestObj.Last = n);
  CheckRBTreeStructure;
end;

procedure TRBTreeTests.AddOneObject_CanBeFound;
begin
  TestObj.Add(FObjs.CreateDataObject(1, 2));
  CheckFindAllAddedObjects;
end;

procedure TRBTreeTests.AddOneObject_CanBeDeleted;
var
  n : TRBNodeP;
begin
  n:=TestObj.Add(FObjs.CreateDataObject(1, 2));
  TestObj.Delete(n);
  CheckTrue(TestObj.Root = nil);
  CheckTrue(TestObj.First = nil);
  CheckTrue(TestObj.Last = nil);
  CheckRBTreeStructure;
end;

procedure TRBTreeTests.AddTwoObjects_HasTwoObjects;
var
  n, n2 : TRBNodeP;
begin
  n:=TestObj.Add(FObjs.CreateDataObject(1, 2));
  n2:=TestObj.Add(FObjs.CreateDataObject(2, 20));
  CheckTrue(n <> nil, 'Added nil object');
  CheckTrue(n2 <> nil, 'Added nil object');
  CheckTrue(TestObj.Root <> nil);
  CheckTrue(TestObj.First = n2);
  CheckTrue(TestObj.Last = n);
  CheckRBTreeStructure;
end;

procedure TRBTreeTests.AddTwoObjects_CanBeFound;
begin
  TestObj.Add(FObjs.CreateDataObject(1, 2));
  TestObj.Add(FObjs.CreateDataObject(2, 20));
  CheckFindAllAddedObjects;
end;

procedure TRBTreeTests.AddTwoObjects_CanBeDeleted;
var
  n, n2 : TRBNodeP;
begin
  n:=TestObj.Add(FObjs.CreateDataObject(1, 2));
  n2:=TestObj.Add(FObjs.CreateDataObject(2, 20));
  CheckRBTreeStructure;
  TestObj.Delete(n);
  CheckTrue(TestObj.Root <> nil);
  CheckTrue(TestObj.First = n2);
  CheckTrue(TestObj.Last = n2);
  CheckRBTreeStructure;
  TestObj.Delete(n2);
  CheckTrue(TestObj.Root = nil);
  CheckTrue(TestObj.First = nil);
  CheckTrue(TestObj.Last = nil);
  CheckRBTreeStructure;
end;

procedure TRBTreeTests.AddManyObjects_CanBeFound;
const
  NUM = 10;
  C_KEYS : array[1..NUM] of integer = (1,2,3,4,5,6,7,8,9,10);
  C_VALS : array[1..NUM] of integer = (7,98,6984,6985,2333,97,6548,215,9,10);
var
  i : integer;
begin
  for i:=low(C_KEYS) to high(C_KEYS) do
  begin
    CheckTrue(TestObj.Add(FObjs.CreateDataObject(C_KEYS[i], C_VALS[i])) <> nil, 'Object not added');
    CheckRBTreeStructure;
  end;
  CheckFindAllAddedObjects;
end;

procedure TRBTreeTests.AddNil_DoesntAdd;
begin
  CheckTrue(TestObj.Add(nil) = nil);
end;

procedure TRBTreeTests.AddSameKeyTwice_DoesntAddSecondTime;
begin
  CheckTrue(TestObj.Add(FObjs.CreateDataObject(1, 2)) <> nil);
  CheckTrue(TestObj.Add(FObjs.CreateDataObject(1, 10)) = nil);
end;

procedure TRBTreeTests.DeleteNil_DoesntDelete;
begin
  TestObj.Delete(nil);
end;

procedure TRBTreeTests.AddTenObjects_CanBeDeleted; 
const
  NUM = 10;
  C_KEYS : array[1..NUM] of integer = (1,4,2,5,3,6,7,8,9,10);
  C_VALS : array[1..NUM] of integer = (7,98,6984,6985,2333,97,6548,215,9,10);
  C_DELETE_ORDER : array[1..NUM] of integer = (2,5,10,9,8,3,4,1,6,7);       
  C_CHECK : array[1..NUM] of boolean = (true, true, false, false, false, false, false, false, false, false);
var
  n : array[1..NUM] of TRBNodeP;
  i : integer;
begin
  for i:=low(C_KEYS) to high(C_KEYS) do
    n[i] := TestObj.Add(FObjs.CreateDataObject(C_KEYS[i], C_VALS[i]));
  CheckRBTreeStructure;
  for i:=low(C_KEYS) to high(C_KEYS) do
  begin
    TestObj.Delete(n[C_DELETE_ORDER[i]]);
    if C_CHECK[i] then
    begin
      CheckTrue(TestObj.Root <> nil);
      CheckTrue(TestObj.First = n[10]);
      CheckTrue(TestObj.Last = n[1]);
      CheckRBTreeStructure;
    end;
  end;
  CheckTrue(TestObj.Root = nil);
  CheckTrue(TestObj.First = nil);
  CheckTrue(TestObj.Last = nil);
  CheckRBTreeStructure;
end;

initialization
  TestFramework.RegisterTest('RBTree', TRBTreeTests.Suite);

end.

