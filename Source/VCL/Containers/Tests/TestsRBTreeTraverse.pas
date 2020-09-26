unit TestsRBTreeTraverse;

interface

uses
  TestFrameWork, RBTree, RBTreeTypes,
  U_RBTreeTestTypes, U_RBTreeTestDataObjects;

type
  TRBTreeTraverseTests = class(TTestCase)
  private
    TestObj : TRBTree;
    FObjs : TRBTreeTestDataObjects;

    procedure CreateTestTree;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure ForwardFromFirst;
    procedure BackwardFromLast;

    procedure ForwardFromLast;
    procedure BackwardFromFirst;
  end;

implementation

uses RBTreeTraverse;

const
  NUM = 10;
  C_KEYS_SORTED : array[1..NUM] of integer = (1,2,3,4,5,6,7,8,9,10);
  C_KEYS : array[1..NUM] of integer = (4,2,10,1,7,9,5,8,6,3);
  C_VALS : array[1..NUM] of integer = (7,98,6984,6985,2333,97,6548,215,9,10);

{ TRBTreeTraverseTests }

procedure TRBTreeTraverseTests.SetUp;
begin
  FObjs:=TRBTreeTestDataObjects.Create;
  TestObj:=TRBTree.Create(CompareRBTreeData);
  CreateTestTree;
end;

procedure TRBTreeTraverseTests.TearDown;
begin
  TestObj.Free;
  FObjs.Free;
end;

procedure TRBTreeTraverseTests.CreateTestTree;
var
  i : integer;
begin
  for i:=low(C_KEYS) to high(C_KEYS) do
    TestObj.Add(FObjs.CreateDataObject(C_KEYS[i], C_VALS[i]));
end;

procedure TRBTreeTraverseTests.ForwardFromFirst;
var
  n : TRBNodeP;
  cnt : integer;
begin
  n:=TestObj.First;
  cnt:=1;
  CheckEquals(C_KEYS_SORTED[NUM], TRBTreeData(n.k).Key);
  while n <> TestObj.Last do
  begin
    RBInc(n);
    CheckEquals(C_KEYS_SORTED[NUM - cnt], TRBTreeData(n.k).Key);
    inc(cnt);
  end;
  CheckEquals(NUM, cnt);
end;

procedure TRBTreeTraverseTests.BackwardFromLast;
var
  n : TRBNodeP;
  cnt : integer;
begin
  n:=TestObj.Last;
  cnt:=1;
  CheckEquals(C_KEYS_SORTED[1], TRBTreeData(n.k).Key);
  while n <> TestObj.First do
  begin
    RBDec(n);
    inc(cnt);
    CheckEquals(C_KEYS_SORTED[cnt], TRBTreeData(n.k).Key);
  end;
  CheckEquals(NUM, cnt);
end;

procedure TRBTreeTraverseTests.ForwardFromLast;
var
  n : TRBNodeP;
begin
  n:=TestObj.Last;
  RBInc(n);
  CheckTrue(n = nil);
end;

procedure TRBTreeTraverseTests.BackwardFromFirst;
var
  n : TRBNodeP;
begin
  n:=TestObj.First;
  RBDec(n);
  CheckTrue(n = nil);
end;

initialization
  TestFramework.RegisterTest('RBTree', TRBTreeTraverseTests.Suite);

end.
