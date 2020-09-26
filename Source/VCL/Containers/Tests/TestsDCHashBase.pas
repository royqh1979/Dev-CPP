unit TestsDCHashBase;

interface

uses
  TestFrameWork, U_DCHashBase;

type
  {
    Base class for testing hash classes
  }
  TDCHashBaseTests = class(TTestCase)
  private
    procedure GenerateRandomIntsTable(ATbl : array of integer);
  protected
    TestObj : TDCHashBase;
    ExpectedHashString, ExpectedHashLongString, ExpectedHashInt : integer;
    procedure TearDown; override;
  published
    procedure HashInt;
    procedure HashString;
    procedure HashLongString;
    procedure HashTheSameValueInt;
    procedure HashTheSameValueString;
    procedure HashEmptyStringTwice;

    procedure HashRandomInts;
    procedure HashRandomStrings;

    procedure HashNonRandomInts;
    procedure HashNonRandomStrings;

    procedure HashSameValuesInts;
    procedure HashSameValuesStrings;
  end;

implementation

uses
  cRandom, SysUtils;

const
  C_STRING_VALUE = 'TestString';
  //from code coverage analysis: jenkins hash requires string of length modulo 12 = 11
  C_LONG_STRING_VALUE = 'aklshdcfnaioywer9pt8yqbv9p837tcpn98auerpg90c7q-34cnp9aerug/>ZX<Lc.ae[0-iryw90485vy90wuer90gu90a';
  C_INTEGER_VALUE = 145;

{ TDCHashBaseTests }

procedure TDCHashBaseTests.TearDown;
begin
  TestObj.Free;
end;

procedure TDCHashBaseTests.HashInt;
begin
  CheckEquals(ExpectedHashInt, TestObj.Hash(C_INTEGER_VALUE));
end;

procedure TDCHashBaseTests.HashString;
begin
  CheckEquals(ExpectedHashString, TestObj.Hash(C_STRING_VALUE));
end;

procedure TDCHashBaseTests.HashLongString;
begin
  CheckEquals(ExpectedHashLongString, TestObj.Hash(C_LONG_STRING_VALUE));
end;

procedure TDCHashBaseTests.HashTheSameValueInt;
begin
  CheckEquals(TestObj.Hash(C_INTEGER_VALUE), TestObj.Hash(C_INTEGER_VALUE));
end;

procedure TDCHashBaseTests.HashTheSameValueString;
begin
  CheckEquals(TestObj.Hash(C_STRING_VALUE), TestObj.Hash(C_STRING_VALUE));
end;

procedure TDCHashBaseTests.HashEmptyStringTwice;
begin
  CheckEquals(TestObj.Hash(''), TestObj.Hash(''));
end;

procedure TDCHashBaseTests.GenerateRandomIntsTable(ATbl: array of integer);
var
  i : integer;
begin
  for i:=low(ATbl) to high(ATbl) do
    ATbl[i]:=RandomInt64;  
end;

procedure TDCHashBaseTests.HashRandomInts;
const
  TBL_LEN = 1000;
  ITERS = 1000;
var
  tbl : array[1..TBL_LEN] of integer;
  res : array[1..TBL_LEN] of integer;
  i,j : integer;
begin
  GenerateRandomIntsTable(tbl);
  for i:=1 to ITERS do
    for j:=1 to TBL_LEN do
      res[j]:=TestObj.Hash(tbl[j]);
end;

procedure TDCHashBaseTests.HashRandomStrings;
const
  TBL_LEN = 100;
  ITERS = 100;
var
  tbl : array[1..TBL_LEN] of integer;
  res : array[1..TBL_LEN] of integer;
  i,j : integer;
begin
  GenerateRandomIntsTable(tbl);
  for i:=1 to ITERS do
    for j:=1 to TBL_LEN do
      res[j]:=TestObj.Hash(inttostr(tbl[j]));
end;

procedure TDCHashBaseTests.HashNonRandomInts;
const
  TBL_LEN = 1000;
  ITERS = 1000;
var
  res : array[1..TBL_LEN] of integer;
  i,j : integer;
begin
  for i:=1 to ITERS do
    for j:=1 to TBL_LEN do
      res[j]:=TestObj.Hash(j);
end;

procedure TDCHashBaseTests.HashNonRandomStrings;
const
  TBL_LEN = 100;
  ITERS = 100;
var
  res : array[1..TBL_LEN] of integer;
  i,j : integer;
begin
  for i:=1 to ITERS do
    for j:=1 to TBL_LEN do
      res[j]:=TestObj.Hash(inttostr(j));
end;

procedure TDCHashBaseTests.HashSameValuesInts;
const
  ITERS = 1000;
var
  i : integer;
begin
  for i:=1 to ITERS do
    CheckEquals(TestObj.Hash(i), TestObj.Hash(i), inttostr(i));
end;

procedure TDCHashBaseTests.HashSameValuesStrings;
const
  ITERS = 1000;
var
  i : integer;
begin
  for i:=1 to ITERS do
    CheckEquals(TestObj.Hash(inttostr(i)), TestObj.Hash(inttostr(i)), inttostr(i));
end;

end.
