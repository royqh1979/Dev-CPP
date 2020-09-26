unit U_DCTreeKeyValue;

interface

uses
  U_DCValue;

type
  PDCTreeKeyValue = ^TDCTreeKeyValue;

  {
    Record stored in RBTree
  }
  TDCTreeKeyValue = record
    Key : integer;    //key
    Value : TDCValue; //object with stored value
  end;

//record creation and destruction
function CreateDCTreeKeyValue(AKey : integer; AValue : TDCValue) : PDCTreeKeyValue;
procedure FreeDCTreeKeyValue(ADCTKVPtr : PDCTreeKeyValue);

//default records comparator based on key values
function CompareDCTreeKeyValue(i1, i2 : Pointer) : Integer;

implementation

function CreateDCTreeKeyValue(AKey : integer; AValue : TDCValue) : PDCTreeKeyValue;
begin
  New(Result);
  Result^.Key:=AKey;
  Result^.Value:=AValue;
end;

procedure FreeDCTreeKeyValue(ADCTKVPtr : PDCTreeKeyValue);
begin
  if ADCTKVPtr^.Value <> nil then
    ADCTKVPtr^.Value.Free;
  Dispose(ADCTKVPtr);
end;

function CompareDCTreeKeyValue(i1, i2 : Pointer) : Integer;
begin
  //subtraction can give overflow on boundary cases
  //so direct comaprision is used
  result:=0;
  if PDCTreeKeyValue(i1)^.Key > PDCTreeKeyValue(i2)^.Key then
    result:=1
  else if PDCTreeKeyValue(i1)^.Key < PDCTreeKeyValue(i2)^.Key then
    result:=-1;
end;

end.
