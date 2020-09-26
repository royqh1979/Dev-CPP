unit U_DCManagerLinkedList;

interface

uses
  Classes, U_DCManagerBase, U_DCTreeKeyValue, U_DCValue;

type
  PManagerListNode = ^TManagerListNode;
  TManagerListNode = Record
    data:PDCTreeKeyValue;
    next:PManagerListNode;
  end;
  {
    RBTree objects manager.
    Internally objects are stored in list.
  }
  TDCManagerLinkedList = class(TDCManagerBase)
  private
    fObjHead: PManagerListNode;
    fCount : integer;
  protected
    function GetItem(AIndex : integer) : PDCTreeKeyValue; override;
    function GetCount : integer; override;
    function CheckIsEmpty : boolean; override;

    function CreateKV(AKey : integer; AValue : TDCValue) : PDCTreeKeyValue;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear; override;

    function CreateObject(AKey, AValue : integer) : PDCTreeKeyValue; override;
    function CreateObject(AKey : integer; const AValue : string) : PDCTreeKeyValue; override;
    function CreateObject(AKey : integer; AValue : TObject) : PDCTreeKeyValue; override;

    procedure DeleteObject(AObjPtr : PDCTreeKeyValue); override;

    function IndexOf(AObjPtr : PDCTreeKeyValue) : integer; override;
  end;

implementation

uses
  U_DCValueInteger, U_DCValueObject, U_DCValueString;

 { TDCManager }

constructor TDCManagerLinkedList.Create;
begin
  new(fObjHead);
  fObjHead^.data := nil;
  fObjHead^.Next := nil;
  fCount := 0;
end;

destructor TDCManagerLinkedList.Destroy;
begin
  Clear;
  dispose(fObjHead);
  inherited;
end;

procedure TDCManagerLinkedList.Clear;
var
  p,q: PManagerListNode;
begin
  fCount := 0;
  p:=fObjHead;
  q:=fObjHead^.Next;
  while q <> nil do begin
    p^.Next:=q^.Next;
    FreeDCTreeKeyValue(q^.Data);
    Dispose(q);
    q:=p^.Next;
  end;
end;

function TDCManagerLinkedList.GetItem(AIndex: integer): PDCTreeKeyValue;
var
  i:integer;
  p:PManagerListNode;
begin
  if (AIndex>fCount-1) then
    Result:=nil
  else begin
    i:=0;
    p:=fObjHead^.next;
    while (i<AIndex) and (p<>nil) do begin
      p:=p^.Next;
    end;
    if (i<>AIndex) or (p =nil) then
      Result:=nil
    else
      Result:=p^.data;
  end;
end;

function TDCManagerLinkedList.GetCount : integer;
begin
  Result:=fCount;
end;

function TDCManagerLinkedList.CheckIsEmpty : boolean;
begin
  Result:=(fCount = 0);
end;

function TDCManagerLinkedList.CreateKV(AKey : integer; AValue : TDCValue) : PDCTreeKeyValue;
var
  p:PManagerListNode;
begin
  new(p);
  Result :=CreateDCTreeKeyValue(AKey, AValue);
  p^.data := Result;
  p^.next:=fObjHead^.next;
  fObjHead^.next:=p;
end;

function TDCManagerLinkedList.CreateObject(AKey, AValue : integer) : PDCTreeKeyValue;
begin
  Result:=CreateKV(AKey, TDCValueInteger.Create(AValue));
end;

function TDCManagerLinkedList.CreateObject(AKey : integer; const AValue : string) : PDCTreeKeyValue;
begin
  Result:=CreateKV(AKey, TDCValueString.Create(AValue));
end;

function TDCManagerLinkedList.CreateObject(AKey : integer; AValue : TObject) : PDCTreeKeyValue;
var
  val : TDCValue;
begin
  val:=nil;
  if AValue <> nil then
    val:=TDCValueObject.Create(AValue);
  Result:=CreateKV(AKey, val);
end;

procedure TDCManagerLinkedList.DeleteObject(AObjPtr : PDCTreeKeyValue);
var
  p,q: PManagerListNode;
begin
  fCount := 0;
  p:=fObjHead;
  q:=fObjHead^.Next;
  while (q <> nil) and (AObjPtr <> q^.data) do begin
    p:=q;
    q:=q^.next;
  end;
  if q = nil then
    Exit;
  FreeDCTreeKeyValue(q^.Data);
  dispose(q);
  p^.Next := q^.Next;
end;

function TDCManagerLinkedList.IndexOf(AObjPtr: PDCTreeKeyValue): integer;
var
  p: PManagerListNode;
  i: integer ;
begin
  Result:=-1;
  fCount := 0;
  p:=fObjHead^.Next;
  i:=0;
  while (p <> nil) and (AObjPtr <> p^.data) do begin
    p:=p^.next;
    inc(i);
  end;
  if p = nil then
    Result:=-1
  else
    Result:=i;
end;

end.

