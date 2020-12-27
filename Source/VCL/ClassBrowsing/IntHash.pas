unit IntHash;
{Copyright 2020, Roy Qu (royqh1979@gmail.com)

 This program may be used or modified for non-commercial purposes
 so long as this original notice remains in place.
 All other rights are reserved
 }

interface

{
uses
  Classes, SysConst, SysUtils;
}

type
  { TIntHash - used internally by TMemIniFile to optimize searches. }

  PPIntHashItem = ^PIntHashItem;
  PIntHashItem = ^TIntHashItem;
  TIntHashItem = record
    Next: PIntHashItem;
    Key: Integer;
    Value: Integer;
  end;

  TIntHash = class
  private
    Buckets: array of PIntHashItem;
  protected
    function Find(const Key: integer): PPIntHashItem;
    function HashOf(const Key: integer): Cardinal; virtual;
  public
    constructor Create(Size: Cardinal = 256);
    destructor Destroy; override;
    procedure Add(const Key: Integer; Value: Integer);
    procedure Clear;
    procedure Remove(const Key: Integer);
    function Modify(const Key: Integer; Value: Integer): Boolean;
    function ValueOf(const Key: Integer): Integer;
  end;

implementation

{ TIntHash }

procedure TIntHash.Add(const Key: Integer; Value: Integer);
var
  Hash: Integer;
  Bucket: PIntHashItem;
begin
  Hash := HashOf(Key) mod Cardinal(Length(Buckets));
  New(Bucket);
  Bucket^.Key := Key;
  Bucket^.Value := Value;
  Bucket^.Next := Buckets[Hash];
  Buckets[Hash] := Bucket;
end;

procedure TIntHash.Clear;
var
  I: Integer;
  P, N: PIntHashItem;
begin
  for I := 0 to Length(Buckets) - 1 do
  begin
    P := Buckets[I];
    while P <> nil do
    begin
      N := P^.Next;
      Dispose(P);
      P := N;
    end;
    Buckets[I] := nil;
  end;
end;

constructor TIntHash.Create(Size: Cardinal);
begin
  inherited Create;
  SetLength(Buckets, Size);
end;

destructor TIntHash.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TIntHash.Find(const Key: Integer): PPIntHashItem;
var
  Hash: Integer;
begin
  Hash := HashOf(Key) mod Cardinal(Length(Buckets));
  Result := @Buckets[Hash];
  while Result^ <> nil do
  begin
    if Result^.Key = Key then
      Exit
    else
      Result := @Result^.Next;
  end;
end;

function TIntHash.HashOf(const Key: Integer): Cardinal;
var
  I: Integer;
begin
  Result := Key;
end;

function TIntHash.Modify(const Key: Integer; Value: Integer): Boolean;
var
  P: PIntHashItem;
begin
  P := Find(Key)^;
  if P <> nil then
  begin
    Result := True;
    P^.Value := Value;
  end
  else
    Result := False;
end;

procedure TIntHash.Remove(const Key: Integer);
var
  P: PIntHashItem;
  Prev: PPIntHashItem;
begin
  Prev := Find(Key);
  P := Prev^;
  if P <> nil then
  begin
    Prev^ := P^.Next;
    Dispose(P);
  end;
end;

function TIntHash.ValueOf(const Key: Integer): Integer;
var
  P: PIntHashItem;
begin
  P := Find(Key)^;
  if P <> nil then
    Result := P^.Value
  else
    Result := -1;
end;


end.

