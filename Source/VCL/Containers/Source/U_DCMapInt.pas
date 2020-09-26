unit U_DCMapInt;

interface

uses
  U_DCRBTContainer, U_DCTreeKeyValue;

type
  {
    Map with integer key.
    Does not use a hashing object - AKey values are used as hashes
  }
  TDCMapInt = class(TDCRBTContainer)
  private
    function FindKeyRaiseIfNotExisting(const AKey : integer) : PDCTreeKeyValue;
    function _GetInt(AKey : integer) : integer;
    function _GetString(AKey : integer) : string;
    function _GetObject(AKey : integer) : TObject;
  public
    function Put(AKey : integer; AValue : integer) : boolean; overload;
    function Put(AKey : integer; const AValue : string) : boolean; overload;
    function Put(AKey : integer; const AValue : TObject) : boolean; overload;

    Procedure PutInt(AKey : integer; AValue : integer);
    Procedure PutString(AKey : integer; const AValue : string);
    Procedure PutObject(AKey : integer; const AValue : TObject);


    function GetInt(AKey : integer; defaultValue: integer) : integer;
    function GetString(AKey : integer; defaultValue: string) : string;
    function GetObject(AKey : integer; defaultValue: TObject=nil) : TObject;

    function Get(AKey : integer) : PDCTreeKeyValue;

    procedure Remove(AKey : integer; ARaiseIfNotFound : boolean = false); overload;
    procedure Remove(AKey : integer; var VValue : TObject; ARaiseIfNotFound : boolean = false); overload;

    property Strings[index: integer]: AnsiString read _GetString write PutString;
    property Int[index: integer]: integer read _GetInt write PutInt;
    property Objects[index: integer]: TObject read _GetObject write PutObject;

  end;

implementation

uses
  U_DCExceptions, SysUtils;

{ TDCMapInt }

function TDCMapInt.FindKeyRaiseIfNotExisting(
  const AKey: integer): PDCTreeKeyValue;
begin
  result:=Get(AKey);
  if result = nil then
    raise EDCKeyNotFound.Create(AKey);
end;

function TDCMapInt.Put(AKey : integer; AValue : integer) : boolean;
begin
  Result:=IntPut(AKey,AValue);
end;

function TDCMapInt.Put(AKey : integer; const AValue : string) : boolean;
begin
  Result:=IntPut(AKey,AValue);
end;

function TDCMapInt.Put(AKey : integer; const AValue : TObject) : boolean;
begin
  Result:=IntPut(AKey,AValue);
end;


Procedure TDCMapInt.PutInt(AKey : integer; AValue : integer);
begin
  if not IntPut(AKey,AValue) then
    Exception.Create('Put Value Failed!');
end;

Procedure TDCMapInt.PutString(AKey : integer; const AValue : string);
begin
  if not IntPut(AKey,AValue) then
    Exception.Create('Put Value Failed!');
end;

Procedure TDCMapInt.PutObject(AKey : integer; const AValue : TObject);
begin
  if not IntPut(AKey,AValue) then
    Exception.Create('Put Value Failed!');
end;

function TDCMapInt._GetInt(AKey : integer) : integer;
var
  ptr:PDCTreeKeyValue;
begin
  ptr := IntGet(AKey);
  if Assigned(ptr) then
    Result:= ptr^.Value.AsInteger
  else
    raise EDCKeyNotFound.Create(AKey);
end;

function TDCMapInt._GetString(AKey : integer) : string;
var
  ptr:PDCTreeKeyValue;
begin
  ptr := IntGet(AKey);
  if Assigned(ptr) then
    Result:= ptr^.Value.AsString
  else
    raise EDCKeyNotFound.Create(AKey)
end;

function TDCMapInt._GetObject(AKey : integer) : TObject;
var
  ptr:PDCTreeKeyValue;
begin
  ptr := IntGet(AKey);
  if Assigned(ptr) then
    Result:= ptr^.Value.AsObject
  else
    raise EDCKeyNotFound.Create(AKey)
end;

function TDCMapInt.GetInt(AKey : integer; DefaultValue:integer) : integer;
var
  ptr:PDCTreeKeyValue;
begin
  ptr := IntGet(AKey);
  if Assigned(ptr) then
    Result:= ptr^.Value.AsInteger
  else
    Result:=DefaultValue;
end;

function TDCMapInt.GetString(AKey : integer; DefaultValue:string) : string;
var
  ptr:PDCTreeKeyValue;
begin
  ptr := IntGet(AKey);
  if Assigned(ptr) then
    Result:= ptr^.Value.AsString
  else
    Result:=DefaultValue;
end;

function TDCMapInt.GetObject(AKey : integer;DefaultValue:TObject) : TObject;
var
  ptr:PDCTreeKeyValue;
begin
  ptr := IntGet(AKey);
  if Assigned(ptr) then
    Result:= ptr^.Value.AsObject
  else
    Result:=DefaultValue;
end;

function TDCMapInt.Get(AKey: integer): PDCTreeKeyValue;
begin
  Result:=IntGet(AKey);
end;

procedure TDCMapInt.Remove(AKey: integer; ARaiseIfNotFound : boolean);
begin
  if not IntRemove(AKey) then
    if ARaiseIfNotFound then
      raise EDCKeyNotFound.Create(AKey);
end;

procedure TDCMapInt.Remove(AKey: integer; var VValue: TObject;
  ARaiseIfNotFound: boolean);
begin
  if not IntRemove(AKey, VValue) then
    if ARaiseIfNotFound then
      raise EDCKeyNotFound.Create(AKey);
end;

end.
