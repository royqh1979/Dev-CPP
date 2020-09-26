unit U_DCMapString;

interface

uses
  U_DCRBTContainer, U_DCTreeKeyValue, U_DCHashBase;

type
  {
    Map with string key
  }
  TDCMapString = class(TDCRBTStringContainer)
  private
    function FindKeyRaiseIfNotExisting(const AKey : string) : PDCTreeKeyValue;

    function _GetInt(const AKey : string) : integer;
    function _GetString(const AKey : string) : string;
    function _GetObject(const AKey : string) : TObject;
  public
    function Put(const AKey : string; AValue : integer) : boolean; overload;
    function Put(const AKey : string; const AValue : string) : boolean; overload;
    function Put(const AKey : string; const AValue : TObject) : boolean; overload;

    Procedure PutInt(const AKey : string; AValue : integer);
    Procedure PutString(const AKey : string; const AValue : string);
    Procedure PutObject(const AKey : string; const AValue : TObject);


    function GetInt(const AKey : string; defaultValue: integer) : integer;
    function GetString(const AKey : string; defaultValue: string) : string;
    function GetObject(const AKey : string; defaultValue: TObject=nil) : TObject;

    function Get(const AKey : string) : PDCTreeKeyValue;

    procedure Remove(const AKey : string; ARaiseIfNotFound : boolean = false); overload;
    procedure Remove(const AKey : string; var VValue : TObject; ARaiseIfNotFound : boolean = false); overload;

    property Strings[const index: string]: string read _GetString write PutString;
    property Int[const index: string]: integer read _GetInt write PutInt;
    property Objects[const index: string]: TObject read _GetObject write PutObject;
  end;

implementation

uses
  U_DCExceptions, SysUtils;

{ TDCMapString }

function TDCMapString.FindKeyRaiseIfNotExisting(
  const AKey: string): PDCTreeKeyValue;
begin
  result:=Get(AKey);
  if result = nil then
    raise EDCKeyNotFound.Create(AKey);
end;

function TDCMapString.Put(const AKey : string; AValue : integer) : boolean;
begin
  Result:=IntPut(FHash.Hash(AKey),AValue);
end;

function TDCMapString.Put(const AKey : string; const AValue : string) : boolean;
begin
  Result:=IntPut(FHash.Hash(AKey),AValue);
end;

function TDCMapString.Put(const AKey : string; const AValue : TObject) : boolean;
begin
  Result:=IntPut(FHash.Hash(AKey),AValue);
end;


Procedure TDCMapString.PutInt(const AKey : string; AValue : integer);
begin
  if not IntPut(FHash.Hash(AKey),AValue) then
    Exception.Create('Put Value Failed!');
end;

Procedure TDCMapString.PutString(const AKey : string; const AValue : string);
begin
  if not IntPut(FHash.Hash(AKey),AValue) then
    Exception.Create('Put Value Failed!');
end;

Procedure TDCMapString.PutObject(const AKey : string; const AValue : TObject);
begin
  if not IntPut(FHash.Hash(AKey),AValue) then
    Exception.Create('Put Value Failed!');
end;

function TDCMapString._GetInt(const AKey : string) : integer;
var
  ptr:PDCTreeKeyValue;
begin
  ptr := IntGet(FHash.Hash(AKey));
  if Assigned(ptr) then
    Result:= ptr^.Value.AsInteger
  else
    raise EDCKeyNotFound.Create(AKey);
end;

function TDCMapString._GetString(const AKey : string) : string;
var
  ptr:PDCTreeKeyValue;
begin
  ptr := IntGet(FHash.Hash(AKey));
  if Assigned(ptr) then
    Result:= ptr^.Value.AsString
  else
    raise EDCKeyNotFound.Create(AKey)
end;

function TDCMapString._GetObject(const AKey : string) : TObject;
var
  ptr:PDCTreeKeyValue;
begin
  ptr := IntGet(FHash.Hash(AKey));
  if Assigned(ptr) then
    Result:= ptr^.Value.AsObject
  else
    raise EDCKeyNotFound.Create(AKey)
end;

function TDCMapString.GetInt(const AKey : string; DefaultValue:integer) : integer;
var
  ptr:PDCTreeKeyValue;
begin
  ptr := IntGet(FHash.Hash(AKey));
  if Assigned(ptr) then
    Result:= ptr^.Value.AsInteger
  else
    Result:=DefaultValue;
end;

function TDCMapString.GetString(const AKey : string; DefaultValue:string) : string;
var
  ptr:PDCTreeKeyValue;
begin
  ptr := IntGet(FHash.Hash(AKey));
  if Assigned(ptr) then
    Result:= ptr^.Value.AsString
  else
    Result:=DefaultValue;
end;

function TDCMapString.GetObject(const AKey : string;DefaultValue:TObject) : TObject;
var
  ptr:PDCTreeKeyValue;
begin
  ptr := IntGet(FHash.Hash(AKey));
  if Assigned(ptr) then
    Result:= ptr^.Value.AsObject
  else
    Result:=DefaultValue;
end;

function TDCMapString.Get(const AKey : string) : PDCTreeKeyValue;
begin
  result:=IntGet(FHash.Hash(AKey));
end;

procedure TDCMapString.Remove(const AKey : string; ARaiseIfNotFound : boolean);
begin
  if not IntRemove(FHash.Hash(AKey)) then
    if ARaiseIfNotFound then
      raise EDCKeyNotFound.Create(AKey);
end;

procedure TDCMapString.Remove(const AKey: string; var VValue: TObject;
  ARaiseIfNotFound: boolean);
begin
  if not IntRemove(FHash.Hash(AKey), VValue) then
    if ARaiseIfNotFound then
      raise EDCKeyNotFound.Create(AKey);
end;

end.

