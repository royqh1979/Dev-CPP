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
  public
    function Add(AKey : integer; AValue : integer) : boolean; overload;
    function Add(AKey : integer; const AValue : string) : boolean; overload;
    function Add(AKey : integer; AValue : TObject) : boolean; overload;

    procedure ReplaceValue(AKey : integer; AValue : integer); overload;
    procedure ReplaceValue(AKey : integer; const AValue : string); overload;
    procedure ReplaceValue(AKey : integer; AValue : TObject); overload;

    function Find(AKey : integer) : PDCTreeKeyValue;

    procedure Remove(AKey : integer; ARaiseIfNotFound : boolean = false); overload;
    procedure Remove(AKey : integer; var VValue : TObject; ARaiseIfNotFound : boolean = false); overload;
  end;

implementation

uses
  U_DCExceptions;

{ TDCMapInt }

function TDCMapInt.FindKeyRaiseIfNotExisting(
  const AKey: integer): PDCTreeKeyValue;
begin
  result:=Find(AKey);
  if result = nil then
    raise EDCKeyNotFound.Create(AKey);
end;

function TDCMapInt.Add(AKey, AValue: integer) : boolean;
begin
  result:=IntAdd(AKey, AValue);
end;

function TDCMapInt.Add(AKey: integer; const AValue: string) : boolean;
begin
  result:=IntAdd(AKey, AValue);
end;

function TDCMapInt.Add(AKey: integer; AValue: TObject) : boolean;
begin
 result:=IntAdd(AKey, AValue);
end;

procedure TDCMapInt.ReplaceValue(AKey, AValue: integer);
begin
  IntReplaceValue(FindKeyRaiseIfNotExisting(AKey), AValue);
end;

procedure TDCMapInt.ReplaceValue(AKey: integer; AValue: TObject);
begin
  IntReplaceValue(FindKeyRaiseIfNotExisting(AKey), AValue);
end;

procedure TDCMapInt.ReplaceValue(AKey: integer; const AValue: string);
begin
  IntReplaceValue(FindKeyRaiseIfNotExisting(AKey), AValue);
end;   

function TDCMapInt.Find(AKey: integer): PDCTreeKeyValue;
begin
  result:=IntFind(AKey);
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
