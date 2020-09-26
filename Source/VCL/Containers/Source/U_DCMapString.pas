unit U_DCMapString;

interface

uses
  U_DCRBTContainer, U_DCTreeKeyValue, U_DCHashBase;

type
  {
    Map with string key
  }
  TDCMapString = class(TDCRBTContainer)
  private
    FHash:
    function FindKeyRaiseIfNotExisting(const AKey : string) : PDCTreeKeyValue;
  public

    function Add(const AKey : string; AValue : integer) : boolean; overload;
    function Add(const AKey : string; const AValue : string) : boolean; overload;
    function Add(const AKey : string; AValue : TObject) : boolean; overload;

    procedure Replace(const AKey : string; AValue : integer); overload;
    procedure Replace(const AKey : string; const AValue : string); overload;
    procedure Replace(const AKey : string; AValue : TObject); overload;

    function Find(const AKey : string) : PDCTreeKeyValue;

    procedure Remove(const AKey : string; ARaiseIfNotFound : boolean = false); overload;
    procedure Remove(const AKey : string; var VValue : TObject; ARaiseIfNotFound : boolean = false); overload;
  end;

implementation

uses
  U_DCExceptions;

{ TDCMapString }

function TDCMapString.FindKeyRaiseIfNotExisting(
  const AKey: string): PDCTreeKeyValue;
begin
  result:=Find(AKey);
  if result = nil then
    raise EDCKeyNotFound.Create(AKey);
end;

function TDCMapString.Add(const AKey : string; AValue : integer) : boolean;
begin
  result:=IntAdd(FHash.Hash(AKey), AValue);
end;

function TDCMapString.Add(const AKey : string; const AValue : string) : boolean;
begin
  result:=IntAdd(FHash.Hash(AKey), AValue);
end;

function TDCMapString.Add(const AKey : string; AValue : TObject) : boolean;
begin
  result:=IntAdd(FHash.Hash(AKey), AValue);
end;

procedure TDCMapString.Replace(const AKey : string; AValue : integer);
begin
  IntReplaceValue(FindKeyRaiseIfNotExisting(AKey), AValue);
end;

procedure TDCMapString.Replace(const AKey : string; AValue : TObject);
begin
  IntReplaceValue(FindKeyRaiseIfNotExisting(AKey), AValue);
end;

procedure TDCMapString.Replace(const AKey : string; const AValue : string);
begin
  IntReplaceValue(FindKeyRaiseIfNotExisting(AKey), AValue);
end;

function TDCMapString.Find(const AKey : string) : PDCTreeKeyValue;
begin
  result:=IntFind(FHash.Hash(AKey));
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

