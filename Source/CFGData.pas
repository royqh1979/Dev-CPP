{
    This file is part of Dev-C++
    Copyright (c) 2004 Bloodshed Software

    Dev-C++ is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    Dev-C++ is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Dev-C++; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

unit CFGData;

interface

uses
  Windows, Classes, sysUtils, TypInfo, IniFiles, Graphics;

type
  TConfigData = class(TPersistent)
  private
    fIni: TIniFile;
    function GetIniFileName: AnsiString;
    procedure SetIniFileName(const s: AnsiString);
  public
    constructor Create;
    destructor Destroy; override;
    procedure SettoDefaults; virtual; abstract;

    procedure ReadSelf;
    procedure SaveSelf;

    procedure EraseSection(const name: AnsiString);

    function ReadI(const key: AnsiString; const entry: AnsiString): integer;
    function ReadB(const key: AnsiString; const entry: AnsiString; const defaultValue: boolean = false): boolean;
    function ReadS(const key: AnsiString; const entry: AnsiString): AnsiString;

    function ReadDefaultI(const key: AnsiString; const entry: AnsiString; default: integer): integer;
    function ReadDefaultB(const key: AnsiString; const entry: AnsiString; default: boolean): boolean;
    function ReadDefaultS(const key: AnsiString; const entry: AnsiString; const default: AnsiString): AnsiString;

    procedure Write(const key: AnsiString; const entry: AnsiString; value: integer); overload;
    procedure Write(const key: AnsiString; const entry: AnsiString; value: boolean); overload;
    procedure Write(const key: AnsiString; const entry: AnsiString; const value: AnsiString); overload;

    procedure ReadObject(const Section: AnsiString; Obj: TPersistent);
    procedure WriteObject(const Section: AnsiString; Obj: TPersistent);

    // Used for reading/writing each item as a separate entry
    procedure ReadStrings(const key: AnsiString; value: TStrings);
    procedure WriteStrings(const key: AnsiString; value: TStrings);

    // Used for reading/writing string lists as a single string entry
    procedure ReadDelimitedString(const key: AnsiString; const entry: AnsiString; value: TStringList);
    procedure WriteDelimitedString(const key: AnsiString; const entry: AnsiString; value: TStringList);

    property INIFileName: AnsiString read GetIniFileName write SetIniFileName;
  end;

function GetPropName(Instance: TPersistent; Index: Integer): AnsiString;
function GetPropCount(Instance: TPersistent): Integer;

implementation

uses utils;
//Returns the number of properties of a given object

function GetPropCount(Instance: TPersistent): Integer;
var
  Data: PTypeData;
begin
  Data := GetTypeData(Instance.Classinfo);
  Result := Data^.PropCount;
end;

//Returns the property name of an instance at a certain index

function GetPropName(Instance: TPersistent; Index: Integer): AnsiString;
var
  PropList: PPropList;
  PropInfo: PPropInfo;
  Data: PTypeData;
begin
  Result := '';
  Data := GetTypeData(Instance.Classinfo);
  GetMem(PropList, Data^.PropCount * Sizeof(PPropInfo));
  try
    GetPropInfos(Instance.ClassInfo, PropList);
    PropInfo := PropList^[Index];
    Result := PropInfo^.Name;
  finally
    FreeMem(PropList, Data^.PropCount * Sizeof(PPropInfo));
  end;
end;

{ TConfigData }

constructor TConfigData.Create;
begin
  inherited Create;
end;

destructor TConfigData.Destroy;
begin
  if Assigned(fIni) then
    fIni.free;
  inherited Destroy;
end;

function TConfigData.GetIniFileName: AnsiString;
begin
  if Assigned(fIni) then
    result := fIni.FileName
  else
    result := '';
end;

procedure TConfigData.SetIniFileName(const s: AnsiString);
begin
  if Assigned(fIni) then
    fini.free;
  fIni := TIniFile.Create(s);
end;

procedure TConfigData.ReadObject(const Section: AnsiString; Obj: TPersistent);
var
  I: integer;
  PropName: AnsiString;
begin
  if not fini.SectionExists(Section) then
    Exit;

  for I := 0 to GetPropCount(Obj) - 1 do begin
    PropName := GetPropName(Obj, I);

    // Ignore properties which aren't listed in the INI (leave defaults)
    if (not fINI.ValueExists(Section, PropName)) and (not fINI.SectionExists(Section + '.' + PropName)) then
      Continue;

    case PropType(Obj, PropName) of
      tkString,
        tkLString,
        tkWString: SetStrProp(Obj, PropName, fINI.ReadString(Section, PropName, ''));

      tkChar,
        tkEnumeration,
        tkInteger: SetOrdProp(Obj, PropName, fINI.ReadInteger(Section, PropName, 1));

      tkInt64: SetInt64Prop(Obj, PropName, StrtoInt(fINI.ReadString(Section, PropName, '0')));

      tkFloat: SetFloatProp(Obj, PropName, StrtoFloat(fINI.ReadString(Section, PropName, '0.0')));

      tkClass: begin
          if TPersistent(GetOrdProp(Obj, PropName)) is TStrings then
            ReadStrings(Section + '.' + PropName, TStrings(GetOrdProp(Obj, PropName)))
          else
            ReadObject(Section + '.' + PropName, TPersistent(GetOrdProp(Obj, PropName)));
        end;
    end;
  end;
end;

procedure TConfigData.WriteObject(const Section: AnsiString; Obj: TPersistent);
var
  I: integer;
  PropName: AnsiString;
begin
  EraseSection(Section);

  for I := 0 to GetPropCount(Obj) - 1 do begin
    PropName := GetPropName(Obj, I);
    case PropType(Obj, PropName) of

      // 11 Jul 2002: mandrav: added double quotes around strings.
      // fixes a bug with stringlists comma-text saved as AnsiString...
      tkString,
        tkLString,
        tkWString: fINI.WriteString(Section, PropName, '"' + GetStrProp(Obj, PropName) + '"');

      tkChar,
        tkEnumeration,
        tkInteger: fINI.WriteInteger(Section, PropName, GetOrdProp(Obj, PropName));

      tkInt64: fINI.WriteString(Section, PropName, InttoStr(GetInt64Prop(Obj, PropName)));

      tkFloat: fINI.WriteString(Section, PropName, FloattoStr(GetFloatProp(Obj, PropName)));

      tkClass: begin
          if TPersistent(GetOrdProp(Obj, PropName)) is TStrings then
            WriteStrings(Section + '.' + PropName, TStrings(GetOrdProp(Obj, PropName)))
          else
            WriteObject(Section + '.' + PropName, TPersistent(GetOrdProp(Obj, PropName)));
        end;
    end;
  end;
end;

procedure TConfigData.ReadSelf;
begin
  if not Assigned(fIni) then
    Exit;

  try
    ReadObject('Options', self);
  except
  end;
end;

procedure TConfigData.SaveSelf;
begin
  if not Assigned(fIni) then
    Exit;

  try
    WriteObject('Options', self);
  except
  end;
end;

procedure TConfigData.ReadStrings(const key: AnsiString; value: TStrings);
begin
  Value.BeginUpdate;
  try
    Value.Clear;
    if fini.SectionExists(key) then
      fini.ReadSectionValues(key, value);
  finally
    Value.EndUpdate;
  end;
end;

procedure TConfigData.WriteStrings(const key: AnsiString; value: TStrings);
var
  I: integer;
begin
  EraseSection(key);
  for I := 0 to Pred(value.Count) do
    if Value.Names[I] = '' then // name/value stuff not found...
      fIni.WriteString(key, IntToStr(I), value[I])
    else
      fIni.WriteString(key, Value.Names[I], value.Values[Value.Names[I]]);
end;

// fix without using StringList.DelimitedText:
// http://stackoverflow.com/questions/1335027/delphi-stringlist-delimiter-is-always-a-space-character-even-if-delimiter-is-se

procedure TConfigData.ReadDelimitedString(const key: AnsiString; const entry: AnsiString; value: TStringList);
var
  S: AnsiString;
begin
  S := ReadS(key, entry);
  //S := StringReplace(S,'";"',';',[rfReplaceAll]);
  // Convert string to string list
  value.Clear;
  if Pos('";"',S)>0 then begin
    if not StartsStr('"',S) then
      S:= '"'+S;
    if not EndsStr('"',S) then
      S:= S + '"';
  end;
  value.Delimiter :=';';
  value.DelimitedText:=S;

  //ExtractStrings([';'], [], PAnsiChar(S), value);
end;

procedure TConfigData.WriteDelimitedString(const key: AnsiString; const entry: AnsiString; value: TStringList);
var
  S: AnsiString;
begin
  // Convert stringlist to string
  value.Delimiter := ';';
  S := value.DelimitedText;
  Write(key, entry, S);
end;

function TConfigData.ReadI(const key, entry: AnsiString): integer;
begin
  result := ReadDefaultI(key, entry, 0);
end;

function TConfigData.ReadB(const key, entry: AnsiString; const defaultValue: boolean): boolean;
begin
  result := ReadDefaultB(key, entry, defaultValue);
end;

function TConfigData.ReadS(const key: AnsiString; const entry: AnsiString): AnsiString;
begin
  result := ReadDefaultS(key, entry, '');
end;

function TConfigData.ReadDefaultI(const key: AnsiString; const entry: AnsiString; default: integer): integer;
begin
  result := fIni.ReadInteger(Key, Entry, default);
end;

function TConfigData.ReadDefaultB(const key: AnsiString; const entry: AnsiString; default: boolean): boolean;
begin
  result := fIni.ReadBool(Key, Entry, default);
end;

function TConfigData.ReadDefaultS(const key: AnsiString; const entry: AnsiString; const default: AnsiString):
  AnsiString;
begin
  result := fIni.ReadString(Key, Entry, default);
end;

procedure TConfigData.Write(const key: AnsiString; const entry: AnsiString; value: integer);
begin
  fIni.WriteInteger(Key, Entry, Value);
end;

procedure TConfigData.Write(const key: AnsiString; const entry: AnsiString; value: boolean);
begin
  fIni.WriteBool(Key, Entry, value);
end;

procedure TConfigData.Write(const key: AnsiString; const entry: AnsiString; const value: AnsiString);
begin
  fIni.WriteString(Key, Entry, Value);
end;

procedure TConfigData.EraseSection(const name: AnsiString);
begin
  if fIni.SectionExists(Name) then
    fIni.EraseSection(Name);
end;

end.

