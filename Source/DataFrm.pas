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

unit DataFrm;

interface

uses
  SysUtils, Classes, Menus, Controls, SynEditHighlighter, SynHighlighterCpp,
  CodeInsList, SynHighlighterRC, ImgList, CBUtils, SynHighlighterCSS,
  SynHighlighterJScript, SynHighlighterHtml, SynHighlighterXML,
  SynHighlighterBat, SynHighlighterUNIXShellScript, SynHighlighterSQL,
  SynHighlighterAsm, SynHighlighterIni, SynHighlighterInno,
  SynHighlighterDOT, SynHighlighterGeneral, autoLinkList,iniFiles,
  SynHighlighterLua;

type
  PMRUItem = ^TMRUItem;
  TMRUItem = record
    FileName: AnsiString;
    MenuItem: TMenuItem;
    Visible: boolean; // not all items in the list are shown
  end;

  TdmMain = class(TDataModule)
    Cpp: TSynCppSyn;
    ProjectImage_Gnome: TImageList;
    MenuImages_Gnome: TImageList;
    Res: TSynRCSyn;
    MenuImages_NewLook: TImageList;
    ProjectImage_NewLook: TImageList;
    GutterImages: TImageList;
    MenuImages_Blue: TImageList;
    ProjectImage_Blue: TImageList;
    ClassImages: TImageList;
    DotSyn: TSynDOTSyn;
    InnoSyn: TSynInnoSyn;
    IniSyn: TSynIniSyn;
    AsmSyn: TSynAsmSyn;
    SQLSyn: TSynSQLSyn;
    UnixShellSyn: TSynUNIXShellScriptSyn;
    BatSyn: TSynBatSyn;
    XMLSyn: TSynXMLSyn;
    HTMLSyn: TSynHTMLSyn;
    JSSyn: TSynJScriptSyn;
    CssSyn: TSynCssSyn;
    GenSyn: TSynGeneralSyn;
    FileBrowserImages_NewLook: TImageList;
    MenuImages_NewLook28: TImageList;
    MenuImages_NewLook24: TImageList;
    MenuImages_NewLook32: TImageList;
    MenuImages_NewLook48: TImageList;
    LuaSyn: TSynHighlighterLua;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    fUnitCount: integer;
    fProjectCount: integer;
    fCodeList: TCodeInsList;
    fAutoLinks : TAutoLinkList;
    fCodeMenu: TMenuItem;
    fCodePop: TMenuItem;
    fCodeEvent: TNotifyEvent;
    fCodeOffset: byte;
    fSymbolUsage: THashedStringList;
    procedure LoadCodeIns;
  public
    property CodeMenu: TMenuItem read fCodeMenu write fCodeMenu;
    property CodePop: TMenuItem read fCodePop write fCodePop;
    property CodeClick: TNotifyEvent read fCodeEvent write fCodeEvent;
    property CodeInserts: TCodeInsList read fCodeList write fCodeList;
    property AutoLinks: TAutoLinkList read fAutoLinks write fAutoLinks;
    property CodeOffset: byte read fCodeOffset write fCodeOffset;
    property SymbolUsage: THashedStringList read fSymbolUsage;
    { MRU List }
  private
    fMRU: TList; // let them store their own location
    fMRUMenu: TMenuItem;
    fMRUMenuParent: TMenuItem;
    fMRUMenuStartIndex: integer;
    fMRUTopSep: TMenuItem;
    fMRUMiddleSep: TMenuItem;
    fMRUBottomSep: TMenuItem;
    fMRUClick: TNotifyEvent;
    procedure FilterHistory; // remove deleted
    procedure LoadHistory;
    procedure SaveHistory;
    procedure LoadSymbolUsage;
    procedure SaveSymbolUsage;
    procedure SetMRUMenu(value: TMenuItem);
  public
    procedure RebuildMRU;
    procedure AddtoHistory(const s: AnsiString);
    procedure RemoveFromHistory(const s: AnsiString);
    procedure ClearHistory;
    property MRUMenu: TMenuItem read fMRUMenu write SetMRUMenu;
    property MRUClick: TNotifyEvent read fMRUClick write fMRUClick;
    property MRU: TList read fMRU;
  public
    procedure LoadDataMod;
    function GetNewProjectNumber: integer;
    function GetNewFileNumber: integer;
    procedure InitHighlighterFirstTime(const name:string);
    procedure UpdateHighlighter;
    function GetHighlighter(const FileName: AnsiString): TSynCustomHighlighter;
  end;

var
  dmMain: TdmMain;

implementation

uses
  devcfg, utils, version, math, MultiLangSupport;

{$R *.dfm}

{ TdmMain }

procedure TdmMain.DataModuleCreate(Sender: TObject);
begin
  fMRU := TList.Create;
  fCodeList := TCodeInsList.Create;
  fAutoLinks := TAutoLinkList.Create;
  fSymbolUsage := THashedStringList.Create;
  fSymbolUsage.CaseSensitive:=True;
  fSymbolUsage.Duplicates := dupIgnore;
  LoadSymbolUsage;
end;

procedure TdmMain.DataModuleDestroy(Sender: TObject);
var
  I: integer;
begin
  saveSymbolUsage;
  SaveHistory;
  for I := 0 to fMRU.Count - 1 do
    Dispose(PMRUItem(fMRU[i]));
  fMRU.Free;
  fSymbolUsage.Free;
  fCodeList.Free;
  fAutoLinks.Free;
end;

procedure TdmMain.InitHighlighterFirstTime(const name: string);
var
  idx,a: integer;
  fINI: TIniFile;
  Attr: TSynHighlighterAttributes;
  tc: TThemeColor;
  Filename:String;
  
  procedure AddSpecial(AttrName: AnsiString);
  var
    a: integer;
    colorStr:String;
  begin
    colorStr:=fINI.ReadString('Editor.Custom',AttrName,
          devEditor.Syntax.Values[AttrName]);
    a := devEditor.Syntax.IndexofName(AttrName);
    if a = -1 then
      devEditor.Syntax.Append(format('%s=%s', [AttrName, colorStr]))
    else
      devEditor.Syntax.Values[AttrName] := colorStr;
  end;

begin
  Filename:=devDirs.Exec + 'Contributes\syntax\'+name+SYNTAX_EXT;
  fINI := TIniFile.Create(FileName);
  try
    for idx := 0 to pred(Cpp.AttrCount) do begin
      Attr := TSynHighlighterAttributes.Create(Cpp.Attribute[idx].Name);
      try
        StrToAttr(Attr, fINI.ReadString('Editor.Custom', Cpp.Attribute[idx].Name,
          devEditor.Syntax.Values[Cpp.Attribute[idx].Name]));
        Cpp.Attribute[idx].Assign(Attr);
        a := devEditor.Syntax.IndexOfName(cpp.Attribute[idx].Name);
        if a = -1 then
          devEditor.Syntax.Append(format('%s=%s', [cpp.Attribute[idx].Name, AttrtoStr(Attr)]))
        else
          devEditor.Syntax.Values[cpp.Attribute[idx].Name] := AttrtoStr(Attr);
      finally
        Attr.Free;
      end;
    end;

    AddSpecial(cBP);
    AddSpecial(cErr);
    AddSpecial(cABP);
    AddSpecial(cGut);
    AddSpecial(cSel);
    AddSpecial(cFld);
    AddSpecial(cAL);
    AddSpecial(cWN);
    AddSpecial(cPNL);
  finally
    fINI.Free;
  end;
end;

procedure TdmMain.UpdateHighlighter;
var
  Attr: TSynHighlighterAttributes;
  aName: AnsiString;
  a,
    idx: integer;
begin
  for idx := 0 to pred(cpp.AttrCount) do begin
    aName := cpp.Attribute[idx].Name;
    a := devEditor.Syntax.IndexOfName(aName);
    if a <> -1 then begin
      Attr := TSynHighlighterAttributes.Create(aName);
      try
        StrtoAttr(Attr, devEditor.Syntax.Values[aname]);
        cpp.Attribute[idx].Assign(attr);
      finally
        Attr.Free;
      end;
    end;
  end;
  // update res highlighter
  with Res do begin
    CommentAttri.Assign(cpp.CommentAttri);
    DirecAttri.Assign(cpp.DirecAttri);
    IdentifierAttri.Assign(cpp.IdentifierAttri);
    KeyAttri.Assign(cpp.KeyAttri);
    NumberAttri.Assign(cpp.NumberAttri);
    SpaceAttri.Assign(cpp.SpaceAttri);
    StringAttri.Assign(cpp.StringAttri);
    SymbolAttri.Assign(cpp.SymbolAttri);
  end;
  with AsmSyn do begin
    CommentAttri.Assign(cpp.CommentAttri);
    IdentifierAttri.Assign(cpp.IdentifierAttri);
    KeyAttri.Assign(cpp.KeyAttri);
    NumberAttri.Assign(cpp.NumberAttri);
    SpaceAttri.Assign(cpp.SpaceAttri);
    StringAttri.Assign(cpp.StringAttri);
    SymbolAttri.Assign(cpp.SymbolAttri);
  end;
  with BatSyn do begin
    CommentAttri.Assign(cpp.CommentAttri);
    IdentifierAttri.Assign(cpp.IdentifierAttri);
    KeyAttri.Assign(cpp.KeyAttri);
    NumberAttri.Assign(cpp.NumberAttri);
    SpaceAttri.Assign(cpp.SpaceAttri);
    VariableAttri.Assign(cpp.VariableAttri);
  end;
  with CssSyn do begin
    CommentAttri.Assign(cpp.CommentAttri);
    PropertyAttri.Assign(cpp.CharAttri);
    KeyAttri.Assign(cpp.KeyAttri);
    SpaceAttri.Assign(cpp.SpaceAttri);
    StringAttri.Assign(cpp.StringAttri);
    ColorAttri.Assign(cpp.DirecAttri);
    SymbolAttri.Assign(cpp.SymbolAttri);
    NumberAttri.Assign(cpp.NumberAttri);
    TextAttri.Assign(cpp.IdentifierAttri);
    ValueAttri.Assign(cpp.VariableAttri);
    UndefPropertyAttri.Assign(cpp.FunctionAttri);
  end;
  with LuaSyn do begin
    CommentAttri.Assign(cpp.CommentAttri);
    IdentifierAttri.Assign(cpp.IdentifierAttri);
    KeyAttri.Assign(cpp.KeyAttri);
    SpaceAttri.Assign(cpp.SpaceAttri);
    StringAttri.Assign(cpp.StringAttri);
    SymbolAttri.Assign(cpp.SymbolAttri);
    LabelAttri.Assign(cpp.DirecAttri);
    NumberAttri.Assign(cpp.NumberAttri);
    OctalAttri.Assign(cpp.OctalAttri);
    HexAttri.Assign(cpp.HexAttri);
    FloatAttri.Assign(cpp.FloatAttri);
    InvalidAttri.Assign(cpp.InvalidAttri);
  end;
  with DotSyn do begin
    CommentAttri.Assign(cpp.CommentAttri);
    IdentifierAttri.Assign(cpp.IdentifierAttri);
    KeyAttri.Assign(cpp.KeyAttri);
    SpaceAttri.Assign(cpp.SpaceAttri);
    StringAttri.Assign(cpp.StringAttri);
    SymbolAttri.Assign(cpp.SymbolAttri);

    ArrowHeadAttri.Assign(cpp.DirecAttri);
    AttributeAttri.Assign(cpp.VariableAttri);
    DirectionsAttri.Assign(cpp.NumberAttri);
    ShapeAttri.Assign(cpp.FunctionAttri);
    ValueAttri.Assign(cpp.ClassAttri);
  end;
  with HTMLSyn do begin
    CommentAttri.Assign(cpp.CommentAttri);
    IdentifierAttri.Assign(cpp.IdentifierAttri);
    KeyAttri.Assign(cpp.KeyAttri);
    SpaceAttri.Assign(cpp.SpaceAttri);
    SymbolAttri.Assign(cpp.SymbolAttri);

    AndAttri.Assign(cpp.VariableAttri);
    TextAttri.Assign(cpp.StringAttri);
    UndefKeyAttri.Assign(cpp.ClassAttri);
    ValueAttri.Assign(cpp.NumberAttri);
  end;
  with IniSyn do begin
    CommentAttri.Assign(cpp.CommentAttri);
    KeyAttri.Assign(cpp.KeyAttri);
    NumberAttri.Assign(cpp.NumberAttri);
    SpaceAttri.Assign(cpp.SpaceAttri);
    StringAttri.Assign(cpp.StringAttri);
    SymbolAttri.Assign(cpp.SymbolAttri);
    TextAttri.Assign(cpp.VariableAttri);
    SectionAttri.Assign(cpp.ClassAttri);
  end;
  with InnoSyn do begin
    CommentAttri.Assign(cpp.CommentAttri);
    IdentifierAttri.Assign(cpp.IdentifierAttri);
    KeyAttri.Assign(cpp.KeyAttri);
    NumberAttri.Assign(cpp.NumberAttri);
    SpaceAttri.Assign(cpp.SpaceAttri);
    StringAttri.Assign(cpp.StringAttri);
    SymbolAttri.Assign(cpp.SymbolAttri);

    ConstantAttri.Assign(cpp.AsmAttri);
    SectionAttri.Assign(cpp.VariableAttri);
    ParameterAttri.Assign(cpp.ClassAttri);
    InvalidAttri.Assign(cpp.InvalidAttri);
  end;
  with JsSyn do begin
    CommentAttri.Assign(cpp.CommentAttri);
    IdentifierAttri.Assign(cpp.IdentifierAttri);
    KeyAttri.Assign(cpp.KeyAttri);
    NumberAttri.Assign(cpp.NumberAttri);
    SpaceAttri.Assign(cpp.SpaceAttri);
    StringAttri.Assign(cpp.StringAttri);
    SymbolAttri.Assign(cpp.SymbolAttri);

    NonReservedKeyAttri.Assign(cpp.VariableAttri);
    EventAttri.Assign(cpp.ClassAttri);
  end;
  with SQLSyn do begin
    CommentAttri.Assign(cpp.CommentAttri);
    IdentifierAttri.Assign(cpp.IdentifierAttri);
    KeyAttri.Assign(cpp.KeyAttri);
    NumberAttri.Assign(cpp.NumberAttri);
    SpaceAttri.Assign(cpp.SpaceAttri);
    StringAttri.Assign(cpp.StringAttri);
    SymbolAttri.Assign(cpp.SymbolAttri);
    ConditionalCommentAttri.Assign(cpp.OctalAttri);
    DataTypeAttri.Assign(cpp.HexAttri);
    DefaultPackageAttri.Assign(cpp.AsmAttri);
    DelimitedIdentifierAttri.Assign(cpp.FloatAttri);
    ExceptionAttri.Assign(cpp.CharAttri);
    FunctionAttri.Assign(cpp.FunctionAttri);
    PLSQLAttri.Assign(cpp.KeyAttri);
    SQLPlusAttri.Assign(cpp.KeyAttri);
    TableNameAttri.Assign(cpp.ClassAttri);
    VariableAttri.Assign(cpp.VariableAttri);
  end;
  with UnixShellSyn do begin
    CommentAttri.Assign(cpp.CommentAttri);
    IdentifierAttri.Assign(cpp.IdentifierAttri);
    KeyAttri.Assign(cpp.KeyAttri);
    NumberAttri.Assign(cpp.NumberAttri);
    SpaceAttri.Assign(cpp.SpaceAttri);
    StringAttri.Assign(cpp.StringAttri);
    SymbolAttri.Assign(cpp.SymbolAttri);

    SecondKeyAttri.Assign(cpp.ClassAttri);
    VarAttri.Assign(cpp.VariableAttri);
  end;
  with XMLSyn do begin
    CommentAttri.Assign(cpp.CommentAttri);
    SpaceAttri.Assign(cpp.SpaceAttri);
    SymbolAttri.Assign(cpp.SymbolAttri);
    ElementAttri.Assign(cpp.VariableAttri);
    TextAttri.Assign(cpp.StringAttri);
    EntityRefAttri.Assign(cpp.ClassAttri);
    ProcessingInstructionAttri.Assign(cpp.FunctionAttri);
    CDATAAttri.Assign(cpp.DirecAttri);
    DocTypeAttri.Assign(cpp.CharAttri);
    AttributeAttri.Assign(cpp.NumberAttri);
    NamespaceAttributeValueAttri.Assign(cpp.OctalAttri);
    AttributeValueAttri.Assign(cpp.HexAttri);
    NamespaceAttributeAttri.Assign(cpp.CharAttri);
  end;

  with GenSyn do begin
    CommentAttri.Assign(cpp.CommentAttri);
    IdentifierAttri.Assign(cpp.IdentifierAttri);
    KeyAttri.Assign(cpp.KeyAttri);
    NumberAttri.Assign(cpp.NumberAttri);
    PreprocessorAttri.Assign(cpp.DirecAttri);
    SpaceAttri.Assign(cpp.SpaceAttri);
    StringAttri.Assign(cpp.StringAttri);
    SymbolAttri.Assign(cpp.SymbolAttri);
  end;
end;

function TdmMain.GetHighlighter(const FileName: AnsiString): TSynCustomHighlighter;
var
  ext,name: AnsiString;
  idx: integer;
  tmp: TStrings;
begin
  UpdateHighlighter;
  result := GenSyn;
  if devEditor.UseSyntax then begin
    if (FileName = '') or (Pos(Lang[ID_UNTITLED], FileName) = 1) then
      result := Cpp
    else begin
      ext := ExtractFileExt(FileName);
      name := ExtractFileName(FileName);
      if CompareText(ext, RC_EXT) = 0 then
        result := Res
      else if (CompareText(ext, '.html')  = 0)
        or (CompareText(ext, '.htm')  = 0)then
        result := HTMLSyn
      else if (CompareText(ext, '.s')  = 0)
        or (CompareText(ext, '.asm')  = 0)then
        result := AsmSyn
      else if (CompareText(ext, '.js')  = 0) then
        result := JSSyn
      else if CompareText(ext, '.css') = 0 then
        result := CSSSyn
      else if CompareText(ext, '.bat') = 0 then
        result := BatSyn
      else if CompareText(ext, '.asm') = 0 then
        result := AsmSyn
      else if CompareText(ext, '.ini') = 0 then
        result := IniSyn
      else if CompareText(ext, '.nsi') = 0 then
        result := InnoSyn
      else if CompareText(ext, '.dot') = 0 then
        result := DotSyn
      else if CompareText(ext, '.lua') = 0 then
        result := LuaSyn
      else if CompareText(ext, '.sql') = 0 then
        result := SQLSyn
      else if CompareText(ext, '.xml') = 0 then
        result := XMLSyn
      else if (CompareText(ext, '.sh') = 0)
        or StartsText('makefile',name) then
        result := UnixShellSyn
      else begin
        tmp := TStringList.Create;
        try
          delete(ext, 1, 1);
          tmp.Delimiter := ';';
          tmp.DelimitedText := devEditor.SyntaxExt;
          if tmp.Count > 0 then
            for idx := 0 to pred(tmp.Count) do
              if CompareText(Ext, tmp[idx]) = 0 then begin
                result := cpp;
                Exit;
              end;
        finally
          tmp.Free;
        end;
      end;
    end;
  end;
end;

function TdmMain.GetNewFileNumber: integer;
begin
  Inc(fUnitCount);
  result := fUnitCount;
end;

function TdmMain.GetNewProjectNumber: integer;
begin
  Inc(fProjectCount);
  result := fProjectCount;
end;

procedure TdmMain.LoadDataMod;
begin
  LoadHistory;
  LoadCodeIns;
  fAutoLinks.Load;
  UpdateHighlighter;
end;

{ ---------- MRU ---------- }

procedure TdmMain.SetMRUMenu(value: TMenuItem);
begin
  fMRUMenu := value;
  fMRUMenuParent := fMRUMenu.Parent;
  if Assigned(fMRUMenuParent) then begin
    // Assume there are three separators above this item
    fMRUMenuStartIndex := fMRUMenu.MenuIndex - 2;
    fMRUTopSep := fMRUMenuParent[fMRUMenuStartIndex - 1];
    fMRUMiddleSep := fMRUMenuParent[fMRUMenuStartIndex];
    fMRUBottomSep := fMRUMenuParent[fMRUMenuStartIndex + 1];
  end;
end;

procedure TdmMain.AddtoHistory(const s: AnsiString);
var
  I: integer;
  newitem: PMRUItem;
begin
  if (s = '') or not FileExists(s) then
    exit;

  // Don't add duplicates!
  for I := 0 to fMRU.Count - 1 do
    if SameText(s, PMRUItem(fMRU[i])^.filename) then
      Exit;

  newitem := new(PMRUItem);
  newitem^.FileName := s;
  newitem^.MenuItem := nil; // to be filled by RebuildMRU
  newitem^.Visible := false; // idem

  if GetFileTyp(s) = utPrj then begin
    fMRU.Insert(0, newitem); // insert first
  end else begin // find last project
    I := 0;
    while (i < fMRU.Count) and (GetFileTyp(PMRUItem(fMRU[I])^.filename) = utPrj) do
      Inc(I);
    fMRU.Insert(I, newitem); // insert after last project
  end;

  RebuildMRU;
end;

procedure TdmMain.RemoveFromHistory(const s: AnsiString);
var
  I: integer;
begin
  // Remove one, duplicates simply aren't present
  for I := 0 to fMRU.Count - 1 do
    if SameText(s, PMRUItem(fMRU[i])^.filename) then begin

      // remove menu item now
      PMRUItem(fMRU[i])^.MenuItem.Free;

      // delete pointed memory
      Dispose(PMRUItem(fMRU[i]));
      fMRU.Delete(i);

      // Rebuild whole list
      RebuildMRU;
      break;
    end;
end;

procedure TdmMain.ClearHistory;
var
  I: integer;
begin
  for I := 0 to fMRU.Count - 1 do begin

    // remove menu item now
    PMRUItem(fMRU[i])^.MenuItem.Free;

    // delete pointed memory
    Dispose(PMRUItem(fMRU[i]));
  end;
  fMRU.Clear;
  RebuildMRU;
end;

procedure TdmMain.FilterHistory; // remove deleted files
var
  I: integer;
begin
  for I := MRU.Count - 1 downto 0 do
    if not FileExists(PMRUItem(fMRU[i])^.filename) then begin
      Dispose(PMRUItem(fMRU[i]));
      fMRU.Delete(i);
    end;
end;

procedure TdmMain.LoadSymbolUsage;
var
  filename:AnsiString;
  i:integer;
  KeyList:TStringList;
  key,s:ansiString;
begin
  filename := devDirs.Config + DEV_SYMBOLUSAGE_INI;

  if FileExists(filename) then begin // no first time launch? load from disk
    KeyList:=TStringList.Create;
    with TINIFile.Create(filename) do try
      fSymbolUsage.Clear;
      fSymbolUsage.BeginUpdate;
      ReadSection('Usage', KeyList);
      for I := 0 to KeyList.Count - 1 do begin
        key := KeyList[i];
        s := StringReplace(key,'_@','@',[rfReplaceAll]);
        s := StringReplace(s,'_#','#',[rfReplaceAll]);
        fSymbolUsage.AddObject(s,pointer(ReadInteger('Usage',key,0)));
      end;
      fSymbolUsage.EndUpdate;
    finally
      free;
      KeyList.Free;
    end;
  end;
end;

procedure TdmMain.SaveSymbolUsage;
var
  filename:AnsiString;
  i : integer;
  s: String;
begin
  filename := devDirs.Config + DEV_SYMBOLUSAGE_INI;

  DeleteFile(filename);
  if fSymbolUsage.Count = 0 then
    Exit;
  with TINIFile.Create(filename) do try
    for i:=0 to fSymbolUsage.Count-1 do begin
      if fSymbolUsage[i]<>'' then begin
        s:=StringReplace(fSymbolUsage[i],'#','_#',[rfReplaceAll]);
        s:=StringReplace(s,'@','_@',[rfReplaceAll]);
        WriteString('Usage',s,IntToStr(integer(fSymbolUsage.Objects[i])));
      end;
    end;
  finally
    free;
  end;
end;

procedure TdmMain.LoadHistory;
var
  I, J: integer;
  sl: TStringList;
  newitem: PMRUItem;
begin
  sl := TStringList.Create;
  try
    // Use already open file handle
    devData.ReadStrings('History', sl);

    // Delete files that don't exist anymore
    for I := sl.Count - 1 downto 0 do
      if not FileExists(sl.ValueFromIndex[I]) then
        sl.Delete(I);

    // Remove duplicates
    for I := 0 to sl.Count - 1 do begin
      J := I + 1;
      while J < sl.Count do begin
        if (sl.ValueFromIndex[i] = sl.ValueFromIndex[j]) then
          sl.Delete(j)
        else
          Inc(j);
      end;
    end;

    // Create struct list
    for I := 0 to sl.Count - 1 do begin
      newitem := new(PMRUItem);
      newitem^.FileName := sl.ValueFromIndex[i];
      newitem^.MenuItem := nil; // to be filled by RebuildMRU
      newitem^.Visible := false;
      fMRU.Add(newitem);
    end;
  finally
    sl.Free;
  end;

  RebuildMRU; // rebuild once
end;

procedure TdmMain.SaveHistory;
var
  I: integer;
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    // Read struct list
    for I := 0 to fMRU.Count - 1 do
      sl.Add(PMRUItem(fMRU[i])^.filename);

    // Use already open file handle
    devData.WriteStrings('History', sl);
  finally
    sl.Free;
  end;
end;

procedure TdmMain.RebuildMRU;
var
  i, AllCount, ProjCount, FileCount: integer;
  Item: TMenuItem;
begin
  // Delete all menu items
  for I := 0 to fMRU.Count - 1 do begin
    FreeAndNil(PMRUItem(fMRU[i])^.MenuItem);
    PMRUItem(fMRU[i])^.visible := false;
  end;

  // Remove deleted files
  FilterHistory;

  // Add menu items up to MRUmax
  AllCount := 0;
  ProjCount := 0;
  FileCount := 0; // other files

  // First add projects
  for I := 0 to min(devData.MRUMax Div 2, fMRU.Count) - 1 do begin
    if GetFileTyp(PMRUItem(fMRU[I])^.filename) = utPrj then begin

      // Add item to main menu
      Item := TMenuItem.Create(fMRUMenuParent);
      Item.Caption := Format('&%1x %s', [AllCount, ExtractFileName(PMRUItem(fMRU[I])^.filename)]);
      Item.Hint := PMRUItem(fMRU[I])^.filename;
      Item.OnClick := fMRUClick;
      Item.Tag := I;
      fMRUMenuParent.Insert(fMRUMenuStartIndex + AllCount, Item);

      // Hand a pointer to the MRU item, so it can remove it itself
      PMRUItem(fMRU[I])^.MenuItem := Item;
      PMRUItem(fMRU[I])^.Visible := true;

      // Keep count...
      Inc(AllCount);
      Inc(ProjCount);
      if AllCount = devData.MRUMax then
        break;
    end;
  end;

  // Then add other stuff
  if AllCount <> devData.MRUMax then begin
    for I := 0 to min(devData.MRUMax, fMRU.Count) - 1 do begin
      if GetFileTyp(PMRUItem(fMRU[I])^.filename) <> utPrj then begin

        // Add item to main menu
        Item := TMenuItem.Create(fMRUMenuParent);
        //Item.Caption := Format('&%1x %s', [AllCount, PMRUItem(fMRU[I])^.filename]);
        Item.Caption := Format('&%1x %s', [AllCount, ExtractFileName(PMRUItem(fMRU[I])^.filename)]);
        Item.Hint := PMRUItem(fMRU[I])^.filename;
        Item.OnClick := fMRUClick;
        Item.Tag := I;
        fMRUMenuParent.Insert(fMRUMenuStartIndex + AllCount + 1, Item); // add AFTER middle separator

        // Hand a pointer to the MRU item, so it can remove it itself
        PMRUItem(fMRU[I])^.MenuItem := Item;
        PMRUItem(fMRU[I])^.Visible := true;

        // Keep count...
        Inc(AllCount);
        Inc(FileCount);
        if AllCount = devData.MRUMax then
          break;
      end;
    end;
  end;

  // Hide unneeded separators and clear history button
  fMRUTopSep.Visible := (AllCount > 0);
  fMRUMiddleSep.Visible := (FileCount > 0) and (ProjCount > 0);
  fMRUBottomSep.Visible := (AllCount > 0);
  fMRUMenu.Visible := (AllCount > 0);

  // Remove invisible items...
  for I := fMRU.Count - 1 downto 0 do begin
    if not PMRUItem(fMRU[I])^.visible then begin
      Dispose(PMRUItem(fMRU[I]));
      fMRU.Delete(I);
    end;
  end;
end;

{ ---------- Code Insert Methods ---------- }

// Loads code inserts, when sep value changes a separator is
// insert only if sep is a higher value then previous sep value.

procedure TdmMain.LoadCodeIns;
var
  cdx,
    idx: integer;
  Item: TMenuItem;
begin
  if not assigned(fCodeMenu) then
    exit;
  fCodeList.LoadCode;

  for idx := pred(fCodeMenu.Count) downto fCodeOffset do
    fCodeMenu[idx].Free;

  if assigned(fCodePop) then
    fCodePop.Clear;

  cdx := 0;
  for idx := 0 to pred(fCodeList.Count) do begin
    Item := TMenuItem.Create(fCodeMenu);
    Item.Caption := fCodeList[idx]^.Caption;
    Item.OnClick := fCodeEvent;
    Item.Tag := idx;
    if fCodeList[idx]^.Section < 0 then
      Continue;
    if fCodeList[idx]^.Section <= cdx then
      fCodeMenu.Add(Item)
    else begin
      cdx := fCodeList[idx]^.Section;
      fCodeMenu.NewBottomLine;
      fCodeMenu.Add(Item);
    end;
  end;
  fCodeMenu.Visible := fCodeMenu.Count > 0;
  if assigned(fCodePop) then
    CloneMenu(fCodeMenu, fCodePop);
end;

end.

