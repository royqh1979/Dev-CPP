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

unit ClassBrowser;

interface

uses
  Windows, Classes, SysUtils, StatementList, Controls, ComCtrls, Graphics,
  CppParser, Forms, cbutils, Messages, SyncObjs, VirtualTrees, iniFiles;

type

  TMemberSelectEvent = procedure(Sender: TObject; Filename: TFilename; Line: integer) of object;

  TImagesRecord = class(TPersistent)
  private
    fGlobalsImg: integer;
    fClassesImg: integer;
    fVariablePrivateImg: integer;
    fVariableProtectedImg: integer;
    fVariablePublicImg: integer;
    fMethodPrivateImg: integer;
    fMethodProtectedImg: integer;
    fMethodPublicImg: integer;
    fInhMethodProtectedImg: integer;
    fInhMethodPublicImg: integer;
    fInhVariableProtectedImg: integer;
    fInhVariablePublicImg: integer;
    fDefineImg: integer;
    fEnumImg: integer;
    fGlobalVarImg: integer;
    fStaticVarImg: integer;
    fGlobalFuncImg: integer;
    fStaticFuncImg: integer;
    fTypeImg: integer;
    fNamespaceImg: integer;
  published
    property Globals: integer read fGlobalsImg write fGlobalsImg;
    property Classes: integer read fClassesImg write fClassesImg;
    property VariablePrivate: integer read fVariablePrivateImg write fVariablePrivateImg;
    property VariableProtected: integer read fVariableProtectedImg write fVariableProtectedImg;
    property VariablePublic: integer read fVariablePublicImg write fVariablePublicImg;
    property MethodPrivate: integer read fMethodPrivateImg write fMethodPrivateImg;
    property MethodProtected: integer read fMethodProtectedImg write fMethodProtectedImg;
    property MethodPublic: integer read fMethodPublicImg write fMethodPublicImg;
    property InheritedMethodProtected: integer read fInhMethodProtectedImg write fInhMethodProtectedImg;
    property InheritedMethodPublic: integer read fInhMethodPublicImg write fInhMethodPublicImg;
    property InheritedVariableProtected: integer read fInhVariableProtectedImg write fInhVariableProtectedImg;
    property InheritedVariablePublic: integer read fInhVariablePublicImg write fInhVariablePublicImg;
    property DefineImg: integer read fDefineImg write fDefineImg;
    property EnumImg: integer read fEnumImg write fEnumImg;
    property GlobalVarImg: integer read fGlobalVarImg write fGlobalVarImg;
    property StaticVarImg: integer read fStaticVarImg write fStaticVarImg;
    property GlobalFuncImg: integer read fGlobalFuncImg write fGlobalFuncImg;
    property StaticFuncImg: integer read fStaticFuncImg write fStaticFuncImg;
    property TypeImg: integer read fTypeImg write fTypeImg;
    property NamespaceImg: integer read fNamespaceImg write fNamespaceImg;
  end;

  TClassBrowser = class(TCustomVirtualStringTree)
  private
    fParser: TCppParser;
    fCriticalSection: TCriticalSection;
    fParserSerialId: string;
    fOnSelect: TMemberSelectEvent;
    fImagesRecord: TImagesRecord;
    fCurrentFile: AnsiString;
    fControlCanvas: TControlCanvas;
    fShowInheritedMembers: boolean;
    fIncludedFiles: TStringList;
    fIsIncludedCacheFileName: AnsiString;
    fIsIncludedCacheResult: boolean;
    fUpdateCount: integer;
    fTabVisible: boolean;
    fLastSelection: AnsiString;
    fSortAlphabetically: boolean;
    fSortByType: boolean;
    fStatementsType: TClassBrowserStatementsType;
    fOnUpdated: TNotifyEvent;
    fUpdating: boolean;
    fColors : array[0..14] of TColor;
    fRootStatements : TList;
    fDummyStatements: THashedStringList;
    procedure SetParser(Value: TCppParser);
    procedure AddMembers;
    procedure FilterChildren(parentStatement:PStatement; children:TList; var filtered:TList);
    {
    procedure AdvancedCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
      State: TCustomDrawState; Stage: TCustomDrawStage; var PaintImages,
      DefaultDraw: Boolean);
    }
    procedure OnNodeChange(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure OnNodeChanging(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    {
    procedure OnParserUpdate(Sender: TObject);
    procedure OnParserBusy(Sender: TObject);
    }
    function CalcImageIndex(Statement: PStatement):integer;
    procedure SetCurrentFile(const Value: AnsiString);
    procedure SetShowInheritedMembers(Value: boolean);
    procedure SetSortAlphabetically(Value: boolean);
    procedure SetSortByType(Value: boolean);
    procedure SetStatementsType(Value: TClassBrowserStatementsType);
    procedure SetTabVisible(Value: boolean);
    procedure ReSelect;
    //procedure DoSort(lock:boolean=False);
    function GetColor(i:integer):TColor;
    procedure SetColor(i:integer; const Color:TColor);
    procedure OnCBInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
    var InitialStates: TVirtualNodeInitStates);
    procedure OnCBFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);    
    procedure OnCBGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: UnicodeString);
    procedure OnCBGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure OnCBDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; const Text: UnicodeString; const CellRect: TRect; var DefaultDraw: Boolean);
    {
    procedure OnCompareByType(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex;
    var Result: Integer);
    procedure OnCompareByAlpha(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex;
    var Result: Integer);
    }
    procedure ClearDummyStatements;
    function GetSelectedFile: String;
    function GetSelectedLine: integer;
    function GetSelectedDefFile: String;
    function GetSelectedDefLine: integer;
    function GetSelectedKind: TStatementKind;
    function GetSelectedCommand: String;
    function GetSelected: PVirtualNode;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateView;
    procedure ClearTree;
    procedure BeginTreeUpdate;
    procedure EndTreeUpdate;
    property OnUpdated: TNotifyEvent read fOnUpdated write fOnUpdated;
    property TreeColors[Index: Integer]: TColor read GetColor write SetColor;
    property SelectedLine : integer read GetSelectedLine;
    property SelectedFile : String read GetSelectedFile;
    property SelectedDefLine : integer read GetSelectedDefLine;
    property SelectedDefFile : String read GetSelectedDefFile;
    property SelectedCommand : String read GetSelectedCommand;
    property SelectedKind : TStatementKind read GetSelectedKind;
    property Colors;
  published
    property Align;
    property Font;
    property Images;
    property Indent;
    property TabOrder;
    property PopupMenu;
    property BorderStyle;
    property Header;
    property OnSelect: TMemberSelectEvent read fOnSelect write fOnSelect;
    property Parser: TCppParser read fParser write SetParser;
    property ItemImages: TImagesRecord read fImagesRecord write fImagesRecord;
    property CurrentFile: AnsiString read fCurrentFile write SetCurrentFile;
    property ShowInheritedMembers: boolean read fShowInheritedMembers write SetShowInheritedMembers;
    property TabVisible: boolean read fTabVisible write SetTabVisible;
    property SortAlphabetically: boolean read fSortAlphabetically write SetSortAlphabetically;
    property SortByType: boolean read fSortByType write SetSortByType;
    property ParserSerialId: string read fParserSerialId;
    property StatementsType: TClassBrowserStatementsType read fStatementsType write SetStatementsType;
  end;

const
  CLASS_FOLDERS_MAGIC = 'DEVCF_1_0';
  TV_FIRST = $1100;
  TVM_SETEXTENDEDSTYLE = TV_FIRST + 44;
  TVM_GETEXTENDEDSTYLE = TV_FIRST + 45;
  TVS_EX_DOUBLEBUFFER = $4;

procedure Register;

implementation

type
  PNodeData = ^TNodeData;
  TNodeData = record
    Level:integer;
    Text: String;
    ImageIndex:integer;
    statement:PStatement;
    ChildrenStatements: TList;
  end;

procedure Register;
begin
  RegisterComponents('Dev-C++', [TClassBrowser]);
end;


function CompareByType(stat1:PStatement; stat2:PStatement):integer;
begin
  if stat1^._Static and (not stat2^._Static) then
    Result:=-1
  else if (not stat1^._Static) and stat2^._Static then
    Result:=1
  else
    if Ord(stat1^._ClassScope) <> Ord(stat2^._ClassScope) then
      Result := Ord(stat1^._ClassScope) - Ord(stat2^._ClassScope)
    else
      Result := Ord(stat1^._Kind) - Ord(stat2^._Kind);
end;

function CompareByAlpha(stat1:PStatement; stat2:PStatement):integer;
begin
  Result := StrIComp(PAnsiChar(stat1^._Command), PAnsiChar(stat2^._Command));
end;

function CompareByAlphaAndType(stat1:PStatement; stat2:PStatement):integer;
begin
  Result:=CompareByType(stat1,stat2);
  if Result = 0 then
    Result := CompareByAlpha(stat1,stat2);
end;

{ TClassBrowser }

constructor TClassBrowser.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCriticalSection := TCriticalSection.Create;
  OnMouseUp := OnNodeChange;
  OnMouseDown := OnNodeChanging;
  DragMode := dmManual;
  fImagesRecord := TImagesRecord.Create;
  fCurrentFile := '';
  ShowHint := True;
  NodeDataSize := sizeof(TNodeData);
  fRootStatements := TList.Create;
  //HideSelection := False;
  //RightClickSelect := True;
  fShowInheritedMembers := False;
  fControlCanvas := TControlCanvas.Create;
  fControlCanvas.Control := Self;
  //fControlCanvas.Font.Assign(Self.Font);
  fIncludedFiles := TStringList.Create;
  fIsIncludedCacheFileName := '';
  fIsIncludedCacheResult := false;
  fUpdateCount := 0;
  fTabVisible := false;
  //RowSelect := true;
  //ShowLines := False;
  fSortAlphabetically:= True;
  fSortByType:=True ;
  fStatementsType := cbstFile;
  fOnUpdated:=nil;
  fUpdating:=False;
  fDummyStatements := THashedStringList.Create;
  TVirtualTreeOptions(TreeOptions).PaintOptions :=
    TVirtualTreeOptions(TreeOptions).PaintOptions - [toShowTreeLines];
  TVirtualTreeOptions(TreeOptions).SelectionOptions :=
    TVirtualTreeOptions(TreeOptions).SelectionOptions + [toFullRowSelect];
  TVirtualTreeOptions(TreeOptions).AutoOptions :=
    TVirtualTreeOptions(TreeOptions).AutoOptions - [toAutoSort, toAutoScrollOnExpand];
  self.OnInitNode := OnCBInitNode;
  self.OnGetText := OnCBGetText;
  self.OnFreeNode := OnCBFreeNode;
  self.OnGetImageIndex := OnCBGetImageIndex;
  self.OnDrawText := OnCBDrawText;
end;

destructor TClassBrowser.Destroy;
begin
  while True do begin
    fCriticalSection.Acquire;
    try
      if not fUpdating then begin
        fUpdating:=True;
        break;
      end;
    finally
      fCriticalSection.Release;
    end;
    Sleep(50);
    Application.ProcessMessages;
  end;
  ClearDummyStatements;
  FreeAndNil(fDummyStatements);
  FreeAndNil(fImagesRecord);
  FreeAndNil(fControlCanvas);
  fIncludedFiles.Free;
  fCriticalSection.Free;
  FreeAndNil(fRootStatements);
  inherited Destroy;
end;

procedure TClassBrowser.BeginTreeUpdate;
begin
  Inc(fUpdateCount);
end;

procedure TClassBrowser.EndTreeUpdate;
begin
  Dec(fUpdateCount);
  if fUpdateCount = 0 then
    UpdateView;
end;

function TClassBrowser.CalcImageIndex(Statement: PStatement):integer;
var
  bInherited: boolean;
begin
  Result := -1;
  bInherited := statement^._Inherited;
  case Statement^._Kind of
    skNamespace: begin
        Result := fImagesRecord.NamespaceImg;
      end;
    skClass: begin
        Result := fImagesRecord.Classes;
      end;
    skPreprocessor: begin
        Result := fImagesRecord.DefineImg;
      end;
    skEnum: begin
        case Statement^._ClassScope of
        scsPrivate: Result := fImagesRecord.VariablePrivate;
        scsProtected: if not bInherited then
            Result := fImagesRecord.VariableProtected
          else
            Result := fImagesRecord.InheritedVariableProtected;
        scsPublic: if not bInherited then
            Result := fImagesRecord.VariablePublic
          else
            Result := fImagesRecord.InheritedVariablePublic;
        scsNone: Result := fImagesRecord.EnumImg;
      end;
    end;
    skTypedef: begin
        Result := fImagesRecord.TypeImg;
      end;
    skVariable,skParameter: case Statement^._ClassScope of
        scsPrivate: Result := fImagesRecord.VariablePrivate;
        scsProtected: if not bInherited then
            Result := fImagesRecord.VariableProtected
          else
            Result := fImagesRecord.InheritedVariableProtected;
        scsPublic: if not bInherited then
            Result := fImagesRecord.VariablePublic
          else
            Result := fImagesRecord.InheritedVariablePublic;
        scsNone: begin
          if Statement^._Static then
            Result := fImagesRecord.StaticVarImg
          else
            Result := fImagesRecord.GlobalVarImg;
        end;
      end;
    skFunction, skConstructor, skDestructor: case Statement^._ClassScope of

        scsPrivate: Result := fImagesRecord.MethodPrivate;
        scsProtected: if not bInherited then
            Result := fImagesRecord.MethodProtected
          else
            Result := fImagesRecord.InheritedMethodProtected;
        scsPublic: if not bInherited then
            Result := fImagesRecord.MethodPublic
          else
            Result := fImagesRecord.InheritedMethodPublic;
        scsNone: begin
          if Statement^._Static then
            Result := fImagesRecord.StaticFuncImg
          else
            Result := fImagesRecord.GlobalFuncImg;
        end;
      end;
  end;
  {
  Node.SelectedIndex := Node.ImageIndex;
  Node.StateIndex := Node.ImageIndex;
  }
end;

procedure TClassBrowser.FilterChildren(parentStatement:PStatement; children:TList; var filtered:TList);
var
  Statement,DummyParent,Dummy: PStatement;
  i,j,idx:integer;
  function CreateDummy(templateStatement:PStatement):PStatement;
  begin
    Result := new(PStatement);
    Result^._ParentScope := templateStatement^._ParentScope;
    Result^._Command := templateStatement^._Command;
    Result^._Args := templateStatement^._Args;
    Result^._NoNameArgs := templateStatement^._NoNameArgs;
    Result^._FullName := templateStatement^._FullName;
    Result^._Kind := templateStatement^._Kind;
    Result^._Type := templateStatement^._Type;
    Result^._Value := templateStatement^._Value;
    Result^._Scope := templateStatement^._Scope;
    Result^._ClassScope := templateStatement^._ClassScope;
    Result^._InProject := templateStatement^._InProject;
    Result^._InSystemHeader := templateStatement^._InSystemHeader;
    Result^._Static := templateStatement^._Static;
    Result^._Inherited := templateStatement^._Inherited;
    Result^._FileName := fCurrentFile;
    Result^._Line := 0;
    Result^._DefinitionFileName := fCurrentFile;
    Result^._DefinitionLine := 0;
    Result^._Children := TList.Create;
    fDummyStatements.AddObject(Result^._FullName,TObject(Result));
  end;
begin
  if not assigned(children) then
    Exit;
  for i:=0 to Children.Count-1 do begin
    Statement := Children[i];
    if not Assigned(Statement) then
      Continue;
    with Statement^ do begin
      // Do not print statements marked invisible for the class browser

      if _Kind = skBlock then
        Continue;

      if _Inherited and not fShowInheritedMembers then // don't show inherited members
        Continue;

      if Statement = ParentStatement then // prevent infinite recursion
        Continue;

      if _Scope = ssLocal then
        Continue;

      if (fStatementsType = cbstProject) then begin
        if not Statement^._InProject then
          Continue;
        if Statement^._Static and not SameText(Statement^._FileName,fCurrentFile)
          and not SameText(Statement^._FileName,fCurrentFile) then
          Continue;
      end;

      // we only test and handle orphan statements in the top level (parentstatment is nil)
      if (Statement^._ParentScope <> ParentStatement)
        and (not Assigned(ParentStatement)) then begin

        // we only handle orphan statements when type is cbstFile
        if fStatementsType <> cbstFile then
          Continue;

        //should not happend, just in case of error
        if not Assigned(Statement^._ParentScope) then
          Continue;

        // Processing the orphan statement
        while assigned(Statement) do begin
          //the statement's parent is in this file, so it's not a real orphan
          if SameText(Statement^._ParentScope^._FileName,fCurrentFile)
            or SameText(Statement^._ParentScope^._DefinitionFileName,fCurrentFile) then
              break;

          idx := fDummyStatements.IndexOf(Statement^._ParentScope^._FullName);
          if idx <> -1 then begin
            PStatement(fDummyStatements.Objects[idx])^._Children.Add(Statement);
            break;
          end;
          DummyParent := CreateDummy(Statement^._ParentScope);
          DummyParent^._Children.Add(Statement);
        //we are adding an orphan statement, just add it
          statement := DummyParent;
          if not Assigned(statement^._ParentScope) then begin
            filtered.Add(Statement);
            break;
          end;
        end;
        Continue;
      end;
      if (Statement^._Kind = skNamespace) then begin
        idx := fDummyStatements.IndexOf(Statement^._FullName);
        if idx <> -1 then begin
          if assigned(statement^._Children) then begin
            for j:=0 to statement^._Children.Count-1 do begin
              PStatement(fDummyStatements.Objects[idx])^._Children.Add(statement^._Children[j]);
            end;
          end;
          continue;
        end;
        Dummy := CreateDummy(Statement);
        if assigned(statement^._Children) then
          Dummy._Children.Assign(statement^._Children);
        filtered.Add(Dummy);
      end else
        filtered.Add(Statement);
    end;
  end;
  if sortAlphabetically and sortByType then begin
    filtered.Sort(@CompareByAlphaAndType);
  end else if sortAlphabetically then begin
    filtered.Sort(@CompareByAlpha);
  end else if sortByType then begin
    filtered.Sort(@CompareByType);
  end;
end;

procedure TClassBrowser.AddMembers;
var
  ParentStatement: PStatement;
  Children: TList;
  P:PFileIncludes;
  {
  procedure AddStatement(Statement: PStatement);
  begin
    if (node=nil) and Assigned(statement^._ParentScope) then begin
      NewNode := Items.AddChildObject(Node, Statement^._FullName, Statement);
    end else begin
      NewNode := Items.AddChildObject(Node, Statement^._Command, Statement);
    end;
    SetNodeImages(NewNode, Statement);
    if Statement^._Kind in [skClass,skNamespace] then
      AddMembers(NewNode, Statement);
  end;
  }
begin
  ParentStatement := nil;
  if fStatementsType = cbstFile then begin
    p:=fParser.FindFileIncludes(fCurrentFile);
    if not Assigned(p) then
      Exit;
    Children := p^.Statements;
  end else
    Children := fParser.Statements.GetChildrenStatements(nil);

//  fParser.Statements.DumpWithScope('f:\browser.txt');
  filterChildren(nil,Children,fRootStatements);
  self.RootNodeCount := fRootStatements.Count;
end;

procedure TClassBrowser.UpdateView;
begin
  fCriticalSection.Acquire;
  try
  if fUpdateCount <> 0 then
    Exit;

  if not Assigned(fParser) then
    Exit;
  if not fParser.Enabled then
    Exit;
  fUpdating:=True;
  try
    ClearTree;
    if not Visible or not TabVisible then
      Exit;
    // We are busy...
    if not fParser.Freeze then
      Exit;
    try
      fParserSerialId := fParser.SerialId;
      if fCurrentFile <> '' then begin
        // Update file includes, reset cache
        fParser.GetFileIncludes(fCurrentFile, fIncludedFiles);
        fIsIncludedCacheFileName := '';
        fIsIncludedCacheResult := false;

        // Add everything recursively
        AddMembers;
        //DoSort;

      // Remember selection
        if fLastSelection <> '' then
          ReSelect;
      end;
    finally
      fParser.Unfreeze;
    end;
  finally
    fUpdating:=False;
  end;
  if Assigned(fOnUpdated) then
    fOnUpdated(Self);
  finally
    fCriticalSection.Release;
  end;
end;

procedure TClassBrowser.OnCBInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
    var InitialStates: TVirtualNodeInitStates);
var
  level :integer;
  data,parentData :PNodeData;
  children,filteredList : TList;
  parentStatement : PStatement;
begin
  fCriticalSection.Acquire;
  try
    if not assigned(fParser) then
      Exit;
    if not fParser.Enabled then
      Exit;
    if not fParser.Freeze(fParserSerialId) then
      Exit;

    try
      parentStatement:= nil;
      level := sender.GetNodeLevel(node);
      data := sender.GetNodeData(node);
      data^.Level := level;
      data^.ImageIndex := 0;
      data^.Statement := nil;
      data^.Text := '';
      if level = 0 then begin
        data^.Statement := fRootStatements[node.Index];
      end else begin
        parentData := sender.GetNodeData(ParentNode);
        if assigned(parentData) then
          parentStatement := parentData^.statement;
        if assigned(parentData) and assigned(parentData^.ChildrenStatements) then
          data^.Statement := parentData^.ChildrenStatements[node.Index];
      end;
      data^.ChildrenStatements:=nil;
      if assigned(data^.statement) then begin
        data^.ImageIndex := CalcImageIndex(data^.statement);
        if (level=0) and (data^.statement^._ParentScope <> parentStatement) then
          data^.Text := data^.statement^._FullName + data^.statement^._Args
        else
          data^.Text := data^.statement^._Command + data^.statement^._Args;
        Children := data^.Statement^._Children;
        if assigned(children) then begin
          filteredList:=TList.Create;
          filterChildren(data^.Statement,Children, filteredList);
          if filteredList.Count > 0 then begin
            data^.ChildrenStatements := filteredList;
            self.ChildCount[node] := data^.ChildrenStatements.Count;
          end else
            filteredList.Free;
        end;
      end;
    finally
      fParser.UnFreeze;
    end;
  finally
    fCriticalSection.Release;
  end;
end;

procedure TClassBrowser.OnCBFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  data:PNodeData;
begin
  data := sender.GetNodeData(node);
  if assigned(data^.ChildrenStatements) then begin
    data^.ChildrenStatements.Free;
  end;
end;

procedure TClassBrowser.OnCBGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  data :PNodeData;
begin
  fCriticalSection.Acquire;
  try
    data := sender.GetNodeData(node);
    ImageIndex := data^.ImageIndex;
  finally
    fCriticalSection.Release;
  end;
end;

procedure TClassBrowser.OnCBGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: UnicodeString);
var
  data :PNodeData;
begin
  fCriticalSection.Acquire;
  try
    data := sender.GetNodeData(node);
    CellText := data^.Text;
  finally
    fCriticalSection.Release;
  end;
end;

procedure TClassBrowser.OnNodeChanging(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Node: PVirtualNode;
  hitInfo:THitInfo;
begin
  inherited;
  fCriticalSection.Acquire;
  try
    if fUpdating then
      Exit;
    GetHitTestInfoAt(X, Y, true, hitInfo);
    Node := hitInfo.HitNode;
    if not Assigned(node) then begin
      self.ClearSelection;
    end else begin
      if not (hiOnItemButton in HitInfo.HitPositions)
        or not (vsHasChildren in HitInfo.HitNode.States) then
        self.SelectNodes(node,node,False);
    end;
    self.Invalidate;
  finally
    fCriticalSection.Release;
  end;
end;

procedure TClassBrowser.OnNodeChange(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Node: PVirtualNode;
  hitInfo:THitInfo;
  Data:PNodeData;
begin
  inherited;

  fCriticalSection.Acquire;
  try
    if not assigned(fParser) then
      Exit;
    if not (fParser.Enabled) then
      Exit;
    if not fParser.Freeze then
      Exit;
    try
      if not samestr(fParser.SerialId, fParserSerialId) then
        Exit;
      GetHitTestInfoAt(X, Y, true, hitInfo);
      Node := hitInfo.HitNode;

      // Did we click on anything?
      if not Assigned(Node) then begin
        fLastSelection := '';
        Exit;
      end;

      // Click on the expand lable
      if (hiOnItemButton in HitInfo.HitPositions)
        and (vsHasChildren in HitInfo.HitNode.States) then begin
        fLastSelection := '';
        Exit;
      end;

      Data := GetNodeData(Node);
      if not Assigned(Data) or not Assigned(Data^.statement) then begin
        fLastSelection := '';
        Exit;
      end;

      // Send to listener
      with Data^.statement^ do begin
        fLastSelection := _Type + ':' + _Command + ':' + _Args;
        if Assigned(fOnSelect) then
          if Button = mbLeft then begin// need definition
            //we navigate to the same file first
            if SameText(_DefinitionFileName,fCurrentFile) then begin
              fOnSelect(Self, _DefinitionFileName, _DefinitionLine)
            end else if SameText(_FileName,fCurrentFile) then begin
              fOnSelect(Self, _FileName, _Line)
            end else begin
              fOnSelect(Self, _DefinitionFileName, _DefinitionLine)
            end;
          end else if Button = mbMiddle then // need declaration
            fOnSelect(Self, _FileName, _Line);
        end;
      finally
        fParser.UnFreeze;
      end;
  finally
    fCriticalSection.Release;
  end;
end;

{
procedure TClassBrowser.OnParserBusy(Sender: TObject);
begin
  Items.Clear;
  BeginUpdate;
end;

procedure TClassBrowser.OnParserUpdate(Sender: TObject);
begin
  EndUpdate;
end;
}

{
procedure TClassBrowser.OnCompareByType(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex;
    var Result: Integer);
var
  Data1,Data2:PNodeData;
begin
  Data1:=Sender.GetNodeData(Node1);
  Data2:=Sender.GetNodeData(Node2);
  if Data1^.statement^._Static and (not Data2^.statement^._Static) then
    Result:=-1
  else if (not Data1^.statement^._Static) and Data2^.statement^._Static then
    Result:=1
  else
    if Ord(Data1^.statement^._ClassScope) <> Ord(Data2^.statement^._ClassScope) then
      Result := Ord(Data1^.statement^._ClassScope) - Ord(Data2^.statement^._ClassScope)
    else
      Result := Ord(Data1^.statement^._Kind) - Ord(Data2^.statement^._Kind);
end;

procedure TClassBrowser.OnCompareByAlpha(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex;
    var Result: Integer);
var
  Data1,Data2:PNodeData;
begin
  Data1:=Sender.GetNodeData(Node1);
  Data2:=Sender.GetNodeData(Node2);
  Result := StrIComp(PAnsiChar(Data1.Text), PAnsiChar(Data2.Text));
end;
}

{
procedure TClassBrowser.DoSort(lock:boolean);
begin
  fCriticalSection.Acquire;
  try
    if not assigned(Parser) then
      Exit;
    if not fParser.Enabled then
      Exit;
    if lock then begin
      if not fParser.Freeze(fParserSerialId) then
        Exit;
    end;
    //Items.BeginUpdate;
    try
      if sortAlphabetically then begin
        self.OnCompareNodes := OnCompareByAlpha;
        self.SortTree(0,sdAscending,True);
      end;
      if sortByType then begin
        self.OnCompareNodes := OnCompareByType;
        self.SortTree(0,sdAscending,True);
      end;
    finally
      //Items.EndUpdate;
      if lock then begin
        fParser.UnFreeze;
      end;
    end;
  finally
    fCriticalSection.Release;
  end;
end;
}
procedure TClassBrowser.ClearDummyStatements;
var
  i:integer;
begin
  for i:=0 to fDummyStatements.Count-1 do begin
    PStatement(fDummyStatements.Objects[i])^._Children.Free;
    dispose(PStatement(fDummyStatements.Objects[i]))
  end;
  fDummyStatements.Clear;
end;
procedure TClassBrowser.ClearTree;
begin
  fCriticalSection.Acquire;
  try
  //Items.BeginUpdate;
  fUpdating:=True;
  try
    self.Clear;
    fRootStatements.Clear;
    ClearDummyStatements;
  finally
    fUpdating:=False;
  end;
  finally
    fCriticalSection.Release;
  end;
end;

procedure TClassBrowser.SetParser(Value: TCppParser);
begin
  if Value = fParser then
    Exit;

  fCriticalSection.Acquire;
  try
    fParser := Value;
    BeginTreeUpdate;
    EndTreeUpdate;
  finally
    fCriticalSection.Release;
  end;
end;

procedure TClassBrowser.SetCurrentFile(const Value: AnsiString);
begin
  fCriticalSection.Acquire;
  try
  if Value = fCurrentFile then
    Exit;
  fCurrentFile := Value;
  BeginTreeUpdate;
  EndTreeUpdate;
  finally
    fCriticalSection.Release;
  end;
end;

procedure TClassBrowser.SetShowInheritedMembers(Value: boolean);
begin
  fCriticalSection.Acquire;
  try
    if Value = fShowInheritedMembers then
      Exit;
    fShowInheritedMembers := Value;
    BeginTreeUpdate;
    EndTreeUpdate;
  finally
    fCriticalSection.Release;
  end;
end;

procedure TClassBrowser.SetSortAlphabetically(Value: boolean);
begin
  fCriticalSection.Acquire;
  try
    if Value = fSortAlphabetically then
      Exit;
    fSortAlphabetically := Value;
    BeginTreeUpdate;
    EndTreeUpdate;
  finally
    fCriticalSection.Release;
  end;
end;

procedure TClassBrowser.SetSortByType(Value: boolean);
begin
  fCriticalSection.Acquire;
  try
    if Value = fSortByType then
      Exit;
    fSortByType := Value;
    BeginTreeUpdate;
    EndTreeUpdate;
  finally
    fCriticalSection.Release;
  end;
end;

procedure TClassBrowser.SetStatementsType(Value: TClassBrowserStatementsType);
begin  
  fCriticalSection.Acquire;
  try
    if Value = fStatementsType then
      Exit;
    fStatementsType := Value;
    BeginTreeUpdate;
    EndTreeUpdate;
  finally
    fCriticalSection.Release;
  end;
end;


procedure TClassBrowser.SetTabVisible(Value: boolean);
begin

  fCriticalSection.Acquire;
  try
    if Value = fTabVisible then
      Exit;
    fTabVisible := Value;
    BeginTreeUpdate;
    EndTreeUpdate;
  finally
    fCriticalSection.Release;
  end;
end;


procedure TClassBrowser.OnCBDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; const Text: UnicodeString; const CellRect: TRect; var DefaultDraw: Boolean);
var
  st: PStatement;
  bInherited: boolean;
  Data:PNodeData;
begin
  fCriticalSection.Acquire;
  try
    if fUpdating then begin
      Exit;
    end;

    if not assigned(fParser) then
      Exit;
    if not fParser.Enabled then
      Exit;
    if not fParser.Freeze(fParserSerialId) then
      Exit;

    try
      data:=GetNodeData(Node);
      if not assigned(data) or not assigned(data^.statement) then
        Exit;
      st := data^.statement;
      bInherited := fShowInheritedMembers and st^._Inherited;
      TargetCanvas.Font.Style := [fsBold];
      while Assigned(st) and (st^._Kind = skAlias) do begin
        st := fParser.FindStatementOf(
          st^._FileName, st^._Type, st^._Line);
      end;
      if not assigned(st) then
        st := data^.statement;
      if  Selected[node] and self.Focused then begin
        TargetCanvas.Brush.Color:=fColors[SelectedBackColor];
//        TargetCanvas.Font.Color := fColors[SelectedForeColor];
      end else begin
        TargetCanvas.Brush.Color:=fColors[BackColor];
        if bInherited then
          TargetCanvas.Font.Color := fColors[InheritedColor]
        else begin
          case st^._Kind of
            skVariable,skParameter:begin
              TargetCanvas.Font.Color := fColors[VarColor];
            end;
            skClass:begin
              TargetCanvas.Font.Color := fColors[ClassColor];
            end;
            skNamespace:begin
              TargetCanvas.Font.Color := fColors[NamespaceColor];
            end;
            skFunction,skConstructor,skDestructor:begin
              TargetCanvas.Font.Color := fColors[FunctionColor];
            end;
            skTypedef:begin
              TargetCanvas.Font.Color := fColors[TypedefColor];
            end;
            skPreprocessor,skEnum:begin
              TargetCanvas.Font.Color := fColors[PreprocessorColor];
            end;
            skEnumType:begin
              TargetCanvas.Font.Color := fColors[EnumColor];
            end;
            else begin
              TargetCanvas.Font.Color := fColors[ForeColor];
            end;
          end;
        end;
      end;
    finally
      fParser.UnFreeze;
    end;
  finally
    fCriticalSection.Release;
  end;
end;

{
procedure TClassBrowser.AdvancedCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState; Stage:
  TCustomDrawStage; var PaintImages, DefaultDraw: Boolean);
var
  DrawRect: TRect;
  DrawPoint: TPoint;
  st: PStatement;
  bInherited: boolean;
  TypeText: AnsiString;
  color : TColor;
begin
  fCriticalSection.Acquire;
  try
  if fUpdating then begin
    PaintImages:=False;
    DefaultDraw:=False;
    Exit;
  end;

  if not assigned(fParser) then
    Exit;
  if not fParser.Enabled then
    Exit;
  if not fParser.Freeze(fParserSerialId) then
    Exit;

  try
  st := PStatement(Node.Data);
  if not Assigned(st) then
    Exit;
  bInherited := fShowInheritedMembers and st^._Inherited;
  if Stage = cdPrePaint then begin
    Sender.Canvas.Font.Style := [fsBold];
    if  Node.Selected and self.Focused then begin
        Sender.Canvas.Brush.Color:=fColors[SelectedBackColor];
//        Sender.Canvas.Font.Color := fColors[SelectedForeColor];
    end else begin
      Sender.Canvas.Brush.Color:=fColors[BackColor];
      if bInherited then
        Sender.Canvas.Font.Color := fColors[InheritedColor]
      else begin
        case st^._Kind of
          skVariable,skParameter:begin
            Sender.Canvas.Font.Color := fColors[VarColor];
          end;
          skClass:begin
            Sender.Canvas.Font.Color := fColors[ClassColor];
          end;
          skNamespace:begin
            Sender.Canvas.Font.Color := fColors[NamespaceColor];
          end;
          skFunction,skConstructor,skDestructor:begin
            Sender.Canvas.Font.Color := fColors[FunctionColor];
          end;
          skTypedef:begin
            Sender.Canvas.Font.Color := fColors[TypedefColor];
          end;
          skPreprocessor,skEnum:begin
            Sender.Canvas.Font.Color := fColors[PreprocessorColor];
          end;
          skEnumType:begin
            Sender.Canvas.Font.Color := fColors[EnumColor];
          end;
          else begin
            Sender.Canvas.Font.Color := fColors[ForeColor];
          end;
        end;
      end;
    end;
  end else if Stage = cdPostPaint then begin
    try
      st := Node.Data;
      if not Assigned(st) then
        Exit;

      // Start drawing at top right corner of DisplayRect
      DrawRect := Node.DisplayRect(true);
      DrawPoint := Point(DrawRect.Right, DrawRect.Top);
      color:=fControlCanvas.Brush.Color;
      if  Node.Selected and self.Focused then begin
        fControlCanvas.Brush.Color:=fColors[SelectedBackColor];
      end else begin
        fControlCanvas.Brush.Color:=fColors[BackColor];
      end;
      // Draw function arguments to the right of the already drawn text
      if st^._Args <> '' then begin
        fControlCanvas.Font.Assign(self.Font);
        if  Node.Selected and self.Focused then begin
          fControlCanvas.Font.Color := fColors[SelectedForeColor];
        end else begin
          if bInherited then
            fControlCanvas.Font.Color := fColors[InheritedColor]
          else
            fControlCanvas.Font.Color := fColors[FunctionColor];
        end;
        fControlCanvas.TextOut(DrawPoint.X, DrawPoint.Y + 2, st^._Args); // center vertically
        Inc(DrawPoint.X, fControlCanvas.TextWidth(st^._Args) + 3); // add some right padding
      end;

      if st^._Kind = skPreprocessor then
        TypeText := st^._Value
      else
        if st^._Type <> '' then
          TypeText := st^._Type
        else
          TypeText := fParser.StatementKindStr(st^._Kind);

      // Then draw node type to the right of the arguments
      if TypeText <> '' then begin
        if  Node.Selected and self.Focused then
          fControlCanvas.Font.Color := fColors[SelectedForeColor]
        else
        fControlCanvas.Font.Color := fColors[ClassColor];
        fControlCanvas.TextOut(DrawPoint.X, DrawPoint.Y + 2, ': ' + TypeText); // center vertically
      end;
      fControlCanvas.Brush.Color:=color;
    except // stick head into sand method. sometimes during painting, the PStatement is invalid
           // this is caused by repainting while the CppParser is busy.
    end;
  end;
  finally
    fParser.UnFreeze;
  end;
  finally
    fCriticalSection.Release;
  end;
end;
}
procedure TClassBrowser.ReSelect;
begin
  {
  for I := 0 to Items.Count - 1 do begin
    Statement := PStatement(Items[I].Data);
    with Statement^ do
      if (_Type + ':' + _Command + ':' + _Args) = fLastSelection then begin
        Selected := Items[I];
        Break;
      end;
  end;
  }
end;

function TClassBrowser.GetColor(i:integer):TColor;
begin
  Result := fColors[i];
end;

procedure TClassBrowser.SetColor(i:integer; const Color:TColor);
begin
  fColors[i] := Color;
  if i=BackColor then begin
    self.Color := Color;
    self.Colors.UnfocusedSelectionColor := Color;
    self.Colors.UnfocusedSelectionBorderColor := Color;
  end;
  if i=SelectedForeColor then begin
    self.Colors.SelectionTextColor := Color;
  end;
  if i=ForeColor then begin
    self.Font.Color := Color;
  end;
  if i=SelectedBackColor then begin
    self.Colors.FocusedSelectionColor := Color;
    self.Colors.FocusedSelectionBorderColor:= Color;
  end;

end;

function TClassBrowser.GetSelected:PVirtualNode;
var
  nodes:TVTVirtualNodeEnumeration;
  enumerator: TVTVirtualNodeEnumerator; 
begin
  Result := nil;
  nodes := self.SelectedNodes(False);
  enumerator := nodes.GetEnumerator;
  if enumerator.MoveNext then
    Result := enumerator.Current;
end;

function TClassBrowser.GetSelectedLine:integer;
var
  node:PVirtualNode;
  data:PNodeData;
begin
  Result := 0;
  fCriticalSection.Acquire;
  try
    if not assigned(fParser) then
      Exit;
    if not fParser.Enabled then
      Exit;
    if not fParser.Freeze(fParserSerialId) then
      Exit;
    try
      node := GetSelected;
      if assigned(node) then begin
        data:=GetNodeData(node);
        if assigned(data) and assigned(data^.statement) then
          Result := data^.statement^._Line;
      end;
    finally
      fParser.UnFreeze;
    end;
  finally
    fCriticalSection.Release;
  end;
end;

function TClassBrowser.GetSelectedFile:String;
var
  node:PVirtualNode;
  data:PNodeData;
begin
  Result := '';
  fCriticalSection.Acquire;
  try
    if not assigned(fParser) then
      Exit;
    if not fParser.Enabled then
      Exit;
    if not fParser.Freeze(fParserSerialId) then
      Exit;
    try
      node := GetSelected;
      if assigned(node) then begin
        data:=GetNodeData(node);
        if assigned(data) and assigned(data^.statement) then
          Result := data^.statement^._FileName;
      end;
    finally
      fParser.UnFreeze;
    end;
  finally
    fCriticalSection.Release;
  end;
end;

function TClassBrowser.GetSelectedDefLine:integer;
var
  node:PVirtualNode;
  data:PNodeData;
begin
  Result := 0;
  fCriticalSection.Acquire;
  try
    if not assigned(fParser) then
      Exit;
    if not fParser.Enabled then
      Exit;
    if not fParser.Freeze(fParserSerialId) then
      Exit;
    try
      node := GetSelected;
      if assigned(node) then begin
        data:=GetNodeData(node);
        if assigned(data) and assigned(data^.statement) then
          Result := data^.statement^._DefinitionLine;
      end;
    finally
      fParser.UnFreeze;
    end;
  finally
    fCriticalSection.Release;
  end;
end;

function TClassBrowser.GetSelectedDefFile:String;
var
  node:PVirtualNode;
  data:PNodeData;
begin
  Result := '';
  fCriticalSection.Acquire;
  try
    if not assigned(fParser) then
      Exit;
    if not fParser.Enabled then
      Exit;
    if not fParser.Freeze(fParserSerialId) then
      Exit;
    try
      node := GetSelected;
      if assigned(node) then begin
        data:=GetNodeData(node);
        if assigned(data) and assigned(data^.statement) then
          Result := data^.statement^._DefinitionFileName;
      end;
    finally
      fParser.UnFreeze;
    end;
  finally
    fCriticalSection.Release;
  end;
end;

function TClassBrowser.GetSelectedCommand:String;
var
  node:PVirtualNode;
  data:PNodeData;
begin
  Result := '';
  fCriticalSection.Acquire;
  try
    if not assigned(fParser) then
      Exit;
    if not fParser.Enabled then
      Exit;
    if not fParser.Freeze(fParserSerialId) then
      Exit;
    try
      node := GetSelected;
      if assigned(node) then begin
        data:=GetNodeData(node);
        if assigned(data) and assigned(data^.statement) then
          Result := data^.statement^._Command;
      end;
    finally
      fParser.UnFreeze;
    end;
  finally
    fCriticalSection.Release;
  end;
end;

function TClassBrowser.GetSelectedKind:TStatementKind;
var
  node:PVirtualNode;
  data:PNodeData;
begin
  Result := skUnknown;
  fCriticalSection.Acquire;
  try
    if not assigned(fParser) then
      Exit;
    if not fParser.Enabled then
      Exit;
    if not fParser.Freeze(fParserSerialId) then
      Exit;
    try
      node := GetSelected;
      if assigned(node) then begin
        data:=GetNodeData(node);
        if assigned(data) and assigned(data^.statement) then
          Result := data^.statement^._Kind;
      end;
    finally
      fParser.UnFreeze;
    end;
  finally
    fCriticalSection.Release;
  end;
end;

end.

