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
  CppParser, Forms, cbutils, Messages, SyncObjs;

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

  TClassBrowser = class(TCustomTreeView)
  private
    fParser: TCppParser;
    fCriticalSection: TCriticalSection;
    fParserSerialId: string;
    fParserFreezed: boolean;
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
    fOnUpdated: TNotifyEvent;
    fUpdating: boolean;
    fColors : array[0..14] of TColor;    
    procedure SetParser(Value: TCppParser);
    procedure AddMembers(Node: TTreeNode; ParentStatement: PStatement);
    procedure AdvancedCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
      State: TCustomDrawState; Stage: TCustomDrawStage; var PaintImages,
      DefaultDraw: Boolean);
    procedure OnNodeChange(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure OnNodeChanging(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    {
    procedure OnParserUpdate(Sender: TObject);
    procedure OnParserBusy(Sender: TObject);
    }
    procedure SetNodeImages(Node: TTreeNode; Statement: PStatement);
    procedure SetCurrentFile(const Value: AnsiString);
    procedure SetShowInheritedMembers(Value: boolean);
    procedure SetSortAlphabetically(Value: boolean);
    procedure SetSortByType(Value: boolean);
    procedure SetTabVisible(Value: boolean);
    procedure ReSelect;
    procedure Sort(lock:boolean=False);
    function GetColor(i:integer):TColor;
    procedure SetColor(i:integer; const Color:TColor);        
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateView;
    procedure Clear;
    procedure BeginUpdate;
    procedure EndUpdate;
    property OnUpdated: TNotifyEvent read fOnUpdated write fOnUpdated;
    property Colors[Index: Integer]: TColor read GetColor write SetColor;
  published
    property Align;
    property Font;
    property Images;
    property ReadOnly;
    property Indent;
    property TabOrder;
    property PopupMenu;
    property BorderStyle;
    property MultiSelect;
    property MultiSelectStyle;
    property RowSelect;
    property ShowLines;
    property OnSelect: TMemberSelectEvent read fOnSelect write fOnSelect;
    property Parser: TCppParser read fParser write SetParser;
    property ItemImages: TImagesRecord read fImagesRecord write fImagesRecord;
    property CurrentFile: AnsiString read fCurrentFile write SetCurrentFile;
    property ShowInheritedMembers: boolean read fShowInheritedMembers write SetShowInheritedMembers;
    property TabVisible: boolean read fTabVisible write SetTabVisible;
    property SortAlphabetically: boolean read fSortAlphabetically write SetSortAlphabetically;
    property SortByType: boolean read fSortByType write SetSortByType;
    property ParserSerialId: string read fParserSerialId;
  end;

const
  CLASS_FOLDERS_MAGIC = 'DEVCF_1_0';
  TV_FIRST = $1100;
  TVM_SETEXTENDEDSTYLE = TV_FIRST + 44;
  TVM_GETEXTENDEDSTYLE = TV_FIRST + 45;
  TVS_EX_DOUBLEBUFFER = $4;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Dev-C++', [TClassBrowser]);
end;

{ TClassBrowser }

constructor TClassBrowser.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnMouseUp := OnNodeChange;
  OnMouseDown := OnNodeChanging;
  DragMode := dmAutomatic;
  fImagesRecord := TImagesRecord.Create;
  fCurrentFile := '';
  fParserFreezed:=False;
  ShowHint := True;
  HideSelection := False;
  RightClickSelect := True;
  fShowInheritedMembers := False;
  fControlCanvas := TControlCanvas.Create;
  fControlCanvas.Control := Self;
  //fControlCanvas.Font.Assign(Self.Font);
  OnAdvancedCustomDrawItem := AdvancedCustomDrawItem;
  fIncludedFiles := TStringList.Create;
  fIsIncludedCacheFileName := '';
  fIsIncludedCacheResult := false;
  fUpdateCount := 0;
  fTabVisible := false;
  RowSelect := true;
  ShowLines := False;
  fSortAlphabetically:= True;
  fSortByType:=True ;
  fOnUpdated:=nil;
  fUpdating:=False;
  fCriticalSection := TCriticalSection.Create;
end;

destructor TClassBrowser.Destroy;
begin
  FreeAndNil(fImagesRecord);
  FreeAndNil(fControlCanvas);
  fIncludedFiles.Free;
  fCriticalSection.Free;
  inherited Destroy;
end;

procedure TClassBrowser.BeginUpdate;
begin
  Inc(fUpdateCount);
end;

procedure TClassBrowser.EndUpdate;
begin
  Dec(fUpdateCount);
  if fUpdateCount = 0 then
    UpdateView;
end;

procedure TClassBrowser.SetNodeImages(Node: TTreeNode; Statement: PStatement);
var
  bInherited: boolean;
begin
  bInherited := statement^._Inherited;

  case Statement^._Kind of
    skNamespace: begin
        Node.ImageIndex := fImagesRecord.NamespaceImg;
      end;
    skClass: begin
        Node.ImageIndex := fImagesRecord.Classes;
      end;
    skPreprocessor: begin
        Node.ImageIndex := fImagesRecord.DefineImg;
      end;
    skEnum: begin
        case Statement^._ClassScope of
        scsPrivate: Node.ImageIndex := fImagesRecord.VariablePrivate;
        scsProtected: if not bInherited then
            Node.ImageIndex := fImagesRecord.VariableProtected
          else
            Node.ImageIndex := fImagesRecord.InheritedVariableProtected;
        scsPublic: if not bInherited then
            Node.ImageIndex := fImagesRecord.VariablePublic
          else
            Node.ImageIndex := fImagesRecord.InheritedVariablePublic;
        scsNone: Node.ImageIndex := fImagesRecord.EnumImg;
      end;
    end;
    skTypedef: begin
        Node.ImageIndex := fImagesRecord.TypeImg;
      end;
    skVariable,skParameter: case Statement^._ClassScope of
        scsPrivate: Node.ImageIndex := fImagesRecord.VariablePrivate;
        scsProtected: if not bInherited then
            Node.ImageIndex := fImagesRecord.VariableProtected
          else
            Node.ImageIndex := fImagesRecord.InheritedVariableProtected;
        scsPublic: if not bInherited then
            Node.ImageIndex := fImagesRecord.VariablePublic
          else
            Node.ImageIndex := fImagesRecord.InheritedVariablePublic;
        scsNone: begin
          if Statement^._Static then
            Node.ImageIndex := fImagesRecord.StaticVarImg
          else
            Node.ImageIndex := fImagesRecord.GlobalVarImg;
        end;
      end;
    skFunction, skConstructor, skDestructor: case Statement^._ClassScope of

        scsPrivate: Node.ImageIndex := fImagesRecord.MethodPrivate;
        scsProtected: if not bInherited then
            Node.ImageIndex := fImagesRecord.MethodProtected
          else
            Node.ImageIndex := fImagesRecord.InheritedMethodProtected;
        scsPublic: if not bInherited then
            Node.ImageIndex := fImagesRecord.MethodPublic
          else
            Node.ImageIndex := fImagesRecord.InheritedMethodPublic;
        scsNone: begin
          if Statement^._Static then
            Node.ImageIndex := fImagesRecord.StaticFuncImg
          else
            Node.ImageIndex := fImagesRecord.GlobalFuncImg;
        end;
      end;
  end;

  Node.SelectedIndex := Node.ImageIndex;
  Node.StateIndex := Node.ImageIndex;
end;

procedure TClassBrowser.AddMembers(Node: TTreeNode; ParentStatement: PStatement);
var
  Statement: PStatement;
  NewNode: TTreeNode;
  Children: TList;
  i:integer;
  P:PFileIncludes;

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
begin
  if Assigned(ParentStatement) then begin
    Children := fParser.Statements.GetChildrenStatements(ParentStatement);
  end else begin
    p:=fParser.FindFileIncludes(fCurrentFile);
    if not Assigned(p) then
      Exit;
    Children := p^.Statements;
  end;

//  fParser.Statements.DumpWithScope('f:\browser.txt');
  if Assigned(Children) then begin
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

        if Statement^._ParentScope <> ParentStatement then begin
          if Assigned(ParentStatement) then
            Continue;
          //we are adding an orphan statement, just add it

          //should not happend, just in case of error
          if not Assigned(Statement^._ParentScope) then
            Continue;

          if SameText(Statement^._ParentScope^._FileName,fCurrentFile)
            or SameText(Statement^._ParentScope^._DefinitionFileName,fCurrentFile) then
              Continue;
        end;

        {
        if SameText(_FileName,CurrentFile) or SameText(_DefinitionFileName,CurrentFile) then
          AddStatement(Statement)
        }
        AddStatement(Statement)
      end;
    end;
  end;
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
  if not Visible or not TabVisible then
    Exit;
  fUpdating:=True;
  try
    Items.BeginUpdate;
    Items.Clear;
    // We are busy...
    if not fParser.Freeze then
      Exit;
    fParserFreezed:=True;
    if fCurrentFile <> '' then begin
      // Update file includes, reset cache
      fParser.GetFileIncludes(fCurrentFile, fIncludedFiles);
      fIsIncludedCacheFileName := '';
      fIsIncludedCacheResult := false;

      // Add everything recursively
      AddMembers(nil, nil);
      Sort;

      // Remember selection
      if fLastSelection <> '' then
        ReSelect;
    end;
  finally
    fUpdating:=False;
    Items.EndUpdate; // calls repaint when needed
    fParser.Unfreeze;
    fParserSerialId := fParser.SerialId;
    fParserFreezed:=False;
  end;
  if Assigned(fOnUpdated) then
    fOnUpdated(Self);
  finally
    fCriticalSection.Release;
  end;
end;

procedure TClassBrowser.OnNodeChanging(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Node: TTreeNode;
begin
  if htOnItem in GetHitTestInfoAt(X, Y) then
    Node := GetNodeAt(X, Y)
  else
    Node := nil;
  Selected := Node;
end;

procedure TClassBrowser.OnNodeChange(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Node: TTreeNode;
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
      fParserFreezed:=True;
      if not samestr(fParser.SerialId, fParserSerialId) then
        Exit;
      // Check if we hit the node
      if htOnItem in GetHitTestInfoAt(X, Y) then
        Node := GetNodeAt(X, Y)
      else
        Node := nil;

      // Dit we click on anything?
      if not Assigned(Node) then begin
        fLastSelection := '';
        Exit;
      end else if not Assigned(Node.Data) then begin
        fLastSelection := '';
        Exit;
      end;

      // Send to listener
      with PStatement(Node.Data)^ do begin
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
        fParserFreezed:=False;
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

function CustomSortTypeProc(Node1, Node2: TTreeNode; Data: Integer): Integer; stdcall;
begin
  if PStatement(Node1.Data)^._Static and (not PStatement(Node2.Data)^._Static) then
    Result:=-1
  else if (not PStatement(Node1.Data)^._Static) and PStatement(Node2.Data)^._Static then
    Result:=1
  else
    if Ord(PStatement(Node1.Data)^._ClassScope) <> Ord(PStatement(Node2.Data)^._ClassScope) then
      Result := Ord(PStatement(Node1.Data)^._ClassScope) - Ord(PStatement(Node2.Data)^._ClassScope)
    else
      Result := Ord(PStatement(Node1.Data)^._Kind) - Ord(PStatement(Node2.Data)^._Kind);
end;

function CustomSortAlphaProc(Node1, Node2: TTreeNode; Data: Integer): Integer; stdcall;
begin
  Result := StrIComp(PAnsiChar(Node1.Text), PAnsiChar(Node2.Text));
end;

procedure TClassBrowser.Sort(lock:boolean);
begin
  fCriticalSection.Acquire;
  try
    if not assigned(Parser) then
      Exit;
    if not fParser.Enabled then
      Exit;
    if lock then begin
      if not fParser.Freeze then
        Exit;
      if fParser.SerialId <> fParserSerialId then begin
        fParser.UnFreeze;
        Exit;
      end;
      fParserFreezed:=True;
    end;
    Items.BeginUpdate;
    try
      if sortAlphabetically then
        CustomSort(@CustomSortAlphaProc, 0);
      if sortByType then
        CustomSort(@CustomSortTypeProc, 0);
    finally
      Items.EndUpdate;
      if lock then begin
        fParser.UnFreeze;
        fParserFreezed:=False;
      end;
    end;
  finally
    fCriticalSection.Release;
  end;
end;

procedure TClassBrowser.Clear;
begin
  fCriticalSection.Acquire;
  try
  Items.BeginUpdate;
  fUpdating:=True;
  try
    Items.Clear;
  finally
    fUpdating:=False;
    Items.EndUpdate;
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
  {
  if Assigned(fParser) then begin
    fParser.OnUpdate := OnParserUpdate;
    fParser.OnBusy := OnParserBusy;
  end;
  }
    UpdateView;
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
  UpdateView;
  finally
    fCriticalSection.Release;
  end;
end;

procedure TClassBrowser.SetShowInheritedMembers(Value: boolean);
begin
  if Value = fShowInheritedMembers then
    Exit;
  fShowInheritedMembers := Value;
  UpdateView;
end;

procedure TClassBrowser.SetSortAlphabetically(Value: boolean);
begin
  if Value = fSortAlphabetically then
    Exit;
  fSortAlphabetically := Value;
  Sort(True);
end;

procedure TClassBrowser.SetSortByType(Value: boolean);
begin
  if Value = fSortByType then
    Exit;
  fSortByType := Value;
  Sort(True);
end;


procedure TClassBrowser.SetTabVisible(Value: boolean);
begin
  if Value = fTabVisible then
    Exit;
  fTabVisible := Value;
  UpdateView;
end;

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

  if not fParserFreezed then begin
    if not assigned(fParser) then
      Exit;
    if not fParser.Enabled then
      Exit;
    if not fParser.Freeze then
      Exit;
    if not SameStr(fParserSerialId, fParser.SerialId) then begin
      fParser.UnFreeze;
      Exit;
    end;
  end;

  try
  st := PStatement(Node.Data);
  if not Assigned(st) then
    Exit;
  bInherited := fShowInheritedMembers and st^._Inherited;
  if Stage = cdPrePaint then begin
    Sender.Canvas.Font.Style := [fsBold];
    if  Node.Selected then begin
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
      if  Node.Selected then begin
        fControlCanvas.Brush.Color:=fColors[SelectedBackColor];
      end else begin
        fControlCanvas.Brush.Color:=fColors[BackColor];
      end;
      // Draw function arguments to the right of the already drawn text
      if st^._Args <> '' then begin
        fControlCanvas.Font.Assign(self.Font);
        if  Node.Selected then begin
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
        if  Node.Selected then
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
    if not fParserFreezed then
      fParser.UnFreeze;
  end;
  finally
    fCriticalSection.Release;
  end;
end;

procedure TClassBrowser.ReSelect;
var
  I: Integer;
  Statement: PStatement;
begin
  for I := 0 to Items.Count - 1 do begin
    Statement := PStatement(Items[I].Data);
    with Statement^ do
      if (_Type + ':' + _Command + ':' + _Args) = fLastSelection then begin
        Selected := Items[I];
        Break;
      end;
  end;
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
  end;
end;

end.

