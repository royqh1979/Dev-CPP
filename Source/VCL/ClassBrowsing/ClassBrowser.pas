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
{$IFDEF WIN32}
  Windows, Classes, SysUtils, IntList, StatementList, Controls, ComCtrls, Graphics,
  CppParser, Forms, cbutils, Messages;
{$ENDIF}
{$IFDEF LINUX}
Classes, SysUtils, QControls, QComCtrls, QForms, QGraphics,
CppParser;
{$ENDIF}

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
    fTypeImg: integer;
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
    property TypeImg: integer read fTypeImg write fTypeImg;
  end;

  TShowFilter = (sfAll, sfProject, sfCurrent, sfSystemFiles);

  TClassBrowser = class(TCustomTreeView)
  private
    fParser: TCppParser;
    fOnSelect: TMemberSelectEvent;
    fImagesRecord: TImagesRecord;
    fShowFilter: TShowFilter;
    fCurrentFile: AnsiString;
    fProjectDir: AnsiString;
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
    procedure SetParser(Value: TCppParser);
    procedure AddMembers(Node: TTreeNode; ParentStatement: PStatement);
    procedure AdvancedCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
      State: TCustomDrawState; Stage: TCustomDrawStage; var PaintImages,
      DefaultDraw: Boolean);
    procedure OnNodeChange(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure OnNodeChanging(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure OnParserUpdate(Sender: TObject);
    procedure OnParserBusy(Sender: TObject);
    procedure SetNodeImages(Node: TTreeNode; Statement: PStatement);
    procedure Sort;
    procedure SetCurrentFile(const Value: AnsiString);
    procedure SetShowFilter(Value: TShowFilter);
    procedure SetShowInheritedMembers(Value: boolean);
    procedure SetTabVisible(Value: boolean);
    function IsIncluded(const FileName: AnsiString): boolean;
    procedure ReSelect;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateView;
    procedure Clear;
    procedure BeginUpdate;
    procedure EndUpdate;
  published
    property Align;
    property Font;
    property Color;
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
    property ShowFilter: TShowFilter read fShowFilter write SetShowFilter;
    property OnSelect: TMemberSelectEvent read fOnSelect write fOnSelect;
    property Parser: TCppParser read fParser write SetParser;
    property ItemImages: TImagesRecord read fImagesRecord write fImagesRecord;
    property CurrentFile: AnsiString read fCurrentFile write SetCurrentFile;
    property ProjectDir: AnsiString read fProjectDir write fProjectDir;
    property ShowInheritedMembers: boolean read fShowInheritedMembers write SetShowInheritedMembers;
    property TabVisible: boolean read fTabVisible write SetTabVisible;
    property SortAlphabetically: boolean read fSortAlphabetically write fSortAlphabetically;
    property SortByType: boolean read fSortByType write fSortByType;
  end;

const
  CLASS_FOLDERS_MAGIC = 'DEVCF_1_0';

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
  fShowFilter := sfCurrent;
  fProjectDir := '';
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
  fTabVisible := true;
  RowSelect := true;
  ShowLines := False;
  fSortAlphabetically:= True;
  fSortByType:=True ;

end;

destructor TClassBrowser.Destroy;
begin
  FreeAndNil(fImagesRecord);
  FreeAndNil(fControlCanvas);
  fIncludedFiles.Free;
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
    skVariable: case Statement^._ClassScope of
        scsPrivate: Node.ImageIndex := fImagesRecord.VariablePrivate;
        scsProtected: if not bInherited then
            Node.ImageIndex := fImagesRecord.VariableProtected
          else
            Node.ImageIndex := fImagesRecord.InheritedVariableProtected;
        scsPublic: if not bInherited then
            Node.ImageIndex := fImagesRecord.VariablePublic
          else
            Node.ImageIndex := fImagesRecord.InheritedVariablePublic;
        scsNone: Node.ImageIndex := fImagesRecord.GlobalVarImg;
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
        scsNone: Node.ImageIndex := fImagesRecord.MethodPublic;
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

  procedure AddStatement(Statement: PStatement);
  begin
    NewNode := Items.AddChildObject(Node, Statement^._Command, Statement);
    SetNodeImages(NewNode, Statement);
    if Statement^._Kind = skClass then
      AddMembers(NewNode, Statement);
  end;
begin
  Children := fParser.Statements.GetChildrenStatements(ParentStatement);

  if Assigned(Children) then begin
    for i:=0 to Children.Count-1 do begin
      Statement := Children[i];
      with Statement^ do begin
        // Do not print statements marked invisible for the class browser
        if _Temporary then
          Continue;

        if _Inherited and not fShowInheritedMembers then // don't show inherited members
          Continue;

        if Statement = ParentStatement then // prevent infinite recursion
          Continue;

        if SameStr(_FileName,CurrentFile) then
          AddStatement(Statement)
        else begin
          case fShowFilter of
            sfAll: begin // sfAll means all open files. not the system headers
              if not _InSystemHeader then // do not show system headers
                AddStatement(Statement);
            end;
            sfSystemFiles: begin
              if _InSystemHeader and IsIncluded(_FileName) then
                AddStatement(Statement); // only show system header stuff
            end;
            sfCurrent: begin
              if not _InSystemHeader and IsIncluded(_FileName) then
                AddStatement(Statement);
            end;
            sfProject: begin
              if _InProject then
                AddStatement(Statement);
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TClassBrowser.UpdateView;
begin
  if not Assigned(fParser) then
    Exit;
  if fUpdateCount <> 0 then
    Exit;
  if not Visible or not TabVisible then
    Exit;

  // We are busy...
  Items.BeginUpdate;
  try
    Clear;
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
    Items.EndUpdate; // calls repaint when needed
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
      if Button = mbLeft then // need definition
        fOnSelect(Self, _DefinitionFileName, _DefinitionLine)
      else if Button = mbMiddle then // need declaration
        fOnSelect(Self, _FileName, _Line);
  end;
end;

procedure TClassBrowser.OnParserBusy(Sender: TObject);
begin
  BeginUpdate;
end;

procedure TClassBrowser.OnParserUpdate(Sender: TObject);
begin
  EndUpdate;
end;

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

procedure TClassBrowser.Sort;
begin
{
  if sortByType and sortAlphabetically then
    CustomSort(@CustomSortTypeAlphaProc, 0)
  else
    CustomSort(@CustomSortTypeProc, 0)
  else
    CustomSort(@CustomSortAlphaProc, 0);
}
  if sortAlphabetically then
    CustomSort(@CustomSortAlphaProc, 0);
  if sortByType then
    CustomSort(@CustomSortTypeProc, 0);

end;

procedure TClassBrowser.Clear;
begin
  Items.Clear;
end;

procedure TClassBrowser.SetParser(Value: TCppParser);
begin
  if Value = fParser then
    Exit;

  fParser := Value;
  if Assigned(fParser) then begin
    fParser.OnUpdate := OnParserUpdate;
    fParser.OnBusy := OnParserBusy;
  end;
  UpdateView;
end;

procedure TClassBrowser.SetCurrentFile(const Value: AnsiString);
begin
  if Value = fCurrentFile then
    Exit;
  fCurrentFile := Value;
  if fShowFilter = sfAll then // content does not depend on current file. do NOT redraw
    Exit;
  UpdateView;
end;

procedure TClassBrowser.SetShowFilter(Value: TShowFilter);
begin
  if fShowFilter = Value then
    Exit;
  fShowFilter := Value;
  UpdateView;
end;

procedure TClassBrowser.SetShowInheritedMembers(Value: boolean);
begin
  if Value = fShowInheritedMembers then
    Exit;
  fShowInheritedMembers := Value;
  UpdateView;
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
  // Assume the node image is correct
  bInherited := fShowInheritedMembers and (Node.ImageIndex in [
    fImagesRecord.fInhMethodProtectedImg,
      fImagesRecord.fInhMethodPublicImg,
      fImagesRecord.fInhVariableProtectedImg,
      fImagesRecord.fInhVariablePublicImg]);

  if Stage = cdPrePaint then begin
    Sender.Canvas.Font.Style := [fsBold];
    if bInherited then
      Sender.Canvas.Font.Color := clGray;
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
        fControlCanvas.Brush.Color:=clHighLight;
      end;
      // Draw function arguments to the right of the already drawn text
      if st^._Args <> '' then begin
        fControlCanvas.Font.Assign(self.Font);
        if  Node.Selected then begin
          if bInherited then
            fControlCanvas.Font.Color := clGray
          else
            fControlCanvas.Font.Color := clHighlightText;
        end else begin
          if bInherited then
            fControlCanvas.Font.Color := clGray
          else
            fControlCanvas.Font.Color := clMaroon;
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
          fControlCanvas.Font.Color := clHighLightText
        else
          fControlCanvas.Font.Color := clBlack;
        fControlCanvas.TextOut(DrawPoint.X, DrawPoint.Y + 2, ': ' + TypeText); // center vertically
      end;
      fControlCanvas.Brush.Color:=color;
    except // stick head into sand method. sometimes during painting, the PStatement is invalid
           // this is caused by repainting while the CppParser is busy.
    end;
  end;
end;

function TClassBrowser.IsIncluded(const FileName: AnsiString): boolean;
begin
  // Only do the slow check if the cache is invalid
  if not SameStr(FileName, fIsIncludedCacheFileName) then begin
    fIsIncludedCacheFileName := FileName;
    fIsIncludedCacheResult := FastIndexOf(fIncludedFiles, FileName) <> -1;
  end;

  // Cache has been updated. Use it.
  Result := fIsIncludedCacheResult;
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

end.

