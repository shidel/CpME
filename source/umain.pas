// Copyright (c) 2025 Jerome Shidel
// BSD-3-Clause license

unit umain;

{$mode objfpc}{$H+}

// TO-DO: when clearing non-437 < 256, make sure list shows change back to using
// the 437 character mapping data.
// TO-DO: add ability to change order of supplemental characters
// TO-DO: move map generator into CpME
// TO-DO: add ability to set path codepage XML path
// TO-DO: add ability to select preview output path
// TO-DO: add ability to create or duplicate a codepage.
// TO-DO: ASCII font character editing.
// TO-DO: No font, use (or clone) 437

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  Menus, ActnList, ExtCtrls, fpImage, PASext, DOScp, DOSfont;

const
  csAscii  = 0;
  csUTF8   = 1;
  csEntity = 2;

  pTotal = 0;
  pIndex = 1;
  pCodes = 2;

type

  { TfMain }

  TfMain = class(TForm)
    aAddChar: TAction;
    aMoveChar: TAction;
    aInsertChar: TAction;
    aDeleteChar: TAction;
    aPreview: TAction;
    alMain: TActionList;
    apMain: TApplicationProperties;
    ControlBar1: TControlBar;
    eEntity: TEdit;
    eIndex: TEdit;
    eUTF8: TEdit;
    ilFont: TImageList;
    iChar: TImage;
    ilActive: TImageList;
    ilDisabled: TImageList;
    lEntity: TLabel;
    lUTF8: TLabel;
    lIndex: TLabel;
    lCodePages: TLabel;
    lbCodePages: TListBox;
    lvCodePage: TListView;
    mmMain: TMainMenu;
    pProps: TPanel;
    pEditIndex: TPanel;
    pCharacter: TPanel;
    pRight: TPanel;
    pLeft: TPanel;
    pClient: TPanel;
    pHeadSpace: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    sBar: TStatusBar;
    tbMain: TToolBar;
    tbPreview: TToolButton;
    tbAddChar: TToolButton;
    tbDeleteChar: TToolButton;
    tbSplitA: TToolButton;
    tbSplitB: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    udIndex: TUpDown;
    procedure aAddCharExecute(Sender: TObject);
    procedure aAddCharUpdate(Sender: TObject);
    procedure aDeleteCharExecute(Sender: TObject);
    procedure aDeleteCharUpdate(Sender: TObject);
    procedure aInsertCharExecute(Sender: TObject);
    procedure aInsertCharUpdate(Sender: TObject);
    procedure aMoveCharUpdate(Sender: TObject);
    procedure aPreviewExecute(Sender: TObject);
    procedure aPreviewUpdate(Sender: TObject);
    procedure eEntityChange(Sender: TObject);
    procedure eIndexChange(Sender: TObject);
    procedure eUTF8Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbCodePagesClick(Sender: TObject);
    procedure lbCodePagesSelectionChange(Sender: TObject; User: boolean);
    procedure lvCodePageChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
  private
    FRecentCP:String;
    FSwitching:boolean;
    FFirstShow : boolean;
    procedure SelectCodePage(CP : integer); overload;
    procedure SelectCodePage(CP : String); overload;
    procedure SelectChar(Index : Integer); overload;
    procedure SelectChar(Index : String); overload;
  public

  end;

var
  fMain: TfMain;

implementation

{$R *.lfm}

{ TfMain }

procedure TfMain.FormCreate(Sender: TObject);
var
  I : integer;
begin
  FFirstShow := True;
  FRecentCP:='';
  FSwitching:=False;
  lbCodePages.Clear;
  for I := 0 to CodePages.Count - 1 do
    lbCodePages.AddItem(CodePages[I].ID,nil);
  if lbCodePages.Count > 0 then
    lbCodePages.Selected[0]:=True;
end;

procedure TfMain.FormShow(Sender: TObject);
begin
  if FFirstShow then begin
    FFirstShow:=False;
    if lvCodePage.ItemIndex < 0 then begin
      eIndex.Text:='137';
      eIndex.Text:='127';
    end;
  end;
end;

procedure TfMain.lbCodePagesClick(Sender: TObject);
var
  J : Integer;
begin
  if (lbCodePages.GetSelectedText = '') and (FRecentCP <> '') then
    for J := 0 to lbCodePages.Count - 1 do
      if lbCodePages.Items[J] = FRecentCP then begin
        lbCodepages.Selected[J] := True;
        break;
      end;
end;

procedure TfMain.eUTF8Change(Sender: TObject);
var
  I : Integer;
begin
  if FSwitching then exit;
  if not Assigned(CodePages.Active) then Exit;
  I:=lvCodePage.ItemIndex;
  CodePages.Active.UTF8[I] := eUTF8.Text;
  lvCodePage.Items[I].SubItems[csUTF8]:=CodePages.Active.UTF8[I];
end;


procedure TfMain.aPreviewExecute(Sender: TObject);
var
  B : TBitmap;
  G : TPicture;
  P : String;
  S : String;
  M : String;
  C, X, Y, Z, R, N, SR, V, T : integer;

  function RowName : String;
  var
    X : Integer;
    S : String;
  begin
    S := '<tr><th>0x' + HexStr(Y * 16, Z) + '</th>' + LF;
    for X := 0 to 15 do begin
      N := Y * 16 + X;
      M := CodePages[C].Entities[N];
      if M = '' then begin
        M := UTF8ToInts(CodePages[C].UTF8[N], True);
        S := S + '<td class="unnamed">';
      end else
        S := S + '<td class="named">';
      S:= S + M;
      S := S + '</td>';
    end;
    Result:=S + '</tr>' + LF;
  end;

  function RowASCII : String;
  var
    X : Integer;
    S : String;
  begin
    S := '<tr><th>ASCII</th>' + LF;
    for X := 0 to 15 do begin
      N := Y * 16 + X;
      S := S + '<td class="ascii">';
      if N <= 255 then begin
        try
          G := TPicture.Create;
          B := CodePages[C].FontFile.Characters[N].asBitMap;
          G.Assign(B);
          M :=P + 'CP' + CodePages[C].ID + '_A' + IntToStr(N) + '.png';
          G.SaveToFile(M, ExtractFileExt(M));
          S := S + '<img src="CpME_Images/' + ExtractFileName(M) + '">';
        finally
          B.Free;
          G.Free;
        end;
      end;
      S := S + '</td>';
    end;
    Result:=S + '</tr>' + LF;
  end;

  function RowUTF8 : String;
  var
    X : Integer;
    S : String;
  begin
    S := '<tr><th>UTF-8</th>' + LF;
    for X := 0 to 15 do begin
      N := Y * 16 + X;
      M := CodePages[C].UTF8[N];
      if (N < 256) and (M='') then M:= Char(N);
      S := S + '<td class="utf8">' + M + '</td>';
    end;
    Result:=S + '</tr>' + LF;
  end;

  function RowCode : String;
  var
    X : Integer;
    S : String;
  begin
    S := '<tr><th>Code</th>' + LF;
    for X := 0 to 15 do begin
      N := Y * 16 + X;
      M  :=UTF8ToInts(CodePages[C].UTF8[N], True);
      M := StringReplace(M, COMMA, ';&#', [rfReplaceAll]);
      S := S + '<td class="Code">' + WhenTrue(M, '&#' + M + ';', '&nbsp;') + '</td>';
    end;
    Result:=S + '</tr>' + LF;
  end;

  function RowEntity : String;
  var
    X : Integer;
    S : String;
  begin
    S := '<tr><th>Entity</th>' + LF;
    for X := 0 to 15 do begin
      N := Y * 16 + X;
      M := CodePages[C].Entities[N];
      M := PopDelim(M, COMMA);
      S := S + '<td class="Entity">' + WhenTrue(M, '&' + M + ';', '&nbsp;') + '</td>';
    end;
    Result:=S + '</tr>' + LF;
  end;

  function RowMore : String;
  var
    X : Integer;
    S : String;
    T : String;
  begin
    S := '<tr><th>More</th>' + LF;
    for X := 0 to 15 do begin
      N := Y * 16 + X;
      M := CodePages[C].Entities[N];
      PopDelim(M, COMMA);
      S := S + '<td class="additional">';
      if M = '' then
        S := S + '&nbsp;'
      else while M <> '' do begin
        T:=Trim(PopDelim(M, COMMA));
        S := S + '&' + T + ';';
        if M <> '' then S := S + '&nbsp;&nbsp;';
      end;
      S := S + '</td>';
    end;
    Result:=S + '</tr>' + LF;
  end;

begin
  P := UserHomePath + 'CpME_Images';
  P := IncludeTrailingPathDelimiter(P);
  if Not DirectoryExists(P) then
    if not CreateDir(P) then begin
      aPreview.Enabled:=False;
      Exit;
    end;
  CodePages.Flush;
  C := lbCodePages.ItemIndex;
  S := '<!DOCTYPE html>' + LF + '<html>' + LF +
    '<head>' + LF +
    '<meta content="text/html; charset=UTF-8" http-equiv="Content-Type"/>' + LF +
    '<title>CpME ' + CodePages[C].ID + ' Preview</title>' + LF +
    '<style>' + LF +
    'div { display:inline-block; }' + LF +
    'body { margin-bottom:2em; }' + LF +
    'body, table { text-align:center; }' + LF +
    'td.named { color:#633; font-style:italic }'  + LF +
    'td.unnamed { color:#411; font-style:italic }'  + LF +
    'div.section { width:90%; font-size:175%; font-weight:bolder; padding:2em; }' + LF +
    'div.chart { font-size:100%; font-weight:normal; }' + LF +
    'img { width:0.5em; height:1em; }' + LF +
    '</style>' + LF +
    '<body>' + LF;
  // for C := 0 to CodePages.Count - 1 do begin
  //  if C > 0 then S := S + '<hr>' + LF;
    Codepages[C].FontFile.Background:=clWhite;
    Codepages[C].FontFile.Foreground:=0;
    S := S + '<div class="section">Codepage ' + CodePages[C].ID + '</div>' + LF;
    S := S + '<div class="chart"><table><tr>' ;
    Z := WhenTrue(CodePages[C].Count > 256, 3, 2);
    R := CodePages[C].Count div 16;
    if CodePages[C].Count mod 16 <> 0 then Inc(R);
    for X := -1 to 15 do
      S := S + '<th>' + WhenTrue(X >= 0, '0x' + HexStr(X, Z)) + '</th>';
    S := S + '</tr>' + LF;
    if CodePages[C].ID = '437' then
      SR := 0
    else begin
      Val(CodePages[C].ID, V, T);
      if (T <> 0) or (V >= 900000) then
        SR :=16
      else
        SR := 8;
    end;
    for Y := SR to R - 1 do begin
      S := S + '<tr class="spaced"><td>&nbsp;</td></tr>' + LF;
      S := S + RowName + WhenTrue(Y <16, RowASCII) + RowUTF8 + RowCode +
        RowEntity + RowMore;
    end;

    S := S + '</table></div>' + LF;
  // end;
  S := S + '</body>' + LF + '</html>' + LF;
  if SaveBinary(UserHomePath+'CpME_'+CodePages[C].ID+'.html', S, false) <> 0 then begin
    aPreview.Enabled:=False;
    Exit;
  end;
end;

procedure TfMain.aPreviewUpdate(Sender: TObject);
var
  Okay : Boolean;
  V, E : Integer;
begin
  Okay:=Assigned(Codepages.Active);
  if Okay then begin
    Val(Codepages.Active.ID, V, E);
    Okay:=(V<>0) and (E=0);
  end;
  aPreview.Enabled:=Okay;
end;

procedure TfMain.aAddCharExecute(Sender: TObject);
var
  I : Integer;
begin
  if Not Assigned(Codepages.Active) then Exit;
  I := Codepages.Active.AddMap;
  SelectCodePage(Codepages.Active.ID);
  if lvCodePage.Items.Count > I then begin
    lvCodePage.Items[I].Selected:=True;
    eUTF8.SetFocus;
  end;
end;

procedure TfMain.aAddCharUpdate(Sender: TObject);
begin
  aAddChar.Enabled:=Assigned(Codepages.Active);
end;

procedure TfMain.aDeleteCharExecute(Sender: TObject);
begin
  if Not Assigned(Codepages.Active) then Exit;
  if lvCodePage.ItemIndex < 256 then Exit;
  CodePages.Active.DeleteMap(lvCodepage.ItemIndex);
  SelectCodePage(Codepages.Active.ID);
end;

procedure TfMain.aDeleteCharUpdate(Sender: TObject);
begin
  aDeleteChar.Enabled := lvCodePage.ItemIndex > 255;
end;

procedure TfMain.aInsertCharExecute(Sender: TObject);
begin
  if Not Assigned(Codepages.Active) then Exit;
  if lvCodePage.ItemIndex < 256 then Exit;
  CodePages.Active.InsertMap(lvCodepage.ItemIndex);
  SelectCodePage(Codepages.Active.ID);
end;

procedure TfMain.aInsertCharUpdate(Sender: TObject);
begin
    aInsertChar.Enabled := lvCodePage.ItemIndex > 255;
end;

procedure TfMain.aMoveCharUpdate(Sender: TObject);
begin
  aMoveChar.Enabled:=False;
  // aMoveChar.Enabled := lvCodePage.ItemIndex > 255;
end;

procedure TfMain.eEntityChange(Sender: TObject);
var
  I : Integer;
begin
  if FSwitching then exit;
  if not Assigned(CodePages.Active) then Exit;
  I:=lvCodePage.ItemIndex;
  CodePages.Active.Entities[I] := eEntity.Text;
  lvCodePage.Items[I].SubItems[csEntity]:=CodePages.Active.Entities[I];
end;

procedure TfMain.eIndexChange(Sender: TObject);
var
  I, E : Integer;
begin
  if FSwitching then exit;
  if not Assigned(CodePages.Active) then Exit;
  Val(eIndex.Text, I, E);
  if E <> 0 then Exit;
  if (I < 0) then begin
    eIndex.Text:='0';
    exit;
  end;
  if (I >= lvCodePage.Items.Count) then begin
    eIndex.Text:=IntToStr(lvCodePage.Items.Count-1);
    exit;
  end;
  if Not lvCodePage.Items[I].Selected then begin
    lvCodePage.Items[I].Selected:=True;
    lvCodePage.Items[I].MakeVisible(false);
  end;
end;

procedure TfMain.lbCodePagesSelectionChange(Sender: TObject; User: boolean);
begin
  if not Assigned(CodePages.Active) then Exit;
  SelectCodePage(lbCodePages.GetSelectedText);
end;

{
procedure TfMain.lvCodePageAdvancedCustomDrawSubItem(Sender: TCustomListView;
  Item: TListItem; SubItem: Integer; State: TCustomDrawState;
  Stage: TCustomDrawStage; var DefaultDraw: Boolean);

var
  R : TRect;
  B : TBitmap;
  W : integer;
begin
  // Event never Fired on MacOS Cocoa in Lazarus 3.4
  // Custom draw causes issues with DefaultDraw subitems on Lazarus 4.0
  DefaultDraw:=True;
  if SubItem <> 1 then Exit;
  if Item.Index > 255 then exit;
  if Not Assigned(Codepages.Active) then Exit;
  if Item.Index >= CodePages.Active.Count then Exit;
  DefaultDraw := False;
  if Stage <> cdPrePaint then Exit;
  R := Item.DisplayRect(drBounds);
  CodePages.Active.FontFile.Background:=clBlue;
  CodePages.Active.FontFile.Foreground:=clWhite;
  B := CodePages.Active.FontFile.Characters[Item.Index].asBitmap;
  // W := Trunc((R.Height - 2)/ CodePages.Active.FontFile.Height * CodePages.Active.FontFile.Width);
  // Sender.Canvas.StretchDraw(Rect(0,1,W, R.Height-1),B);
  Sender.Canvas.Draw(0,1,B);
  B.Free;
end;
}

procedure TfMain.lvCodePageChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  if not Assigned(Item) then Exit;
  if not Item.Selected then Exit;
  SelectChar(Item.Index);
end;

procedure TfMain.SelectCodePage(CP: integer);
begin
  SelectCodePage(IntToStr(CP));
end;

procedure TfMain.SelectCodePage(CP: String);
var
  J, X : integer;

  procedure AddItem;
  var
    LI : TListItem;
  begin
    if lvCodepage.Items.Count <= J then begin
      LI:=lvCodePage.Items.Add;
      LI.Caption:=' ' + IntToStr(J);
      LI.ImageIndex:=-1;
      if J < 255 then LI.ImageIndex:=J;
      LI.SubItems.Add('');
      LI.SubItems.Add(CodePages.Active.UTF8[J]);
      LI.SubItems.Add(CodePages.Active.Entities[J]);
    end else begin
      LI := lvCodepage.Items[J];
      LI.SubItems[csUTF8]:=CodePages.Active.UTF8[J];
      LI.SubItems[csEntity]:=CodePages.Active.Entities[J];
    end;
  end;

begin
  if CP = '' then Exit;
  FRecentCP:=CP;
  CodePages.Select(CP);
  X := lvCodePage.ItemIndex;
  if X < 0 then X := 0;
  CodePages.Active.FontFile.Background:=clGray;
  CodePages.Active.FontFile.Foreground:=clWhite;
  lvCodePage.BeginUpdate;
  lvCodePage.SmallImages:=nil;
  CodePages.Active.FontFile.ToImageList(ilFont);
  for J := 0 to CodePages.Active.Count - 1 do AddItem;
  for J := lvCodepage.Items.Count - 1 downto CodePages.Active.Count do
    lvCodePage.Items[J].Delete;
  lvCodePage.SmallImages:=ilFont;
  lvCodePage.EndUpdate;
  if X >= lvCodepage.Items.Count then
    X := lvCodepage.Items.Count - 1;
  if X >= 0 then begin
    lvCodePage.Items[X].Selected:=True;
    SelectChar(X);
  end;
  sBar.Panels[pTotal].Text:='Entries: ' +
    WhenTrue(CodePages.Active, IntToStr(CodePages.Active.Count),'n/a');
end;

procedure TfMain.SelectChar(Index: Integer);
begin
  if FSwitching then Exit;
  if lvCodePage.Items.Count = 0 then Exit;
  if Index < 0 then Exit;
  FSwitching :=True;
  if Index > lvCodePage.Items.Count - 1 then
    Index:=lvCodePage.Items.Count - 1;
  if Assigned(CodePages.Active) then begin
    lvCodePage.Items[Index].MakeVisible(false);
    CodePages.Active.FontFile.Background:=clBlue;
    CodePages.Active.FontFile.Foreground:=clYellow;
    Codepages.Active.FontFile.Characters[Index].ToImage(iChar);
    eIndex.Text:=IntToStr(Index);
    eUTF8.Text:=CodePages.Active.UTF8[Index];
    eEntity.Text:=CodePages.Active.Entities[Index];
  end else begin
    iChar.Picture.Clear;
    eUTF8.Text:='';
    eEntity.Text:='';
  end;
  sBar.Panels[pIndex].Text:='Index: ' +
    WhenTrue(CodePages.Active, 'x' + IntToHex(Index,4),'n/a');
  sBar.Panels[pCodes].Text:=' Code: ' +
    WhenTrue(eUTF8.Text, UTF8toInts(eUTF8.Text,true),'n/a');
  FSwitching :=False;
end;

procedure TfMain.SelectChar(Index: String);
var
  I, E : Integer;
begin
  Val(Index, I, E);
  if E <> 0 then I := 0;
  SelectChar(I);
end;


end.

