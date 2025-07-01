unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  Menus, ActnList, ExtCtrls, fpImage, PASext, DOScp;

type

  { TfMain }

  TfMain = class(TForm)
    aAddChar: TAction;
    aDeleteChar: TAction;
    aPreview: TAction;
    alMain: TActionList;
    apMain: TApplicationProperties;
    ControlBar1: TControlBar;
    eCode: TEdit;
    eMore: TEdit;
    eEntity: TEdit;
    eIndex: TEdit;
    eUTF8: TEdit;
    ilFont: TImageList;
    iChar: TImage;
    ilActive: TImageList;
    ilDisabled: TImageList;
    lEntity: TLabel;
    lCode: TLabel;
    lMore: TLabel;
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
    StatusBar1: TStatusBar;
    tbMain: TToolBar;
    tbPreview: TToolButton;
    tbAddChar: TToolButton;
    tbDeleteChar: TToolButton;
    udIndex: TUpDown;
    procedure aPreviewExecute(Sender: TObject);
    procedure eCodeChange(Sender: TObject);
    procedure eEntityChange(Sender: TObject);
    procedure eIndexChange(Sender: TObject);
    procedure eMoreChange(Sender: TObject);
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
  if lvCodePage.ItemIndex < 0 then begin
    eIndex.Text:='255';
    eIndex.Text:='127';
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
  lvCodePage.Items[I].SubItems[1]:=CodePages.Active.UTF8[I];
end;

procedure TfMain.eCodeChange(Sender: TObject);
var
  I : Integer;
begin
  if FSwitching then exit;
  if not Assigned(CodePages.Active) then Exit;
  I:=lvCodePage.ItemIndex;
  CodePages.Active.Code[I] := eCode.Text;
  lvCodePage.Items[I].SubItems[2]:=CodePages.Active.Code[I];
end;

procedure TfMain.aPreviewExecute(Sender: TObject);
var
  B : TBitmap;
  G : TPicture;
  P : String;
  F : String;
  S : String;
  M : String;
  T : String;
  C, X, Y, Z, R, N : integer;
begin
  P := UserHomePath + 'CpME_preview';
  F := P + '.html';
  P := IncludeTrailingPathDelimiter(P);
  if Not DirectoryExists(P) then
    if not CreateDir(P) then begin
      aPreview.Enabled:=False;
      Exit;
    end;
  S := '<!DOCTYPE html>' + LF + '<html>' + LF +
    '<head>' + LF +
    '<meta content="text/html; charset=UTF-8" http-equiv="Content-Type"/>' + LF +
    '<title>CpME Character Preview</title>' + LF +
    '<style>' + LF +
    'div { display:inline-block; }' + LF +
    'body, table { text-align:center; }' + LF +
    // 'tr.spaced { background:#777; }'  + LF +
    'div.section { width:90%; font-size:175%; font-weight:bolder; }' + LF +
    'div.chart { font-size:100%; font-weight:normal; }' + LF +
    'img { width:0.5em; height:1em; }' + LF +
    '</style>' + LF +
    '<body>' + LF;
  for C := 0 to CodePages.Count - 1 do begin
    if C > 0 then S := S + '<hr>' + LF;
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
    for Y := 0 to R - 1 do begin
      S := S + '<tr class="spaced"><td>&nbsp;</td></tr>' + LF;
      S := S + '<tr><th>0x' + HexStr(Y * 16, Z) + '<br>UTF-8<br>Code<br>HTML<br>more</th>' + LF;
      for X := 0 to 15 do begin
        N := Y * 16 + X;
        S := S + '<!-- ASCII #' + INtToStr(N) + ' --><td>';
        S := S + '<span class="' + WhenTrue(N > 255, 'extra', 'ascii') +'">';
        if N <= 255 then begin
          try
            G := TPicture.Create;
            B := CodePages[C].FontFile.Characters[N].asBitMap;
            G.Assign(B);
            M :=P + 'CP' + CodePages[C].ID + '_A' + IntToStr(N) + '.png';
            G.SaveToFile(M, ExtractFileExt(M));
            S := S + '<img src="CpME_preview/' + ExtractFileName(M) + '">';
          finally
            B.Free;
            G.Free;
          end;
        end;
        S := S + '</span><br>';
        M := CodePages[C].UTF8[N];
        S := S + '<span class="utf8">' + WhenTrue(M, M, Char(N)) + '</span><br>';
        M := CodePages[C].Code[N];
        if M <> '' then M := '&#' + M + ';';
        S := S + '<span class="code">' + WhenTrue(M, M, '&nbsp;')+ '</span><br>';
        M := CodePages[C].Entity[N];
        if M <> '' then M := '&' + M + ';';
        S := S + '<span class="entity">' + WhenTrue(M, M, '&nbsp;')+ '</span><br>';

        S := S + '<span class="additional">';
        M := CodePages[C].Additional[N];
        if M = '' then
          S := S + '&nbsp;'
        else while M <> '' do begin
          T:=Trim(PopDelim(M, COMMA));
          S := S + '&' + T + ';';
          if M <> '' then S := S + '&nbsp;&nbsp;';
        end;
        S := S + '</span></td>' + LF;
      end;
      S := S + '</tr>' + LF;
    end;
    S := S + '</table></div>' + LF;
  end;
  S := S + '</body>' + LF + '</html>' + LF;
  if SaveBinary(F, S, false) <> 0 then begin
    aPreview.Enabled:=False;
    Exit;
  end;
end;

procedure TfMain.eEntityChange(Sender: TObject);
var
  I : Integer;
begin
  if FSwitching then exit;
  if not Assigned(CodePages.Active) then Exit;
  I:=lvCodePage.ItemIndex;
  CodePages.Active.Entity[I] := eEntity.Text;
  lvCodePage.Items[I].SubItems[3]:=CodePages.Active.Entity[I];
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

procedure TfMain.eMoreChange(Sender: TObject);
var
  I : Integer;
begin
  if FSwitching then exit;
  if not Assigned(CodePages.Active) then Exit;
  I:=lvCodePage.ItemIndex;
  CodePages.Active.Additional[I] := eMore.Text;
  lvCodePage.Items[I].SubItems[4]:=CodePages.Active.Additional[I];
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
  SelectChar(Item.Index);
end;

procedure TfMain.SelectCodePage(CP: integer);
begin
  SelectCodePage(IntToStr(CP));
end;

procedure TfMain.SelectCodePage(CP: String);
var
  J : integer;

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
      LI.SubItems.Add(CodePages.Active.Code[J]);
      LI.SubItems.Add(CodePages.Active.Entity[J]);
      LI.SubItems.Add(CodePages.Active.Additional[J]);
    end else begin
      LI := lvCodepage.Items[J];
      LI.SubItems[1]:=CodePages.Active.UTF8[J];
      LI.SubItems[2]:=CodePages.Active.Code[J];
      LI.SubItems[3]:=CodePages.Active.Entity[J];
      LI.SubItems[4]:=CodePages.Active.Additional[J];
    end;
  end;

begin
  if CP = '' then Exit;
  FRecentCP:=CP;
  CodePages.Select(CP);
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
  SelectChar(lvCodePage.ItemIndex);
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
    eCode.Text:=CodePages.Active.Code[Index];
    eEntity.Text:=CodePages.Active.Entity[Index];
    eMore.Text:=CodePages.Active.Additional[Index];
  end else begin
    iChar.Picture.Clear;
    eUTF8.Text:='';
    eCode.Text:='';
    eEntity.Text:='';
    eMore.Text:='';
  end;
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

