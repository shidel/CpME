// Copyright (c) 2025 Jerome Shidel
// BSD-3-Clause license

unit DOScp;

{$mode ObjFPC}{$H+}

// TO-DO: when non-437 matches 437 < 256 remove entry
// TO-DO: when first edit non-437 < 256, clone 437 data

interface

uses
  Classes, SysUtils, XMLConf, PASext {$IFDEF LCL}, DOSfont {$ENDIF};

type
  TCodePageEntry = record
    UTF8, HTML : UnicodeString;
  end;
  TCodePages = class;

  { TCodePage }

  TCodePage = class
  private
    FCheckPrimary: boolean;
    FCount: integer;
    FOwner: TCodePages;
    FXML : TXMLConfig;
    FFileName : String;
    {$IFDEF LCL}
    FFontFile : TFontFile;
    {$ENDIF}
    function GetASCII(Index : integer): String;
    function GetCodePageEntry(Index : integer): TCodePageEntry;
    function GetEntities(Index : integer): String;
    function GetID: String;
    function GetUTF8(Index : integer): String;
    procedure SetCheckPrimary(AValue: boolean);
    procedure SetCodePageEntry(Index : integer; AValue: TCodePageEntry);
    procedure SetCount(AValue: integer);
    procedure SetEntities(Index : integer; AValue: String);
    procedure SetFileName(AValue: String);
    {$IFDEF LCL}
    procedure SetFontFile(AValue: TFontFile);
    procedure SetOwner(AValue: TCodePages);
    {$ENDIF}
    procedure SetUTF8(Index : integer; AValue: String);
  protected
    function GetKey(Index : Integer; Attribute : String) : String;
    function NotEmpty(Index : integer; Strict : boolean = false) : boolean;
    function Empty(Index : integer; Strict : boolean = false) : boolean;
    property Entry[Index : integer] : TCodePageEntry read GetCodePageEntry write SetCodePageEntry;
  public
    constructor Create(AFileName : String);
    destructor Destroy; override;
    property Owner : TCodePages read FOwner write SetOwner;
    property FileName : String read FFileName write SetFileName;
    property ID : String read GetID;
    property CheckPrimary : boolean read FCheckPrimary write SetCheckPrimary;
    {$IFDEF LCL}
    property FontFile : TFontFile read FFontFile write SetFontFile;
    {$ENDIF}
    property Count : integer read FCount write SetCount;
    property ASCII[Index : integer] : String read GetASCII;
    property UTF8[Index : integer] : String read GetUTF8 write SetUTF8;
    property Entities[Index : integer] : String read GetEntities Write SetEntities;
    function AddMap : integer;
    procedure DeleteMap (Index:Integer);
    procedure InsertMap (Index:Integer);
    procedure Flush;
  published
  end;

  { TCodePages }

  TCodePages = class
  private
    FActive: TCodePage;
    FCodePages : array of TCodePage;
    FPrimary: TCodePage;
    function GetCodePage(Index : integer): TCodePage;
    function GetCount: integer;
    procedure SetActive(AValue: TCodePage);
    procedure SetCodePage(Index : integer; AValue: TCodePage);
    procedure SetPrimary(AValue: TCodePage);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Reset;
    property Count : integer read GetCount;
    property CodePage[Index : integer] : TCodePage read GetCodePage write SetCodePage; default;
    property Primary : TCodePage read FPrimary write SetPrimary;
    property Active : TCodePage read FActive write SetActive;
    function Find(ACodePage : integer) : TCodePage; overload;
    function Find(ACodePage : String) : TCodePage; overload;
    function Select(ACodePage : integer) : TCodePage; overload;
    function Select(ACodePage : String) : TCodePage; overload;
    procedure Flush;
  published
  end;

var
  CodePages : TCodePages;

  function UTF8toInts(C : TUTF8CodePoint; Hex : boolean = false) : String;
  function IntsToUTF8(S : String) : TUTF8CodePoint;

implementation

var
  CodePageFilePath : string;

function UTF8toInts(C: TUTF8CodePoint; Hex : boolean = false): String;
var
  L : integer;
begin
  UTF8toInts:='';
  while C <> '' do begin
    L := CodePointLength(C);
    if Hex then
      UTF8toInts:=UTF8toInts+'x'+IntToHex(CodePointToValue(C),2) + COMMA
    else
      UTF8toInts:=UTF8toInts+IntToStr(CodePointToValue(C)) + COMMA;
    Delete(C, 1, L);
  end;
  UTF8toInts:=ExcludeTrailing(COMMA,UTF8toInts);
end;

function IntsToUTF8(S : String) : TUTF8CodePoint;
var
  T : String;
  V : int32;
begin
  IntsToUTF8:='';
  While S <> '' do begin
    T:=PopDelim(S, COMMA);
    if HexVal(T, V) then
      IntsToUTF8:=IntsToUTF8+ValueToCodePoint(V);
  end;
end;

{ TCodePage }

{$PUSH}
{$WARN 4104 off : Implicit string type conversion from "$1" to "$2"}
{$WARN 4105 off : Implicit string type conversion with potential data loss from "$1" to "$2"}

function TCodePage.GetID: String;
begin
  GetID:=UpperCase(ExtractFileBase(FFileName));
end;

function TCodePage.GetASCII(Index : integer): String;
begin
  if (Index > 255) or (Index < 0) then
    GetASCII:=''
  else
    GetAscii:=Char(Index);
end;

function TCodePage.GetCodePageEntry(Index : integer): TCodePageEntry;
begin
  GetCodePageEntry.UTF8:=IntsToUTF8(FXML.GetValue(GetKey(Index, 'UTF8'), ''));
  GetCodePageEntry.HTML:=FXML.GetValue(GetKey(Index, 'ENTITIES'), '');
end;

function TCodePage.GetEntities(Index : integer): String;
begin
  if Assigned(FOwner) and Assigned(FOwner.FPrimary) and (Self<>FOwner.FPrimary)
  and CheckPrimary and (Index < 256) and Empty(Index, True) then begin
    GetEntities:=FOwner.FPrimary.Entities[Index];
  end else
    GetEntities:=Entry[Index].HTML;
end;

function TCodePage.GetUTF8(Index : integer): String;
begin
  if Assigned(FOwner) and Assigned(FOwner.FPrimary) and (Self<>FOwner.FPrimary)
  and CheckPrimary and (Index < 256) and Empty(Index, True) then begin
    GetUTF8:=FOwner.FPrimary.UTF8[Index];
  end else
    GetUTF8:=Entry[Index].UTF8;
end;

procedure TCodePage.SetCheckPrimary(AValue: boolean);
begin
  if FCheckPrimary=AValue then Exit;
  FCheckPrimary:=AValue;
end;

procedure TCodePage.SetCodePageEntry(Index : integer; AValue: TCodePageEntry);
begin
  if AValue.HTML = '' then
    FXML.DeleteValue(GetKey(Index, 'ENTITIES'))
  else
    FXML.SetValue(GetKey(Index, 'ENTITIES'), AValue.HTML);
  if AValue.UTF8 = '' then
    FXML.DeleteValue(GetKey(Index, 'UTF8'))
  else
    FXML.SetValue(GetKey(Index, 'UTF8'), UTF8ToInts(AValue.UTF8));
  if not Assigned(FOwner) then exit;
  if not Assigned(FOwner.FPrimary) then exit;
  if not CheckPrimary then exit;
  if Self=FOwner.FPrimary then exit;

end;

function TCodePage.NotEmpty(Index: integer; Strict : boolean = false): boolean;
begin
  NotEmpty:=True;
  if (not Strict) and (Index < 256) then Exit;
  if FXML.GetValue(GetKey(Index, 'UTF8'), '') <> '' then Exit;
  if FXML.GetValue(GetKey(Index, 'ENTITIES'), '') <> '' then Exit;
  NotEmpty:=False;
end;

function TCodePage.Empty(Index: integer; Strict : boolean = false): boolean;
begin
  Empty:=Not NotEmpty(Index, Strict); // Double negative :-)
end;

procedure TCodePage.SetCount(AValue: integer);
var
  I : integer;
begin
  if AValue < 256 then Exit;
  if FCount=AValue then Exit;
  FXML.SetValue('SUPPLEMENT_' + UnicodeString(ID) + '/COUNT', AValue - 256);
  for I := AValue to FCount - 1 do begin
    FXML.DeleteValue(GetKey(I, 'UTF8'));
    FXML.DeleteValue(GetKey(I, 'ENTITIES'))
  end;
  FCount:=AValue;
end;

procedure TCodePage.SetEntities(Index : integer; AValue: String);
var
  E : TCodePageEntry;
begin
  AValue:=StringReplace(AValue, '&', '', [rfReplaceAll]);
  AValue:=StringReplace(AValue, ';', ',', [rfReplaceAll]);
  AValue:=StringReplace(AValue, ',,', ',', [rfReplaceAll]);
  AValue:=ExcludeTrailing(',', AValue);
  AValue:=Trim(AValue);
  E:=Entry[Index];
  if E.HTML = AValue then exit;
  E.HTML:=AValue;
  Entry[Index]:=E;
end;

procedure TCodePage.SetFileName(AValue: String);
begin
  if FFileName=AValue then Exit;
  FFileName:=AValue;
end;

{$IFDEF LCL}
procedure TCodePage.SetFontFile(AValue: TFontFile);
begin
  if FFontFile=AValue then Exit;
  FFontFile:=AValue;
end;

procedure TCodePage.SetOwner(AValue: TCodePages);
begin
  if FOwner=AValue then Exit;
  FOwner:=AValue;
end;

{$ENDIF}

procedure TCodePage.SetUTF8(Index : integer; AValue: String);
var
  E : TCodePageEntry;
begin
  if AValue <> SPACE then
    AValue:=Trim(AValue);
  if AValue = ASCII[Index] then AValue:='';
  E:=Entry[Index];
  if E.UTF8 = AValue then exit;
  E.UTF8:=AValue;
  Entry[Index]:=E;
end;

function TCodePage.GetKey(Index: Integer; Attribute : String): String;
begin
  if Index > 255 then
    GetKey:='SUPPLEMENT_' + UnicodeString(ID) + '/x' + UnicodeString(IntToHex(Index-256,4)) +'/'+Attribute
  else
    GetKey:='CODEPAGE_' + UnicodeString(ID) + '/x' + UnicodeString(IntToHex(Index,2)) + '/' + Attribute
end;

constructor TCodePage.Create(AFileName : String);
var
  S: String;
begin
  inherited Create;
  FOwner := nil;
  FCount := 0;
  FCheckPrimary:=True;
  FFileName:=CodePageFilePath + AFileName;
  {$IFDEF LCL}
  FFontFile:=TFontFile.Create(FileChangeExt(AFileName, '.fnt'));
  {$ENDIF}
  FXML:=Nil;
  if FileExists(FFileName) then begin
    FXML:=TXMLConfig.Create(nil);
    try
      FXML.LoadFromFile(FFileName);
      S:=AnsiString(FXML.GetValue('CODEPAGES',''));
      if S <> ID then
        Raise Exception.Create('Multiple or Incorrect Codepage Identifier');
      FCount:=256+ FXML.GetValue('SUPPLEMENT_' + UnicodeString(ID) + '/COUNT', 0);
    except
      FreeAndNil(FXML);
    end;
  end;
end;

destructor TCodePage.Destroy;
begin
  if Assigned(FXML) then
    FreeAndNil(FXML);
  {$IFDEF LCL}
  if Assigned(FFontFile) then
    FreeAndNil(FFontFile);
  {$ENDIF}
  inherited Destroy;
end;

function TCodePage.AddMap: integer;
begin
  AddMap := FCount;
  SetCount(FCount + 1);
end;

procedure TCodePage.DeleteMap(Index: Integer);
var
  I : Integer;
begin
  if Index < 256 then Exit;
  for I := Index to FCount - 1 do begin
    UTF8[I] := UTF8[I+1];
    Entities[I] := Entities[I+1];
  end;
  SetCount(FCount - 1);
  Flush;
end;

procedure TCodePage.InsertMap(Index: Integer);
var
  I : integer;
begin
  if Index < 256 then Exit;
  for I := FCount - 1 downto Index do begin
    UTF8[I+1] := UTF8[I];
    Entities[I+1] := Entities[I];
  end;
  SetCount(FCount+1);
  UTF8[I] := '';
  Entities[I] := '';
end;

procedure TCodePage.Flush;
begin
  FXML.Flush;
end;

{$POP}

{ TCodePages }

function TCodePages.GetCount: integer;
begin
  GetCount:=Length(FCodePages);
end;

procedure TCodePages.SetActive(AValue: TCodePage);
begin
  if FActive=AValue then Exit;
  FActive:=AValue;
end;

function TCodePages.GetCodePage(Index : integer): TCodePage;
begin
  GetCodepage:=FCodePages[Index];
end;

procedure TCodePages.SetCodePage(Index : integer; AValue: TCodePage);
begin
  FCodePages[Index]:=AValue;
end;

procedure TCodePages.SetPrimary(AValue: TCodePage);
begin
  if FPrimary=AValue then Exit;
  FPrimary:=AValue;
end;

constructor TCodePages.Create;
begin
  inherited Create;
  FCodePages:=[];
  Clear;
end;

destructor TCodePages.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TCodePages.Clear;
var
  I : integer;
begin
  FActive:=Nil;
  FPrimary:=Nil;
  for I := 0 to Count - 1 do
    FCodePages[I].Free;
  SetLength(FCodePages, 0);
end;

procedure TCodePages.Reset;
var
  I : Integer;
  D : TStringList;
begin
  Clear;
  D := TStringList.Create;
  DirScan(D, CodePageFilePath + DirWildCard + '.xml');
  D.Sort;
  SetLength(FCodePages, D.Count);
  for I := 0 to D.Count - 1 do begin
    FCodePages[I] := TCodePage.Create(D[I]);
    if Assigned(FCodePages[I]) then
      FCodePages[I].FOwner:=Self;
  end;
  D.Free;
  Select(437);
  FPrimary:=FActive;
end;

function TCodePages.Find(ACodePage: integer): TCodePage;
begin
  Find:=Find(IntToStr(ACodePage));
end;

function TCodePages.Find(ACodePage: String): TCodePage;
var
  I : Integer;
begin
  ACodePage:=LowerCase(ACodePage);
  Find:=nil;
  for I := 0 to Count - 1 do
    if LowerCase(FCodePages[I].ID) = ACodePage then begin
      Find:=FCodePages[I];
      Break;
    end;
end;

function TCodePages.Select(ACodePage: integer): TCodePage;
begin
  Select:=Select(IntToStr(ACodePage));
end;

function TCodePages.Select(ACodePage: String): TCodePage;
begin
  FActive:=Find(ACodePage);
  if (not Assigned(FActive)) and (Count > 0) then
    FActive:=FCodePages[0];
  Select:=FActive;
end;

procedure TCodePages.Flush;
var
  I : integer;
begin
  for I := 0 to Count - 1 do
    FCodePages[I].Flush;
end;

initialization

  CodePageFilePath := IncludeTrailingPathDelimiter(AppBasePath + 'codepages');
  if Not DirectoryExists(CodePageFilePath) then
    CodePageFilePath := IncludeTrailingPathDelimiter(AppBasePath + 'codepage');
  if Not DirectoryExists(CodePageFilePath) then
    CodePageFilePath := IncludeTrailingPathDelimiter(AppBasePath);

  CodePages := TCodePages.Create;
  CodePages.Reset;

finalization

  CodePages.Free;

end.

