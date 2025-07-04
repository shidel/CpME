// Copyright (c) 2025 Jerome Shidel
// BSD-3-Clause license

unit DOScp;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, XMLConf, PASext {$IFDEF LCL}, DOSfont {$ENDIF};

type

  { TCodePage }

  TCodePage = class
  private
    FCount: integer;
    FXML : TXMLConfig;
    FFileName : String;
    {$IFDEF LCL}
    FFontFile : TFontFile;
    {$ENDIF}
    function GetAdditional(Index : integer): String;
    function GetASCII(Index : integer): String;
    function GetCode(Index : integer): String;
    function GetEmpty(Index : integer): boolean;
    function GetEntity(Index : integer): String;
    function GetID: String;
    function GetUTF8(Index : integer): String;
    procedure SetAdditional(Index : integer; AValue: String);
    procedure SetCode(Index : integer; AValue: String);
    function NotEmpty(Index : integer) : boolean;
    procedure SetEmpty(Index : integer);
    procedure SetEntity(Index : integer; AValue: String);
    procedure SetFileName(AValue: String);
    {$IFDEF LCL}
    procedure SetFontFile(AValue: TFontFile);
    {$ENDIF}
    procedure SetUTF8(Index : integer; AValue: String);
  protected
    function GetKey(Index : Integer; Attribute : String) : String;
  public
    constructor Create(AFileName : String);
    destructor Destroy; override;
    property FileName : String read FFileName write SetFileName;
    property ID : String read GetID;
    {$IFDEF LCL}
    property FontFile : TFontFile read FFontFile write SetFontFile;
    {$ENDIF}
    property Count : integer read FCount;
    property ASCII[Index : integer] : String read GetASCII;
    property UTF8[Index : integer] : String read GetUTF8 write SetUTF8;
    property Code[Index : integer] : String read GetCode write SetCode;
    property Entity[Index : integer] : String read GetEntity Write SetEntity;
    property Additional[Index : integer] : String read GetAdditional Write SetAdditional;
    property Empty[Index : integer] : boolean read GetEmpty;
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
    function GetCodePage(Index : integer): TCodePage;
    function GetCount: integer;
    procedure SetActive(AValue: TCodePage);
    procedure SetCodePage(Index : integer; AValue: TCodePage);
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Reset;
    property Count : integer read GetCount;
    property CodePage[Index : integer] : TCodePage read GetCodePage write SetCodePage; default;
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

implementation

var
  CodePageFilePath : string;

{ TCodePage }

{$PUSH}
{$WARN 4104 off : Implicit string type conversion from "$1" to "$2"}
{$WARN 4105 off : Implicit string type conversion with potential data loss from "$1" to "$2"}

function TCodePage.GetID: String;
begin
  GetID:=ExtractFileBase(FFileName);
end;

function TCodePage.GetASCII(Index : integer): String;
begin
  if (Index > 255) or (Index < 0) then
    GetASCII:=''
  else
    GetAscii:=Char(Index);
end;

function TCodePage.GetAdditional(Index : integer): String;
begin
  GetAdditional:=FXML.GetValue(GetKey(Index, 'MORE'), '');
end;

function TCodePage.GetCode(Index : integer): String;
begin
  GetCode:=FXML.GetValue(GetKey(Index, 'CODE'), '');
end;

function TCodePage.GetEmpty(Index : integer): boolean;
begin
  GetEmpty:=FXML.GetValue(GetKey(Index, 'EMPTY'), '') <> '';
end;

function TCodePage.GetEntity(Index : integer): String;
begin
  GetEntity:=FXML.GetValue(GetKey(Index, 'HTML'), '');
end;

function TCodePage.GetUTF8(Index : integer): String;
begin
  GetUTF8:=FXML.GetValue(GetKey(Index, 'UTF8'), '');
  if GetUTF8 = '' then
    GetUTF8:='' // ASCII[Index]
  else
    GetUTF8:=IntsToStr(GetUTF8);
  SetEmpty(Index);
end;

procedure TCodePage.SetAdditional(Index : integer; AValue: String);
begin
  AValue:=StringReplace(AValue, '&', '', [rfReplaceAll]);
  AValue:=StringReplace(AValue, ';', ',', [rfReplaceAll]);
  AValue:=StringReplace(AValue, ',,', ',', [rfReplaceAll]);
  AValue:=ExcludeTrailing(',', AValue);
  AValue:=Trim(AValue);
  if AValue = '' then
    FXML.DeleteValue(GetKey(Index, 'MORE'))
  else
    FXML.SetValue(GetKey(Index, 'MORE'), AValue);
  SetEmpty(Index);
end;

procedure TCodePage.SetCode(Index : integer; AValue: String);
begin
  AValue:=StringReplace(AValue, '&', '', [rfReplaceAll]);
  AValue:=StringReplace(AValue, '#', '', [rfReplaceAll]);
  AValue:=StringReplace(AValue, ';', ',', [rfReplaceAll]);
  AValue:=StringReplace(AValue, ',,', ',', [rfReplaceAll]);
  AValue:=ExcludeTrailing(',', AValue);
  AValue:=Trim(AValue);
  if AValue = '' then
    FXML.DeleteValue(GetKey(Index, 'CODE'))
  else
    FXML.SetValue(GetKey(Index, 'CODE'), AValue);
  SetEmpty(Index);
end;

function TCodePage.NotEmpty(Index: integer): boolean;
begin
  NotEmpty:=True;
  if Index < 256 then Exit;
  if FXML.GetValue(GetKey(Index, 'UTF8'), '') <> '' then Exit;
  if FXML.GetValue(GetKey(Index, 'CODE'), '') <> '' then Exit;
  if FXML.GetValue(GetKey(Index, 'HTML'), '') <> '' then Exit;
  if FXML.GetValue(GetKey(Index, 'MORE'), '') <> '' Then Exit;
  NotEmpty:=False;
end;

procedure TCodePage.SetEmpty(Index : integer);
begin
  if (Index >= FCount) or NotEmpty(Index) then
    FXML.DeleteValue(GetKey(Index, 'EMPTY'))
  else
    FXML.SetValue(GetKey(Index, 'EMPTY'), 'True');
end;

procedure TCodePage.SetEntity(Index : integer; AValue: String);
begin
  AValue:=StringReplace(AValue, '&', '', [rfReplaceAll]);
  AValue:=StringReplace(AValue, ';', ',', [rfReplaceAll]);
  AValue:=StringReplace(AValue, ',,', ',', [rfReplaceAll]);
  AValue:=ExcludeTrailing(',', AValue);
  AValue:=Trim(AValue);
  if AValue = '' then
    FXML.DeleteValue(GetKey(Index, 'HTML'))
  else
    FXML.SetValue(GetKey(Index, 'HTML'), AValue);
  SetEmpty(Index);
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
{$ENDIF}

procedure TCodePage.SetUTF8(Index : integer; AValue: String);
begin
  if AValue <> SPACE then
    AValue:=Trim(AValue);
  if AValue = ASCII[Index] then AValue:='';
  if AValue <> '' then AValue:=StrToInts(AValue);
  if AValue = '' then
    FXML.DeleteValue(GetKey(Index, 'UTF8'))
  else
    FXML.SetValue(GetKey(Index, 'UTF8'), AValue);
  SetEmpty(Index);
end;

function TCodePage.GetKey(Index: Integer; Attribute : String): String;
begin
  GetKey:='CODEPAGE/' + WhenTrue(Index > 255, 'MAP', 'ASCII') + '_' +
    IntToStr(Index) + '/' + Attribute;
end;

constructor TCodePage.Create(AFileName : String);
var
  S: String;
begin
  inherited Create;
  FCount := 0;
  FFileName:=CodePageFilePath + AFileName;
  {$IFDEF LCL}
  FFontFile:=TFontFile.Create(FileChangeExt(AFileName, '.fnt'));
  {$ENDIF}
  FXML:=Nil;
  if FileExists(FFileName) then begin
    FXML:=TXMLConfig.Create(nil);
    try
      FXML.LoadFromFile(FFileName);
      S:=FXML.GetValue('CODEPAGE/IDENTIFIER','');
      if S <> ID then
        FXML.SetValue('CODEPAGE/IDENTIFIER',ID);
      FCount:=256;
      While (FXML.GetValue(GetKey(FCount, 'UTF8'), '') <> '') or
      (FXML.GetValue(GetKey(FCount, 'CODE'), '') <> '') or
      (FXML.GetValue(GetKey(FCount, 'HTML'), '') <> '') or
      (FXML.GetValue(GetKey(FCount, 'MORE'), '') <> '') or
      (FXML.GetValue(GetKey(FCount, 'EMPTY'), '') <> '') do
        Inc(FCount);
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
  SetUTF8(FCount, Char(0));
  AddMap := FCount;
  Inc(FCount);
end;

procedure TCodePage.DeleteMap(Index: Integer);
var
  I : Integer;
begin
  if Index < 256 then Exit;
  Dec(FCount);
  for I := Index to FCount do begin
    UTF8[I] := UTF8[I+1];
    CODE[I] := CODE[I+1];
    Entity[I] := Entity[I+1];
    Additional[I] := Additional[I+1];
  end;
  FXML.DeleteValue(GetKey(FCount, 'UTF8'));
  FXML.DeleteValue(GetKey(FCount, 'CODE'));
  FXML.DeleteValue(GetKey(FCount, 'HTML'));
  FXML.DeleteValue(GetKey(FCount, 'MORE'));
  FXML.DeleteValue(GetKey(FCount, 'EMPTY'));
end;

procedure TCodePage.InsertMap(Index: Integer);
var
  I : integer;
begin
  if Index < 256 then Exit;
  for I := FCount - 1 downto Index do begin
    UTF8[I+1] := UTF8[I];
    CODE[I+1] := CODE[I];
    Entity[I+1] := Entity[I];
    Additional[I+1] := Additional[I];
  end;
  Inc(FCount);
  UTF8[I] := '';
  CODE[I] := '';
  Entity[I] := '';
  Additional[I] := '';
  FXML.SetValue(GetKey(I, 'EMPTY'), 'True');
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

constructor TCodePages.Create;
begin
  inherited Create;
  FCodePages:=[];
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
  for I := 0 to D.Count - 1 do
    FCodePages[I] := TCodePage.Create(D[I]);
  D.Free;
  Select(437);
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

