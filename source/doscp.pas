unit DOScp;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, XMLConf, PASext, DOSfont;

type

  { TCodePage }

  TCodePage = class
  private
    FCount: integer;
    FXML : TXMLConfig;
    FFileName : String;
    FFontFile : TFontFile;
    function GetAdditional(Index : integer): String;
    function GetASCII(Index : integer): String;
    function GetCode(Index : integer): String;
    function GetEntity(Index : integer): String;
    function GetID: String;
    function GetUTF8(Index : integer): String;
    procedure SetAdditional(Index : integer; AValue: String);
    procedure SetCode(Index : integer; AValue: String);
    procedure SetEntity(Index : integer; AValue: String);
    procedure SetFileName(AValue: String);
    procedure SetFontFile(AValue: TFontFile);
    procedure SetUTF8(Index : integer; AValue: String);
  protected

  public
    constructor Create(AFileName : String);
    destructor Destroy; override;
    property FileName : String read FFileName write SetFileName;
    property ID : String read GetID;
    property FontFile : TFontFile read FFontFile write SetFontFile;
    property Count : integer read FCount;
    property ASCII[Index : integer] : String read GetASCII;
    property UTF8[Index : integer] : String read GetUTF8 write SetUTF8;
    property Code[Index : integer] : String read GetCode write SetCode;
    property Entity[Index : integer] : String read GetEntity Write SetEntity;
    property Additional[Index : integer] : String read GetAdditional Write SetAdditional;
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
  GetAdditional:=FXML.GetValue('CODEPAGE/ASCII_' + IntToStr(Index) + '/MORE', '');
end;

function TCodePage.GetCode(Index : integer): String;
begin
  GetCode:=FXML.GetValue('CODEPAGE/ASCII_' + IntToStr(Index) + '/CODE', '');
end;

function TCodePage.GetEntity(Index : integer): String;
begin
  GetEntity:=FXML.GetValue('CODEPAGE/ASCII_' + IntToStr(Index) + '/HTML', '');
end;

function TCodePage.GetUTF8(Index : integer): String;
begin
  GetUTF8:=FXML.GetValue('CODEPAGE/ASCII_' + IntToStr(Index) + '/UTF8', '');
  if GetUTF8 = '' then
    GetUTF8:='' // ASCII[Index]
  else
    GetUTF8:=IntsToStr(GetUTF8);
end;

procedure TCodePage.SetAdditional(Index : integer; AValue: String);
begin
  AValue:=StringReplace(AValue, '&', '', [rfReplaceAll]);
  AValue:=StringReplace(AValue, ';', ',', [rfReplaceAll]);
  AValue:=StringReplace(AValue, ',,', ',', [rfReplaceAll]);
  AValue:=ExcludeTrailing(',', AValue);
  AValue:=Trim(AValue);
  if AValue = '' then
    FXML.DeleteValue('CODEPAGE/ASCII_' + IntToStr(Index) + '/MORE')
  else
    FXML.SetValue('CODEPAGE/ASCII_' + IntToStr(Index) + '/MORE', AValue);
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
    FXML.DeleteValue('CODEPAGE/ASCII_' + IntToStr(Index) + '/CODE')
  else
    FXML.SetValue('CODEPAGE/ASCII_' + IntToStr(Index) + '/CODE', AValue);
end;

procedure TCodePage.SetEntity(Index : integer; AValue: String);
begin
  AValue:=StringReplace(AValue, '&', '', [rfReplaceAll]);
  AValue:=StringReplace(AValue, ';', ',', [rfReplaceAll]);
  AValue:=StringReplace(AValue, ',,', ',', [rfReplaceAll]);
  AValue:=ExcludeTrailing(',', AValue);
  AValue:=Trim(AValue);
  if AValue = '' then
    FXML.DeleteValue('CODEPAGE/ASCII_' + IntToStr(Index) + '/HTML')
  else
    FXML.SetValue('CODEPAGE/ASCII_' + IntToStr(Index) + '/HTML', AValue);
end;

procedure TCodePage.SetFileName(AValue: String);
begin
  if FFileName=AValue then Exit;
  FFileName:=AValue;
end;

procedure TCodePage.SetFontFile(AValue: TFontFile);
begin
  if FFontFile=AValue then Exit;
  FFontFile:=AValue;
end;

procedure TCodePage.SetUTF8(Index : integer; AValue: String);
begin
  AValue:=Trim(AValue);
  if AValue = ASCII[Index] then AValue:='';
  if AValue <> '' then AValue:=StrToInts(AValue);
  if AValue = '' then
    FXML.DeleteValue('CODEPAGE/ASCII_' + IntToStr(Index) + '/UTF8')
  else
    FXML.SetValue('CODEPAGE/ASCII_' + IntToStr(Index) + '/UTF8', AValue);
end;

constructor TCodePage.Create(AFileName : String);
var
  S: String;
begin
  inherited Create;
  FCount := 0;
  FFileName:=CodePageFilePath + AFileName;
  FFontFile:=TFontFile.Create(FileChangeExt(AFileName, '.fnt'));
  FXML:=Nil;
  if FileExists(FFileName) then begin
    FXML:=TXMLConfig.Create(nil);
    try
      FXML.LoadFromFile(FFileName);
      S:=FXML.GetValue('CODEPAGE/IDENTIFIER','');
      if S <> ID then
        FXML.SetValue('CODEPAGE/IDENTIFIER',ID);
      FCount:=256;
      While FXML.GetValue('CODEPAGE/ASCII_' + IntToStr(FCount) + '/UTF8', '') <> '' do
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
  if Assigned(FFontFile) then
    FreeAndNil(FFontFile);
  inherited Destroy;
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

