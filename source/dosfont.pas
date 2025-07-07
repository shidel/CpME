// Copyright (c) 2025 Jerome Shidel
// BSD-3-Clause license

unit DOSfont;

{$mode ObjFPC}{$H+}

interface

{$IFDEF LCL}
uses
  Classes, SysUtils, Graphics, Controls, ExtCtrls, PASext;

type

  TFontFile = class;

  { TFontChar }

  TFontChar = class
  private
    FIndex: integer;
    FOwner: TFontFile;
    function GetHeight: integer;
    function GetPixel(X : integer; Y: Integer): boolean;
    function GetWidth: integer;
    procedure SetPixel(X : integer; Y: Integer; AValue: boolean);
  protected
  public
    constructor Create(AOwner : TFontFile; AIndex : integer);
    destructor Destroy; override;
    property Owner : TFontFile read FOwner;
    property Index : integer read FIndex;
    property Width : integer read GetWidth;
    property Height : integer read GetHeight;
    property Pixel[X : integer; Y:Integer] : boolean read GetPixel write SetPixel;
    function asBitmap : TBitmap;
    procedure ToImage(var Image : TImage);
  published
  end;


  { TFontFile }

  TFontFile = class
  private
    FBackground: TColor;
    FFileName : String;
    FFontData: TArrayOfBytes;
    FFontChars : array [0..255] of TFontChar;
    FForeground: TColor;
    function GetCharacters(Index : integer): TFontChar;
    function GetHeight: integer;
    function GetWidth: integer;
    procedure SetBackground(AValue: TColor);
    procedure SetCharacters(Index : integer; AValue: TFontChar);
    procedure SetFileName(AValue: String);
    procedure SetFontData(AValue: TArrayOfBytes);
    procedure SetForeground(AValue: TColor);
  protected
    property FontData : TArrayOfBytes read FFontData write SetFontData;
  public
    constructor Create(AFileName : String);
    destructor Destroy; override;
    property FileName : String read FFileName write SetFileName;
    property Width : integer read GetWidth;
    property Height : integer read GetHeight;
    function Load : boolean;
    function Save : boolean;
    property Characters [Index : integer] : TFontChar read GetCharacters write SetCharacters;
    procedure ToImageList(var Images : TImageList);
  published
    property Foreground : TColor read FForeground write SetForeground;
    property Background : TColor read FBackground write SetBackground;
  end;

{$ENDIF}
implementation
{$IFDEF LCL}

var
  FontFilePath : String;

{ TFontFile }

procedure TFontFile.SetFileName(AValue: String);
begin
  if FFileName=AValue then Exit;
  FFileName:=AValue;
end;

function TFontFile.GetHeight: integer;
begin
  if Length(FFontData) = 0 then
    GetHeight := 0
  else
    GetHeight:=Length(FFontData) div 256;
end;

function TFontFile.GetCharacters(Index : integer): TFontChar;
begin
  GetCharacters:=FFontChars[Index];
end;

function TFontFile.GetWidth: integer;
begin
  if Length(FFontData) = 0 then
    GetWidth := 0
  else
    GetWidth := 8;
end;

procedure TFontFile.SetBackground(AValue: TColor);
begin
  if FBackGround=AValue then Exit;
  FBackGround:=AValue;
end;

procedure TFontFile.SetCharacters(Index : integer; AValue: TFontChar);
begin
  FFontChars[Index]:=AValue;
end;

procedure TFontFile.SetFontData(AValue: TArrayOfBytes);
begin
  if FFontData=AValue then Exit;
  FFontData:=AValue;
end;

procedure TFontFile.SetForeground(AValue: TColor);
begin
  if FForeground=AValue then Exit;
  FForeground:=AValue;
end;

constructor TFontFile.Create(AFileName: String);
var
  I : integer;
begin
  inherited Create;
  FForeground:=clBlack;
  FBackground:=clWhite;
  FFileName:=AFileName;
  FFontData:=[];
  for I := Low(FFontChars) to High(FFontChars) do
    FFontChars[I] := TFontChar.Create(Self, I);
  Load;
end;

destructor TFontFile.Destroy;
var
  I : integer;
begin
  for I := Low(FFontChars) to High(FFontChars) do
    FreeAndNil(FFontChars[I]);
  inherited Destroy;
end;

function TFontFile.Load: boolean;
begin
  Load := LoadBinary(FontFilePath + FFileName, FFontData, false) = 0;
end;

function TFontFile.Save: boolean;
begin
  Save := SaveBinary(FontFilePath + FFileName, FFontData, false) = 0;
end;

procedure TFontFile.ToImageList(var Images: TImageList);
var
  I : integer;
  B, X : TBitmap;
begin
  Images.Clear;
  for I := Low(FFontChars) to High(FFontChars) do
    try
      B := FFontChars[I].asBitmap;
      if (Images.Width = Width) and (Images.Height = Height) then
        Images.AddMasked(B, Background)
      else try
        X := TBitmap.Create;
        X.SetSize(Images.Width, Images.Height);
        X.Canvas.Brush.Color:=BackGround;
        X.Canvas.FillRect(0,0,X.Width, X.Height);
        X.Canvas.StretchDraw(Rect(0, 0, X.Width, X.Height), B);
        Images.AddMasked(X, Background);
      finally
        FreeAndNil(X);
      end;
    finally
      FreeAndNil(B);
    end;
end;

{ TFontChar }

function TFontChar.GetHeight: integer;
begin
  GetHeight:=FOwner.GetHeight;
end;

function TFontChar.GetPixel(X : integer; Y: Integer): boolean;
begin
  GetPixel := (FOwner.FFontData[(FIndex * Height) + Y] and (1 shl (7-x))) <> 0;
end;

function TFontChar.GetWidth: integer;
begin
  GetWidth:=FOwner.GetWidth;
end;

procedure TFontChar.SetPixel(X : integer; Y: Integer; AValue: boolean);
begin
  Y:=(FIndex * Height) + Y;
  X:=1 shl (7-x);
  if AValue then
    FOwner.FFontData[Y] := FOwner.FFontData[Y] or X
  else
    FOwner.FFontData[Y] := FOwner.FFontData[Y] and (not X);
end;

constructor TFontChar.Create(AOwner: TFontFile; AIndex: integer);
begin
  inherited Create;
  FOwner:=AOwner;
  FIndex:=AIndex;
end;

destructor TFontChar.Destroy;
begin
  inherited Destroy;
end;

function TFontChar.asBitmap: TBitmap;
var
  X, Y : integer;
begin
  asBitmap := TBitmap.Create;
  try
    asBitmap.SetSize(Width, Height);
    asBitmap.Canvas.Brush.Color:=Owner.BackGround;
    asBitmap.Canvas.FillRect(0,0,asBitmap.Width,asBitmap.Height);
    for Y := 0 to Height - 1 do
      for X := 0 to Width - 1 do
        if Pixel[X,Y] then
          asBitmap.Canvas.Pixels[X, Y] := Owner.Foreground;
  except
    FreeAndNil(asBitMap);
  end;
end;

procedure TFontChar.ToImage(var Image: TImage);
var
  B : TBitmap;
begin
  try
    B:=asBitmap;
    Image.Picture.Assign(B);
  finally
    FreeAndNil(B);
  end;
end;


initialization

  FontFilePath := IncludeTrailingPathDelimiter(AppBasePath + 'fonts');
  if Not DirectoryExists(FontFilePath) then
    FontFilePath := IncludeTrailingPathDelimiter(AppBasePath + 'font');
  if Not DirectoryExists(FontFilePath) then
    FontFilePath := IncludeTrailingPathDelimiter(AppBasePath);

finalization
{$ENDIF}
end.

