program genmaps;

{$mode objfpc}{$H+}

uses Classes, SysUtils, XMLConf, PASext;

var
  UTF8, HTML, Show : TStringList;

type
  TEntry = record
    ASCII : Integer;
    UTF8 : UnicodeString;
    CODE : UnicodeString;
    HTML : TArrayOfUnicode;
  end;

procedure Prepare;
begin
  UTF8:=TStringList.Create;
  UTF8.Duplicates:=dupIgnore;
  UTF8.Sorted:=True;
  HTML:=TStringList.Create;
  HTML.Duplicates:=dupIgnore;
  HTML.Sorted:=True;
  Show:=TStringList.Create;
  Show.Duplicates:=dupIgnore;
  Show.Sorted:=True;
end;

procedure Finish;
begin
  Show.Free;
  HTML.Free;
  UTF8.Free;
end;

function Key(Index : Integer; Attribute : UnicodeString) : UnicodeString;
var
  T : UnicodeString;
begin
  T := '_' + UnicodeString(IntToStr(Index)) + '/';
  if Index > 255 then
    Key:='CODEPAGE/MAP' + T + Attribute
  else
    Key:='CODEPAGE/ASCII' + T + Attribute
end;

function ReadEntry(var X : TXMLConfig; Index : Integer; Out Entry : TEntry): boolean;
var
  L, T : UnicodeString;
begin
  Entry.ASCII:=Index;
  Entry.HTML:=[];
  Entry.UTF8:=X.GetValue(Key(Index, 'UTF8'),'');
  Entry.CODE:=X.GetValue(Key(Index, 'CODE'),'');
  if Entry.CODE <> '' then
    Entry.Code := '&#' + Entry.Code + ';';
  T:=Trim(X.GetValue(Key(Index, 'HTML'),''));
  if T <> '' then
    AddToArray(Entry.HTML, '&' + T + ';');
  L:=X.GetValue(Key(Index, 'MORE'),'');
  while L <> '' do begin
    T:=Trim(PopDelim(L, COMMA));
    if T <> '' then
      AddToArray(Entry.HTML, '&' + T + ';');
  end;
  ReadEntry:=((Entry.UTF8<>'') and ((Entry.Code<>'') or (Length(Entry.HTML)>0)))
    or (Index <= 255);
end;

procedure AddUTF8(const Entry : TEntry);
var
  S : UnicodeString;
  I : Integer;
begin
  S := Entry.UTF8 + '/' + Entry.Code + '/';
  for I := 0 to Length(Entry.HTML) - 1 do begin
    if I > 0 then S := S + '/';
    S := S + Entry.HTML[I];
  end;
  UTF8.Add(AnsiString(S));
end;

procedure AddHTML(const Entry : TEntry);
var
  I : Integer;
begin
  if Entry.Code <> '' then
    HTML.Add(AnsiString(Entry.Code + '/' + Entry.UTF8));
  for I := 0 to Length(Entry.HTML) - 1 do
    HTML.Add(AnsiString(Entry.HTML[I] + '/' + Entry.UTF8));
end;

procedure AddShow(const Entry : TEntry);
var
  I : Integer;
begin
  if Entry.Code <> '' then
    Show.Add(AnsiString(IntsToUnicode(Entry.UTF8) + TAB + Entry.Code));
  for I := 0 to Length(Entry.HTML) - 1 do
    Show.Add(AnsiString(IntsToUnicode(Entry.UTF8) + TAB + Entry.HTML[I]));
end;

procedure ReadXML(FileName : String);
var
  X : TXMLConfig;
  I : Integer;
  E : TEntry;
begin
  WriteLn('Read Codepage XML: ', FileName);
  X := TXMLConfig.Create(nil);
  try
    X.Filename:=FileName;
    I := 0;
    while ReadEntry(X, I, E) do begin
      Inc(I);
      if (E.UTF8='') or ((E.CODE='') and (Length(E.HTML)=0)) then continue;
      AddUTF8(E);
      AddHTML(E);
      AddShow(E);
      Inc(I);
    end;
  finally
    X.Free;
  end;
end;

procedure ReadAllData;
var
  D : TArrayOfStrings;
  I : integer;
begin
  DirScanByName(D, 'codepages/' + DirWildCard);
  for I := 0 to Length(D) - 1 do
    ReadXML('codepages/' + D[I]);
end;

procedure SaveMaps;
begin
end;

procedure Summary;
var
  I : Integer;
begin
  for I := 0 to Show.Count - 1 do
    WriteLn(Show[I]);
  WriteLn(UTF8.Count, '/', HTML.Count, '/', Show.Count);
end;

begin
  Prepare;
  ReadAllData;
  SaveMaps;
  Summary;
  Finish;
end.

