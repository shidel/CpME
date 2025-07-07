// Copyright (c) 2025 Jerome Shidel
// BSD-3-Clause license

program test;

{$mode objfpc}{$H+}

uses Classes, SysUtils, PASext, XMLConf;

var
  BX : TXMLConfig;
  CP : String;

type
  TOldEntry = record
    Empty : Boolean;
    ASCII : Integer;
    OKDOS : boolean;
    UTF8 : UnicodeString;
    CODE : UnicodeString;
    HTML : TArrayOfStrings;
  end;


function OldKey(Index : Integer; Attribute : UnicodeString) : UnicodeString;
var
  T : UnicodeString;
begin
  T := '_' + UnicodeString(IntToStr(Index)) + '/';
  if Index > 255 then
    OldKey:='CODEPAGE/MAP' + T + Attribute
  else
    OldKey:='CODEPAGE/ASCII' + T + Attribute
end;

function Exists(var X : TXMLConfig; Index: integer): boolean;
begin
  Exists:=True;
  if X.GetValue(OldKey(Index, 'UTF8'), '') <> '' then Exit;
  if X.GetValue(OldKey(Index, 'CODE'), '') <> '' then Exit;
  if X.GetValue(OldKey(Index, 'HTML'), '') <> '' then Exit;
  if X.GetValue(OldKey(Index, 'MORE'), '') <> '' Then Exit;
  if X.GetValue(OldKey(Index, 'EMPTY'), '') <> '' Then Exit;
  Exists:=False;
end;

function Needed(var X : TXMLConfig; Index: integer): boolean;
begin
  Needed:=Exists(X,Index);
  if X.Filename = BX.Filename then exit;
  if X.GetValue(OldKey(Index, 'UTF8'), '') <> BX.GetValue(OldKey(Index, 'UTF8'), '') then Exit;
  if X.GetValue(OldKey(Index, 'CODE'), '') <> BX.GetValue(OldKey(Index, 'CODE'), '') then Exit;
  if X.GetValue(OldKey(Index, 'HTML'), '') <> BX.GetValue(OldKey(Index, 'HTML'), '') then Exit;
  if X.GetValue(OldKey(Index, 'MORE'), '') <> BX.GetValue(OldKey(Index, 'MORE'), '') Then Exit;
  Needed:=False;
end;

function OldReadEntry(var X : TXMLConfig; Index : Integer; Out Entry : TOldEntry): boolean;
var
  L, T : UnicodeString;
begin
  Entry.ASCII:=Index;
  Entry.OKDOS:=False;
  Entry.HTML:=[];
  Entry.Empty:=X.GetValue(OldKey(Index, 'EMPTY'),'') <> '';
  Entry.UTF8:=X.GetValue(OldKey(Index, 'UTF8'),'');
  Entry.CODE:=X.GetValue(OldKey(Index, 'CODE'),'');
  if Entry.CODE <> '' then begin
    Entry.CODE:=UnicodeString(StringReplace(Trim(AnsiString(Entry.CODE)), COMMA, ';&#', [rfReplaceAll]));
    // Entry.CODE:='&#' + Entry.CODE + ';';
  end;
  T:=Trim(X.GetValue(OldKey(Index, 'HTML'),''));
  if T <> '' then
    AddToArray(Entry.HTML, AnsiString(T)); // '&' + T + ';');
  L:=X.GetValue(OldKey(Index, 'MORE'),'');
  while L <> '' do begin
    T:=Trim(PopDelim(L, COMMA));
    if T <> '' then
      AddToArray(Entry.HTML, AnsiString(T)); // '&' + T + ';');
  end;
  if (Entry.UTF8 = '') and ((Entry.Code<>'') or (Length(Entry.HTML)>0)) and (Index < 256) then
  begin
    Entry.OKDOS:=TRUE;
    Entry.UTF8:=UnicodeString(IntToStr(Index));
  end;
  Entry.UTF8:=UnicodeString(IntsToStr(AnsiString(Entry.UTF8)));
  OldReadEntry:=(Entry.UTF8<>'') or (Entry.Code<>'') or (Length(Entry.HTML)>0)
    or (Index < 256) or (Entry.Empty);
end;

function NewKey(Index : Integer; Attribute : UnicodeString) : UnicodeString;
begin
  if Index > 255 then
    NewKey:='SUPPLEMENT_' + UnicodeString(CP) + '/x' + UnicodeString(IntToHex(Index-256,4)) +'/'+Attribute
  else
    NewKey:='CODEPAGE_' + UnicodeString(CP) + '/x' + UnicodeString(IntToHex(Index,2)) + '/' + Attribute
end;

function DecodeUTF8(S : AnsiString) : AnsiString;
var
  L : integer;
begin
  DecodeUTF8:='';
  while S <> '' do begin
    L := CodePointLength(S);
    DecodeUTF8:=DecodeUTF8+IntToStr(CodePointToValue(S)) + COMMA;
    Delete(S, 1, L);
  end;
  DecodeUTF8:=ExcludeTrailing(COMMA,DecodeUTF8);
end;

procedure ConvertXML(var OX, NX : TXMLConfig);
var
  I : integer;
  O : TOldEntry;
  S, V : String;
begin
  I := 0;
  while (I <256) or Exists(OX, I) do begin
    if Needed(OX, I) then begin
       OldReadEntry(OX, I, O);
       S := Implode(O.HTML, ',');
       V:=DecodeUTF8(AnsiString(O.UTF8));
       WriteLn(I, ' "', O.UTF8, '" ', V, ' ', S);
       NX.SetValue(NewKey(I, 'UTF8'), UnicodeString(V));
       if S <> '' then
         NX.SetValue(NewKey(I, 'ENTITIES'), UnicodeString(S));
    end;
    Inc(I);
  end;
  if I > 256 then
    NX.SetValue('SUPPLEMENT_' + UnicodeString(CP) + '/COUNT', I-256);

end;

procedure ConvertAll;
var
  D : TArrayOfStrings;
  I : Integer;
  OX, NX : TXMLConfig;
begin
  BX := TXMLConfig.Create(nil);
  BX.Filename := 'codepages/437.xml';
  DirScan(D, 'codepages/*');
  for I := 0 to Length(D) -1 do begin
    // DeleteFile(D[I]);
    WriteLn(D[I]);
    CP:=UpperCase(ExtractFileBase(D[I]));
    NX := TXMLConfig.Create(nil);
    if D[I] = '437.xml' then
      OX := BX
    else begin
      OX := TXMLConfig.Create(nil);
      OX.Filename := 'codepages/' + D[I];
    end;
    NX.Filename := D[I];
    NX.SetValue('CODEPAGES', UnicodeString(CP));
    ConvertXML(OX, NX);
    if D[I] <> '437.xml' then
      OX.Free;
    NX.Free;
  end;
  BX.Free;
end;

begin
  ConvertAll;
end.

