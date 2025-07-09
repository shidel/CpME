// Copyright (c) 2025 Jerome Shidel
// BSD-3-Clause license

program genmaps;

{$mode objfpc}{$H+}

uses Classes, SysUtils, XMLConf, PASext; // DOScp, DOSfont;

var
  UTF8, HTML, ASCII, Info : TStringList;
  Data : String;
  ID : String;
  App_Build: String;
  DAC : TArrayOfStrings;

type
  TEntry = record
    Index : integer;
    UTF8 : UnicodeString;
    HTML : TArrayOfUnicode;
  end;

procedure Prepare;
begin
  App_Build:=BUILD_DATE;
  App_Build:='// Created by CpME' +QUOTE +'s map creator v'
  +PopDelim(App_Build) + LF +
  '// https://github.com/shidel/CpME/' + LF;
  DAC:=[];
  SetLength(DAC, 256);
  UTF8:=TStringList.Create;
  UTF8.Duplicates:=dupIgnore;
  UTF8.Sorted:=True;
  UTF8.CaseSensitive:=True;
  HTML:=TStringList.Create;
  HTML.Duplicates:=dupIgnore;
  HTML.Sorted:=True;
  HTML.CaseSensitive:=True;
  ASCII:=TStringList.Create;
  ASCII.Duplicates:=dupIgnore;
  ASCII.Sorted:=True;
  ASCII.CaseSensitive:=True;
  Info:=TStringList.Create;
  Info.Duplicates:=dupIgnore;
  Info.Sorted:=True;
  Info.CaseSensitive:=True;
end;

procedure Finish;
begin
  Info.Free;
  ASCII.Free;
  HTML.Free;
  UTF8.Free;
end;

function Key(Index : Integer; Attribute : UnicodeString) : UnicodeString;
begin
  if Index > 255 then
    Key:='SUPPLEMENT_' + UnicodeString(ID) + '/x' + UnicodeString(IntToHex(Index-256,4)) +'/'+Attribute
  else
    Key:='CODEPAGE_' + UnicodeString(ID) + '/x' + UnicodeString(IntToHex(Index,2)) + '/' + Attribute
end;

function ReadEntry(var X : TXMLConfig; Index : Integer; Out Entry : TEntry): boolean;
var
  L, T : UnicodeString;
begin
  Entry.Index:=Index;
  Entry.HTML:=[];
  Entry.UTF8:=X.GetValue(Key(Index, 'UTF8'),'');
  L:=Trim(X.GetValue(Key(Index, 'ENTITIES'),''));
  while L <> '' do begin
    T:=Trim(PopDelim(L, COMMA));
    if T <> '' then
      AddToArray(Entry.HTML, T);
  end;
  if (Entry.UTF8 = '') and (Length(Entry.HTML)>0) and (Index < 256) then
    Entry.UTF8:=UnicodeString(ValueToCodePoint(Index));
  ReadEntry:=(Entry.UTF8<>'') or (Length(Entry.HTML)>0) or (Index < 256);
end;

procedure AddUTF8(const Entry : TEntry);
var
  S : UnicodeString;
  I : Integer;
begin
  S := Entry.UTF8 + '/';
  for I := 0 to Length(Entry.HTML) - 1 do begin
    if I > 0 then S := S + '/';
    S := S + Entry.HTML[I];
  end;
  UTF8.Add(AnsiString(S));
end;

procedure AddASCII(const Entry : TEntry; CodePage : String);
var
  V, E : integer;
begin
  if (Entry.Index > 255) then exit;
  Val(CodePage, V, E);
  if (E <> 0) or (V >= 900000) then exit;
  ASCII.Add(AnsiString(Entry.UTF8) + '/' + CodePage ); // + ',' + IntToStr(Entry.Index));
end;

procedure AddHTML(const Entry : TEntry);
var
  I : Integer;
begin
  for I := 0 to Length(Entry.HTML) - 1 do
    HTML.Add(AnsiString(Entry.HTML[I] + '/' + Entry.UTF8));
end;

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


procedure AddInfo(const Entry : TEntry);
var
  I : Integer;
begin
  for I := 0 to Length(Entry.HTML) - 1 do
    Info.Add(IntsToUTF8(AnsiString(Entry.UTF8)) + TAB + '&' + AnsiString(Entry.HTML[I]) + ';');
end;

procedure ReadXML(FileName : String);
var
  X : TXMLConfig;
  I : Integer;
  E : TEntry;
  C, N: String;
  K : UnicodeString;
  V, T, M : Integer;
begin
  N := ExtractFileBase(FileName);
  ID:=UpperCase(N);
  C := N;
  Val(C, V, T);
  // if T <> 0 then Exit;
  if (V >= 900000) or (T <> 0) then begin
    C:='SUP';
    N:=N+' Supplemental';
  end;
  Data := '// DOS Codepage to UTF-8 conversion map' + LF +
  App_Build +LF + LF+
  '{$DEFINE CP' + C + 'toUTF8Remap}' + LF +
  'const' + LF + '  CP' + C + 'toUTF8RemapList : TArrayOfLongInts = (' + LF;
  WriteLn('Reading Codepage ', N, ' XML mazpping file.');
  X := TXMLConfig.Create(nil);
  try
    X.Filename:=FileName;
    M:=256+X.GetValue('SUPPLEMENT_' + UnicodeString(ID) + '/COUNT', 0);
    for I := 0 to M -1 do begin
      ReadEntry(X, I, E);
      K :=Trim(E.UTF8);
      if (I < 256) then begin
        K:=UnicodeString(WhenTrue(AnsiString(K),AnsiString(K), '-1'));
        if ID='437' then
          DAC[I] := AnsiString(K)
        else if K = '-1' then
          K:=UnicodeString(DAC[I]);
        Data:=Data + '    ' +
        '{' + IntToStr(I) + '} ' +
        TAB +
        AnsiString(K) + WhenTrue(I<255, ',') +
        LF;
        Exchange(E.UTF8, K);
        AddASCII(E, C);
      end;
      if (E.UTF8='') or (Length(E.HTML)=0) then continue;
      AddUTF8(E);
      AddHTML(E);
      // if V <= 900000 then
        AddInfo(E);
    end;
    Data:=Data+ '  );' + LF + LF;
    if C <> 'SUP' then
      SaveBinary('map_' + C + '.inc', Data);
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
  ReadXML('codepages/437.xml');
  for I := 0 to Length(D) - 1 do
    if D[I] <> '437.xml' then
      ReadXML('codepages/' + D[I]);
end;

function EntryTypeDef : String;
begin
  EntryTypeDef := LF +
  '{$IFNDEF TextRemapEntries}' + LF +
  '{$DEFINE TextRemapEntries}' + LF +
  'type' + LF +
  '  TTextRemapEntry = record' + LF +
  '    Value : AnsiString;' + LF +
  '    Data : AnsiString;' + LF +
  '  end;' + LF +
  '  TTextRemapEntries = array of TTextRemapEntry;' + LF +
  '{$ENDIF}' + LF + LF;
end;

procedure AddItem(D : String; RPad:Integer; AllData : boolean = false);
var
  T, S : String;
begin
  if D = '' then exit;
  T := PopDelim(D, '/');
  if Not AllData then
    D := PopDelim(D, '/'); // this will discard any remaining delimited fields
  S := '    (Value:' + RightPad(QUOTE + T + QUOTE + SEMICOLON, RPad) + SPACE +
  'Data:' + QUOTE + D + QUOTE + ')';
  if Data <> '' then
    Data := Data + ',' + LF + S
  else
    Data:=S;
end;

procedure ItemsUTF8(L, H : integer);
var
  M : integer;
begin
  if H < L then Exit;
  M := L + (H - L) div 2;
  AddItem(UTF8[M], 16);
  if L = H then Exit;
  if L < M then ItemsUTF8(L, M-1);
  if M < H then ItemsUTF8(M + 1, H);
end;

procedure ItemsHTML(L, H : integer);
var
  M : integer;
begin
  if H < L then Exit;
  M := L + (H - L) div 2;
  AddItem(HTML[M],36);
  if L = H then Exit;
  if L < M then ItemsHTML(L, M-1);
  if M < H then ItemsHTML(M + 1, H);
end;

procedure ItemsASCII(L, H : integer);
var
  M : integer;
begin
  if H < L then Exit;
  M := L + (H - L) div 2;
  AddItem(ASCII[M], 16, True);
  if L = H then Exit;
  if L < M then ItemsASCII(L, M-1);
  if M < H then ItemsASCII(M + 1, H);
end;

procedure SaveUTF8(Filename : String);
begin
  Data := '';
  ItemsUTF8(0, UTF8.Count - 1);
  Data := '// UTF-8 to HTML conversion map' + LF +
  App_Build +LF +
  EntryTypeDef + LF +
  '{$DEFINE UTF8toHTMLRemap}' + LF +
  'const' + LF +
  '  UTF8toHTMLRemapList : TTextRemapEntries = (' + LF +
  Data + LF +
  '  );' + LF + LF;
  SaveBinary(Filename, Data);
end;

procedure SaveHTML(Filename : String);
begin
  Data := '';
  ItemsHTML(0, HTML.Count - 1);
  Data := '// HTML to UTF-8 conversion map' + LF +
  App_Build +LF +
  EntryTypeDef + LF +
  '{$DEFINE HTMLtoUTF8Remap}' + LF +
  'const' + LF +
  '  HTMLtoUTF8RemapList : TTextRemapEntries = (' + LF +
  Data + LF +
  '  );' + LF + LF;
  SaveBinary(Filename, Data);
end;

procedure SaveASCII(Filename : String);
begin
  Data := '';
  ItemsASCII(0, ASCII.Count - 1);
  Data := '// UTF-8 to ASCII compatibility map' + LF +
  App_Build +LF +
  EntryTypeDef + LF +
  '{$DEFINE UTF8toASCIIRemap}' + LF +
  'const' + LF +
  '  UTF8toASCIIRemapList : TTextRemapEntries = (' + LF +
  Data + LF +
  '  );' + LF + LF;
  SaveBinary(Filename, Data);
end;

procedure SaveInfo(Filename : String);
var
  I, N : Integer;
  S : String;
  L, D : String;
  C, X: String;
begin
  S :='<!DOCTYPE html>' + LF +
  '<html lang="en">' + LF +
  '<head>' + LF +
  '<title>Codepage Map Summary</title>' + LF +
  '<meta http-equiv="content-type" content="text/html; charset=utf-8">' + LF +
  '<style>' + LF +
  'body { white-space:pre; font-size:200%; }' + LF +
  'span.names { white-space:default; font-size:20%; }' + LF +
  '</style>' + LF +
  '</head>' + LF +
  '<body>';
  L := '';
  D := '';
  N := 0;
  for I := 0 to Info.Count - 1 do begin
    X:=Info[I];
    C := PopDelim(X, TAB);
    if C = '' then Continue;
    if C <> L then begin
      S := S + D + LF; // + '<span class="names">' + E + '</span>' + LF;
      D := ZeroPad(N, 4) + ':' + TAB + C;
      Inc(N);
      L := C;
    end;
    D := D + TAB + X;
  end;
  S := S + D + LF; // + '<span class="names">' + E + '</span>' + LF;
  S := S + '</body>' + LF;
  SaveBinary(Filename, S);
end;

procedure ShrinkASCII;
var
  I : integer;
  L, T, D : String;
begin
  ASCII.Sorted:=False;
  I := 0;
  L := '';
  while I < ASCII.Count do begin
    D := ASCII[I];
    T := PopDelim(D, '/');
    if T = L then begin
      if D <> '' then
        ASCII[I-1] := ASCII[I-1] + '/' + D;
      ASCII.Delete(I);
    end else begin
      L := T;
      Inc(I);
    end;
  end;
  ASCII.Sorted:=True;
end;


procedure SaveMaps;
begin
  SaveUTF8('map_utf8.inc');
  SaveHTML('map_html.inc');
  SaveASCII('map_uchk.inc');
  SaveInfo('summary.html');
end;

procedure Summary;
begin
  WriteLn('UTF-8 to HTML Entries: ', UTF8.Count);
  WriteLn('HTML to UTF-8 Entries: ', HTML.Count);
end;

begin
  Prepare;
  ReadAllData;
  ShrinkASCII;
  SaveMaps;
  Summary;
  Finish;
end.

