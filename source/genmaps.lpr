// Copyright (c) 2025 Jerome Shidel
// BSD-3-Clause license

program genmaps;

{$mode objfpc}{$H+}

uses Classes, SysUtils, XMLConf, PASext;

var
  UTF8, HTML, ASCII, Info : TStringList;
  Data : String;

type
  TEntry = record
    Empty : Boolean;
    ASCII : Integer;
    OKDOS : boolean;
    UTF8 : UnicodeString;
    VALS : TArrayOfBytes;
    CODE : UnicodeString;
    HTML : TArrayOfUnicode;
  end;

procedure Prepare;
begin
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
var
  T : UnicodeString;
begin
  T := '_' + UnicodeString(IntToStr(Index)) + '/';
  if Index > 255 then
    Key:='CODEPAGE/MAP' + T + Attribute
  else
    Key:='CODEPAGE/ASCII' + T + Attribute
end;

function SetVALS(var Entry : TEntry) : boolean;
var
  T, N : UnicodeString;
  V, E: Integer;
  B : Boolean;
  D : TArrayOfBytes;
begin
  SetVALS:=False;
  D := [];
  if Entry.UTF8<>'' then begin
    T := Entry.UTF8;
    while T <> '' do begin
      N:=Trim(PopDelim(T, SPACE));
      if N = '' then Exit;
      Val(N, V, E);
      if E <> 0 then Exit;
      SetLength(D, Length(D)+1);
      D[High(D)]:=V;
    end;
    if Length(D) > 0 then begin
      B:=False;
      // This was done completely by guess work assuming there had to be some
      // sort of logical pattern to the length of UTF-8 Characters. At present,
      // it seems correct when tested against the thousands of UTF-8 equivalent
      // to the Named HTML entities. But, you never know.
      if (D[Low(D)] = $7f) then // special 127, basically ignored followed by 3
        B := Length(D) = 4
      else if (D[Low(D)] and $f0) = $f0 then
        B := Length(D) = 4
      else if (D[Low(D)] and $e0) = $e0 then
        B := Length(D) = 3
      else if (D[Low(D)] and $e0) = $c0 then
        B := Length(D) = 2
      else if (D[Low(D)] and $80) = $00 then
        B := Length(D) = 1;
      if not B then
        WriteLn('Invalid UTF-8 code: ', IntToHex(D[Low(D)]), ' ', IntToBin(D[Low(D)]), ' ', Entry.UTF8);
      if not B then Exit;
    end;
  end;

  Entry.VALS:=D;
  SetVALS:=True;
end;

function ReadEntry(var X : TXMLConfig; Index : Integer; Out Entry : TEntry): boolean;
var
  L, T : UnicodeString;
begin
  Entry.ASCII:=Index;
  Entry.OKDOS:=False;
  Entry.HTML:=[];
  ENTRY.VALS:=[];
  Entry.Empty:=X.GetValue(Key(Index, 'EMPTY'),'') <> '';
  Entry.UTF8:=X.GetValue(Key(Index, 'UTF8'),'');
  Entry.CODE:=X.GetValue(Key(Index, 'CODE'),'');
  if Entry.CODE <> '' then begin
    Entry.CODE:=UnicodeString(StringReplace(Trim(AnsiString(Entry.CODE)), COMMA, ';&#', [rfReplaceAll]));
    Entry.CODE:='&#' + Entry.CODE + ';';
  end;
  T:=Trim(X.GetValue(Key(Index, 'HTML'),''));
  if T <> '' then
    AddToArray(Entry.HTML, '&' + T + ';');
  L:=X.GetValue(Key(Index, 'MORE'),'');
  while L <> '' do begin
    T:=Trim(PopDelim(L, COMMA));
    if T <> '' then
      AddToArray(Entry.HTML, '&' + T + ';');
  end;
  if (Entry.UTF8 = '') and ((Entry.Code<>'') or (Length(Entry.HTML)>0)) and (Index < 256) then
  begin
    Entry.OKDOS:=TRUE;
    Entry.UTF8:=UnicodeString(IntToStr(Index));
  end;
  SetVALS(Entry);
  ReadEntry:=(Entry.UTF8<>'') or (Entry.Code<>'') or (Length(Entry.HTML)>0)
    or (Index < 256) or (Entry.Empty);
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

procedure AddASCII(const Entry : TEntry; CodePage : String);
var
  S : AnsiString;
begin
  S := AnsiString(Entry.UTF8);
  if (Entry.ASCII < 256) and (CodePage <> 'SUP')  then
    S := S + '/' + CodePage + ',' + IntToStr(Entry.ASCII);
  ASCII.Add(S);
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

procedure AddInfo(const Entry : TEntry);
var
  I : Integer;
begin
  if Entry.Code <> '' then
    Info.Add(AnsiString(IntsToUnicode(Entry.UTF8) + TAB + Entry.Code));
  for I := 0 to Length(Entry.HTML) - 1 do
    Info.Add(AnsiString(IntsToUnicode(Entry.UTF8) + TAB + Entry.HTML[I]));
end;

procedure ReadXML(FileName : String);
var
  X : TXMLConfig;
  I : Integer;
  E : TEntry;
  C, N : String;
  V, T : Integer;
begin
  N := ExtractFileBase(FileName);
  C := N;
  Val(C, V, T);
  if T <> 0 then Exit;
  if (V >= 900000) then begin
    C:='SUP';
    N:=N+' Supplemental';
  end;
  Data := '// DOS Codepage to UTF-8 conversion map' + LF +
  '{$DEFINE CP' + C + 'toUTF8Remap}' + LF +
  'const' + LF + '  CP' + C + 'toUTF8RemapList : TArrayOfStrings = (' + LF;
  WriteLn('Reading Codepage ', N, ' XML mapping file.');
  X := TXMLConfig.Create(nil);
  try
    X.Filename:=FileName;
    I := 0;
    while ReadEntry(X, I, E) do begin
      if I < 256 then
        Data:=Data + '    ' + QUOTE + WhenTrue(E.OKDOS, '', AnsiString(E.UTF8)) +
          QUOTE + WhenTrue(I<255, ',') + LF;
      Inc(I);
      if (E.Empty) then Continue;
      if (E.UTF8='') or ((E.CODE='') and (Length(E.HTML)=0)) then continue;
      AddUTF8(E);
      AddHTML(E);
      AddASCII(E, C);
      if V <= 900000 then
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
  for I := 0 to Length(D) - 1 do
    ReadXML('codepages/' + D[I]);
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

function EntryTypeDef : String;
begin
  EntryTypeDef := LF +
  '{$IFNDEF TextRemapEntries}' + LF +
  '{$DEFINE TextRemapEntries}' + LF +
  'type' + LF +
  '  TTextRemapEntry = record' + LF +
  '    Value : UInt32;' + LF +
  '    Data : String;' + LF +
  '  end;' + LF +
  '  TTextRemapEntries = array of TTextRemapEntry;' + LF +
  '{$ENDIF}' + LF + LF;
end;

procedure AddItem(V; UInt32; D : String; AllData : boolean = false);
var
  T, S, C : String;
begin
  if M = '' then exit;
  C := PopDelim(D, '/');
  if Not AllData then
    C := PopDelim(C, '/'); // this will discard any remaining delimited fields
  S := '    (Value:' + QUOTE + T + QUOTE + SEMICOLON + SPACE +
  'Converted:' + QUOTE + C + WhenTrue(M, '/' + M) + QUOTE + ')';
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
  AddItem(UTF8[M]);
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
  AddItem(HTML[M]);
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
  AddItem(ASCII[M], True);
  if L = H then Exit;
  if L < M then ItemsASCII(L, M-1);
  if M < H then ItemsASCII(M + 1, H);
end;

procedure SaveUTF8(Filename : String);
begin
  Data := '';
  ItemsUTF8(0, UTF8.Count - 1);
  Data := '// UTF-8 to HTML conversion map' + LF + EntryTypeDef + LF +
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
  Data := '// HTML to UTF-8 conversion map' + LF + EntryTypeDef + LF +
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
  Data := '// UTF-8 to ASCII conversion map' + LF + EntryTypeDef + LF +
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

procedure SaveMaps;
begin
  SaveUTF8('map_utf8.inc');
  SaveHTML('map_html.inc');
  SaveASCII('map_uasc.inc');
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

