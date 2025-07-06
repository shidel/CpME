// Copyright (c) 2025 Jerome Shidel
// BSD-3-Clause license

program test;

{$mode objfpc}{$H+}

uses Classes, SysUtils, PASext;

{$I map_utf8.inc}

type
  TUTF8CodePoint = AnsiString;
  TUTF8Value = UInt32;

function CodePointLength(C : TUTF8CodePoint) : integer;
var
  V : byte;
begin
  CodePointLength:=0;
  if C = '' then exit;
  V:=Byte(C[Low(C)]);

  if (V and $80) = $00 then begin
    if (V = $7f) then // special 127, basically ignored followed by 3
      CodePointLength := 4
    else
      CodePointLength := 1
  end
  else if (V and $f0) = $f0 then
    CodePointLength := 4
  else if (V and $e0) = $e0 then
    CodePointLength := 3
  else if (V and $e0) = $c0 then
    CodePointLength := 2;
end;

function CodePointToValue(C : TUTF8CodePoint) : UInt32;
var
  P, L, M, V : integer;
begin
   CodePointToValue:=0;
   if Length(C) = 0 then Exit;
   L := 0;
   P := 0;
   V := Byte(C[Low(C) + P]);
   if (V and $80) = 0 then begin // High bit not set
     L := 1;
     M := $7f;
     if V = $7f then begin
       // Not really a valid UTF-8 character
       // probably followed by value x2302 (226 140 130)
       // Browsers either ignore it or display a Box. Then the x2302 "house"
       // symbol. So, I will treat as a UTF-8 Encoding error
       exit;
     end;
   end
   else if (V and $f0) = $f0 then begin // 1111????
     L := 4;
     M := $0f
   end
   else if (V and $e0) = $e0 then begin // 1110????
     L := 3;
     M := $0f
   end
   else if (V and $e0) = $c0 then begin // 110?????
     M := $1f;
     L := 2;
   end else begin                       // 10??????
     // I dont think this is possible for the first character
     M := $3f;
     L := 1;
   end;
   if Length(C) < L then Exit; // error, string not long enough
   V := 0;
   while P < L do begin
     if (P > 0) and ((Byte(C[Low(C) + P]) and $c0) <> $80) then Exit; // After first, 10??????
     V:=(V shl 6) + (Byte(C[Low(C) + P]) and M);
     M := $3f;
     Inc(P);
   end;
   CodePointToValue:= V;
end;

function ValueToCodePoint(V : UInt32) : TUTF8CodePoint;
begin
  ValueToCodePoint:='X';
end;

procedure SaveInfo(Filename : String);
var
  U : String;
  H : String;
  T : String;
  S : String;
  V : UInt32;
  I : integer;
begin
  S :='<!DOCTYPE html>' + LF +
  '<html lang="en">' + LF +
  '<head>' + LF +
  '<title>Test Page</title>' + LF +
  '<meta http-equiv="content-type" content="text/html; charset=utf-8">' + LF +
  '<style>' + LF +
  'body { white-space:pre; font-size:200%; }' + LF +
  'span.names { white-space:default; font-size:20%; }' + LF +
  '</style>' + LF +
  '</head>' + LF +
  '<body>' + LF;
  for I := Low(UTF8toHTMLRemapList) to High(UTF8toHTMLRemapList) do begin
    U := IntsToStr(UTF8toHTMLRemapList[I].Original);
    V := CodePointToValue(U);
    H := 'x' + IntToHex(V, 6);
    T := ValueToCodePoint(V);
    S := S + H + TAB + U + TAB + '&#' + H + ';' + LF;
  end;
  S := S + '</body>' + LF;
  SaveBinary(Filename, S);
end;


begin
  SaveInfo('testpage.html');
end.

