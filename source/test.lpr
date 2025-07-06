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
    CodePointLength := 2
  else // I don't think is actually a "legal" character.
    CodePointLength := 1;
end;

const
  CodePointMasks : array[1..4] of record A, O : byte end = (
    (A:$7f; O:$80),
    (A:$1f; O:$c0),
    (A:$0f; O:$e0),
    (A:$0f; O:$f0)
  );

function CodePointToValue(C : TUTF8CodePoint; out Value : TUTF8Value) : Boolean; overload;
var
  P, L, M, V : integer;
begin
  CodePointToValue:=False;
  Value:=0;
  if Length(C) = 0 then Exit;
  L := 0;
  P := 0;
  V := Byte(C[Low(C) + P]);
  if (V and $80) = 0 then begin // High bit not set
    L := 1;
    if V = $7f then begin
      // Not really a valid UTF-8 character
      // probably followed by value x2302 (226 140 130)
      // Browsers either ignore it or display a Box. Then the x2302 "house"
      // symbol. So, I will treat as a UTF-8 Encoding error
      exit;
    end;
  end
  else if (V and $f0) = $f0 then  // 1111????
    L := 4
  else if (V and $e0) = $e0 then  // 1110????
    L := 3
  else if (V and $e0) = $c0 then  // 110?????
    L := 2
  else                            // 10??????
    // I dont think this is possible for the first character
    // M := $3f;
    // L := 1;
    Exit;
  if Length(C) < L then Exit; // error, string not long enough
  V := 0;
  M := CodePointMasks[L].A;
  while P < L do begin
    if (P > 0) and ((Byte(C[Low(C) + P]) and $c0) <> $80) then Exit; // After first, 10??????
    V:=(V shl 6) + (Byte(C[Low(C) + P]) and M);
    M := $3f;
    Inc(P);
  end;
  Value :=V;
  CodePointToValue:= True;
end;

function CodePointToValue(C : TUTF8CodePoint) : TUTF8Value; overload;
begin
  CodePointToValue(C,CodePointToValue);
end;

function ValueToCodePoint(Value : TUTF8Value; out C : TUTF8CodePoint) : boolean; overload;
var
  P, L, A, O : integer;
begin
  C := '';
  ValueToCodePoint:=False;
  if Value > $10ffff then Exit; // Too big
  if Value > $ffff then
    P := 4
  else
  if Value > $07ff then
    P := 3
  else
  if Value > $007f then
    P := 2
  else
  if Value = $007f then
    Exit // Treat $7f as invalid
  else
    P := 1;
  SetLength(C, P);
  L := P;
  A := $3f;
  O := $80;
  while P > 0 do begin
    if P = 1 then begin
      A := CodePointMasks[L].A;
      if L > 1 then
        O := CodePointMasks[L].O
      else
        O := 0;
    end;
    Byte(C[Low(C) + P - 1]) := (Value and A) or O;
    Value := Value shr 6;
    Dec(P);
  end;
  ValueToCodePoint:=True;
end;

function ValueToCodePoint(Value : TUTF8Value): TUTF8CodePoint; overload;
begin
  ValueToCodePoint(Value, ValueToCodePoint);
end;

procedure SaveInfo(Filename : String);
var
  U : String;
  H : String;
  T : String;
  X : String;
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
  'body { white-space:pre; font-size:150%; }' + LF +
  'span.names { white-space:default; font-size:20%; }' + LF +
  'td { padding-right:0.25em; padding-left:0.25em; }' + LF +
  '</style>' + LF +
  '</head>' + LF +
  '<body>' + LF + '<table>' + LF;
  for I := Low(UTF8toHTMLRemapList) to High(UTF8toHTMLRemapList) do begin
    U := IntsToStr(UTF8toHTMLRemapList[I].Original);
    V := CodePointToValue(U);
    H := 'x' + IntToHex(V, 6);
    T := ValueToCodePoint(V);
    X := 'x' + IntToHex(CodePointToValue(T), 6);
    S := S + '<tr>' +
      '<td>' + H + '</td>' +
      '<td>' + U +'</td>' +
      '<td>' + '&#' + H + ';' + '</td>' +
      '<td>' + T + '</td>' +
      '<td>' + X + '</td>' +
      '</tr>' + LF;
  end;
  S := S + '</body>' + LF;
  SaveBinary(Filename, S);
end;


begin
  SaveInfo('testpage.html');
end.

