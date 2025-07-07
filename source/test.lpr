// Copyright (c) 2025 Jerome Shidel
// BSD-3-Clause license

program test;

{$mode objfpc}{$H+}

uses Classes, SysUtils, PASext;

{$I map_utf8.inc}

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

