// Copyright (c) 2025 Jerome Shidel
// BSD-3-Clause license

program test;

{$mode objfpc}{$H+}

uses Classes, SysUtils, PASext, XMLConf;

{$I map_utf8.inc}

var
  BX : TXMLConfig;

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

function NewKey(Index : Integer; Attribute : UnicodeString) : UnicodeString;
begin
  if Index > 255 then
    NewKey:='SUPPLEMENT/x' + UnicodeString(IntToHex(Index-256,4)) +'/'+Attribute
  else
    NewKey:='CODEPAGE/x' + UnicodeString(IntToHex(Index,2)) + '/' + Attribute
end;

procedure ConvertXML(var OX, NX : TXMLConfig);
var
  I : integer;
begin
  I := 0;
  while (I <256) or Exists(OX, I) do begin
    if I > 127 then Break; // temporary
    if Needed(OX, I) then begin
       WriteLn(I);
    end;
    Inc(I);
  end;

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
    DeleteFile(D[I]);
    WriteLn(D[I]);
    NX := TXMLConfig.Create(nil);
    if D[I] = '437.xml' then
      OX := BX
    else begin
      OX := TXMLConfig.Create(nil);
      OX.Filename := 'codepages/' + D[I];
    end;
    NX.Filename := D[I];
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

