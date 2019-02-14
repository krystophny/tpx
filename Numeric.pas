unit Numeric;

interface

type
//  TMatrix = array[1..1, 1..1] of Double;
  TMatrix = array of array of Double;
  PMatrix = TMatrix;

procedure Householder(X: TMatrix; const NRows, NCols, NCols1: Integer);
procedure Test;

implementation

uses SysUtils, SysBasic;

function MatToStr(X: TMatrix; const NRows, NCols: Integer): string;
var
  I, J: Integer;
begin
  Result := '';
  for I := 0 to Pred(NRows) do
  begin
    for J := 0 to Pred(NCols) do
    begin
      if J > 0 then Result := Result + #9;
      Result := Result + FloatToStr(X[I, J]); //FloatToStr(I * 10 + J);
    end;
    if I < Pred(NRows) then Result := Result + #13#10;
  end;
end;

procedure Householder(X: TMatrix; const NRows, NCols, NCols1: Integer);
var
  I, J, K: Integer;
  Tkk, Tkj, AbsA, Sgnm: Double;
begin
  MessageBoxInfo(MatToStr(X, NRows, NCols));
  for K := 0 to Pred(NCols1) do
  begin
    if X[K, K] > 0 then
      Sgnm := 1
    else if X[K, K] < 0 then
      Sgnm := -1
    else
      Sgnm := 0;
    Tkk := 0;
    for I := K to Pred(NRows) do
      Tkk := Tkk + Sqr(X[I, K]);
    Tkk := Sqrt(Tkk);
    AbsA := Abs(X[K, K]) + Tkk;
    for J := Succ(K) to Pred(NCols) do
    begin
      Tkj := 0;
      for I := K to Pred(NRows) do
        Tkj := Tkj + X[I, K] * X[I, J];
      Tkj := Tkj / Tkk;
      for I := Succ(K) to Pred(NRows) do
        X[I, J] := X[I, J] - X[I, K] / AbsA * (X[K, J] * Sgnm + Tkj);
      X[K, J] := Tkj;
    end;
    X[K, K] := Tkk;
    for I := Succ(K) to Pred(NRows) do
      X[I, K] := 0;
    MessageBoxInfo(MatToStr(X, NRows, NCols));
  end;
end;

procedure OLS_Householder(Y, X: TMatrix; const NObs, NDep, NRegs: Integer;
  var RSS: Double; B, E: TMatrix);
var
  I, J, K: Integer;
  A: TMatrix;
  S: Double;
begin
//if x=nil then exit;
  SetLength(A, NRegs + NObs, NRegs + NDep);
  for K := 0 to Pred(NRegs) do
    for J := 0 to Pred(NRegs + NDep) do
      A[K, J] := 0;
  for I := 0 to Pred(NObs) do
  begin
    for J := 0 to Pred(NRegs) do
      A[I + NRegs, J] := X[I, J];
    for J := 0 to Pred(NDep) do
      A[I + NRegs, J + NRegs] := Y[I, J];
  end;
  Householder(A, NRegs + NObs, NRegs + NDep, NRegs);
  RSS := 0;
  if NDep > 0 then
    for I := 0 to Pred(NObs) do
      for J := 0 to Pred(NDep) do
        RSS := RSS + Sqr(A[I + NRegs, J + NRegs]);
  if B <> nil then
  begin
    for I := Pred(NRegs) downto 0 do
      for K := 0 to Pred(NDep) do
      begin
        S := A[I, K + NRegs];
        for J := Succ(I) to Pred(NRegs) do
          S := S - A[I, J] * B[J, K];
        B[I, K] := S / A[I, I];
      end;
  end;
  if E <> nil then
    for I := 0 to Pred(NObs) do
      for J := 0 to Pred(NDep) do
        E[I, J] := A[I + NRegs, J + NRegs];
//    MessageBoxInfo(MatToStr(A, NRegs + NObs, NRegs + NDep));
end;

procedure Test;
var
//  A: array[1..4, 1..4] of Double;
//  A: TMatrix;
  Y, X, B, E: TMatrix;
  RSS: Double;
begin
  {SetLength(A, 4, 4);
  A[0, 0] := 1;
  A[1, 0] := 1;
  A[2, 0] := 1;
  A[3, 0] := 1;
  A[0, 1] := -2;
  A[1, 1] := -1;
  A[2, 1] := 2;
  A[3, 1] := 7;
  A[0, 2] := 0;
  A[1, 2] := 2;
  A[2, 2] := 5;
  A[3, 2] := 3;
  A[0, 3] := -3;
  A[1, 3] := 1;
  A[2, 3] := 2;
  A[3, 3] := 6;}
  {
  SetLength(A, 7, 4);
  A[0, 0] := 0;
  A[1, 0] := 0;
  A[2, 0] := 0;
  A[3, 0] := 1;
  A[4, 0] := 1;
  A[5, 0] := 1;
  A[6, 0] := 1;
  A[0, 1] := 0;
  A[1, 1] := 0;
  A[2, 1] := 0;
  A[3, 1] := -4;
  A[4, 1] := -2;
  A[5, 1] := 4;
  A[6, 1] := 14;
  A[0, 2] := 0;
  A[1, 2] := 0;
  A[2, 2] := 0;
  A[3, 2] := 0;
  A[4, 2] := 4;
  A[5, 2] := 10;
  A[6, 2] := 6;
  A[0, 3] := 0;
  A[1, 3] := 0;
  A[2, 3] := 0;
  A[3, 3] := -6;
  A[4, 3] := 2;
  A[5, 3] := 4;
  A[6, 3] := 12;
  }
  SetLength(X, 4, 3);
  X[0, 0] := 1;
  X[1, 0] := 1;
  X[2, 0] := 1;
  X[3, 0] := 1;
  X[0, 1] := -4;
  X[1, 1] := -2;
  X[2, 1] := 4;
  X[3, 1] := 14;
  X[0, 2] := 0;
  X[1, 2] := 4;
  X[2, 2] := 10;
  X[3, 2] := 6;
  SetLength(Y, 4, 1);
  Y[0, 0] := -6;
  Y[1, 0] := 2;
  Y[2, 0] := 4;
  Y[3, 0] := 12;
  SetLength(B, 3, 1);
  SetLength(E, 4, 1);
  OLS_Householder(Y, X, 4, 1, 3, RSS, B, E);
  MessageBoxInfo(FloatToStr(RSS));
  MessageBoxInfo(MatToStr(B, 3, 1));
  MessageBoxInfo(MatToStr(E, 4, 1));
//  Householder(A, 7, 4, 3);
//  MessageBoxInfo(MatToStr(A, 7, 4));
end;

end.

