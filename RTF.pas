unit RTF;

interface
uses Dialogs, SysUtils;

{Rtftype.h}
//type bool: shortint;
const fTrue = 1;
const fFalse = 0;

type char_prop = record
    fBold: ShortInt;
    fUnderline: ShortInt;
    fItalic: ShortInt;
  end;
type T_CHP = char_prop; // CHaracter Properties

type T_JUST = (justL, justR, justC, justF);
type para_prop = record
    xaLeft: Integer; // left indent in twips
    xaRight: Integer; // right indent in twips
    xaFirst: Integer; // first line indent in twips
    just: T_JUST; // justification
  end;
type T_PAP = para_prop; // PAragraph Properties

type T_SBK = (sbkNon, sbkCol, sbkEvn, sbkOdd, sbkPg);
type PGN = (pgDec, pgURom, pgLRom, pgULtr, pgLLtr);
type sect_prop = record
    cCols: Integer; // number of columns
    sbk: T_SBK; // section break type
    xaPgn: Integer; // x position of page number in twips
    yaPgn: Integer; // y position of page number in twips
    pgnFormat: PGN; // how the page number is formatted
  end;
type T_SEP = sect_prop; // SEction Properties

type doc_prop = record
    xaPage: Integer; // page width in twips
    yaPage: Integer; // page height in twips
    xaLeft: Integer; // left margin in twips
    yaTop: Integer; // top margin in twips
    xaRight: Integer; // right margin in twips
    yaBottom: Integer; // bottom margin in twips
    pgnStart: Integer; // starting page number in twips
    fFacingp: ShortInt; // facing pages enabled?
    fLandscape: ShortInt; // landscape or portrait?
  end;
type T_DOP = doc_prop; // DOcument Properties

type T_RDS = (rdsNorm, rdsSkip); // Rtf Destination State
type T_RIS = (risNorm, risBin, risHex); // Rtf Internal State

// property save structure
type p_save = ^Save;
  Save = record
    PNext: p_save; // next save
    chp: T_CHP;
    pap: T_PAP;
    Sep: T_SEP;
    dop: T_DOP;
    rds: T_RDS;
    ris: T_RIS;
  end;
type T_Save = Save;

// What types of properties are there?
type T_IPROP = (ipropBold, ipropItalic, ipropUnderline, ipropLeftInd,
    ipropRightInd, ipropFirstInd, ipropCols, ipropPgnX,
    ipropPgnY, ipropXaPage, ipropYaPage, ipropXaLeft,
    ipropXaRight, ipropYaTop, ipropYaBottom, ipropPgnStart,
    ipropSbk, ipropPgnFormat, ipropFacingp, ipropLandscape,
    ipropJust, ipropPard, ipropPlain, ipropSectd,
    ipropMax);

type T_ACTN = (actnSpec, actnByte, actnWord);
type PROPTYPE = (propChp, propPap, propSep, propDop);

type propmod = record
    actn: T_ACTN; // size of value
    prop: PROPTYPE; // structure containing value
    offset: Integer; // offset of value from base of structure
  end;
type T_PROP = propmod;

type T_IPFN = (ipfnBin, ipfnHex, ipfnSkipDest);
type T_IDEST = (idestPict, idestSkip);
type T_KWD = (kwdChar, kwdDest, kwdProp, kwdSpec);

type symbol = record
    {*} szKeyword: ShortInt; // RTF keyword
    dflt: Integer; // default value to use
    fPassDflt: Boolean; // true to use default value from this table
    kwd: T_KWD; // base action to take
    IDX: Integer; // index into property table if kwd = kwdProp
                            // index into destination table if kwd = kwdDest
                            // character to prif: integer kwd = kwdChar
end; type T_SYM = symbol;


{Rtfdecl.h}
// RTF parser declarations

function ecRtfParse({*} var FP: TextFile): Integer;
function ecPushRtfState(): Integer;
function ecPopRtfState(): Integer;
function ecParseRtfKeyword({*} var FP: TextFile): Integer;
function ecParseChar(C: Char): Integer;
function ecTranslateKeyword({*} szKeyword: array of Char; param: Integer;
  fParam:
  Boolean): Integer;
function ecPrintChar(Ch: Integer): Integer;
function ecEndGroupAction(rds: T_RDS): Integer;
function ecApplyPropChange(iprop: T_IPROP; Val: Integer): Integer;
function ecChangeDest(idest: T_IDEST): Integer;
function ecParseSpecialKeyword(ipfn: T_IPFN): Integer;
function ecParseSpecialProperty(iprop: T_IPROP; Val: Integer): Integer;
function ecParseHexByte(): Integer;

// RTF variable declarations
var
{extern} cGroup: Integer;
{extern} rds: T_RDS;
{extern} ris: T_RIS;

{extern} chp: T_CHP;
{extern} pap: T_PAP;
{extern} Sep: T_SEP;
{extern} dop: T_DOP;

{extern}//*psave: T_SAVE;
  psave: p_save;
{extern} cbBin: Longint;
{extern} LParam: Longint;
{extern} fSkipDestIfUnk: Boolean;
//{extern}{*}fpIn: textfile;

// RTF parser error codes

const ecOK = 0; // Everything's finenot
const ecStackUnderflow = 1; // Unmatched 'curly)'
const ecStackOverflow = 2; // Too many 'curly(' -- memory exhausted
const ecUnmatchedBrace = 3; // RTF ended during an open group.
const ecInvalidHex = 4; // invalid hex character found in data
const ecBadTable = 5; // RTF table (sym or prop) invalid
const ecAssertion = 6; // Assertion failure
const ecEndOfFile = 7; // End of file reached while reading RTF
implementation

{ Rtfreadr.c
uses <stdio.h>
uses <stdlib.h>
uses <ctype.h>
uses 'rtftype.h'
uses 'rtfdecl.h'

cGroup: integer;
fSkipDestIfUnk: boolean;
cbBin: longint;
lParam: longint;

 rds: T_RDS;
 ris: T_RIS;

 chp: T_CHP;
 pap: T_PAP;
 sep: T_SEP;
 dop: T_DOP;

 *psave: T_SAVE;}
//{*}const fpIn: textfile;

//
// %%Function: main
//
// Main loop. Initialize and parse RTF.
//

function Main(argc: Integer; argv: array of ShortInt): Integer;
var
    {*} FP: TextFile;
  ec: Integer;
begin
  AssignFile(FP {fpIn}, 'test.rtf');
  Reset(FP {fpIn});
    //fp := fpIn;
  if IOResult <> 0 then
  begin
    ShowMessage('Can''t open test file!');
    Result := 1;
  end;
  ec := ecRtfParse(FP);
  if ec <> ecOK then
    ShowMessage(Format('error %d parsing rtf', [ec]))
  else
    ShowMessage('Parsed RTF file OK');
  CloseFile(FP);
  Result := 0;
end;

//
// %%Function: ecRtfParse
//
// Step 1:
// Isolate RTF keywords and send them to ecParseRtfKeyword;
// Push and pop state at the start and end of RTF groups;
// Send text to ecParseChar for further processing.
//

function ecRtfParse({*} var FP: TextFile): Integer;
var
  Ch: Char;
  ec: Integer;
  cNibble: Integer;
  B: Integer;
begin
  cNibble := 2;
  B := 0;
    //read(fp,ch);
  while not EOF(FP) do
  begin
    if cGroup < 0 then
      Result := ecStackUnderflow;
    if ris = risBin then // if we're parsing binary data, handle it directly
    begin
      ec := ecParseChar(Ch);
      if ec <> ecOK then
        Result := ec;
    end
    else
    begin
      case Ch of
        '{': begin
            ec := ecPushRtfState();
            if ec <> ecOK then
              Result := ec; end;
                //break;
        '}': begin
            ec := ecPopRtfState();
            if ec <> ecOK then
              Result := ec; end;
                //break;
             //'\\': begin
        '\': begin
            ec := ecParseRtfKeyword(FP);
            if ec <> ecOK then
              Result := ec; end;
                //break;
        #13, #10: begin // cr and lf are noise characters...
          end;
      else begin
          if ris = risNorm then
          begin
            ec := ecParseChar(Ch);
            if ec <> ecOK then
              Result := ec;
          end
          else
          begin // parsing hex data
            if ris <> risHex then
              Result := ecAssertion;
            B := B shl 4;
            if Ch in ['0'..'9'] then
              B := B + Ord(Ch) - Ord('0')
            else
            begin
              if Ch in ['a'..'z'] then
              begin
                if (Ch < 'a') or (Ch > 'f') then
                  Result := ecInvalidHex;
                B := B + Ord(Ch) - Ord('a');
              end
              else
              begin
                if (Ch < 'A') or (Ch > 'F') then
                  Result := ecInvalidHex;
                B := B + Ord(Ch) - Ord('A');
              end;
            end;
            Dec(cNibble);
                    //if not cNibble then
            if cNibble <= 0 then //??
            begin
              ec := ecParseChar(Chr(B));
              if ec <> ecOK then
                Result := ec;
              cNibble := 2;
              B := 0;
              ris := risNorm;
            end;
          end; // end else (ris <> risNorm)
        end;
      end; // switch
    end; // else (ris <> risBin)
  end; // while
  if cGroup < 0 then
    Result := ecStackUnderflow;
  if cGroup > 0 then
    Result := ecUnmatchedBrace;
  Result := ecOK;
end;

//
// %%Function: ecPushRtfState
//
// Save relevant info on a linked list of T_SAVE structures.
//

function ecPushRtfState(): Integer;
var
  psaveNew: p_save;
begin
  GetMem(psaveNew, SizeOf(T_Save));
  if psaveNew = nil then
  begin
    Result := ecStackOverflow;
    Exit;
  end;

  psaveNew^.PNext := psave;
  psaveNew.chp := chp;
  psaveNew.pap := pap;
  psaveNew.Sep := Sep;
  psaveNew.dop := dop;
  psaveNew.rds := rds;
  psaveNew.ris := ris;
  ris := risNorm;
  psave := psaveNew;
  Inc(cGroup);
  Result := ecOK;
end;

//
// %%Function: ecPopRtfState
//
// If we're ending a destination (that is, the destination is changing),
// call ecEndGroupAction.
// Always restore relevant info from the top of the T_SAVE list.
//

function ecPopRtfState(): Integer;
var
  psaveOld: p_save;
  ec: Integer;
begin

  if psave = nil then
  begin
    Result := ecStackUnderflow;
    Exit;
  end;

  if rds <> psave.rds then
  begin
    ec := ecEndGroupAction(rds);
    if ec <> ecOK then
      Result := ec;
  end;
  chp := psave.chp;
  pap := psave.pap;
  Sep := psave.Sep;
  dop := psave.dop;
  rds := psave.rds;
  ris := psave.ris;

  psaveOld := psave;
  psave := psave.PNext;
  Dec(cGroup);
  FreeMem(psaveOld);
  Result := ecOK;
end;

//
// %%Function: ecParseRtfKeyword
//
// Step 2:
// get a control word (and its associated value) and
// call ecTranslateKeyword to dispatch the control.
//

function isalpha(Ch: Char): Boolean;
begin
  //??
end;

function ecParseRtfKeyword({*} var FP: TextFile): Integer;
var
  Ch: Char;
  fParam: Boolean;
  fNeg: Boolean;
  param: Integer;
  PCh: ^Char;
  szKeyword: array[0..29] of Char;
  szParameter: array[0..19] of Char;
begin
  fParam := False;
  fNeg := False;
  param := 0;

  szKeyword[0] := #0;
  szParameter[0] := #0;
    //if (ch := getc(fp)) = EOF then
  if EOF(FP) then
  begin
    Result := ecEndOfFile;
    Exit;
  end;
  if not isalpha(Ch) then // a control symbol; no delimiter.
  begin
    szKeyword[0] := Ch;
    szKeyword[1] := #0;
    begin
      Result := ecTranslateKeyword(szKeyword, 0, fParam);
      Exit;
    end;
  end;
  {for PCh := szKeyword to isalpha(Ch) do Ch := getc(FP) do
    * PCh + + := (Char)Ch;
  * PCh := #0;}
  PCh := @szKeyword[0];
  while isalpha(Ch) do
  begin
    PCh^ := Ch;
    Inc(PCh);
    Read(FP, Ch);
  end;
  PCh^ := #0;
  if Ch = '-' then
  begin
    fNeg := True;
    if EOF(FP) then
    begin
      Result := ecEndOfFile;
      Exit;
    end;
  end;
  if Ch in ['0'..'9'] then
  begin
    fParam := True; // a digit after the control means we have a parameter
    {for PCh := szParameter to isdigit(Ch) do Ch := getc(FP) do
      * PCh + + := (Char)Ch;
    * PCh := #0;}
    PCh := @szKeyword[0];
    Read(FP, Ch);
    while Ch in ['0'..'9'] do
    begin
      PCh^ := Ch;
      Inc(PCh);
      Read(FP, Ch);
    end;
    PCh^ := #0;
    param := StrToInt(szParameter);
    if fNeg then
      param := -param;
    LParam := StrToInt64(szParameter);
    if fNeg then
      param := -param;
  end;
  if Ch <> ' ' then
    ungetc(Ch, FP);
  Result := ecTranslateKeyword(szKeyword, param, fParam);
end;

//
// %%Function: ecParseChar
//
// Route the character to the appropriate destination stream.
//

function ecParseChar(Ch: Char): Integer;
begin
  if ris = risBin && - -cbBin <= 0 then
    ris := risNorm;
  case rds of
    rdsSkip: begin
        // Toss this character.
        Result := ecOK; end;
    rdsNorm: begin
        // Output a character. Properties are valid at this point.
        Result := ecPrintChar(Ch); end;
  else
    // handle other destinations....
    Result := ecOK;
  end;
end;
//
// %%Function: ecPrintChar
//
// Send a character to the output file.
//

function ecPrintChar(Ch: Integer): Integer;
begin
    // unfortunately, we don't do a whole lot here as far as layout goes...
  PutChar(Ch);
  Result := ecOK;
end;

 {RTFACTN.C}
uses < stdio.H >
uses < string.H >
uses < stddef.H >
uses < ctype.H >
uses 'rtftype.h'
uses 'rtfdecl.h'

// RTF parser tables

// Property descriptions
  T_PROP rgprop[ipropMax] := begin
    actnByte, propChp, offsetof(T_CHP, fBold), // ipropBold
      actnByte, propChp, offsetof(T_CHP, fItalic), // ipropItalic
      actnByte, propChp, offsetof(T_CHP, fUnderline), // ipropUnderline
      actnWord, propPap, offsetof(T_PAP, xaLeft), // ipropLeftInd
      actnWord, propPap, offsetof(T_PAP, xaRight), // ipropRightInd
      actnWord, propPap, offsetof(T_PAP, xaFirst), // ipropFirstInd
      actnWord, propSep, offsetof(T_SEP, cCols), // ipropCols
      actnWord, propSep, offsetof(T_SEP, xaPgn), // ipropPgnX
      actnWord, propSep, offsetof(T_SEP, yaPgn), // ipropPgnY
      actnWord, propDop, offsetof(T_DOP, xaPage), // ipropXaPage
      actnWord, propDop, offsetof(T_DOP, yaPage), // ipropYaPage
      actnWord, propDop, offsetof(T_DOP, xaLeft), // ipropXaLeft
      actnWord, propDop, offsetof(T_DOP, xaRight), // ipropXaRight
      actnWord, propDop, offsetof(T_DOP, yaTop), // ipropYaTop
      actnWord, propDop, offsetof(T_DOP, yaBottom), // ipropYaBottom
      actnWord, propDop, offsetof(T_DOP, pgnStart), // ipropPgnStart
      actnByte, propSep, offsetof(T_SEP, sbk), // ipropSbk
      actnByte, propSep, offsetof(T_SEP, pgnFormat), // ipropPgnFormat
      actnByte, propDop, offsetof(T_DOP, fFacingp), // ipropFacingp
      actnByte, propDop, offsetof(T_DOP, fLandscape), // ipropLandscape
      actnByte, propPap, offsetof(T_PAP, just), // ipropJust
      actnSpec, propPap, 0, // ipropPard
      actnSpec, propChp, 0, // ipropPlain
      actnSpec, propSep, 0, // ipropSectd
  end; ;

// Keyword descriptions
T_SYM rgsymRtf[] := begin
//  keyword     dflt    fPassDflt   kwd         idx
  'b', 1, fFalse, kwdProp, ipropBold,
    'u', 1, fFalse, kwdProp, ipropUnderline,
    'i', 1, fFalse, kwdProp, ipropItalic,
    'li', 0, fFalse, kwdProp, ipropLeftInd,
    'ri', 0, fFalse, kwdProp, ipropRightInd,
    'fi', 0, fFalse, kwdProp, ipropFirstInd,
    'cols', 1, fFalse, kwdProp, ipropCols,
    'sbknone', sbkNon, fTrue, kwdProp, ipropSbk,
    'sbkcol', sbkCol, fTrue, kwdProp, ipropSbk,
    'sbkeven', sbkEvn, fTrue, kwdProp, ipropSbk,
    'sbkodd', sbkOdd, fTrue, kwdProp, ipropSbk,
    'sbkpage', sbkPg, fTrue, kwdProp, ipropSbk,
    'pgnx', 0, fFalse, kwdProp, ipropPgnX,
    'pgny', 0, fFalse, kwdProp, ipropPgnY,
    'pgndec', pgDec, fTrue, kwdProp, ipropPgnFormat,
    'pgnucrm', pgURom, fTrue, kwdProp, ipropPgnFormat,
    'pgnlcrm', pgLRom, fTrue, kwdProp, ipropPgnFormat,
    'pgnucltr', pgULtr, fTrue, kwdProp, ipropPgnFormat,
    'pgnlcltr', pgLLtr, fTrue, kwdProp, ipropPgnFormat,
    'qc', justC, fTrue, kwdProp, ipropJust,
    'ql', justL, fTrue, kwdProp, ipropJust,
    'qr', justR, fTrue, kwdProp, ipropJust,
    'qj', justF, fTrue, kwdProp, ipropJust,
    'paperw', 12240, fFalse, kwdProp, ipropXaPage,
    'paperh', 15480, fFalse, kwdProp, ipropYaPage,
    'margl', 1800, fFalse, kwdProp, ipropXaLeft,
    'margr', 1800, fFalse, kwdProp, ipropXaRight,
    'margt', 1440, fFalse, kwdProp, ipropYaTop,
    'margb', 1440, fFalse, kwdProp, ipropYaBottom,
    'pgnstart', 1, fTrue, kwdProp, ipropPgnStart,
    'facingp', 1, fTrue, kwdProp, ipropFacingp,
    'landscape', 1, fTrue, kwdProp, ipropLandscape,
    'par', 0, fFalse, kwdChar, 0 x0a,
    '\0x0a', 0, fFalse, kwdChar, 0 x0a,
    '\0x0d', 0, fFalse, kwdChar, 0 x0a,
    'tab', 0, fFalse, kwdChar, 0 x09,
    'ldblquote', 0, fFalse, kwdChar, ''',
    'rdblquote', 0, fFalse, kwdChar, ''',
    'bin', 0, fFalse, kwdSpec, ipfnBin,
    '*', 0, fFalse, kwdSpec, ipfnSkipDest,
    ''',        0,      fFalse,     kwdSpec,    ipfnHex,
    'author', 0, fFalse, kwdDest, idestSkip,
    'buptim', 0, fFalse, kwdDest, idestSkip,
    'colortbl', 0, fFalse, kwdDest, idestSkip,
    'comment', 0, fFalse, kwdDest, idestSkip,
    'creatim', 0, fFalse, kwdDest, idestSkip,
    'doccomm', 0, fFalse, kwdDest, idestSkip,
    'fonttbl', 0, fFalse, kwdDest, idestSkip,
    'footer', 0, fFalse, kwdDest, idestSkip,
    'footerf', 0, fFalse, kwdDest, idestSkip,
    'footerl', 0, fFalse, kwdDest, idestSkip,
    'footerr', 0, fFalse, kwdDest, idestSkip,
    'footnote', 0, fFalse, kwdDest, idestSkip,
    'ftncn', 0, fFalse, kwdDest, idestSkip,
    'ftnsep', 0, fFalse, kwdDest, idestSkip,
    'ftnsepc', 0, fFalse, kwdDest, idestSkip,
    'header', 0, fFalse, kwdDest, idestSkip,
    'headerf', 0, fFalse, kwdDest, idestSkip,
    'headerl', 0, fFalse, kwdDest, idestSkip,
    'headerr', 0, fFalse, kwdDest, idestSkip,
    'info', 0, fFalse, kwdDest, idestSkip,
    'keywords', 0, fFalse, kwdDest, idestSkip,
    'operator', 0, fFalse, kwdDest, idestSkip,
    'pict', 0, fFalse, kwdDest, idestSkip,
    'printim', 0, fFalse, kwdDest, idestSkip,
    'private1', 0, fFalse, kwdDest, idestSkip,
    'revtim', 0, fFalse, kwdDest, idestSkip,
    'rxe', 0, fFalse, kwdDest, idestSkip,
    'stylesheet', 0, fFalse, kwdDest, idestSkip,
    'subject', 0, fFalse, kwdDest, idestSkip,
    'tc', 0, fFalse, kwdDest, idestSkip,
    'title', 0, fFalse, kwdDest, idestSkip,
    'txe', 0, fFalse, kwdDest, idestSkip,
    'xe', 0, fFalse, kwdDest, idestSkip,
    'curly(', 0, fFalse, kwdChar, 'curly(',
    'curly)', 0, fFalse, kwdChar, 'curly)',
    '\\', 0, fFalse, kwdChar, '\\'
end; ;
isymMax: Integer := SizeOf(rgsymRtf) / SizeOf(T_SYM);

//
// %%Function: ecApplyPropChange
//
// Set the property identified by _iprop_ to the value _val_.
//
//

function ecApplyPropChange(iprop: T_IPROP, Val: Integer): Integer;
begin
  * PB: ShortInt;

  if rds = rdsSkip then // If we're skipping text,
    Result := ecOK; // don't do anything.

  case rgprop[iprop].prop of
    propDop: begin
        PB := (* : ShortInt)&dop;
      end;
    propSep: begin
        PB := (* : ShortInt)&sep;
      end;
    propPap: begin
        PB := (* : ShortInt)&pap;
      end;
    propChp: begin
        PB := (* : ShortInt)&chp;
      end;
  else
    if rgprop[iprop].actn <> actnSpec then
      Result := ecBadTable;
  end;
  case rgprop[iprop].actn of
    actnByte: begin
        PB[rgprop[iprop].offset] := (unsigned Char)Val;
      end;
    actnWord: begin
        (* (* : Integer)(PB + rgprop[iprop].offset)) := Val;
      end;
    actnSpec: begin
        Result := ecParseSpecialProperty(iprop, Val); end;
    Break;
  else
    Result := ecBadTable;
  end;
  Result := ecOK;
end;

//
// %%Function: ecParseSpecialProperty
//
// Set a property that requires code to evaluate.
//

function ecParseSpecialProperty(iprop: T_IPROP, Val: Integer): Integer;
begin
  case iprop of
    ipropPard: begin
        memset(&pap, 0, SizeOf(pap));
        Result := ecOK; end;
    ipropPlain: begin
        memset(&chp, 0, SizeOf(chp));
        Result := ecOK; end;
    ipropSectd: begin
        memset(&sep, 0, SizeOf(Sep));
        Result := ecOK; end;
  else
    Result := ecBadTable;
  end;
  Result := ecBadTable;
end;

//
// %%Function: ecTranslateKeyword.
//
// Step 3.
// Search rgsymRtf for szKeyword and evaluate it appropriately.
//
// Inputs:
// szKeyword:   The RTF control to evaluate.
// param:       The parameter of the RTF control.
// fParam:      fTrue if the control had a parameter; (that is, if param is valid)
//              fFalse if it did not.
//

function ecTranslateKeyword(* szKeyword: ShortInt, param: Integer, fParam:
  Boolean): Integer;
begin
  isym: Integer;

    // search for szKeyword in rgsymRtf

  for isym := 0 to isym < isymMax do isym + + do
    if strcmp(szKeyword, rgsymRtf[isym].szKeyword) = 0 then
      Break;
  if isym = isymMax then // control word not found
  begin
    if fSkipDestIfUnk then // if this is a new destination
      rds := rdsSkip; // skip the destination
                                    // else just discard it
    fSkipDestIfUnk := fFalse;
    Result := ecOK;
  end;

    // found itnot   use kwd and idx to determine what to do with it.

  fSkipDestIfUnk := fFalse;
  case rgsymRtf[isym].kwd of
    kwdProp: begin
        if rgsymRtf[isym].fPassDflt or not fParam then
          param := rgsymRtf[isym].dflt;
        Result := ecApplyPropChange(rgsymRtf[isym].IDX, param); end;
    kwdChar: begin
        Result := ecParseChar(rgsymRtf[isym].IDX); end;
    kwdDest: begin
        Result := ecChangeDest(rgsymRtf[isym].IDX); end;
    kwdSpec: begin
        Result := ecParseSpecialKeyword(rgsymRtf[isym].IDX); end;
  else
    Result := ecBadTable;
  end;
  Result := ecBadTable;
end;

//
// %%Function: ecChangeDest
//
// Change to the destination specified by idest.
// There's usually more to do here than this...
//

function ecChangeDest(idest: T_IDEST): Integer;
begin
  if rds = rdsSkip then // if we're skipping text,
    Result := ecOK; // don't do anything

  case idest of
  else
    rds := rdsSkip; // when in doubt, skip it...
        //break;
  end;
  Result := ecOK;
end;

//
// %%Function: ecEndGroupAction
//
// The destination specified by rds is coming to a close.
// If there's any cleanup that needs to be done, do it now.
//

function ecEndGroupAction(T_RDS rds): Integer;
begin
  Result := ecOK;
end;

//
// %%Function: ecParseSpecialKeyword
//
// Evaluate an RTF control that needs special processing.
//

function ecParseSpecialKeyword(ipfn: T_IPFN): Integer;
begin
  if rds = rdsSkip && ipfn <> ipfnBin then // if we're skipping, and it's not
    Result := ecOK; // the \bin keyword, ignore it.
  case ipfn of
    ipfnBin: begin
        ris := risBin;
        cbBin := LParam;
      end;
    ipfnSkipDest: begin
        fSkipDestIfUnk := fTrue;
      end;
    ipfnHex: begin
        ris := risHex;
      end;
  else
    Result := ecBadTable;
  end;
  Result := ecOK;
end;

end.

