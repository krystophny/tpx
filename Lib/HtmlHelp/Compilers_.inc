// Code based on work done by Brad Stowers DFS.inc file (www.delphifreestuff.com).
// (See also article by Brad at http://www.howtodothings.com/showarticle.asp?article=189)
// and Mike Lischke's compilers.inc file (http://www.delphi-gems.com)
//----------------------------------------------------------------------------------------------------------------------

{$ifdef Win32}
  {$ifdef VER150}
    {$define COMPILER_7}
    {$define DELPHI}
    {$define DELPHI_7}
  {$endif}

  {$ifdef VER140}
    {$define COMPILER_6}
    {$define DELPHI}
    {$define DELPHI_6}
  {$endif}

  {$ifdef VER130}
    {$define COMPILER_5}
    {$ifdef BCB}
      {$define CPPB}
      {$define CPPB_5}
    {$else}
      {$define DELPHI}
      {$define DELPHI_5}
    {$endif}
  {$endif}

  {$ifdef VER125}
    {$define COMPILER_4}
    {$define CPPB}
    {$define CPPB_4}
  {$endif}

  {$ifdef VER120}
    {$define COMPILER_4}
    {$define DELPHI}
    {$define DELPHI_4}
  {$endif}

  {$ifdef VER110}
    {$define COMPILER_3}
    {$define CPPB}
    {$define CPPB_3}
  {$endif}

  {$ifdef VER100}
    {$define COMPILER_3}
    {$define DELPHI}
    {$define DELPHI_3}
  {$endif}

  {$ifdef VER93}
    {$define COMPILER_2} // C_UP_UPB v1 compiler is really v2 
    {$define CPPB}
    {$define CPPB_1}
  {$endif}

  {$ifdef VER90}
    {$define COMPILER_2}
    {$define DELPHI}
    {$define DELPHI_2}
  {$endif}

  {$ifdef VER80}
    {$define COMPILER_1}
    {$define DELPHI}
    {$define DELPHI_1}
  {$endif}

  {$ifdef COMPILER_1}
    {$define COMPILER_1_UP}
  {$endif}

  {$ifdef COMPILER_2}
    {$define COMPILER_1_UP}
    {$define COMPILER_2_UP}
  {$endif}

  {$ifdef COMPILER_3}
    {$define COMPILER_1_UP}
    {$define COMPILER_2_UP}
    {$define COMPILER_3_UP}
  {$endif}

  {$ifdef COMPILER_4}
    {$define COMPILER_1_UP}
    {$define COMPILER_2_UP}
    {$define COMPILER_3_UP}
    {$define COMPILER_4_UP}
  {$endif}

  {$ifdef COMPILER_5}
    {$define COMPILER_1_UP}
    {$define COMPILER_2_UP}
    {$define COMPILER_3_UP}
    {$define COMPILER_4_UP}
    {$define COMPILER_5_UP}
  {$endif}

  {$ifdef COMPILER_6}
    {$define COMPILER_1_UP}
    {$define COMPILER_2_UP}
    {$define COMPILER_3_UP}
    {$define COMPILER_4_UP}
    {$define COMPILER_5_UP}
    {$define COMPILER_6_UP}
  {$endif}

  {$ifdef COMPILER_7}
    {$define COMPILER_1_UP}
    {$define COMPILER_2_UP}
    {$define COMPILER_3_UP}
    {$define COMPILER_4_UP}
    {$define COMPILER_5_UP}
    {$define COMPILER_6_UP}
    {$define COMPILER_7_UP}
  {$endif}

  {$ifdef DELPHI_2}
    {$define DELPHI_2_UP}
  {$endif}

  {$ifdef DELPHI_3}
    {$define DELPHI_2_UP}
    {$define DELPHI_3_UP}
  {$endif}

  {$ifdef DELPHI_4}
    {$define DELPHI_2_UP}
    {$define DELPHI_3_UP}
    {$define DELPHI_4_UP}
  {$endif}

  {$ifdef DELPHI_5}
    {$define DELPHI_2_UP}
    {$define DELPHI_3_UP}
    {$define DELPHI_4_UP}
    {$define DELPHI_5_UP}
  {$endif}

  {$ifdef DELPHI_6}
    {$define DELPHI_2_UP}
    {$define DELPHI_3_UP}
    {$define DELPHI_4_UP}
    {$define DELPHI_5_UP}
    {$define DELPHI_6_UP}
  {$endif}

  {$ifdef DELPHI_7}
    {$define DELPHI_2_UP}
    {$define DELPHI_3_UP}
    {$define DELPHI_4_UP}
    {$define DELPHI_5_UP}
    {$define DELPHI_6_UP}
    {$define DELPHI_7_UP}
  {$endif}

  {$ifdef CPPB_3}
    {$define CPPB_3_UP}
  {$endif}

  {$ifdef CPPB_4}
    {$define CPPB_3_UP}
    {$define CPPB_4_UP}
  {$endif}

  {$ifdef CPPB_5}
    {$define CPPB_3_UP}
    {$define CPPB_4_UP}
    {$define CPPB_5_UP}
  {$endif}

  {$ifdef CPPB_3_UP}
    // C++ Builder requires this if you use Delphi components in run-time packages.
    {$ObjExportAll On}
  {$endif}

{$else (not Windows)}
  // Linux is the target
  {$define KYLIX}
  {$define KYLIX_1}
  {$define KYLIX_1_UP}
{$endif}

/////////////////////////////////////////////////////////////////////////////
//  Stop the Platform messages
/////////////////////////////////////////////////////////////////////////////

{$IFDEF DELPHI_6_UP}
{$warn SYMBOL_PLATFORM off}
{$warn UNIT_PLATFORM off}
{$ENDIF}

//----------------------------------------------------------------------------------------------------------------------

