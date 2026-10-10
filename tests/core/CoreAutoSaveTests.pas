unit CoreAutoSaveTests;

{$mode delphi}{$H+}

interface

implementation

uses SysUtils, Classes, DocumentFormats, AutoSaveCore, AutoSavePreferences,
  CoreTestSupport;

function UnicodePathChar(Codepoint: Word): TDocumentPath;
var Value: UnicodeString;
begin
  SetLength(Value, 1);
  Value[1] := WideChar(Codepoint);
  Result := UTF8Encode(Value);
  CheckCore((Length(Result) = 2) and (Byte(Result[1]) = $CE) and
    (Byte(Result[2]) = (Codepoint and $FF)),
    'Unicode path fixture must contain the expected UTF-8 bytes');
end;

procedure TestDefaultsAndCapability;
var
  C: TAutoSaveCoordinator;
  Work: TAutoSaveWork;
  Ticket: TAutoSaveTicket;
begin
  C := TAutoSaveCoordinator.Create;
  try
    CheckCore(not C.AutoSaveEnabled, 'AutoSave must default off');
    CheckCore(C.RecoveryEnabled, 'crash recovery must default on');
    C.BindDocument(1, 2, False, True, True, False, 0);
    CheckCore(C.NextDelayMS(0) < 0,
      'a clean idle document must not schedule periodic work');
    CheckCore(not C.TakeDue(10000, Work, Ticket),
      'an idle document must produce no background save callback');
    CheckCore(not C.AutoSaveEnabled,
      'an unsupported document must not enable source AutoSave');
    C.NotifyLocalEdit(3, 100, True);
    CheckCore(C.NextDelayMS(100) = AutoSaveCoalesceMS,
      'dirty unsupported documents still schedule recovery');
    CheckCore(C.TakeDue(850, Work, Ticket), 'recovery work should become due');
    CheckCore(Work = [aswRecoveryDraft],
      'unsupported documents must schedule only a recovery draft');
    CheckCore(C.CanStart(aswRecoveryDraft, Ticket),
      'recovery should remain available for unsupported documents');
    CheckCore(not C.CanStart(aswSourceSave, Ticket),
      'unsupported documents must not start source saves');
    C.SetCanSaveBack(True);
    C.SetAutoSaveEnabled(True, 900);
    C.CloseDocument;
    CheckCore(not C.DocumentOpen and not C.AutoSaveEnabled and
      (C.NextDelayMS(2000) < 0),
      'closing a document must clear its toggle and pending work');
    C.BindDocument(4, 5, False, False, True, True, 2000);
    CheckCore(C.NextDelayMS(2000) = AutoSaveCoalesceMS,
      'a deliberately restored dirty draft must schedule a fresh recovery');
  finally
    C.Free;
  end;
end;

procedure TestCompletedInteractionAndIndependentDebounce;
var
  C: TAutoSaveCoordinator;
  Work: TAutoSaveWork;
  Ticket: TAutoSaveTicket;
  I: Integer;
begin
  C := TAutoSaveCoordinator.Create;
  try
    C.BindDocument(0, 7, True, False, True, False, 0);
    C.NotifyLocalEdit(1, 100, True);
    C.SetAutoSaveEnabled(True, 200);
    CheckCore(C.NextDelayMS(200) = 650,
      'enabling AutoSave must not postpone the already scheduled recovery');
    CheckCore(C.TakeDue(850, Work, Ticket), 'recovery should be due first');
    CheckCore(Work = [aswRecoveryDraft],
      'AutoSave and recovery deadlines must remain independent');
    CheckCore(C.TakeDue(950, Work, Ticket), 'source save should be due later');
    CheckCore(Work = [aswSourceSave],
      'the later deadline should contain only source-save work');
    C.NotifyLocalEdit(13, 1000, True);
    C.SetAutoSaveEnabled(False, 1050);
    CheckCore(C.NextDelayMS(1050) = 700,
      'disabling AutoSave must leave the recovery deadline untouched');
    CheckCore(C.TakeDue(1750, Work, Ticket) and
      (Work = [aswRecoveryDraft]),
      'disabling source publication must not disable recovery');
    C.SetAutoSaveEnabled(True, 1800);

    C.BeginInteraction;
    for I := 2 to 12 do
      C.NotifyLocalEdit(I, 1000 + I, True);
    CheckCore(C.NextDelayMS(2000) < 0,
      'intermediate interaction changes must not become due work');
    C.EndInteraction(2000);
    CheckCore(C.NextDelayMS(2000) = AutoSaveCoalesceMS,
      'interaction completion should start one final coalescing delay');
    CheckCore(not C.TakeDue(2749, Work, Ticket),
      'the final state must not be serialized before the delay ends');
    CheckCore(C.TakeDue(2750, Work, Ticket),
      'completed interaction should produce due work');
    CheckCore(Ticket.LocalRevision = 12,
      'the ticket must describe the last edit in the interaction');
    CheckCore(Work = [aswSourceSave, aswRecoveryDraft],
      'the completed burst should coalesce to one combined due callback');
  finally
    C.Free;
  end;
end;

procedure TestConflictFailuresAndGenerationInvalidation;
var
  C: TAutoSaveCoordinator;
  Work: TAutoSaveWork;
  Ticket: TAutoSaveTicket;
begin
  C := TAutoSaveCoordinator.Create;
  try
    C.BindDocument(10, 20, True, True, True, False, 0);
    C.NotifyLocalEdit(11, 100, True);
    C.SetConflict(True);
    CheckCore(C.NextDelayMS(100) = AutoSaveCoalesceMS,
      'conflict should preserve the independent recovery deadline');
    CheckCore(C.TakeDue(850, Work, Ticket), 'recovery should survive conflict');
    CheckCore(Work = [aswRecoveryDraft],
      'conflict must suspend source writes while retaining recovery');
    CheckCore(not C.CanStart(aswSourceSave, Ticket),
      'a conflict must prevent an automatic source write');
    C.WorkFailed(aswRecoveryDraft, Ticket, 'recovery disk failure');
    CheckCore(C.Dirty and C.Conflict,
      'recovery failure must preserve dirty and conflict states');
    CheckCore(C.LastFailure = 'recovery disk failure',
      'recovery failure should remain available for persistent status');
    CheckCore(C.NextDelayMS(850) < 0,
      'a failed recovery write must not create a retry timer');

    C.SetConflict(False);
    CheckCore(C.Conflict,
      'Keep Local must not clear an observed external conflict');
    C.NotifyLocalEdit(12, 1000, True);
    CheckCore(C.TakeDue(1750, Work, Ticket),
      'a later completed edit may schedule new work');
    CheckCore(Work = [aswRecoveryDraft],
      'a later edit after Keep Local must remain suspended from source writes');
    CheckCore(not C.CanStart(aswSourceSave, Ticket),
      'a latched conflict must continue blocking automatic publication');
    CheckCore(C.AcceptSavedRevision(C.CurrentTicket, 21) and not C.Conflict,
      'only a successfully accepted explicit save may clear the conflict');
    C.NotifyLocalEdit(13, 1500, True);
    CheckCore(C.TakeDue(2250, Work, Ticket) and
      (Work = [aswSourceSave, aswRecoveryDraft]),
      'an edit after an accepted conflict resolution may be saved again');
    C.WorkFailed(aswSourceSave, Ticket, 'source replace failed');
    CheckCore(C.Dirty and (C.LastFailure = 'source replace failed'),
      'failed publication must retain the dirty state and its error');
    CheckCore(C.NextDelayMS(2250) < 0,
      'a failed publication must not spin or retry periodically');

    C.NotifyLocalEdit(14, 2500, True);
    CheckCore(C.TakeDue(3250, Work, Ticket), 'new revision should become due');
    C.NotifyReloadCommitted(14, 21);
    CheckCore(not C.Dirty and not C.Conflict,
      'a committed reload should establish a clean accepted revision');
    CheckCore(not C.IsTicketCurrent(Ticket) and (C.NextDelayMS(4000) < 0),
      'reload must invalidate old tickets without scheduling a save');
  finally
    C.Free;
  end;
end;

procedure TestAcceptedSaveAndSubsequentUndoEdit;
var
  C: TAutoSaveCoordinator;
  Work: TAutoSaveWork;
  Ticket: TAutoSaveTicket;
begin
  C := TAutoSaveCoordinator.Create;
  try
    C.BindDocument(1, 4, True, False, True, False, 0);
    C.NotifyLocalEdit(2, 100, True);
    Ticket := C.CurrentTicket;
    CheckCore(C.AcceptSavedRevision(Ticket, 5),
      'a committed explicit save should accept its exact current revision');
    CheckCore(not C.Dirty and (C.WatchGeneration = 5) and
      not C.IsTicketCurrent(Ticket),
      'accepting a save should establish a clean baseline and invalidate old work');
    CheckCore(C.NextDelayMS(1000) < 0,
      'accepting a save must cancel pending recovery work');

    C.NotifyLocalEdit(3, 1100, True);
    CheckCore(C.Dirty and (C.NextDelayMS(1100) = AutoSaveCoalesceMS),
      'an undo or other later edit must become dirty against the saved baseline');
    CheckCore(C.TakeDue(1850, Work, Ticket) and
      (Work = [aswRecoveryDraft]),
      'a subsequent edit after the save must schedule fresh recovery work');
  finally
    C.Free;
  end;
end;

procedure TestWorkFailuresClearOnlyAfterMatchingSuccess;
var
  C: TAutoSaveCoordinator;
  Work: TAutoSaveWork;
  Ticket: TAutoSaveTicket;
begin
  C := TAutoSaveCoordinator.Create;
  try
    C.BindDocument(0, 1, False, False, True, False, 0);
    C.NotifyLocalEdit(1, 100, True);
    CheckCore(C.TakeDue(850, Work, Ticket),
      'the initial recovery write should become due');
    C.WorkFailed(aswRecoveryDraft, Ticket, 'recovery write failed');
    C.NotifyLocalEdit(2, 1000, True);
    CheckCore(C.TakeDue(1750, Work, Ticket),
      'a later edit should make one new recovery attempt available');
    C.WorkSucceeded(aswRecoveryDraft, Ticket);
    CheckCore(C.LastFailure = '',
      'a successful retry should clear its own persistent failure');

    C.SetCanSaveBack(True);
    C.SetAutoSaveEnabled(True, 2000);
    C.NotifyLocalEdit(3, 2100, True);
    CheckCore(C.TakeDue(2850, Work, Ticket),
      'both independent mechanisms should become due together');
    C.WorkFailed(aswSourceSave, Ticket, 'source publication failed');
    C.WorkSucceeded(aswRecoveryDraft, Ticket);
    CheckCore(C.LastFailure = 'source publication failed',
      'recovery success must not hide an unrelated source-save failure');
  finally
    C.Free;
  end;
end;

procedure TestPerDocumentPreference;
var
  Values: TStringList;
  Enabled: Boolean;
  FileName, UpperGreekPath, LowerGreekPath: TDocumentPath;
begin
  Values := TStringList.Create;
  try
    FileName := IncludeTrailingPathDelimiter(GetTempDir(False)) +
      'tpx-autosave-preference.tpx';
    CheckCore(not ReadDocumentAutoSavePreference(Values, FileName, Enabled),
      'a missing per-document preference should be distinguishable');
    CheckCore(not Enabled, 'a missing preference should use AutoSave-off default');
    WriteDocumentAutoSavePreference(Values, FileName, True);
    CheckCore(ReadDocumentAutoSavePreference(Values,
      IncludeTrailingPathDelimiter(GetTempDir(False)) + '.' + PathDelim +
      'tpx-autosave-preference.tpx', Enabled) and Enabled,
      'per-document preference should survive lexical path normalization');
    WriteDocumentAutoSavePreference(Values, FileName, False);
    CheckCore(ReadDocumentAutoSavePreference(Values, FileName, Enabled) and
      not Enabled, 'a per-document preference should be independently disableable');
    CheckCore(not ReadDocumentAutoSavePreference(Values, '', Enabled) and
      not Enabled, 'untitled documents use the default without a path key');
    UpperGreekPath := TDocumentPath(UTF8String(GetCurrentDir)) + PathDelim + 'drawing-' +
      UnicodePathChar($0391) + '.tpx';
    LowerGreekPath := TDocumentPath(UTF8String(GetCurrentDir)) + PathDelim + 'drawing-' +
      UnicodePathChar($03B1) + '.tpx';
    {$IFDEF MSWINDOWS}
    CheckCore(SameDocumentPath(UpperGreekPath, LowerGreekPath),
      'Windows document path identity should fold Unicode case');
    {$ENDIF}
    CheckCore(DocumentPreferenceKey(UpperGreekPath) <>
      DocumentPreferenceKey(TDocumentPath(GetTempDir(False)) +
        'drawing-?.tpx'),
      'preference keys must hash Unicode path codepoints instead of ACP replacement');
    WriteDocumentAutoSavePreference(Values, UpperGreekPath, True);
    {$IFDEF MSWINDOWS}
    CheckCore(ReadDocumentAutoSavePreference(Values, LowerGreekPath, Enabled) and
      Enabled, 'Windows document preferences should compare Unicode paths case-insensitively');
    {$ELSE}
    CheckCore(not ReadDocumentAutoSavePreference(Values, LowerGreekPath, Enabled),
      'case-sensitive platforms should keep distinct Unicode path preferences');
    {$ENDIF}
  finally
    Values.Free;
  end;
end;

initialization
  RegisterCoreTest('autosave-defaults-capability', @TestDefaultsAndCapability);
  RegisterCoreTest('autosave-interaction-debounce',
    @TestCompletedInteractionAndIndependentDebounce);
  RegisterCoreTest('autosave-conflict-failure-generations',
    @TestConflictFailuresAndGenerationInvalidation);
  RegisterCoreTest('autosave-save-undo-reschedule',
    @TestAcceptedSaveAndSubsequentUndoEdit);
  RegisterCoreTest('autosave-failure-status-independent',
    @TestWorkFailuresClearOnlyAfterMatchingSuccess);
  RegisterCoreTest('autosave-document-preference', @TestPerDocumentPreference);

end.
