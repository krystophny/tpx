unit CoreAutoReloadTests;

{$mode delphi}{$H+}

interface

implementation

uses SysUtils, AutoReload, CoreTestSupport;

procedure TestReloadCoordinator;
var
  Coordinator: TReloadCoordinator;
  Ticket, StaleTicket: TReloadReadTicket;
  Decision: TReloadDecision;
  ErrorAgain, Started: Boolean;
begin
  Coordinator := TReloadCoordinator.Create(100);
  try
    Coordinator.Bind(7, 1, 'disk-a-id1', 'content-a', False);
    CheckCore(not Coordinator.NotifyFileEvent(6, 10),
      'Stale watcher generation started a settle timer');
    CheckCore(Coordinator.NotifyFileEvent(7, 10),
      'First file event did not start its settle timer');
    CheckCore(not Coordinator.NotifyFileEvent(7, 40),
      'Burst event restarted the bounded settle timer');
    CheckCore(Coordinator.SettleAt = 110,
      'Event burst exceeded the first-event coalescing deadline');
    CheckCore(not Coordinator.TakeSettledRead(109, Ticket),
      'Read started before the settle deadline');
    CheckCore(Coordinator.TakeSettledRead(110, StaleTicket),
      'Settled event did not start a snapshot read');

    CheckCore(not Coordinator.TakeSettledRead(1000000, Ticket),
      'Idle time created an unsolicited read or timer');
    CheckCore(not Coordinator.NotifyFileEvent(8, 120),
      'Wrong generation started work');
    CheckCore(Coordinator.NotifyFileEvent(7, 120),
      'Event during a read did not schedule a fresh settle');
    Started := Coordinator.CompleteRead(StaleTicket, 'disk-b-id1',
      'content-b', True, True, 1, False, Decision, ErrorAgain);
    CheckCore(not Started and (Decision = rdIgnore),
      'Older read completion was allowed to replace newer event data');
    CheckCore(Coordinator.TakeSettledRead(220, Ticket),
      'Latest event did not start a new read');
    CheckCore(Coordinator.CompleteRead(Ticket, 'disk-a-id2', 'content-a',
      True, True, 1, False, Decision, ErrorAgain) and
      (Decision = rdNoChange),
      'Same content after file replacement caused a scene reload');
    CheckCore(Coordinator.BaseRevision = 'disk-a-id2',
      'No-op refresh did not advance the accepted file identity');

    CheckCore(Coordinator.NotifyFileEvent(7, 230),
      'Changed content event did not start settling');
    CheckCore(Coordinator.TakeSettledRead(330, Ticket),
      'Changed content did not reach the snapshot reader');
    CheckCore(Coordinator.CompleteRead(Ticket, 'disk-b-id2', 'content-b',
      True, True, 1, False, Decision, ErrorAgain) and
      (Decision = rdApply),
      'Clean document did not accept a valid external revision');
    CheckCore(Coordinator.PrepareReloadCommit(Ticket, 'disk-b-id2',
      'content-b', 2, 1, False),
      'Latest clean candidate failed its pre-commit eligibility check');
    CheckCore((Coordinator.BaseRevision = 'disk-a-id2') and
      (Coordinator.LocalRevision = 1),
      'Preparing a scene commit changed accepted state before scene replacement');
    Coordinator.CompleteReloadCommit;
    CheckCore((Coordinator.BaseRevision = 'disk-b-id2') and
      (Coordinator.LocalRevision = 2) and not Coordinator.Dirty,
      'Committed revision state was not rebased');

    CheckCore(Coordinator.NotifyFileEvent(7, 320) and
      Coordinator.TakeSettledRead(420, StaleTicket),
      'Queued snapshot was not available before explicit reload');
    CheckCore(Coordinator.BeginExplicitReload(Ticket),
      'Explicit reload did not supersede a queued snapshot');
    CheckCore(not Coordinator.CompleteRead(StaleTicket, 'stale-id',
      'stale-content', True, True, 2, False, Decision, ErrorAgain) and
      (Decision = rdIgnore),
      'Explicit reload allowed an older snapshot completion');
    CheckCore(Coordinator.CompleteRead(Ticket, 'disk-b-id2', 'content-b',
      True, True, 2, False, Decision, ErrorAgain) and
      (Decision = rdApply),
      'Explicit reload failed after superseding a queued snapshot');
    CheckCore(Coordinator.CommitReload(Ticket, 'disk-b-id2', 'content-b', 3),
      'Superseding explicit reload did not complete its accepted state');

    CheckCore(Coordinator.NotifyFileEvent(7, 335),
      'Second clean revision event did not start settling');
    CheckCore(Coordinator.TakeSettledRead(435, Ticket),
      'Second clean revision did not reach the snapshot reader');
    CheckCore(Coordinator.CompleteRead(Ticket, 'disk-c-id2', 'content-c',
      True, True, 3, False, Decision, ErrorAgain) and
      (Decision = rdApply),
      'Clean second revision was rejected before commit');
    Coordinator.SetLocalState(3, True);
    CheckCore(not Coordinator.CanCommitReload(Ticket, 3, True) and
      not Coordinator.PrepareReloadCommit(Ticket, 'disk-c-id2',
        'content-c', 4, 3, True) and
      not Coordinator.CommitReload(Ticket, 'disk-c-id2', 'content-c', 4) and
      Coordinator.Conflict,
      'Dirty transition with unchanged local revision was discarded at commit');

    Coordinator.SetLocalState(3, True);
    CheckCore(Coordinator.NotifyFileEvent(7, 445),
      'Dirty conflict event did not start settling');
    CheckCore(Coordinator.TakeSettledRead(545, Ticket),
      'Dirty conflict event did not reach the snapshot reader');
    CheckCore(Coordinator.CompleteRead(Ticket, 'disk-d-id2', 'content-d',
      True, True, 3, True, Decision, ErrorAgain) and
      (Decision = rdConflict) and Coordinator.Conflict,
      'Dirty drawing did not preserve a persistent conflict');
    Coordinator.KeepLocal;
    CheckCore(Coordinator.Conflict,
      'Keep local cleared the external conflict');

    CheckCore(Coordinator.BeginExplicitReload(Ticket),
      'Explicit reload was unavailable after Keep local');
    CheckCore(Coordinator.CompleteRead(Ticket, 'disk-d-id2', 'content-d',
      True, True, 3, True, Decision, ErrorAgain) and
      (Decision = rdApply),
      'Confirmed explicit reload could not proceed from a dirty conflict');
    CheckCore(Coordinator.CanCommitReload(Ticket, 3, True),
      'Confirmed explicit reload failed its non-destructive commit gate');
    CheckCore(Coordinator.CommitReload(Ticket, 'disk-d-id2', 'content-d', 4),
      'Explicit reload did not commit after discard was confirmed');
    CheckCore((Coordinator.LocalRevision = 4) and not Coordinator.Dirty and
      not Coordinator.Conflict,
      'Explicit reload did not clear dirty state after the scene commit');

    Coordinator.AcceptSavedRevision(7, 2, 'disk-b-id2', 'content-b', False);
    CheckCore(Coordinator.NotifyFileEvent(7, 336) and
      Coordinator.TakeSettledRead(436, Ticket),
      'Local-edit-during-parse event did not reach the reader');
    CheckCore(Coordinator.CompleteRead(Ticket, 'disk-d-id2', 'content-d',
      True, True, 3, True, Decision, ErrorAgain) and
      (Decision = rdConflict) and Coordinator.Conflict,
      'A local edit during candidate parsing did not block replacement');

    Coordinator.SetLocalState(5, True);
    CheckCore(Coordinator.BeginExplicitReload(Ticket),
      'Explicit reload was unavailable for local edits with an unchanged file');
    CheckCore(Coordinator.CompleteRead(Ticket, 'disk-d-id2', 'content-d',
      True, True, 5, True, Decision, ErrorAgain) and
      (Decision = rdApply),
      'Explicit reload treated unchanged disk bytes as a no-op over local edits');
    CheckCore(Coordinator.CanCommitReload(Ticket, 5, True) and
      Coordinator.CommitReload(Ticket, 'disk-d-id2', 'content-d', 6) and
      not Coordinator.Dirty,
      'Explicit reload could not discard local edits from unchanged disk bytes');

    CheckCore(Coordinator.NotifyFileEvent(7, 450),
      'Invalid revision event did not start settling');
    CheckCore(Coordinator.TakeSettledRead(550, Ticket),
      'Invalid revision did not reach the snapshot reader');
    CheckCore(Coordinator.CompleteRead(Ticket, 'disk-invalid', 'bad-content',
      True, False, 6, False, Decision, ErrorAgain) and
      (Decision = rdInvalid) and ErrorAgain and
      Coordinator.InvalidExternal,
      'First invalid revision was not reported');
    CheckCore(Coordinator.NotifyFileEvent(7, 560),
      'Repeated invalid revision event did not settle');
    CheckCore(Coordinator.TakeSettledRead(660, Ticket),
      'Repeated invalid revision did not reach the reader');
    CheckCore(Coordinator.CompleteRead(Ticket, 'disk-invalid', 'bad-content',
      True, False, 6, False, Decision, ErrorAgain) and
      (Decision = rdInvalid) and not ErrorAgain,
      'Same failing revision was reported more than once');

    CheckCore(Coordinator.NotifyFileEvent(7, 665),
      'Unreadable source event did not start settling');
    CheckCore(Coordinator.TakeSettledRead(765, Ticket),
      'Unreadable source did not reach the reader');
    CheckCore(Coordinator.CompleteRead(Ticket, '', '', True, False, 6, False,
      Decision, ErrorAgain) and (Decision = rdInvalid) and ErrorAgain,
      'First I/O failure without a revision key was hidden');
    CheckCore(Coordinator.NotifyFileEvent(7, 766) and
      Coordinator.TakeSettledRead(866, Ticket),
      'Repeated unreadable-source event did not settle');
    CheckCore(Coordinator.CompleteRead(Ticket, '', '', True, False, 6, False,
      Decision, ErrorAgain) and (Decision = rdInvalid) and not ErrorAgain,
      'Repeated I/O failure without a revision key was reported each time');

    Coordinator.SetLocalState(5, True);
    CheckCore(Coordinator.NotifyFileEvent(7, 670),
      'Deletion event did not start settling');
    CheckCore(Coordinator.TakeSettledRead(770, Ticket),
      'Deletion did not reach the snapshot reader');
    CheckCore(Coordinator.CompleteRead(Ticket, '', '', False, False, 5, True,
      Decision, ErrorAgain) and (Decision = rdMissing) and
      Coordinator.Missing and Coordinator.Conflict,
      'Deleting a dirty source lost its conflict state');
    CheckCore(Coordinator.NotifyFileEvent(7, 780),
      'Recreation event did not start settling');
    CheckCore(Coordinator.TakeSettledRead(880, Ticket),
      'Recreated source did not reach the snapshot reader');
    CheckCore(Coordinator.CompleteRead(Ticket, 'disk-e-id3', 'content-e',
      True, True, 5, True, Decision, ErrorAgain) and
      (Decision = rdConflict) and not Coordinator.Missing,
      'Recreated source did not recover into the dirty conflict state');

    CheckCore(Coordinator.NotifyFileEvent(7, 880),
      'Pending Save As event did not start settling');
    Coordinator.AcceptSavedRevision(8, 6, 'saved-a-id', 'content-a', False);
    CheckCore(Coordinator.IsStaleEvent(7) and
      not Coordinator.TakeSettledRead(1000, Ticket),
      'Save As did not invalidate a pending callback from the prior source');
    CheckCore(Coordinator.NotifyFileEvent(8, 890) and
      not Coordinator.NotifyFileEvent(8, 900),
      'Save event burst did not remain coalesced');
    CheckCore(Coordinator.TakeSettledRead(990, Ticket),
      'Save event burst did not reach the snapshot reader');
    CheckCore(Coordinator.CompleteRead(Ticket, 'external-b-id', 'content-b',
      True, True, 6, False, Decision, ErrorAgain) and
      (Decision = rdApply),
      'External edit immediately following save was mistaken for self-save');

    CheckCore(Coordinator.NotifyFileEvent(8, 1000),
      'Pending-close event did not start settling');
    Coordinator.Close;
    CheckCore(Coordinator.IsStaleEvent(7) and
      not Coordinator.BeginExplicitReload(Ticket),
      'Close did not invalidate outstanding watcher work');
    CheckCore(not Coordinator.TakeSettledRead(2000, Ticket),
      'Close did not cancel a pending settle callback');
  finally
    Coordinator.Free;
  end;
end;


initialization
  RegisterCoreTest('event-driven-auto-reload-coordinator', TestReloadCoordinator);

end.
