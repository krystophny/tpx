unit AutoReload;

{$mode Delphi}

interface

uses SysUtils;

type
  TReloadDecision = (rdIgnore, rdNoChange, rdApply, rdConflict,
    rdMissing, rdInvalid, rdWatchUnavailable);

  TReloadReadTicket = record
    WatchGeneration: QWord;
    EventGeneration: QWord;
    LocalRevision: QWord;
    BaseRevision: string;
    ExplicitReload: Boolean;
  end;

  { Format-neutral state for event-driven source refresh. The UI owns timers,
    file reads, parsing and scene commits; this class only arbitrates revisions
    and generations. Revision keys are opaque to the coordinator. }
  TReloadCoordinator = class
  private
    FWatchGeneration: QWord;
    FEventGeneration: QWord;
    FLocalRevision: QWord;
    FBaseRevision: string;
    FBaseContent: string;
    FLastInvalidRevision: string;
    FHasInvalidRevision: Boolean;
    FInvalidExternal: Boolean;
    FSettleAt: QWord;
    FSettleDelayMS: Cardinal;
    FOpen: Boolean;
    FPaused: Boolean;
    FDirty: Boolean;
    FConflict: Boolean;
    FMissing: Boolean;
    FWatchUnavailable: Boolean;
    FSettlePending: Boolean;
    FReadPending: Boolean;
    FCurrentTicket: TReloadReadTicket;
    FCommitPrepared: Boolean;
    FPreparedRevision: string;
    FPreparedContent: string;
    FPreparedLocalRevision: QWord;
    function TicketIsCurrent(const Ticket: TReloadReadTicket): Boolean;
  public
    constructor Create(SettleDelayMS: Cardinal = 100);
    procedure Bind(WatchGeneration, LocalRevision: QWord;
      const BaseRevision, BaseContent: string; Dirty: Boolean);
    procedure Close;
    procedure SetPaused(Value: Boolean);
    procedure SetWatchUnavailable(Value: Boolean);
    procedure SetLocalState(LocalRevision: QWord; Dirty: Boolean);
    function NotifyFileEvent(WatchGeneration, NowMS: QWord): Boolean;
    function BeginExplicitReload(out Ticket: TReloadReadTicket): Boolean;
    function TakeSettledRead(NowMS: QWord;
      out Ticket: TReloadReadTicket): Boolean;
    function CompleteRead(const Ticket: TReloadReadTicket;
      const DiskRevision, DiskContent: string; Exists, Valid: Boolean;
      LocalRevisionNow: QWord; DirtyNow: Boolean;
      out Decision: TReloadDecision; out ErrorAgain: Boolean): Boolean;
    function CanCommitReload(const Ticket: TReloadReadTicket;
      LocalRevisionNow: QWord; DirtyNow: Boolean): Boolean;
    function PrepareReloadCommit(const Ticket: TReloadReadTicket;
      const DiskRevision, DiskContent: string;
      LocalRevisionAfterCommit, LocalRevisionNow: QWord;
      DirtyNow: Boolean): Boolean;
    procedure CompleteReloadCommit;
    procedure CancelReloadCommit;
    function CommitReload(const Ticket: TReloadReadTicket;
      const DiskRevision, DiskContent: string;
      LocalRevisionAfterCommit: QWord): Boolean;
    procedure KeepLocal;
    procedure AcceptSavedRevision(WatchGeneration, LocalRevision: QWord;
      const DiskRevision, DiskContent: string; Dirty: Boolean);
    function IsStaleEvent(WatchGeneration: QWord): Boolean;
    property Open: Boolean read FOpen;
    property Paused: Boolean read FPaused write SetPaused;
    property Dirty: Boolean read FDirty;
    property Conflict: Boolean read FConflict;
    property InvalidExternal: Boolean read FInvalidExternal;
    property Missing: Boolean read FMissing;
    property WatchUnavailable: Boolean read FWatchUnavailable;
    property SettlePending: Boolean read FSettlePending;
    property SettleAt: QWord read FSettleAt;
    property BaseRevision: string read FBaseRevision;
    property BaseContent: string read FBaseContent;
    property LocalRevision: QWord read FLocalRevision;
    property WatchGeneration: QWord read FWatchGeneration;
    property EventGeneration: QWord read FEventGeneration;
  end;

implementation

constructor TReloadCoordinator.Create(SettleDelayMS: Cardinal);
begin
  inherited Create;
  if SettleDelayMS = 0 then SettleDelayMS := 1;
  FSettleDelayMS := SettleDelayMS;
end;

procedure TReloadCoordinator.Bind(WatchGeneration, LocalRevision: QWord;
  const BaseRevision, BaseContent: string; Dirty: Boolean);
begin
  FWatchGeneration := WatchGeneration;
  FEventGeneration := 0;
  FLocalRevision := LocalRevision;
  FBaseRevision := BaseRevision;
  FBaseContent := BaseContent;
  FLastInvalidRevision := '';
  FHasInvalidRevision := False;
  FInvalidExternal := False;
  FOpen := True;
  FDirty := Dirty;
  FConflict := False;
  FMissing := False;
  FWatchUnavailable := False;
  FSettlePending := False;
  FReadPending := False;
  FCommitPrepared := False;
  FPreparedRevision := '';
  FPreparedContent := '';
  FPreparedLocalRevision := 0;
end;

procedure TReloadCoordinator.Close;
begin
  FOpen := False;
  Inc(FWatchGeneration);
  Inc(FEventGeneration);
  FSettlePending := False;
  FReadPending := False;
  FCommitPrepared := False;
  FPreparedRevision := '';
  FPreparedContent := '';
  FPreparedLocalRevision := 0;
  FConflict := False;
  FMissing := False;
  FLastInvalidRevision := '';
  FHasInvalidRevision := False;
  FInvalidExternal := False;
end;

procedure TReloadCoordinator.SetPaused(Value: Boolean);
begin
  if FPaused = Value then Exit;
  FPaused := Value;
  if Value then
  begin
    FSettlePending := False;
    FReadPending := False;
    Inc(FEventGeneration);
  end;
end;

procedure TReloadCoordinator.SetWatchUnavailable(Value: Boolean);
begin
  FWatchUnavailable := Value;
end;

procedure TReloadCoordinator.SetLocalState(LocalRevision: QWord;
  Dirty: Boolean);
begin
  FLocalRevision := LocalRevision;
  FDirty := Dirty;
end;

function TReloadCoordinator.NotifyFileEvent(WatchGeneration,
  NowMS: QWord): Boolean;
begin
  Result := False;
  if not FOpen or FPaused or IsStaleEvent(WatchGeneration) then Exit;
  Inc(FEventGeneration);
  FReadPending := False;
  FMissing := False;
  if not FSettlePending then
  begin
    FSettleAt := NowMS + FSettleDelayMS;
    FSettlePending := True;
    Result := True;
  end;
end;

function TReloadCoordinator.BeginExplicitReload(
  out Ticket: TReloadReadTicket): Boolean;
begin
  { An explicit user request supersedes a queued or in-progress snapshot. The
    event generation makes any older completion stale before the new read. }
  Result := FOpen;
  if not Result then Exit;
  FSettlePending := False;
  FReadPending := False;
  Inc(FEventGeneration);
  Ticket.WatchGeneration := FWatchGeneration;
  Ticket.EventGeneration := FEventGeneration;
  Ticket.LocalRevision := FLocalRevision;
  Ticket.BaseRevision := FBaseRevision;
  Ticket.ExplicitReload := True;
  FCurrentTicket := Ticket;
  FReadPending := True;
end;

function TReloadCoordinator.TakeSettledRead(NowMS: QWord;
  out Ticket: TReloadReadTicket): Boolean;
begin
  Result := FOpen and not FPaused and FSettlePending and
    (NowMS >= FSettleAt) and not FReadPending;
  if not Result then Exit;
  FSettlePending := False;
  Ticket.WatchGeneration := FWatchGeneration;
  Ticket.EventGeneration := FEventGeneration;
  Ticket.LocalRevision := FLocalRevision;
  Ticket.BaseRevision := FBaseRevision;
  Ticket.ExplicitReload := False;
  FCurrentTicket := Ticket;
  FReadPending := True;
end;

function TReloadCoordinator.TicketIsCurrent(
  const Ticket: TReloadReadTicket): Boolean;
begin
  Result := FOpen and FReadPending and
    (Ticket.WatchGeneration = FWatchGeneration) and
    (Ticket.EventGeneration = FCurrentTicket.EventGeneration) and
    (Ticket.EventGeneration = FEventGeneration);
end;

function TReloadCoordinator.CompleteRead(const Ticket: TReloadReadTicket;
  const DiskRevision, DiskContent: string; Exists, Valid: Boolean;
  LocalRevisionNow: QWord; DirtyNow: Boolean;
  out Decision: TReloadDecision; out ErrorAgain: Boolean): Boolean;
begin
  Result := TicketIsCurrent(Ticket);
  ErrorAgain := False;
  Decision := rdIgnore;
  if not Result then
  begin
    if FReadPending and
      (Ticket.WatchGeneration = FCurrentTicket.WatchGeneration) and
      (Ticket.EventGeneration = FCurrentTicket.EventGeneration) then
      FReadPending := False;
    Exit;
  end;
  FReadPending := False;
  FLocalRevision := LocalRevisionNow;
  FDirty := DirtyNow;
  if not Exists then
  begin
    FMissing := True;
    FInvalidExternal := False;
    FConflict := FDirty;
    Decision := rdMissing;
    Exit;
  end;
  FMissing := False;
  if not Valid then
  begin
    FInvalidExternal := True;
    FConflict := FDirty;
    Decision := rdInvalid;
    ErrorAgain := not FHasInvalidRevision or
      (FLastInvalidRevision <> DiskRevision);
    FLastInvalidRevision := DiskRevision;
    FHasInvalidRevision := True;
    Exit;
  end;
  FLastInvalidRevision := '';
  FHasInvalidRevision := False;
  FInvalidExternal := False;
  if (DiskContent = FBaseContent) and not Ticket.ExplicitReload then
  begin
    FBaseRevision := DiskRevision;
    Decision := rdNoChange;
    FConflict := False;
    Exit;
  end;
  if FDirty and not Ticket.ExplicitReload then
  begin
    FConflict := True;
    Decision := rdConflict;
    Exit;
  end;
  if LocalRevisionNow <> Ticket.LocalRevision then
  begin
    FConflict := True;
    Decision := rdConflict;
    Exit;
  end;
  Decision := rdApply;
end;

function TReloadCoordinator.CanCommitReload(
  const Ticket: TReloadReadTicket; LocalRevisionNow: QWord;
  DirtyNow: Boolean): Boolean;
begin
  Result := not FCommitPrepared and FOpen and
    (Ticket.WatchGeneration = FWatchGeneration) and
    (Ticket.EventGeneration = FEventGeneration) and
    (Ticket.EventGeneration = FCurrentTicket.EventGeneration) and
    (Ticket.LocalRevision = LocalRevisionNow) and
    (not DirtyNow or Ticket.ExplicitReload);
end;

function TReloadCoordinator.PrepareReloadCommit(
  const Ticket: TReloadReadTicket; const DiskRevision,
  DiskContent: string; LocalRevisionAfterCommit,
  LocalRevisionNow: QWord; DirtyNow: Boolean): Boolean;
begin
  Result := CanCommitReload(Ticket, LocalRevisionNow, DirtyNow) and
    (DiskRevision <> '');
  if not Result then Exit;
  FCommitPrepared := True;
  FPreparedRevision := DiskRevision;
  FPreparedContent := DiskContent;
  FPreparedLocalRevision := LocalRevisionAfterCommit;
end;

procedure TReloadCoordinator.CompleteReloadCommit;
begin
  if not FCommitPrepared then
    raise Exception.Create('No reload commit was prepared');
  FBaseRevision := FPreparedRevision;
  FBaseContent := FPreparedContent;
  FLocalRevision := FPreparedLocalRevision;
  FDirty := False;
  FConflict := False;
  FMissing := False;
  FInvalidExternal := False;
  FLastInvalidRevision := '';
  FHasInvalidRevision := False;
  FCommitPrepared := False;
  FPreparedRevision := '';
  FPreparedContent := '';
  FPreparedLocalRevision := 0;
end;

procedure TReloadCoordinator.CancelReloadCommit;
begin
  FCommitPrepared := False;
  FPreparedRevision := '';
  FPreparedContent := '';
  FPreparedLocalRevision := 0;
end;

function TReloadCoordinator.CommitReload(const Ticket: TReloadReadTicket;
  const DiskRevision, DiskContent: string;
  LocalRevisionAfterCommit: QWord): Boolean;
begin
  Result := PrepareReloadCommit(Ticket, DiskRevision, DiskContent,
    LocalRevisionAfterCommit, FLocalRevision, FDirty);
  if Result then CompleteReloadCommit
  else if FOpen and (DiskRevision <> FBaseRevision) and
    (FDirty or (Ticket.LocalRevision <> FLocalRevision)) then
    FConflict := True;
end;

procedure TReloadCoordinator.KeepLocal;
begin
  if FOpen then FConflict := True;
end;

procedure TReloadCoordinator.AcceptSavedRevision(WatchGeneration,
  LocalRevision: QWord; const DiskRevision, DiskContent: string;
  Dirty: Boolean);
begin
  Bind(WatchGeneration, LocalRevision, DiskRevision, DiskContent, Dirty);
end;

function TReloadCoordinator.IsStaleEvent(WatchGeneration: QWord): Boolean;
begin
  Result := (not FOpen) or (WatchGeneration <> FWatchGeneration);
end;

end.
