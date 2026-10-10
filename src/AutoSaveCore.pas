unit AutoSaveCore;

{$mode delphi}{$H+}

interface

type
  TAutoSaveWorkKind = (aswSourceSave, aswRecoveryDraft);
  TAutoSaveWork = set of TAutoSaveWorkKind;

  TAutoSaveTicket = record
    DocumentGeneration: QWord;
    LocalRevision: QWord;
    WatchGeneration: QWord;
  end;

  { LCL-free policy for the one-shot AutoSave and recovery callbacks. The UI
    arms one timer for NextDelayMS, disables it on fire, calls TakeDue, and
    arms it again only when another edit schedules work. }
  TAutoSaveCoordinator = class
  private
    FDocumentGeneration: QWord;
    FLocalRevision: QWord;
    FWatchGeneration: QWord;
    FDocumentOpen: Boolean;
    FCanSaveBack: Boolean;
    FAutoSaveEnabled: Boolean;
    FRecoveryEnabled: Boolean;
    FDirty: Boolean;
    FConflict: Boolean;
    FInteractionDepth: Integer;
    FSaveDueSet: Boolean;
    FRecoveryDueSet: Boolean;
    FSaveDueMS: QWord;
    FRecoveryDueMS: QWord;
    FLastFailure: string;
    FLastFailureKind: TAutoSaveWorkKind;
    procedure AdvanceDocumentGeneration;
    procedure CancelSourceSave;
    procedure CancelRecovery;
    procedure ScheduleSourceSave(NowMS: QWord);
    procedure ScheduleRecoveryDraft(NowMS: QWord);
    procedure SchedulePending(NowMS: QWord);
  public
    constructor Create;
    procedure BindDocument(LocalRevision, WatchGeneration: QWord;
      CanSaveBack, AutoSaveEnabled, RecoveryEnabled, Dirty: Boolean;
      NowMS: QWord);
    procedure CloseDocument;
    procedure BeginInteraction;
    procedure EndInteraction(NowMS: QWord);
    procedure NotifyLocalEdit(LocalRevision, NowMS: QWord;
      Dirty: Boolean);
    procedure NotifyReloadCommitted(LocalRevision, WatchGeneration: QWord);
    procedure SetAutoSaveEnabled(Value: Boolean; NowMS: QWord);
    procedure SetRecoveryEnabled(Value: Boolean; NowMS: QWord);
    procedure SetCanSaveBack(Value: Boolean);
    procedure SetConflict(Value: Boolean);
    function NextDelayMS(NowMS: QWord): Integer;
    function CurrentTicket: TAutoSaveTicket;
    function TakeDue(NowMS: QWord; out Work: TAutoSaveWork;
      out Ticket: TAutoSaveTicket): Boolean;
    function IsTicketCurrent(const Ticket: TAutoSaveTicket): Boolean;
    function CanStart(const Kind: TAutoSaveWorkKind;
      const Ticket: TAutoSaveTicket): Boolean;
    function AcceptSavedRevision(const Ticket: TAutoSaveTicket;
      NewWatchGeneration: QWord): Boolean;
    procedure SourceSaveSucceeded(const Ticket: TAutoSaveTicket;
      NewWatchGeneration: QWord);
    procedure WorkSucceeded(const Kind: TAutoSaveWorkKind;
      const Ticket: TAutoSaveTicket);
    procedure WorkFailed(const Kind: TAutoSaveWorkKind;
      const Ticket: TAutoSaveTicket; const ErrorText: string);
    procedure SourceSaveFailed(const Ticket: TAutoSaveTicket;
      const ErrorText: string);
    procedure ClearFailure;
    property DocumentGeneration: QWord read FDocumentGeneration;
    property LocalRevision: QWord read FLocalRevision;
    property WatchGeneration: QWord read FWatchGeneration;
    property DocumentOpen: Boolean read FDocumentOpen;
    property CanSaveBack: Boolean read FCanSaveBack;
    property AutoSaveEnabled: Boolean read FAutoSaveEnabled;
    property RecoveryEnabled: Boolean read FRecoveryEnabled;
    property Dirty: Boolean read FDirty;
    property Conflict: Boolean read FConflict;
    property LastFailure: string read FLastFailure;
  end;

const
  AutoSaveCoalesceMS = 750;

implementation

constructor TAutoSaveCoordinator.Create;
begin
  inherited Create;
  FAutoSaveEnabled := False;
  FRecoveryEnabled := True;
end;

procedure TAutoSaveCoordinator.AdvanceDocumentGeneration;
begin
  Inc(FDocumentGeneration);
  if FDocumentGeneration = 0 then Inc(FDocumentGeneration);
end;

procedure TAutoSaveCoordinator.CancelSourceSave;
begin
  FSaveDueSet := False;
end;

procedure TAutoSaveCoordinator.CancelRecovery;
begin
  FRecoveryDueSet := False;
end;

procedure TAutoSaveCoordinator.ScheduleSourceSave(NowMS: QWord);
begin
  if not FDocumentOpen or (FInteractionDepth > 0) then
    Exit;
  if FDirty and FAutoSaveEnabled and FCanSaveBack and not FConflict then
  begin
    FSaveDueMS := NowMS + AutoSaveCoalesceMS;
    FSaveDueSet := True;
  end;
end;

procedure TAutoSaveCoordinator.ScheduleRecoveryDraft(NowMS: QWord);
begin
  if not FDocumentOpen or (FInteractionDepth > 0) then
    Exit;
  if FDirty and FRecoveryEnabled then
  begin
    FRecoveryDueMS := NowMS + AutoSaveCoalesceMS;
    FRecoveryDueSet := True;
  end;
end;

procedure TAutoSaveCoordinator.SchedulePending(NowMS: QWord);
begin
  ScheduleSourceSave(NowMS);
  ScheduleRecoveryDraft(NowMS);
end;

procedure TAutoSaveCoordinator.BindDocument(LocalRevision,
  WatchGeneration: QWord; CanSaveBack, AutoSaveEnabled,
  RecoveryEnabled, Dirty: Boolean; NowMS: QWord);
begin
  AdvanceDocumentGeneration;
  FDocumentOpen := True;
  FLocalRevision := LocalRevision;
  FWatchGeneration := WatchGeneration;
  FCanSaveBack := CanSaveBack;
  FAutoSaveEnabled := AutoSaveEnabled and CanSaveBack;
  FRecoveryEnabled := RecoveryEnabled;
  FDirty := Dirty;
  FConflict := False;
  FInteractionDepth := 0;
  FSaveDueSet := False;
  FRecoveryDueSet := False;
    FLastFailure := '';
  if FDirty then SchedulePending(NowMS);
end;

procedure TAutoSaveCoordinator.CloseDocument;
begin
  AdvanceDocumentGeneration;
  FDocumentOpen := False;
  FCanSaveBack := False;
  FAutoSaveEnabled := False;
  FDirty := False;
  FConflict := False;
  FInteractionDepth := 0;
  FSaveDueSet := False;
  FRecoveryDueSet := False;
  FLastFailure := '';
end;

procedure TAutoSaveCoordinator.BeginInteraction;
begin
  if not FDocumentOpen then Exit;
  Inc(FInteractionDepth);
  FSaveDueSet := False;
  FRecoveryDueSet := False;
end;

procedure TAutoSaveCoordinator.EndInteraction(NowMS: QWord);
begin
  if FInteractionDepth <= 0 then Exit;
  Dec(FInteractionDepth);
  if FInteractionDepth = 0 then SchedulePending(NowMS);
end;

procedure TAutoSaveCoordinator.NotifyLocalEdit(LocalRevision,
  NowMS: QWord; Dirty: Boolean);
begin
  if not FDocumentOpen then Exit;
  FLocalRevision := LocalRevision;
  FDirty := Dirty;
  CancelSourceSave;
  CancelRecovery;
  if FDirty then SchedulePending(NowMS);
end;

procedure TAutoSaveCoordinator.NotifyReloadCommitted(LocalRevision,
  WatchGeneration: QWord);
begin
  if not FDocumentOpen then Exit;
  AdvanceDocumentGeneration;
  FLocalRevision := LocalRevision;
  FWatchGeneration := WatchGeneration;
  FDirty := False;
  FConflict := False;
  FInteractionDepth := 0;
  FSaveDueSet := False;
  FRecoveryDueSet := False;
  FLastFailure := '';
end;

procedure TAutoSaveCoordinator.SetAutoSaveEnabled(Value: Boolean;
  NowMS: QWord);
begin
  FAutoSaveEnabled := Value and FCanSaveBack;
  CancelSourceSave;
  if FAutoSaveEnabled and FDirty and not FConflict then
    ScheduleSourceSave(NowMS);
end;

procedure TAutoSaveCoordinator.SetRecoveryEnabled(Value: Boolean;
  NowMS: QWord);
begin
  FRecoveryEnabled := Value;
  CancelRecovery;
  if FRecoveryEnabled and FDirty then ScheduleRecoveryDraft(NowMS);
end;

procedure TAutoSaveCoordinator.SetCanSaveBack(Value: Boolean);
begin
  FCanSaveBack := Value;
  if not Value then
  begin
    FAutoSaveEnabled := False;
    CancelSourceSave;
  end;
end;

procedure TAutoSaveCoordinator.SetConflict(Value: Boolean);
begin
  { A conflict is evidence of an external revision, not a transient pause.
    Keep Local must not clear it; only a successful accepted source write,
    committed reload, or a new document binding establishes a new baseline. }
  if not Value then Exit;
  FConflict := True;
  CancelSourceSave;
end;

function TAutoSaveCoordinator.NextDelayMS(NowMS: QWord): Integer;
var
  Due: QWord;
begin
  Result := -1;
  if not FDocumentOpen or (FInteractionDepth > 0) then
    Exit;
  if FSaveDueSet then Due := FSaveDueMS
  else if FRecoveryDueSet then Due := FRecoveryDueMS
  else Exit;
  if FRecoveryDueSet and (FRecoveryDueMS < Due) then Due := FRecoveryDueMS;
  if Due <= NowMS then Result := 1
  else if Due - NowMS > High(Integer) then Result := High(Integer)
  else Result := Integer(Due - NowMS);
end;

function TAutoSaveCoordinator.CurrentTicket: TAutoSaveTicket;
begin
  Result.DocumentGeneration := FDocumentGeneration;
  Result.LocalRevision := FLocalRevision;
  Result.WatchGeneration := FWatchGeneration;
end;

function TAutoSaveCoordinator.TakeDue(NowMS: QWord;
  out Work: TAutoSaveWork; out Ticket: TAutoSaveTicket): Boolean;
begin
  Work := [];
  Ticket.DocumentGeneration := FDocumentGeneration;
  Ticket.LocalRevision := FLocalRevision;
  Ticket.WatchGeneration := FWatchGeneration;
  Result := False;
  if not FDocumentOpen or (FInteractionDepth > 0) then
    Exit;
  if FSaveDueSet and (FSaveDueMS <= NowMS) then
  begin
    Include(Work, aswSourceSave);
    FSaveDueSet := False;
  end;
  if FRecoveryDueSet and (FRecoveryDueMS <= NowMS) then
  begin
    Include(Work, aswRecoveryDraft);
    FRecoveryDueSet := False;
  end;
  Result := Work <> [];
end;

function TAutoSaveCoordinator.IsTicketCurrent(
  const Ticket: TAutoSaveTicket): Boolean;
begin
  Result := FDocumentOpen and
    (Ticket.DocumentGeneration = FDocumentGeneration) and
    (Ticket.LocalRevision = FLocalRevision) and
    (Ticket.WatchGeneration = FWatchGeneration);
end;

function TAutoSaveCoordinator.CanStart(const Kind: TAutoSaveWorkKind;
  const Ticket: TAutoSaveTicket): Boolean;
begin
  Result := IsTicketCurrent(Ticket) and FDirty;
  if not Result then Exit;
  if Kind = aswSourceSave then
    Result := FAutoSaveEnabled and FCanSaveBack and not FConflict
  else
    Result := FRecoveryEnabled;
end;

function TAutoSaveCoordinator.AcceptSavedRevision(
  const Ticket: TAutoSaveTicket; NewWatchGeneration: QWord): Boolean;
begin
  Result := IsTicketCurrent(Ticket);
  if not Result then Exit;
  AdvanceDocumentGeneration;
  FWatchGeneration := NewWatchGeneration;
  FDirty := False;
  FConflict := False;
  FLastFailure := '';
  CancelSourceSave;
  CancelRecovery;
end;

procedure TAutoSaveCoordinator.SourceSaveSucceeded(
  const Ticket: TAutoSaveTicket; NewWatchGeneration: QWord);
begin
  if not CanStart(aswSourceSave, Ticket) then Exit;
  AcceptSavedRevision(Ticket, NewWatchGeneration);
end;

procedure TAutoSaveCoordinator.WorkSucceeded(const Kind: TAutoSaveWorkKind;
  const Ticket: TAutoSaveTicket);
begin
  if not IsTicketCurrent(Ticket) then Exit;
  if (FLastFailure <> '') and (FLastFailureKind = Kind) then
    FLastFailure := '';
end;

procedure TAutoSaveCoordinator.WorkFailed(const Kind: TAutoSaveWorkKind;
  const Ticket: TAutoSaveTicket; const ErrorText: string);
begin
  if not IsTicketCurrent(Ticket) then Exit;
  FLastFailure := ErrorText;
  FLastFailureKind := Kind;
  if Kind = aswSourceSave then CancelSourceSave
  else CancelRecovery;
end;

procedure TAutoSaveCoordinator.SourceSaveFailed(
  const Ticket: TAutoSaveTicket; const ErrorText: string);
begin
  WorkFailed(aswSourceSave, Ticket, ErrorText);
end;

procedure TAutoSaveCoordinator.ClearFailure;
begin
  FLastFailure := '';
end;

end.
