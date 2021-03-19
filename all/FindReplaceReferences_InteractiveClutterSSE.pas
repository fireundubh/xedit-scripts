{
    Find and Replace References for Interactive Clutter SSE

    Author:  fireundubh <fireundubh@gmail.com>
    Version: 1.0
}

unit UserScript;

const
  ICSSE_Name     = 'Interactive Clutter SSE';
  ICSSE_FileName = 'Interactive Clutter SSE.esp';

var
  WorkingFile : IwbFile;


function Initialize: integer;
begin
  WorkingFile := nil;
end;


function Process(e: IwbElement): integer;
begin
  if not Assigned(WorkingFile) then
    try
      WorkingFile := AddNewFileName(ICSSE_Name + ' - ' + GetFileName(GetFile(e)), False);
    except
      WorkingFile := FileByName(ICSSE_Name + ' - ' + GetFileName(GetFile(e)));
    end;

  if Assigned(WorkingFile) then
    AddMasterIfMissing(WorkingFile, GetFileName(GetFile(e)), True);

  Exit;
end;


function Finalize: integer;
var
  f                : IwbFile;

  Statics          : IwbMainRecord;
  MiscObjects      : IwbMainRecord;

  StaticForms      : IwbElement;
  MiscObjectForms  : IwbElement;

  StaticRecord     : IwbMainRecord;

  i                : integer;
begin
  if not Assigned(WorkingFile) then
    Exit;

  f := FileByName(ICSSE_FileName);

  if not Assigned(f) then
    Exit;

  AddMasterIfMissing(WorkingFile, ICSSE_FileName, True);

  Statics         := RecordByFormID(f, $0600085C, False);
  MiscObjects     := RecordByFormID(f, $0600085D, False);

  StaticForms     := ElementByName(Statics,     'FormIDs');
  MiscObjectForms := ElementByName(MiscObjects, 'FormIDs');

  for i := 0 to Pred(ElementCount(StaticForms)) do
  begin
    StaticRecord := MasterOrSelf(LinksTo(ElementByIndex(StaticForms, i)));
    TryToReplaceReference(StaticRecord, MiscObjectForms, i);
  end;
end;


procedure AddMastersIfMissing(AFile: IwbFile; ARecord: IwbMainRecord);
var
  f : IwbFile;
  i : integer;
begin
  f := GetFile(ARecord);
  AddMasterIfMissing(AFile, GetFileName(f), True);
  for i := 0 to Pred(MasterCount(f)) do
    AddMasterIfMissing(AFile, GetFileName(MasterByIndex(f, i)), True);
end;


procedure TryToReplaceReference(AParent: IwbMainRecord; AForms: IwbElement; AIndex: integer);
var
  r                : IwbMainRecord;
  o                : IwbMainRecord;
  MiscObjectRecord : IwbMainRecord;
  WorkingRecord    : IwbMainRecord;
  i                : integer;
begin
  AddMessage('Processing: ' + Name(AParent));

  MiscObjectRecord := LinksTo(ElementByIndex(AForms, AIndex));

  AddMastersIfMissing(WorkingFile, MiscObjectRecord);

  for i := 0 to Pred(ReferencedByCount(AParent)) do begin
    r := ReferencedByIndex(AParent, i);

    // skip references in main plugin
    if GetFileName(GetFile(r)) = 'Interactive Clutter SSE.esp' then
      Continue;

    o := WinningOverride(r);

    if not ContainsStr('REFR', Signature(o)) then
      Continue;

    if not ContainsStr('FURN MSTT STAT', Signature(BaseRecord(o))) then
      Continue;

    try
      AddMastersIfMissing(WorkingFile, o);
      WorkingRecord := wbCopyElementToFile(o, WorkingFile, False, True);
    except
      on E: Exception do
        AddMessage(Name(o) + ' cannot be copied because: ' + E.Message);
      Continue;
    end;

    SetElementNativeValues(WorkingRecord, 'NAME', GetLoadOrderFormID(MiscObjectRecord));
  end;
end;


function FileByName(AFileName: string): IwbFile;
var
  f : IwbFile;
  i : integer;
begin
  Result := nil;

  for i := 0 to Pred(FileCount) do
  begin
    f := FileByIndex(i);

    if SameText(AFileName, GetFileName(f)) then
    begin
      Result := f;
      Break;
    end;
  end;
end;

end.
