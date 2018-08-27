unit UserScript;

var
	kFile: IwbFile;

// -----------------------------------------------------------------------------
// EVENTS
// -----------------------------------------------------------------------------

function Initialize: Integer;
begin
	kFile := FileByName('Fallout4.esm');
end;

function Process(e: IInterface): Integer;
begin
	if Signature(e) <> 'COBJ' then
		exit;

	FixScrapRecord(e);
end;

// -----------------------------------------------------------------------------
// FUNCTIONS
// -----------------------------------------------------------------------------

// ripped from mteFunctions

function FileByName(s: String): IInterface;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FileCount - 1 do begin
    if GetFileName(FileByIndex(i)) = s then begin
      Result := FileByIndex(i);
      break;
    end;
  end;
end;

// ripped from mteFunctions

function HexFormID(e: IInterface): String;
var
  s: String;
begin
  s := GetElementEditValues(e, 'Record Header\FormID');
  if SameText(Signature(e), '') then
    Result := '00000000'
  else
    Result := Copy(s, Pos('[' + Signature(e) + ':', s) + Length(Signature(e)) + 2, 8);
end;

// ripped from dubhFunctions

function HasString(const asNeedle, asHaystack: String; const abCaseSensitive: Boolean): Boolean;
begin
	if abCaseSensitive then
		Result := Pos(asNeedle, asHaystack) > 0
	else
		Result := Pos(Lowercase(asNeedle), Lowercase(asHaystack)) > 0;
end;

// Fix Scrap Record

function FixScrapRecord(e: IInterface): Integer;
var
	i: Integer;
	kComponents, kComponent, kComponentElement, kComponentRecord, kComponentScrap: IInterface;
	sComponentId, sScrapId: String;
begin
	kComponents := ElementBySignature(e, 'FVPA');

	for i := 0 to ElementCount(kComponents) - 1 do begin
		kComponent := ElementByIndex(kComponents, i);
		kComponentElement := ElementByIndex(kComponent, 0);
		kComponentRecord := LinksTo(kComponentElement);

		if Signature(kComponentRecord) <> 'CMPO' then
			continue;

		sComponentId := EditorID(kComponentRecord);

		if HasString('scrap', sComponentId, False) then
			continue;

		sScrapId := sComponentId + '_scrap';

		kComponentScrap := MainRecordByEditorID(GroupBySignature(kFile, 'MISC'), sScrapId);

		SetEditValue(kComponentElement, HexFormID(kComponentScrap));

		FixScrapRecord(e);
	end;
end;

end.
