{
	Purpose: To generate "loose mods" for object modification records
	Games: Fallout 4
	Author: fireundubh <fireundubh@gmail.com>

	INSTRUCTIONS:

	1. Ensure that sEditorPrefix is desired.
	2. Select one or many OMOD records, and apply the script.
	3. Select the file where the new MISC records should be saved.

	NOTE: This script will automatically assign the "loose mods" to the OMOD records.
	However, if the respective OMOD records cannot be edited, overrides will be
	automatically created in the target file, and assignment will take place there.
}

unit UserScript;

uses dubhFunctions;

var
	kTargetFile: IwbFile;
	kTemplate: IInterface;
	sEditorPrefix: String;
	lsMasters: TStringList;

// ----------------------------------------------------------------------------
function Initialize: integer;
begin
	// CHANGE, AS NEEDED
	sEditorPrefix := 'miscmod_'; // miscmod_mod_ or miscmod_PA_, for example

	// DO NOT CHANGE ANYTHING ELSE
	kTargetFile := FileSelect('Select the target file:');
	kTemplate := RecordByHexFormID('0015503D'); // miscmod_mod_HuntingRifle_Scope_SightsIron [MISC:0015503D]

  lsMasters := TStringList.Create;
  lsMasters.Duplicates := dupIgnore;
end;

// ----------------------------------------------------------------------------
function Process(e: IInterface): integer;
var
	kGroup, kTargetRecord, kOverride: IInterface;
	i: Integer;
begin
	if Signature(e) <> 'OMOD' then
		exit;

	AddMastersToList(GetFile(e), lsMasters);
	if lsMasters.Count > 0 then
		AddMastersToFile(kTargetFile, lsMasters, True);

	kGroup := AddGroupBySignature(kTargetFile, 'MISC');
	if not Assigned(kGroup) then
		exit;

	kTargetRecord := TryToCreateRecord(e, kGroup);

	if IsEditable(e) then begin
		SetElementEditValues(e, 'LNAM', SmallNameEx(kTargetRecord));
		Log('Generated loose mod: ' + SmallNameEx(kTargetRecord) + ' | Assigned to: ' + SmallNameEx(e));
	end else begin
		kOverride := wbCopyElementToFile(e, kTargetFile, False, True);
		SetElementEditValues(kOverride, 'LNAM', SmallNameEx(kTargetRecord));
		Log('Generated loose mod: ' + SmallNameEx(kTargetRecord) + ' | Assigned to new override: ' + SmallNameEx(kOverride));
	end;

end;

// ----------------------------------------------------------------------------
function TryToCreateRecord(e, akGroup: IInterface): IInterface;
var
	r, kSource, kTarget: IInterface;
begin
	r := AddNewRecordToGroup(akGroup, 'MISC');
	if not Assigned(r) then
		exit;

	// ------------------------------------------------------------------
	// VMAD
	// ------------------------------------------------------------------
	kSource := ElementBySignature(e, 'VMAD');
	if Assigned(kSource) then
		wbCopyElementToRecord(kSource, r, False, True);

	// ------------------------------------------------------------------
	// Editor ID
	// ------------------------------------------------------------------
	kSource := ElementBySignature(e, 'EDID');
	if Assigned(kSource) then begin
		kTarget := AddElementByString(r, 'EDID');
		if Assigned(kTarget) then
			SetEditValue(kTarget, sEditorPrefix + GetEditValue(kSource));
	end;

	// ------------------------------------------------------------------
	// Object Bounds
	// ------------------------------------------------------------------
	kSource := ElementBySignature(kTemplate, 'OBND');
	if Assigned(kSource) then
		wbCopyElementToRecord(kSource, r, False, True);

	// ------------------------------------------------------------------
	// Transform
	// ------------------------------------------------------------------
	kSource := ElementBySignature(kTemplate, 'PTRN');
	if Assigned(kSource) then
		wbCopyElementToRecord(kSource, r, False, True);

	// ------------------------------------------------------------------
	// Name *required
	// ------------------------------------------------------------------
	kSource := ElementBySignature(e, 'FULL');
	kTarget := AddElementByString(r, 'FULL');
	if Assigned(kSource) then
		SetEditValue(kTarget, GetEditValue(kSource));

	// ----------------------------------------------------------------
	// Model\MODL
	// ----------------------------------------------------------------
	kSource := ElementByName(kTemplate, 'Model');
	if Assigned(kSource) then
		wbCopyElementToRecord(kSource, r, False, True);

	// ----------------------------------------------------------------
	// KSIZ
	// ----------------------------------------------------------------
	kSource := ElementBySignature(kTemplate, 'KSIZ');
	if Assigned(kSource) then
		wbCopyElementToRecord(kSource, r, False, True);

	// ----------------------------------------------------------------
	// KWDA
	// ----------------------------------------------------------------
	kSource := ElementBySignature(kTemplate, 'KWDA');
	if Assigned(kSource) then
		wbCopyElementToRecord(kSource, r, False, True);

	// ----------------------------------------------------------------
	// DATA
	// ----------------------------------------------------------------
	kSource := ElementBySignature(kTemplate, 'DATA');
	if Assigned(kSource) then
		wbCopyElementToRecord(kSource, r, False, True);

	Result := r;

end;


end.
