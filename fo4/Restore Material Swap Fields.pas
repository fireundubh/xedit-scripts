unit UserScript;
uses dubhFunctions;

var
	sourceFile: IwbFile;

function Initialize: Integer;
begin
	sourceFile := FileSelect('Select source file:');
end;

function Process(e: IInterface): Integer;
var
	e_edid: String;
	e_model, e_mods, r, r_model, r_mods: IInterface;
begin
	e_edid := EditorID(e);

	r := MainRecordByEditorID(GroupBySignature(GetFile(sourceFile), Signature(e)), e_edid);
	if not Assigned(r) then exit;

	r_model := GetElement(r, 'Model');
	if not Assigned(r_model) then exit;

	r_mods := GetElement(r_model, 'MODS');
	if not Assigned(r_mods) then exit;

	e_model := AddElementByString(e, 'Model');
	e_mods := AddElementByString(e_model, 'MODS');

	if gev(e_mods) <> gev(r_mods) then begin
		sev(e_mods, gev(r_mods));
		Log('Restored Material Swap field to ' + SmallNameEx(e) + #13#10);
	end;	
end;

end.
