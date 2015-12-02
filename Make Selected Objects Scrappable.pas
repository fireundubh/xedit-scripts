{
	Purpose: Make Selected Objects Scrappable
	Games: Fallout 4
	Author: fireundubh <fireundubh@gmail.com>
	Version: 0.1
}

unit UserScript;
uses dubhFunctions;

var
	targetFile: IwbFile;
	flst, flst_ids: IInterface;
	sAuthorName: String;

function Initialize: Integer;
var
	grup_cobj, grup_flst, cobj, cobj_edid, cobj_fvpa, cobj_desc, cobj_cnam, cobj_fnam, cobj_intv, flst_edid: IInterface;
	sObjectName: String;
begin
	// prompt to select file
	targetFile := FileSelect('Select file to target:');

	// prompt to name form list
	if not InputQuery('Object Name', 'Name:', sObjectName) then
		exit;
	sObjectName := TrimSpaces(sObjectName);

	//---------------------------------------------------------------------------
	// FLST
	//---------------------------------------------------------------------------

	// get formlist group, or add if needed
	grup_flst := AddGroupBySignature(targetFile, 'FLST');

	// add a new formlist
	flst := AddNewRecordToGroup(grup_flst, 'FLST');

	{EDID}		flst_edid := AddElementByString(flst, 'EDID');
	{FormIDs}	flst_ids  := AddElementByString(flst, 'FormIDs');

	// rename formlist
	sev(flst_edid, 'workshopScrapRecipe_' + sObjectName);

	//---------------------------------------------------------------------------
	// COBJ
	//---------------------------------------------------------------------------

	// get cobj group, or add if needed
	grup_cobj := AddGroupBySignature(targetFile, 'COBJ');

	// add a new cobj
	cobj := AddNewRecordToGroup(grup_cobj, 'COBJ');

	// get cobj elements, or add if needed
	cobj_edid := AddElementByString(cobj, 'EDID');
	cobj_fvpa := AddElementByString(cobj, 'FVPA');
	cobj_desc := AddElementByString(cobj, 'DESC');
	cobj_cnam := AddElementByString(cobj, 'CNAM');
	cobj_fnam := AddElementByString(cobj, 'FNAM');
	cobj_intv := AddElementByString(cobj, 'INTV');

	{EDID} sev(cobj_edid, 'workshop_co_Scrap' + sObjectName);
	{FVPA} seev(cobj_fvpa, '[0]\Component', '000731A3');
	{FVPA} seev(cobj_fvpa, '[0]\Count', '1');
	{DESC} sev(cobj_desc, 'Generated for ' + SmallNameEx(flst));
	{CNAM} sev(cobj_cnam, SmallNameEx(flst));
	{FNAM} seev(cobj_fnam, '[0]', '00106D8F');
	{INTV} seev(cobj_intv, 'Created Object Count', '1');

end;

function Process(e: IInterface): Integer;
begin
	AddRecordToFormlist(flst_ids, e);
	AddOverrideToFile(targetFile, e);
	Log('Converted: ' + SmallNameEx(e));
end;

function Finalize: Integer;
begin
	RemoveByIndex(flst_ids, 0, true); // remove null lnam from formlist
	Log('Selected objects made scrappable and added to: ' + GetFileName(targetFile));
end;

end.
