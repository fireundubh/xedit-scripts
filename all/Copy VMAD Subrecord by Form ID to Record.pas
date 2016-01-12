unit UserScript;

uses dubhFunctions;

var
	lsMasters: TStringList;
	aSource: IInterface;

function Initialize: Integer;
var
	aRecord: IInterface;
	sFormID: String;
begin
	// populate masters list
	lsMasters := TStringList.Create;
	lsMasters.Duplicates := dupIgnore;
	lsMasters.Add('Fallout4.esm');

	// prompt for source data form id
	if not InputQuery('VMAD Template', 'Form ID', sFormID) then
		exit;

	aRecord  := RecordByHexFormID(sFormID);

	// add masters to list
	AddMastersToList(GetFile(aRecord), lsMasters);

	// store source vmad
	aSource := ElementBySignature(aRecord, 'VMAD');
end;

function Process(e: IInterface): Integer;
begin
	AddMastersToFile(GetFile(e), lsMasters, False);
	wbCopyElementToRecord(aSource, e, False, True);
end;

end.