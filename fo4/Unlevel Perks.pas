unit UserScript;

uses dubhFunctions;

var
	outputFile: IwbFile;
	lsMasters, lsPerks: TStringList;
	copyIdenticalToMaster: Boolean;

function Initialize: Integer;
begin
	// create list for master files
	lsMasters := TStringList.Create;
	lsMasters.Duplicates := dupIgnore;

	lsPerks := TStringList.Create;
	
	copyIdenticalToMaster := False;
end;

function Process(e: IInterface): Integer;
var
	i: Integer;
	item, items: IInterface;
begin
	if Signature(e) <> 'FLST' then
		Raise Exception.Create('Script must be run on a formlist record.');

	outputFile := GetFile(e);

	items := ElementByName(e, 'FormIDs');
	for i := 0 to ElementCount(items) - 1 do begin
		item := LastOverride(LinksTo(ElementByIndex(items, i)));
		CopyRecordAsOverride(item);
	end;
end;

function Finalize: Integer;
begin
	lsPerks.Free;
	lsMasters.Free;
end;

procedure GeneratePerkList(p: IInterface; l: TStringList);
var
	masterPerk, previousPerk, nextPerk: IInterface;
begin
	masterPerk := LastOverride(p);

	// do nothing with perks where there is no next perk
	nextPerk := ElementBySignature(masterPerk, 'NNAM');
	if not Assigned(nextPerk) then begin
		l.Add(HexFormID(masterPerk));
		exit;
	end;

	previousPerk := masterPerk;

	repeat
		l.Add(HexFormID(previousPerk));
		nextPerk := LastOverride(LinksTo(ElementBySignature(previousPerk, 'NNAM')));
		previousPerk := nextPerk;
	until Equals(nextPerk, masterPerk);
end;

procedure CopyRecordAsOverride(e: IInterface);
var
	i: Integer;
	perk, level, override, playable: IInterface;
begin
	// playable only
	playable := ElementByPath(e, 'DATA\Playable');
	if HasString('False', GetEditValue(playable), True) then
		exit;

	// create a list of form ids for perk series
	GeneratePerkList(e, lsPerks);

	// process list
	for i := 0 to lsPerks.Count - 1 do begin
		perk := LastOverride(RecordByHexFormID(lsPerks[i]));

		// playable only
		playable := ElementByPath(perk, 'DATA\Playable');
		if HasString('False', GetEditValue(playable), True) then
			continue;

		if not copyIdenticalToMaster then begin
			level := ElementByPath(perk, 'DATA\Level');
			
			if not Assigned(level) or GetNativeValue(level) = i then 
				continue;
		end;

		// add masters
		AddMastersToList(GetFile(perk), lsMasters);
		AddMastersToFile(outputFile, lsMasters, True);

		// copy perk as override
		override := wbCopyElementToFile(perk, outputFile, False, True);

		// change perk level in new override record
		level := ElementByPath(override, 'DATA\Level');
		SetNativeValue(level, i);
	end;
	
	lsPerks.Clear;
end;

end.
