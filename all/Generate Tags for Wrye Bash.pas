{
	Purpose: Automatic Bash Tag Generation for Wrye Bash and Mator Smash
	Games: FO3/FNV/TES4/TES5/SSE
	Author: fireundubh <fireundubh@gmail.com>

	Requirements:
	- https://raw.githubusercontent.com/fireundubh/xedit-scripts/master/lib/dubhFunctions.pas
	- https://raw.githubusercontent.com/matortheeternal/TES5EditScripts/master/Edit%20Scripts/mteFunctions.pas
}

unit BashTagsDetector;

uses dubhFunctions;

var
	kFile: IwbFile;
	lsLog, lsSuggestedTags: TStringList;
	sFileName, sTag, sScriptName, sScriptVersion, sScriptAuthor, sScriptEmail: String;
	optionAddTags, optionOutputLog: Integer;
	bDebug: Boolean;

function Initialize: Integer;
begin
	sScriptName    := 'Tag Generator'; // working name
	sScriptVersion := '1.6.1.2';
	sScriptAuthor  := 'fireundubh';
	sScriptEmail   := 'fireundubh@gmail.com';

	// clear
	ClearMessages();

	// show script header
	AddMessage(sScriptName + ' v' + sScriptVersion + ' by ' + sScriptAuthor + ' <' + sScriptEmail + '>');

	// prompt to write tags to file header
	optionAddTags := MessageDlg('Do you want to add suggested tags to the file header?', mtConfirmation, [mbYes, mbNo, mbAbort], 0);
	if optionAddTags = mrAbort then
		exit;

	optionOutputLog := MessageDlg('Do you want a log of successful bash tag tests?', mtConfirmation, [mbYes, mbNo, mbAbort], 0);
	if optionOutputLog = mrAbort then
		exit;

	// create list of log entries
	lsLog := TStringList.Create;
	lsLog.Sorted := False;
	lsLog.Duplicates := dupAccept;

	// create list of tags
	lsSuggestedTags := TStringList.Create;
	lsSuggestedTags.Sorted := True;
	lsSuggestedTags.Duplicates := dupIgnore;
	lsSuggestedTags.Delimiter := ','; // separated by comma

	Separator(True);

	if wbGameMode = gmFO3 then
		AddMessage('Using record structure for Fallout 3');
	if wbGameMode = gmFNV then
		AddMessage('Using record structure for Fallout: New Vegas');
	if wbGameMode = gmFO4 then
		Raise Exception.Create('Fallout 4 is not supported yet.');
	if wbGameMode = gmTES4 then
		AddMessage('Using record structure for TES IV: Oblivion');
	if wbGameMode = gmTES5 then
		AddMessage('Using record structure for TES V: Skyrim');
	if wbGameMode = gmSSE then
		AddMessage('Using record structure for TES V: Skyrim Special Edition');

	Separator(False);
end;

function Process(e: IInterface): Integer;
var
	o: IInterface;
	sElement, sTag, sSignature: String;
	ConflictState: TConflictThis;
	bRunOnce: Boolean;
	i, iFormID: Integer;
begin
	bRunOnce := False;

	// exit conditions
	ConflictState := ConflictAllForMainRecord(e);

	// get record signature
	sSignature := Signature(e);

	if (optionAddTags = mrAbort)
	or (optionOutputLog = mrAbort)
	or (sSignature = 'TES4')
	or (ConflictState = caUnknown)
	or (ConflictState = caOnlyOne)
	or (ConflictState = caNoConflict) then
		exit;

	// get file and file name
	if not bRunOnce then
	begin
		kFile := GetFile(e);
		sFileName := GetFileName(kFile);
		bRunOnce := True;
	end;

	// exit if the record should not be processed
	if (sFileName = 'Dawnguard.esm') then
	begin
		iFormID := FileFormID(e);
		if (iFormID = $00016BCF)
		or (iFormID = $0001EE6D)
		or (iFormID = $0001FA4C)
		or (iFormID = $00039F67)
		or (iFormID = $0006C3B6) then
			exit;
	end;

	// get master record
	o := Master(e);

	// exit if the override does not exist
	if not Assigned(o) then
		exit;

	// if record overrides several masters, then get the last one
	if OverrideCount(o) > 1 then
		o := OverrideByIndex(o, OverrideCount(o) - 2);

	// stop processing deleted records to avoid errors
	if GetIsDeleted(e)
	or GetIsDeleted(o) then
		exit;

	sElement := 'ACBS\Template Flags';

	// -------------------------------------------------------------------------------
	// GROUP: Supported tags exclusive to FNV
	// -------------------------------------------------------------------------------
	if wbGameMode = gmFNV then
		if sSignature = 'WEAP' then
			ProcessTag('WeaponMods', e, o);

	// -------------------------------------------------------------------------------
	// GROUP: Supported tags exclusive to TES4
	// -------------------------------------------------------------------------------
	if wbGameMode = gmTES4 then
	begin
		if InDelimitedList(sSignature, 'CREA NPC_', ' ') then
		begin
			ProcessTag('Actors.Spells', e, o);

			if sSignature = 'CREA' then
				ProcessTag('Creatures.Blood', e, o);
		end;

		// TODO: Npc.EyesOnly - NOT IMPLEMENTED
		// TODO: Npc.HairOnly - NOT IMPLEMENTED
		// TODO: R.AddSpells - NOT IMPLEMENTED

		if sSignature = 'RACE' then
		begin
			ProcessTag('R.ChangeSpells', e, o);
			ProcessTag('R.Attributes-F', e, o);
			ProcessTag('R.Attributes-M', e, o);
		end;

		if sSignature = 'ROAD' then
			ProcessTag('Roads', e, o);

		if sSignature = 'SPEL' then
			ProcessTag('SpellStats', e, o);
	end;

	// -------------------------------------------------------------------------------
	// GROUP: Supported tags exclusive to TES5 and SSE
	// -------------------------------------------------------------------------------
	if InDelimitedList(wbAppName, 'TES5 SSE', ' ') then
		if sSignature = 'CELL' then
		begin
			ProcessTag('C.Location', e, o);
			ProcessTag('C.Regions', e, o);
			ProcessTag('C.SkyLighting', e, o);
		end;

	// -------------------------------------------------------------------------------
	// GROUP: Supported tags exclusive to FO3 and FNV
	// -------------------------------------------------------------------------------
	if InDelimitedList(wbAppName, 'FO3 FNV', ' ') then
	begin
		sTag := 'Destructible';
		if InDelimitedList(sSignature, 'ACTI ALCH AMMO BOOK CONT DOOR FURN IMOD KEYM MISC MSTT PROJ TACT TERM WEAP', ' ') then
			ProcessTag(sTag, e, o);

		// special handling for CREA and NPC_ record types
		if InDelimitedList(sSignature, 'CREA NPC_', ' ') then
			if not CompareFlags(sTag, e, o, sElement, 'Use Model/Animation', False, False) then
				ProcessTag(sTag, e, o);
	end;

	// -------------------------------------------------------------------------------
	// GROUP: Supported tags exclusive to FO3, FNV, and TES4
	// -------------------------------------------------------------------------------
	if InDelimitedList(wbAppName, 'FO3 FNV TES4', ' ') then
	begin
		if InDelimitedList(sSignature, 'CREA NPC_', ' ') then
		begin
			sTag := 'Factions';
			if wbGameMode = gmTES4 then
				ProcessTag(sTag, e, o)
			else
				if not CompareFlags(sTag, e, o, sElement, 'Use Factions', False, False) then
					ProcessTag(sTag, e, o);
		end;

		if sSignature = 'FACT' then
			ProcessTag('Relations', e, o);
	end;

	// -------------------------------------------------------------------------------
	// GROUP: Supported tags exclusive to FO3, FNV, TES5, and SSE
	// -------------------------------------------------------------------------------
	if not (wbAppName = 'TES4') then
	begin
		if InDelimitedList(sSignature, 'CREA NPC_', ' ') then
		begin
			sTag := 'Actors.ACBS';
			if not CompareFlags(sTag, e, o, sElement, 'Use Stats', False, False) then
				ProcessTag(sTag, e, o);

			sTag := 'Actors.AIData';
			if not CompareFlags(sTag, e, o, sElement, 'Use AI Data', False, False) then
				ProcessTag(sTag, e, o);

			sTag := 'Actors.AIPackages';
			if not CompareFlags(sTag, e, o, sElement, 'Use AI Packages', False, False) then
				ProcessTag(sTag, e, o);

			if sSignature = 'CREA' then
				if not CompareFlags(sTag, e, o, sElement, 'Use Model/Animation', False, False) then
					ProcessTag('Actors.Anims', e, o);

			if not CompareFlags(sTag, e, o, sElement, 'Use Traits', False, False) then
			begin
				ProcessTag('Actors.CombatStyle', e, o);
				ProcessTag('Actors.DeathItem', e, o);
			end;

			sTag := 'Actors.Skeleton';
			if not CompareFlags(sTag, e, o, sElement, 'Use Model/Animation', False, False) then
				ProcessTag(sTag, e, o);

			sTag := 'Actors.Stats';
			if not CompareFlags(sTag, e, o, sElement, 'Use Stats', False, False) then
				ProcessTag(sTag, e, o);

			// TODO: IIM - NOT IMPLEMENTED
			// TODO: MustBeActiveIfImported - NOT IMPLEMENTED

			if sSignature = 'NPC_' then
			begin
				sTag := 'NPC.Class';
				if not CompareFlags(sTag, e, o, sElement, 'Use Traits', False, False) then
					ProcessTag(sTag, e, o);

				sTag := 'NPC.Race';
				if not CompareFlags(sTag, e, o, sElement, 'Use Traits', False, False) then
					ProcessTag(sTag, e, o);

				sTag := 'NpcFaces';
				if not CompareFlags(sTag, e, o, sElement, 'Use Model/Animation', False, False) then
					ProcessTag(sTag, e, o);
			end;

			sTag := 'Scripts';
			if not CompareFlags(sTag, e, o, sElement, 'Use Script', False, False) then
				ProcessTag(sTag, e, o);
		end;

		if sSignature = 'CELL' then
		begin
			ProcessTag('C.Acoustic', e, o);
			ProcessTag('C.Encounter', e, o);
			ProcessTag('C.ImageSpace', e, o);
		end;

		if sSignature = 'RACE' then
		begin
			ProcessTag('Body-F', e, o);
			ProcessTag('Body-M', e, o);
			ProcessTag('Body-Size-F', e, o);
			ProcessTag('Body-Size-M', e, o);
			ProcessTag('Eyes', e, o);
			ProcessTag('Hair', e, o);
			ProcessTag('R.Description', e, o);
			ProcessTag('R.Ears', e, o);
			ProcessTag('R.Head', e, o);
			ProcessTag('R.Mouth', e, o);
			ProcessTag('R.Relations', e, o);
			ProcessTag('R.Skills', e, o);
			ProcessTag('R.Teeth', e, o);
			ProcessTag('Voice-F', e, o);
			ProcessTag('Voice-M', e, o);
		end;

		// TODO: ScriptContents - SHOULD NOT BE IMPLEMENTED
		// -- According to the Wrye Bash Readme: "Should not be used. Can cause serious issues."

		if InDelimitedList(sSignature, 'ACTI ALCH ARMO CONT DOOR FLOR FURN INGR KEYM LIGH LVLC MISC QUST WEAP', ' ') then
			ProcessTag('Scripts', e, o);
	end;

	// -------------------------------------------------------------------------------
	// GROUP: Supported tags exclusive to FO3, FNV, TES4, TES5, and SSE
	// -- All except FO4, but FO4 raises exception so don't check wbAppName.
	// -------------------------------------------------------------------------------
	if (sSignature = 'CELL') then
	begin
		ProcessTag('C.Climate', e, o);
		ProcessTag('C.Light', e, o);
		ProcessTag('C.Music', e, o);
		ProcessTag('C.Name', e, o);
		ProcessTag('C.Owner', e, o);
		ProcessTag('C.RecordFlags', e, o);
		ProcessTag('C.Water', e, o);
	end;

	// TODO: Deactivate - NOT IMPLEMENTED

	// TAG: Delev, Relev
	if InDelimitedList(sSignature, 'LVLC LVLI LVLN LVSP', ' ') then
		ProcessDelevRelevTags(e, o);

	// TODO: Filter - NOT IMPLEMENTED

	if InDelimitedList(sSignature, 'ACTI ALCH AMMO APPA ARMO BOOK BSGN CLAS CLOT DOOR FLOR FURN INGR KEYM LIGH MGEF MISC SGST SLGM WEAP', ' ') then
	begin
		ProcessTag('Graphics', e, o);
		ProcessTag('Names', e, o);
		ProcessTag('Stats', e, o);

		if InDelimitedList(sSignature, 'ACTI DOOR LIGH MGEF', ' ') then
			ProcessTag('Sound', e, o);
	end;

	if InDelimitedList(sSignature, 'CREA EFSH GRAS LSCR LTEX REGN STAT TREE', ' ') then
		ProcessTag('Graphics', e, o);

	if sSignature = 'CONT' then
	begin
		ProcessTag('Invent', e, o);
		ProcessTag('Names', e, o);
		ProcessTag('Sound', e, o);
	end;

	if InDelimitedList(sSignature, 'DIAL ENCH EYES FACT HAIR QUST RACE SPEL WRLD', ' ') then
		ProcessTag('Names', e, o);

	// TODO: NoMerge - NOT IMPLEMENTED

	if (sSignature = 'WTHR') then
		ProcessTag('Sound', e, o);

	// special handling for CREA and NPC_
	if InDelimitedList(sSignature, 'CREA NPC_', ' ') then
	begin
		if wbGameMode = gmTES4 then
		begin
			ProcessTag('Invent', e, o);
			ProcessTag('Names', e, o);

			if sSignature = 'CREA' then
				ProcessTag('Sound', e, o);

		end;

		if wbGameMode <> gmTES4 then
		begin
			sTag := 'Invent';
			if not CompareFlags(sTag, e, o, sElement, 'Use Inventory', False, False) then
				ProcessTag(sTag, e, o);

			// special handling for CREA and NPC_ record types
			sTag := 'Names';
			if not CompareFlags(sTag, e, o, sElement, 'Use Base Data', False, False) then
				ProcessTag(sTag, e, o);

			// special handling for CREA record type
			sTag := 'Sound';
			if sSignature = 'CREA' then
				if not CompareFlags(sTag, e, o, sElement, 'Use Model/Animation', False, False) then
					ProcessTag(sTag, e, o);
		end;
	end;
end;

function Finalize: Integer;
var
	kHeader, kDescription: IInterface;
	lsExistingTags, lsDifferentTags, lsBadTags: TSTringList;
	sDescription: String;
begin
	lsExistingTags := TStringList.Create; // existing tags
	lsDifferentTags := TStringList.Create; // different tags
	lsBadTags := TStringList.Create; // bad tags

	// exit conditions
	if (optionAddTags = mrAbort)
	or (optionOutputLog = mrAbort)
	or not Assigned(lsSuggestedTags)
	or not Assigned(sFileName) then
		exit;

	// output file name
	AddMessage(Uppercase(sFileName));

	// output log
	if optionOutputLog = mrYes then
		if lsLog.Count > 0 then
		begin
			Separator(False);
			AddMessage(lsLog.Text);
		end;

	// if any suggested tags were generated
	if lsSuggestedTags.Count > 0 then
	begin
		kHeader := ElementBySignature(kFile, 'TES4');

		// determine if the header record exists
		if Assigned(kHeader) then
		begin
			kDescription := ElementBySignature(kHeader, 'SNAM');
			sDescription := GetEditValue(kDescription);

			// categorize tag list
			lsExistingTags.CommaText := Substring(sDescription, '{{BASH:', '}}');
			lsDifferentTags := Diff(lsSuggestedTags, lsExistingTags);
			lsBadTags := Diff(lsExistingTags, lsSuggestedTags);
			lsSuggestedTags.AddStrings(lsDifferentTags);

			// exit if existing and suggested tags are the same
			if SameText(lsExistingTags.CommaText, lsSuggestedTags.CommaText) then
			begin
				Separator(False);
				FormatTags(lsExistingTags, 'existing tag found:', 'existing tags found:', 'No existing tags found.');
				FormatTags(lsSuggestedTags, 'suggested tag:', 'suggested tags:', 'No suggested tags.');
				AddMessage('No tags to add.' + #13#10);
				Separator(False);
				exit;
			end;

		// exit if the header record doesn't exist
		end else begin
			Separator(False);
			AddMessage('Header record not found. Nothing to do. Exiting.' + #13#10);
			Separator(False);
			exit;
		end;

		// write tags
		if optionAddTags = mrYes then
		begin
			// if the description element doesn't exist, add the element
			kDescription := AddElementByString(kHeader, 'SNAM');

			if not SameText(lsExistingTags.CommaText, lsSuggestedTags.CommaText) then
			begin
				sDescription := GetEditValue(kDescription);
				sDescription := Trim(RemoveFromEnd(sDescription, Format('{{BASH:%s}}', [lsExistingTags.DelimitedText])));
				SetEditValue(kDescription, sDescription + #13#10 + #13#10 + Format('{{BASH:%s}}', [lsSuggestedTags.DelimitedText]));
			end;

			Separator(False);
			FormatTags(lsBadTags, 'bad tag removed:', 'bad tags removed:', 'No bad tags found.');
			FormatTags(lsDifferentTags, 'tag added to file header:', 'tags added to file header:', 'No tags added.');
			Separator(False)
		end;

		// suggest tags only and output to log
		if optionAddTags = mrNo then
		begin
			Separator(False);
			FormatTags(lsExistingTags, 'existing tag found:', 'existing tags found:', 'No existing tags found.');
			FormatTags(lsBadTags, 'bad tag found:', 'bad tags found:', 'No bad tags found.');
			FormatTags(lsDifferentTags, 'suggested tag to add:', 'suggested tags to add:', 'No suggested tags to add.');
			FormatTags(lsSuggestedTags, 'suggested tag overall:', 'suggested tags overall:', 'No suggested tags overall.');
			Separator(False)
		end;
	end;

	FreeAndNil(lsLog);
	FreeAndNil(lsSuggestedTags);
	FreeAndNil(lsExistingTags);
	FreeAndNil(lsDifferentTags);
	FreeAndNil(lsBadTags);
end;

function StrToBool(s: String): Boolean;
begin
	if (s <> '0') and (s <> '1') then
		Result := nil
	else
		if (s = '1') then
			Result := True
		else
			Result := False;
end;

function CompareAssignment(asTag: String; e, m: IInterface): Boolean;
var
	bAssignedE, bAssignedM: Boolean;
begin
	if TagExists(asTag) then
		exit;

	Result := False;

	bAssignedE := Assigned(e);
	bAssignedM := Assigned(m);

	if (not bAssignedE and not bAssignedM)
	or (bAssignedE and bAssignedM) then
		exit;

	if bAssignedE <> bAssignedM then
	begin
		if optionOutputLog = mrYes then
			AddLogEntry(asTag, '[Assigned]', e, m);
		AddTag(asTag);
		Result := True;
	end;
end;

function CompareElementCount(asTag: String; e, m: IInterface): Boolean;
var
	iCountE, iCountM: Integer;
begin
	if TagExists(asTag) then
		exit;

	Result := False;

	iCountE := ElementCount(e);
	iCountM := ElementCount(m);

	if iCountE = iCountM then
		exit;

	if iCountE <> iCountM then
	begin
		if optionOutputLog = mrYes then
			AddLogEntry(asTag, '[ElementCount]', e, m);
		AddTag(asTag);
		Result := True;
	end;
end;

function CompareEditValue(asTag: String; e, m: IInterface): Boolean;
var
	sValueE, sValueM: String;
begin
	if TagExists(asTag) then
		exit;

	Result := False;

	sValueE := GetEditValue(e);
	sValueM := GetEditValue(m);

	if SameText(sValueE, sValueM) then
		exit;

	if not SameText(sValueE, sValueM) then
	begin
		if optionOutputLog = mrYes then
			AddLogEntry(asTag, '[GetEditValue]', e, m);
		AddTag(asTag);
		Result := True;
	end;
end;

function CompareFlags(asTag: String; e, m: IInterface; sPath, sFlagName: String; bAddTag, bOperation: Boolean): Boolean;
var
	x, y, a, b: IInterface;
	sa, sb, sTestName: String;
	bResult: Boolean;
begin
	if TagExists(asTag) then
		exit;

	Result := False;

	// flags arrays
	x := ElementByPath(e, sPath);
	y := ElementByPath(m, sPath);

	// individual flags
	a := ElementByName(x, sFlagName);
	b := ElementByName(y, sFlagName);

	// individual flag edit values
	sa := GetEditValue(a);
	sb := GetEditValue(b);

	if bOperation then
		bResult := not SameText(sa, sb)  // only used for Behave Like Exterior, Use Sky Lighting, and Has Water
	else
		bResult := StrToBool(sa) or StrToBool(sb);

	if bAddTag and bResult then
	begin
		if bOperation then
			sTestName := '[CompareFlags:NOT]'
		else
			sTestName := '[CompareFlags:OR]';

		if optionOutputLog = mrYes then
			AddLogEntry(asTag, sTestName, x, y);
		AddTag(asTag);
	end;

	Result := bResult;
end;

function CompareKeys(asTag: String; e, m: IInterface): Boolean;
var
	bResult: Boolean;
	sKeyE, sKeyM: String;
	ConflictState: TConflictThis;
begin
	if TagExists(asTag) then
		exit;

	Result := False;

	ConflictState := ConflictAllForMainRecord(ContainingMainRecord(e));

	if (ConflictState = caUnknown)
	or (ConflictState = caOnlyOne)
	or (ConflictState = caNoConflict) then
		exit;

	sKeyE := SortKeyEx(e);
	sKeyM := SortKeyEx(m);

	// empty check
	if (IsEmptyKey(sKeyE) and IsEmptyKey(sKeyM))
	or SameText(sKeyE, sKeyM) then
		exit;

	// case sensitive comparison
	if not SameText(sKeyE, sKeyM) then
	begin
		if optionOutputLog = mrYes then
			AddLogEntry(asTag, '[CompareKeys]', e, m);
		AddTag(asTag);
		Result := True;
	end;
end;

function CompareNativeValues(asTag: String; e, m: IInterface; asPath: String): Boolean;
var
	x, y: IInterface;
begin
	if TagExists(asTag) then
		exit;

	Result := False;

	x := ElementByPath(e, asPath);
	y := ElementByPath(m, asPath);

	if GetNativeValue(x) = GetNativeValue(y) then
		exit;

	if GetNativeValue(x) <> GetNativeValue(y) then
	begin
		if optionOutputLog = mrYes then
			AddLogEntry(asTag, '[CompareNativeValues]', e, m);
		AddTag(asTag);
		Result := True;
	end;
end;

function SortedArrayElementByValue(e: IInterface; sPath, sValue: String): IInterface;
var
	i: Integer;
	kEntry: IInterface;
begin
	Result := nil;
	for i := 0 to ElementCount(e) - 1 do
	begin
		kEntry := ElementByIndex(e, i);
		if SameText(GetElementEditValues(kEntry, sPath), sValue) then
		begin
			Result := kEntry;
			exit;
		end;
	end;
end;

function Substring(sSubstring, sExpression1, sExpression2: String): String;
var
	pos1, pos2, len: Integer;
begin
  pos1 := ItPos(sExpression1, sSubstring, 1) + Length(sExpression1);
  pos2 := ItPos(sExpression2, sSubstring, 1);
  len := pos2 - pos1;
  Result := Copy(sSubstring, pos1, len);
end;

function Diff(lsList1, lsList2: TStringList): TStringList;
var
	i: Integer;
	tmp: TStringList;
begin
	tmp := TStringList.Create;
	for i := 0 to lsList1.Count - 1 do
		if lsList2.IndexOf(lsList1[i]) < 0 then
			tmp.Add(lsList1[i]);
	Result := tmp;
end;

// TODO: speed this up!
function IsEmptyKey(asSortKey: String): Boolean;
var
	i: Integer;
begin
	for i := 1 to Length(asSortKey) do
	begin
		if asSortKey[i] = '1' then
		begin
			Result := False;
			exit;
		end;
		Result := True;
	end;
end;

function FormatTags(lsTags: TStringList; asSingular, asPlural, asNull: String): Integer;
begin
	if (lsTags.Count = 1) then
		AddMessage(IntToStr(lsTags.Count) + ' ' + asSingular);

	if (lsTags.Count > 1) then
		AddMessage(IntToStr(lsTags.Count) + ' ' + asPlural);

	if (lsTags.Count > 0) then
		AddMessage(Format('{{BASH:%s}}', [lsTags.DelimitedText]) + #13#10)
	else
		AddMessage(asNull + #13#10);
end;

procedure Separator(bInsertNewlineBefore: Boolean);
var
	sLine: String;
begin
	sLine := '-------------------------------------------------------------------------------';
	if bInsertNewlineBefore then
		sLine := #13#10 + sLine
	else
		sLine := sLine + #13#10;
	AddMessage(sLine);
end;

function TagExists(asTag: String): Boolean;
begin
	Result := (lsSuggestedTags.IndexOf(asTag) <> -1);
end;

procedure AddTag(asTag: String);
begin
	if not TagExists(asTag) then
		lsSuggestedTags.Add(asTag);
end;

procedure Evaluate(asTag: String; e, m: IInterface);
begin
	// exit if the tag already exists
	if TagExists(asTag) then
		exit;

	// Suggest tag if one element exists while the other does not
	if CompareAssignment(asTag, e, m) then
		exit;

	// exit if the first element does not exist
	if not Assigned(e) then
		exit;

	// suggest tag if the two elements are different
	if CompareElementCount(asTag, e, m) then
		exit;

	// suggest tag if the edit values of the two elements are different
	if CompareEditValue(asTag, e, m) then
		exit;

	// compare any number of elements with CompareKeys
	if CompareKeys(asTag, e, m) then
		exit;
end;

procedure EvaluateByPath(asTag: String; e, m: IInterface; asPath: String);
var
	x, y: IInterface;
begin
	x := ElementByPath(e, asPath);
	y := ElementByPath(m, asPath);

	Evaluate(asTag, x, y);
end;

procedure ProcessTag(asTag: String; e, m: IInterface);
var
	{e, m,} x, y, a, b, j, k: IInterface;
	sElement, sSignature: String;
begin
	if TagExists(asTag) then
		exit;

	sSignature := Signature(e);

	// Bookmark: Actors.ACBS
	if asTag = 'Actors.ACBS' then
	begin
		// assign ACBS elements
		sElement := 'ACBS';
		x := ElementBySignature(e, sElement);
		y := ElementBySignature(m, sElement);

		// evaluate Flags if the Use Base Data flag is not set
		sElement := 'Flags';
		a := ElementByName(x, sElement);
		b := ElementByName(y, sElement);

		if wbGameMode = gmTES4 then
			if CompareKeys(asTag, a, b) then
				exit;

		if wbGameMode <> gmTES4 then
			if not CompareFlags(asTag, x, y, 'Template Flags', 'Use Base Data', False, False) then
				if CompareKeys(asTag, a, b) then
					exit;

		// evaluate properties
		EvaluateByPath(asTag, x, y, 'Fatigue');
		EvaluateByPath(asTag, x, y, 'Level');
		EvaluateByPath(asTag, x, y, 'Calc min');
		EvaluateByPath(asTag, x, y, 'Calc max');
		EvaluateByPath(asTag, x, y, 'Speed Multiplier');
		EvaluateByPath(asTag, e, m, 'DATA\Base Health');

		// evaluate Barter Gold if the Use AI Data flag is not set
		sElement := 'Barter gold';
		if wbGameMode = gmTES4 then
			EvaluateByPath(asTag, x, y, sElement)
		else
			if not CompareFlags(asTag, x, y, 'Template Flags', 'Use AI Data', False, False) then
				EvaluateByPath(asTag, x, y, sElement);
	end;

	// Bookmark: Actors.AIData
	if asTag = 'Actors.AIData' then
	begin
		// assign AIDT elements
		sElement := 'AIDT';
		x := ElementBySignature(e, sElement);
		y := ElementBySignature(m, sElement);

		// evaluate AIDT properties
		EvaluateByPath(asTag, x, y, 'Aggression');
		EvaluateByPath(asTag, x, y, 'Confidence');
		EvaluateByPath(asTag, x, y, 'Energy level');
		EvaluateByPath(asTag, x, y, 'Responsibility');
		EvaluateByPath(asTag, x, y, 'Teaches');
		EvaluateByPath(asTag, x, y, 'Maximum training level');

		// check flags for Buys/Sells and Services
		if CompareNativeValues(asTag, x, y, 'Buys/Sells and Services') then
			exit;
	end;

	// Bookmark: Actors.AIPackages
	if asTag = 'Actors.AIPackages' then
		EvaluateByPath(asTag, e, m, 'Packages');

	// Bookmark: Actors.Anims
	if asTag = 'Actors.Anims' then
		EvaluateByPath(asTag, e, m, 'KFFZ');

	// Bookmark: Actors.CombatStyle
	if asTag = 'Actors.CombatStyle' then
		EvaluateByPath(asTag, e, m, 'ZNAM');

	// Bookmark: Actors.DeathItem
	if asTag = 'Actors.DeathItem' then
		EvaluateByPath(asTag, e, m, 'INAM');

	// Bookmark: Actors.Skeleton
	if asTag = 'Actors.Skeleton' then
	begin
		// assign Model elements
		sElement := 'Model';
		x := ElementByName(e, sElement);
		y := ElementByName(m, sElement);

		// exit if the Model property does not exist in the control record
		if not Assigned(x) then
			exit;

		// evaluate properties
		EvaluateByPath(asTag, x, y, 'MODL');
		EvaluateByPath(asTag, x, y, 'MODB');
		EvaluateByPath(asTag, x, y, 'MODT');
	end;

	// Bookmark: Actors.Spells
	if asTag = 'Actors.Spells' then
		EvaluateByPath(asTag, e, m, 'Spells');

	// Bookmark: Actors.Stats
	if asTag = 'Actors.Stats' then
	begin
		// assign DATA elements
		sElement := 'DATA';
		x := ElementBySignature(e, sElement);
		y := ElementBySignature(m, sElement);

		// evaluate CREA properties
		if sSignature = 'CREA' then
		begin
			EvaluateByPath(asTag, x, y, 'Health');
			EvaluateByPath(asTag, x, y, 'Combat Skill');
			EvaluateByPath(asTag, x, y, 'Magic Skill');
			EvaluateByPath(asTag, x, y, 'Stealth Skill');
			EvaluateByPath(asTag, x, y, 'Attributes');
		end;

		// evaluate NPC_ properties
		if sSignature = 'NPC_' then
		begin
			EvaluateByPath(asTag, x, y, 'Base Health');
			EvaluateByPath(asTag, x, y, 'Attributes');
			EvaluateByPath(asTag, e, m, 'DNAM\Skill Values');
			EvaluateByPath(asTag, e, m, 'DNAM\Skill Offsets');
		end;
	end;

	// Bookmark: Body-F
	if asTag = 'Body-F' then
		EvaluateByPath(asTag, e, m, 'Body Data\Female Body Data\Parts');

	// Bookmark: Body-M
	if asTag = 'Body-M' then
		EvaluateByPath(asTag, e, m, 'Body Data\Male Body Data\Parts');

	// Bookmark: Body-Size-F
	if asTag = 'Body-Size-F' then
	begin
		EvaluateByPath(asTag, e, m, 'DATA\Female Height');
		EvaluateByPath(asTag, e, m, 'DATA\Female Weight');
	end;

	// Bookmark: Body-Size-M
	if asTag = 'Body-Size-M' then
	begin
		EvaluateByPath(asTag, e, m, 'DATA\Male Height');
		EvaluateByPath(asTag, e, m, 'DATA\Male Weight');
	end;

	// Bookmark: C.Acoustic
	if asTag = 'C.Acoustic' then
		EvaluateByPath(asTag, e, m, 'XCAS');

	// Bookmark: C.Climate
	if asTag = 'C.Climate' then
	begin
		// add tag if the Behave Like Exterior flag is set ine one record but not the other
		if CompareFlags(asTag, e, m, 'DATA', 'Behave Like Exterior', True, True) then
			exit;

		// evaluate additional property
		EvaluateByPath(asTag, e, m, 'XCCM');
	end;

	// Bookmark: C.Encounter
	if asTag = 'C.Encounter' then
		EvaluateByPath(asTag, e, m, 'XEZN');

	// Bookmark: C.ImageSpace
	if asTag = 'C.ImageSpace' then
		EvaluateByPath(asTag, e, m, 'XCIM');

	// Bookmark: C.Light
	if asTag = 'C.Light' then
		EvaluateByPath(asTag, e, m, 'XCLL');

	// Bookmark: C.Location
	if asTag = 'C.Location' then
		EvaluateByPath(asTag, e, m, 'XLCN');

	// Bookmark: C.Music
	if asTag = 'C.Music' then
		EvaluateByPath(asTag, e, m, 'XCMO');

	// Bookmark: FULL (C.Name, Names, SpellStats)
	if (asTag = 'C.Name') or (asTag = 'Names') or (asTag = 'SpellStats') then
		EvaluateByPath(asTag, e, m, 'FULL');

	// Bookmark: C.Owner
	if asTag = 'C.Owner' then
		EvaluateByPath(asTag, e, m, 'Ownership');

	// Bookmark: C.RecordFlags
	if asTag = 'C.RecordFlags' then
	begin
		// store Record Flags elements
		sElement := 'Record Header\Record Flags';
		x := ElementByPath(e, sElement);
		y := ElementByPath(m, sElement);

		// compare Record Flags elements
		if CompareKeys(asTag, x, y) then
			exit;
	end;

	// Bookmark: C.Regions
	if asTag = 'C.Regions' then
		EvaluateByPath(asTag, e, m, 'XCLR');

	// Bookmark: C.SkyLighting
	if asTag = 'C.SkyLighting' then
		// add tag if the Behave Like Exterior flag is set ine one record but not the other
		if CompareFlags(asTag, e, m, 'DATA', 'Use Sky Lighting', True, True) then
			exit;

	// Bookmark: C.Water
	if asTag = 'C.Water' then
	begin
		// add tag if Has Water flag is set in one record but not the other
		if CompareFlags(asTag, e, m, 'DATA', 'Has Water', True, True) then
			exit;

		// exit if Is Interior Cell is set in either record
		if CompareFlags(asTag, e, m, 'DATA', 'Is Interior Cell', False, False) then
			exit;

		// evaluate properties
		EvaluateByPath(asTag, e, m, 'XCLW');
		EvaluateByPath(asTag, e, m, 'XCWT');
	end;

	// Bookmark: Creatures.Blood
	if asTag = 'Creatures.Blood' then
	begin
		EvaluateByPath(asTag, e, m, 'NAM0');
		EvaluateByPath(asTag, e, m, 'NAM1');
	end;

	// Bookmark: Destructible
	if asTag = 'Destructible' then
	begin
		// assign Destructable elements
		sElement := 'Destructable';
		x := ElementByName(e, sElement);
		y := ElementByName(m, sElement);

		if CompareAssignment(asTag, x, y) then
			exit;

		sElement := 'DEST';
		a := ElementBySignature(x, sElement);
		b := ElementBySignature(y, sElement);

		// evaluate Destructable properties
		EvaluateByPath(asTag, a, b, 'Health');
		EvaluateByPath(asTag, a, b, 'Count');
		EvaluateByPath(asTag, x, y, 'Stages');

		// assign Destructable flags
		if not InDelimitedList(wbAppName, 'TES5 SSE', ' ') then
		begin
			sElement := 'Flags';
			j := ElementByName(a, sElement);
			k := ElementByName(b, sElement);

			if Assigned(j) or Assigned(k) then
			begin
				// add tag if Destructable flags exist in one record
				if CompareAssignment(asTag, j, k) then
					exit;

				// evaluate Destructable flags
				if CompareKeys(asTag, j, k) then
					exit;
			end;
		end;
	end;

	// Bookmark: Eyes
	if asTag = 'Eyes' then
		EvaluateByPath(asTag, e, m, 'ENAM');

	// Bookmark: Factions
	if asTag = 'Factions' then
	begin
		// assign Factions properties
		x := ElementByName(e, asTag);
		y := ElementByName(m, asTag);

		// add tag if the Factions properties differ
		if CompareAssignment(asTag, x, y) then
			exit;

		// exit if the Factions property in the control record does not exist
		if not Assigned(x) then
			exit;

		// evaluate Factions properties
		if CompareKeys(asTag, x, y) then
			exit;
	end;

	// Bookmark: Graphics
	if asTag = 'Graphics' then
	begin
		// evaluate Icon and Model properties
		if InDelimitedList(sSignature, 'ALCH AMMO APPA BOOK INGR KEYM LIGH MGEF MISC SGST SLGM TREE WEAP', ' ') then
		begin
			EvaluateByPath(asTag, e, m, 'Icon');
			EvaluateByPath(asTag, e, m, 'Model');
		end;

		// evaluate Icon properties
		if InDelimitedList(sSignature, 'BSGN CLAS LSCR LTEX REGN', ' ') then
			EvaluateByPath(asTag, e, m, 'Icon');

		// evaluate Model properties
		if InDelimitedList(sSignature, 'ACTI DOOR FLOR FURN GRAS STAT', ' ') then
			EvaluateByPath(asTag, e, m, 'Model');

		// evaluate ARMO properties
		if sSignature = 'ARMO' then
		begin
			// Shared
			EvaluateByPath(asTag, e, m, 'Male world model');
			EvaluateByPath(asTag, e, m, 'Female world model');

			// ARMO - Oblivion
			if wbGameMode = gmTES4 then
			begin
				// evaluate Icon properties
				EvaluateByPath(asTag, e, m, 'Icon');
				EvaluateByPath(asTag, e, m, 'Icon 2 (female)');

				// assign First Person Flags elements
				sElement := 'BODT\First Person Flags';

				x := ElementByPath(e, sElement);
				if not Assigned(x) then
					exit;

				y := ElementByPath(m, sElement);

				// evaluate First Person Flags
				if CompareKeys(asTag, x, y) then
					exit;

				// assign General Flags elements
				sElement := 'BODT\General Flags';

				x := ElementByPath(e, sElement);
				if not Assigned(x) then
					exit;

				y := ElementByPath(m, sElement);

				// evaluate General Flags
				if CompareKeys(asTag, x, y) then
					exit;
			end;

			// ARMO - FO3, FNV
			if InDelimitedList(wbAppName, 'FO3 FNV', ' ') then
			begin
				// evaluate Icon properties
				EvaluateByPath(asTag, e, m, 'ICON');
				EvaluateByPath(asTag, e, m, 'ICO2');

				// assign First Person Flags elements
				sElement := 'BMDT\Biped Flags';

				x := ElementByPath(e, sElement);
				if not Assigned(x) then
					exit;

				y := ElementByPath(m, sElement);

				// evaluate First Person Flags
				if CompareKeys(asTag, x, y) then
					exit;

				// assign General Flags elements
				sElement := 'BMDT\General Flags';

				x := ElementByPath(e, sElement);
				if not Assigned(x) then
					exit;

				y := ElementByPath(m, sElement);

				// evaluate General Flags
				if CompareKeys(asTag, x, y) then
					exit;
			end;

			// ARMO - TES5
			if InDelimitedList(wbAppName, 'TES5 SSE', ' ') then
			begin
				// evaluate Icon properties
				EvaluateByPath(asTag, e, m, 'Icon');
				EvaluateByPath(asTag, e, m, 'Icon 2 (female)');

				// evaluate Biped Model properties
				EvaluateByPath(asTag, e, m, 'Male world model');
				EvaluateByPath(asTag, e, m, 'Female world model');

				// assign First Person Flags elements
				sElement := 'BOD2\First Person Flags';

				x  := ElementByPath(e, sElement);
				if not Assigned(x) then
					exit;

				y := ElementByPath(m, sElement);

				// evaluate First Person Flags
				if CompareKeys(asTag, x, y) then
					exit;

				// assign General Flags elements
				sElement := 'BOD2\General Flags';

				x := ElementByPath(e, sElement);
				if not Assigned(x) then
					exit;

				y := ElementByPath(m, sElement);

				// evaluate General Flags
				if CompareKeys(asTag, x, y) then
					exit;
			end;
		end;

		// evaluate CREA properties
		if sSignature = 'CREA' then
		begin
			EvaluateByPath(asTag, e, m, 'NIFZ');
			EvaluateByPath(asTag, e, m, 'NIFT');
		end;

		// evaluate EFSH properties
		if sSignature = 'EFSH' then
		begin
			// evaluate Record Flags
			sElement := 'Record Header\Record Flags';

			x := ElementByPath(e, sElement);
			y := ElementByPath(m, sElement);

			if CompareKeys(asTag, x, y) then
				exit;

			// evaluate Icon properties
			EvaluateByPath(asTag, e, m, 'ICON');
			EvaluateByPath(asTag, e, m, 'ICO2');

			// evaluate other properties
			EvaluateByPath(asTag, e, m, 'NAM7');

			if InDelimitedList(wbAppName, 'TES5 SSE', ' ') then
			begin
				EvaluateByPath(asTag, e, m, 'NAM8');
				EvaluateByPath(asTag, e, m, 'NAM9');
			end;

			EvaluateByPath(asTag, e, m, 'DATA');
		end;

		// evaluate MGEF properties
		if InDelimitedList(wbAppName, 'TES5 SSE', ' ') then
			if sSignature = 'MGEF' then
			begin
				EvaluateByPath(asTag, e, m, 'Magic Effect Data\DATA\Casting Light');
				EvaluateByPath(asTag, e, m, 'Magic Effect Data\DATA\Hit Shader');
				EvaluateByPath(asTag, e, m, 'Magic Effect Data\DATA\Enchant Shader');
			end;

		// evaluate Material property
		if sSignature = 'STAT' then
			EvaluateByPath(asTag, e, m, 'DNAM\Material');
	end;

	// Bookmark: Hair
	if asTag = 'Hair' then
		EvaluateByPath(asTag, e, m, 'HNAM');

	// Bookmark: Invent
	if asTag = 'Invent' then
	begin
		// assign Items properties
		sElement := 'Items';

		x := ElementByName(e, sElement);
		y := ElementByName(m, sElement);

		// add tag if Items properties exist in one record but not the other
		if CompareAssignment(asTag, x, y) then
			exit;

		// exit if Items property does not exist in control record
		if not Assigned(x) then
			exit;

		// Items are sorted, so we don't need to compare by individual item
		// SortKey combines all the items data
		if CompareKeys(asTag, x, y) then
			exit;
	end;

	// Bookmark: NPC.Class
	if asTag = 'NPC.Class' then
		EvaluateByPath(asTag, e, m, 'CNAM');

	// Bookmark: NPC.Race
	if asTag = 'NPC.Race' then
		EvaluateByPath(asTag, e, m, 'RNAM');

	// Bookmark: NpcFaces
	if asTag = 'NpcFaces' then
	begin
		EvaluateByPath(asTag, e, m, 'HNAM');
		EvaluateByPath(asTag, e, m, 'LNAM');
		EvaluateByPath(asTag, e, m, 'ENAM');
		EvaluateByPath(asTag, e, m, 'HCLR');
		EvaluateByPath(asTag, e, m, 'FaceGen Data');
	end;

	// Bookmark: R.Attributes-F
	if asTag = 'R.Attributes-F' then
		EvaluateByPath(asTag, e, m, 'ATTR\Female');

	// Bookmark: R.Attributes-M
	if asTag = 'R.Attributes-M' then
		EvaluateByPath(asTag, e, m, 'ATTR\Male');

	// Bookmark: R.ChangeSpells
	if asTag = 'R.ChangeSpells' then
		EvaluateByPath(asTag, e, m, 'Spells');

	// Bookmark: R.Description
	if asTag = 'R.Description' then
		EvaluateByPath(asTag, e, m, 'DESC');

	// Bookmark: R.Ears
	if asTag = 'R.Ears' then
	begin
		EvaluateByPath(asTag, e, m, 'Head Data\Male Head Data\Parts\[1]');
		EvaluateByPath(asTag, e, m, 'Head Data\Female Head Data\Parts\[1]');
	end;

	// Bookmark: R.Head
	if asTag = 'R.Head' then
	begin
		EvaluateByPath(asTag, e, m, 'Head Data\Male Head Data\Parts\[0]');
		EvaluateByPath(asTag, e, m, 'Head Data\Female Head Data\Parts\[0]');
		EvaluateByPath(asTag, e, m, 'FaceGen Data');
	end;

	// Bookmark: R.Mouth
	if asTag = 'R.Mouth' then
	begin
		EvaluateByPath(asTag, e, m, 'Head Data\Male Head Data\Parts\[2]');
		EvaluateByPath(asTag, e, m, 'Head Data\Female Head Data\Parts\[2]');
	end;

	// Bookmark: R.Relations
	if asTag = 'R.Relations' then
		EvaluateByPath(asTag, e, m, 'Relations');

	// Bookmark: R.Skills
	if asTag = 'R.Skills' then
		EvaluateByPath(asTag, e, m, 'DATA\Skill Boosts');

	// Bookmark: R.Teeth
	if asTag = 'R.Teeth' then
	begin
		EvaluateByPath(asTag, e, m, 'Head Data\Male Head Data\Parts\[3]');
		EvaluateByPath(asTag, e, m, 'Head Data\Female Head Data\Parts\[3]');

		// FO3
		if wbGameMode = gmFO3 then
		begin
			EvaluateByPath(asTag, e, m, 'Head Data\Male Head Data\Parts\[4]');
			EvaluateByPath(asTag, e, m, 'Head Data\Female Head Data\Parts\[4]');
		end;
	end;

	// Bookmark: Relations
	if asTag = 'Relations' then
		EvaluateByPath(asTag, e, m, 'Relations');

	// Bookmark: Roads
	if asTag = 'Roads' then
		EvaluateByPath(asTag, e, m, 'PGRP');

	// Bookmark: Scripts
	if asTag = 'Scripts' then
		EvaluateByPath(asTag, e, m, 'SCRI');

	// Bookmark: Sound
	if asTag = 'Sound' then
	begin
		// Activators, Containers, Doors, and Lights
		if (sSignature = 'ACTI')
		or (sSignature = 'CONT')
		or (sSignature = 'DOOR')
		or (sSignature = 'LIGH') then
		begin
			EvaluateByPath(asTag, e, m, 'SNAM');

			// Activators
			if sSignature = 'ACTI' then
				EvaluateByPath(asTag, e, m, 'VNAM');

			// Containers
			if sSignature = 'CONT' then
			begin
				EvaluateByPath(asTag, e, m, 'QNAM');
				if not InDelimitedList(wbAppName, 'FO3 TES5 SSE', ' ') then
					EvaluateByPath(asTag, e, m, 'RNAM'); // FO3, TESV, and SSE don't have this element
			end;

			// Doors
			if sSignature = 'DOOR' then
			begin
				EvaluateByPath(asTag, e, m, 'ANAM');
				EvaluateByPath(asTag, e, m, 'BNAM');
			end;
		end;

		// Creatures
		if sSignature = 'CREA' then
		begin
			EvaluateByPath(asTag, e, m, 'WNAM');
			EvaluateByPath(asTag, e, m, 'CSCR');
			EvaluateByPath(asTag, e, m, 'Sound Types');
		end;

		// Magic Effects
		if sSignature = 'MGEF' then
		begin
			// TES5, SSE
			if InDelimitedList(wbAppName, 'TES5 SSE', ' ') then
				EvaluateByPath(asTag, e, m, 'SNDD');

			// FO3, FNV, TES4, SSE
			if not InDelimitedList(wbAppName, 'TES5 SSE', ' ') then
			begin
				EvaluateByPath(asTag, e, m, 'DATA\Effect sound');
				EvaluateByPath(asTag, e, m, 'DATA\Bolt sound');
				EvaluateByPath(asTag, e, m, 'DATA\Hit sound');
				EvaluateByPath(asTag, e, m, 'DATA\Area sound');
			end;
		end;

		// Weather
		if sSignature = 'WTHR' then
			EvaluateByPath(asTag, e, m, 'Sounds');
	end;

	// Bookmark: SpellStats
	if asTag = 'SpellStats' then
		EvaluateByPath(asTag, e, m, 'SPIT');

	// Bookmark: Stats
	if asTag = 'Stats' then
	begin
		if InDelimitedList(sSignature, 'ALCH AMMO APPA ARMO BOOK CLOT INGR KEYM LIGH MISC SGST SLGM WEAP', ' ') then
		begin
			EvaluateByPath(asTag, e, m, 'EDID');
			EvaluateByPath(asTag, e, m, 'DATA');

			if InDelimitedList(sSignature, 'ARMO WEAP', ' ') then
				EvaluateByPath(asTag, e, m, 'DNAM');

			if sSignature = 'WEAP' then
				EvaluateByPath(asTag, e, m, 'CRDT');
		end;

		if sSignature = 'ARMA' then
			EvaluateByPath(asTag, e, m, 'DNAM');
	end;

	// Bookmark: Voice-F
	if asTag = 'Voice-F' then
		EvaluateByPath(asTag, e, m, 'VTCK\Voice #1 (Female)');

	// Bookmark: Voice-M
	if asTag = 'Voice-M' then
		EvaluateByPath(asTag, e, m, 'VTCK\Voice #0 (Male)');

	// Bookmark: WeaponMods
	if asTag = 'WeaponMods' then
		EvaluateByPath(asTag, e, m, 'Weapon Mods');

end;

// Bookmark: Delev, Relev
procedure ProcessDelevRelevTags(e, m: IInterface);
var
	i, j: integer;
	kEntries, kEntriesMaster, kEntry, kEntryMaster: IInterface;
	kCOED, kCOEDMaster: IInterface; // extra data
	sElement, sSortKey, sSortKeyMaster, sTag: String; // sortkeys for extra data, sortkey is a compact text representation of element's values
begin
	// nothing to do if already tagged
	if TagExists('Delev') and TagExists('Relev') then
		exit;

	// get Leveled List Entries
	sElement := 'Leveled List Entries';
	kEntries := ElementByName(e, sElement);
	kEntriesMaster := ElementByName(m, sElement);

	if not Assigned(kEntries)
	or not Assigned(kEntriesMaster) then
		exit;

	// initalize count matched on reference entries
	j := 0;

	// iterate through all entries
	for i := 0 to ElementCount(kEntries) - 1 do
	begin
		sElement := 'LVLO\Reference';
		kEntry := ElementByIndex(kEntries, i);
		kEntryMaster := SortedArrayElementByValue(kEntriesMaster, sElement, GetElementEditValues(kEntry, sElement));

		if Assigned(kEntryMaster) then
		begin
			Inc(j);

			sTag := 'Relev';
			if not TagExists(sTag) then
			begin
				if CompareNativeValues(sTag, kEntry, kEntryMaster, 'LVLO\Level')
				or CompareNativeValues(sTag, kEntry, kEntryMaster, 'LVLO\Count') then
					exit;

				// Relev check for changed level, count, extra data
				if wbGameMode <> gmTES4 then
				begin
					sElement := 'COED';
					kCOED := ElementBySignature(kEntry, sElement);
					kCOEDMaster := ElementBySignature(kEntryMaster, sElement);

					sSortKey := SortKeyEx(kCOED);
					sSortKeyMaster := SortKeyEx(kCOEDMaster);

					if not SameText(sSortKey, sSortKeyMaster) then
					begin
						if optionOutputLog = mrYes then
							AddLogEntry(sTag, '[Assigned]', sSortKey, sSortKeyMaster);
						AddTag(sTag);
						exit;
					end;
				end;
			end;
		end;
	end;

	// if number of matched entries less than in master list
	sTag := 'Delev';
	if not TagExists(sTag) then
	begin
		if j < ElementCount(kEntriesMaster) then
		begin
			if optionOutputLog = mrYes then
				AddLogEntry(sTag, '[ElementCount]', kEntries, kEntriesMaster);
			AddTag(sTag);
			exit;
		end;
	end;
end;

function AddLogEntry(asTag, asTestName: String; e, m: IInterface): String;
var
	sPrefix, sString: String;
begin
	if optionOutputLog = mrNo then
		exit;

	sPrefix := asTestName + ' ' + asTag + ': ';

	sString := sPrefix + FullPath(m) + #13#10 + sPrefix + FullPath(e);
	lsLog.Add(sString);
end;

end.