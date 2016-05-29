{
	Purpose: Automatic Bash Tag Generation
	Games: FO3/FNV/TES4/TES5
	Author: fireundubh <fireundubh@gmail.com>
	Version: 1.6

	Description: This script detects up to 58 bash tags in FO3, FNV, TES4, and TES5 plugins.
	Tags can be automatically added to the Description in the File Header. Wrye Bash/Flash can
	then use these tags to help you create more intelligent bashed patches.

	Requires mteFunctions:
	https://github.com/matortheeternal/TES5EditScripts/blob/master/trunk/Edit%20Scripts/mteFunctions.pas
}

unit BashTagsDetector;

uses dubhFunctions;

var
	f: IwbFile;
	lsTags, lsLog: TStringList;
	fn, sTag, game: String;
	optionAddTags, optionOutputLog: Integer;
	bDebug: Boolean;

// ******************************************************************
// FUNCTIONS
// ******************************************************************


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

	sValueE := Lowercase(GetEditValue(e));
	sValueM := Lowercase(GetEditValue(m));

	if sValueE = sValueM then
		exit;

	if sValueE <> sValueM then
	begin
		AddLogEntry(asTag, '[GetEditValue]', e, m);
		AddTag(asTag);
		Result := True;
	end;
end;

// ----------------------------------------------------------------------------
// Returns True if any two flags are different or set, and False if not
// ----------------------------------------------------------------------------
function CompareFlags(asTag: String; e, m: IInterface; sPath, sFlagName: String; bAddTag, bOperation: Boolean): Boolean;
var
	x, y: IInterface;
	bResult: Boolean;
begin
	if TagExists(asTag) then
		exit;

	Result := False;

	x := GetElement(e, sPath);
	y := GetElement(m, sPath);

	// TRUE: Behave Like Exterior, Use Sky Lighting, Has Water
	if bOperation then
	begin
		bResult := HasFlag(x, sFlagName) <> HasFlag(y, sFlagName);
		if bAddTag and bResult then
			AddLogEntry(asTag, '[CompareFlags:NOT]', x, y);
	end;

	// FALSE: everywhere else
	if not bOperation then
	begin
		bResult := HasFlag(x, sFlagName) or HasFlag(y, sFlagName);
		if bAddTag and bResult then
			AddLogEntry(asTag, '[CompareFlags:OR]', x, y);
	end;

	if bAddTag and bResult then
		AddTag(asTag);

	Result := bResult;
end;

// ----------------------------------------------------------------------------
// Returns True if the set flags are different and False if not
// ----------------------------------------------------------------------------
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

	sKeyE := Lowercase(SortKeyEx(e));
	sKeyM := Lowercase(SortKeyEx(m));

	// empty check
	if (IsEmptyKey(sKeyE) and IsEmptyKey(sKeyM))
	or (sKeyE = sKeyM) then
		exit;

	// case sensitive comparison
	if sKeyE <> sKeyM then
	begin
		AddLogEntry(asTag, '[CompareKeys]', e, m);
		AddTag(asTag);
		Result := True;
	end;
end;

// ----------------------------------------------------------------------------
// Returns True if the native values are different and False if not
// ----------------------------------------------------------------------------
function CompareNativeValues(asTag: String; e, m: IInterface; asPath: String): Boolean;
var
	x, y: IInterface;
begin
	if TagExists(asTag) then
		exit;

	Result := False;

	x := GetElement(e, asPath);
	y := GetElement(m, asPath);

	if GetNativeValue(x) = GetNativeValue(y) then
		exit;

	if GetNativeValue(x) <> GetNativeValue(y) then
	begin
		AddLogEntry(asTag, '[CompareNativeValues]', e, m);
		AddTag(asTag);
		Result := True;
	end;
end;

// ----------------------------------------------------------------------------
// Get element from list by some value
// ----------------------------------------------------------------------------
function SortedArrayElementByValue(e: IInterface; sPath, sValue: String): IInterface;
var
	i: Integer;
	kEntry: IInterface;
begin
	Result := nil;
	for i := 0 to ElementCount(e) - 1 do
	begin
		kEntry := ElementByIndex(e, i);
		if GetElementEditValues(kEntry, sPath) = sValue then
		begin
			Result := kEntry;
			exit;
		end;
	end;
end;

// ----------------------------------------------------------------------------
// Retrieve a string between two expressions
// ----------------------------------------------------------------------------
function Substring(sSubstring, sExpression1, sExpression2: String): String;
var
	pos1, pos2, len: Integer;
begin
  pos1 := ItPos(sExpression1, sSubstring, 1) + Length(sExpression1);
  pos2 := ItPos(sExpression2, sSubstring, 1);
  len := pos2 - pos1;
  Result := Copy(sSubstring, pos1, len);
end;

// ----------------------------------------------------------------------------
// Generate a list from the differences between list1 and list2
// -- list1: existing tags, for example
// -- list2: suggested tags, for example
// ----------------------------------------------------------------------------
function Diff(lsExistingTags, lsSuggestedTags: TStringList): String;
var
	i, j: Integer;
begin
	for i := lsExistingTags.Count - 1 downto 0 do
		for j := lsSuggestedTags.Count - 1 downto 0 do
			if lsSuggestedTags[j] = lsExistingTags[i] then
				lsSuggestedTags.Delete(j);

	Result := lsSuggestedTags.CommaText;
end;

// ----------------------------------------------------------------------------
// Return True if specific flag is set and False if not
// ----------------------------------------------------------------------------
function HasFlag(f: IInterface; asFlagName: String): Boolean;
var
	flags, templateFlags, cellFlags, recordFlags: TStringList;
	i: Integer;
begin
	// create flag lists
	flags := TStringList.Create;
	templateFlags := TStringList.Create;
	cellFlags := TStringList.Create;
	recordFlags := TStringList.Create;

	// assign flag lists
	templateFlags.DelimitedText := '"Use Traits=1", "Use Stats=2", "Use Factions=4", "Use Spell List=8", "Use Actor Effect List=8", "Use AI Data=16", "Use AI Packages=32", "Use Model/Animation=64", "Use Base Data=128", "Use Inventory=256", "Use Script=512", "Use Def Pack List=1024", "Use Attack Data=2048", "Use Keywords=4096"';
	cellFlags.DelimitedText := '"Is Interior Cell=1", "Has Water=2", "Behave Like Exterior=128", "Use Sky Lighting=256"';
	recordFlags.DelimitedText := '"ESM=1", "Deleted=32", "Border Region=64", "Turn Off Fire=128", "Casts Shadows=512", "Persistent Reference=1024", "Initially Disabled=2048", "Ignored=4096", "Visible When Distant=32768", "Dangerous=131072", "Compressed=262144", "Cant Wait=524288"';

	// merge flag lists
	flags.AddStrings(templateFlags);
	flags.AddStrings(cellFlags);
	flags.AddStrings(recordFlags);

	// find index
	i := StrToInt(Lowercase(flags.Values[asFlagName]));

	// free flag lists
	flags.Free;
	templateFlags.Free;
	cellFlags.Free;
	recordFlags.Free;

	// return result
	Result := (GetNativeValue(f) and i > 0);
end;

// ----------------------------------------------------------------------------
// Returns True if the string contains only zeroes and False if not
// ----------------------------------------------------------------------------
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

// ----------------------------------------------------------------------------
// Output bad tag messages to log
// ----------------------------------------------------------------------------
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

// ----------------------------------------------------------------------------
// Check if the tag already exists
// ----------------------------------------------------------------------------
function TagExists(asTag: String): Boolean;
begin
	Result := (lsTags.IndexOf(asTag) <> -1);
end;

// ******************************************************************
// PROCEDURES
// ******************************************************************

// ----------------------------------------------------------------------------
// Add the tag if the tag does not exist
// ----------------------------------------------------------------------------
procedure AddTag(asTag: String);
begin
	if not TagExists(asTag) then
		lsTags.Add(asTag);
end;

// ----------------------------------------------------------------------------
// Evaluate
// Determines whether two elements are different and suggests tags
// Not to be used when you need to know how two elements differ
// ----------------------------------------------------------------------------
procedure Evaluate(asTag: String; e, m: IInterface);
begin
	// do not try to evaluate nothing
	{if Length(DefTypeString(e)) = 0 then
		exit;}

	// exit if the tag already exists
	if TagExists(asTag) then
		exit;

	// exit if element is unknown or a flags element
	// removed

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

// ----------------------------------------------------------------------------
// EvaluateByPath
// ----------------------------------------------------------------------------
procedure EvaluateByPath(asTag: String; e, m: IInterface; asPath: String);
var
	x, y: IInterface;
begin
	x := GetElement(e, asPath);
	y := GetElement(m, asPath);

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
		if CompareFlags(asTag, e, m, {sPath} 'DATA', {sFlagName} 'Behave Like Exterior', True, True) then
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
		if CompareFlags(asTag, e, m, {sPath} 'DATA', {sFlagName} 'Use Sky Lighting', True, True) then
			exit;

	// Bookmark: C.Water
	if asTag = 'C.Water' then
	begin
		// add tag if Has Water flag is set in one record but not the other
		if CompareFlags(asTag, e, m, {sPath} 'DATA', {sFlagName} 'Has Water', True, True) then
			exit;

		// exit if Is Interior Cell is set in either record
		if CompareFlags(asTag, e, m, {sPath} 'DATA', {sFlagName} 'Is Interior Cell', False, False) then
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
		if wbGameMode <> gmTES5 then
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
		if (sSignature = 'ALCH') or (sSignature = 'AMMO') or (sSignature = 'APPA')
		or (sSignature = 'BOOK') or (sSignature = 'INGR')	or (sSignature = 'KEYM')
		or (sSignature = 'LIGH') or (sSignature = 'MGEF') or (sSignature = 'MISC')
		or (sSignature = 'SGST') or (sSignature = 'SLGM')	or (sSignature = 'TREE')
		or (sSignature = 'WEAP') then
		begin
			EvaluateByPath(asTag, e, m, 'Icon');
			EvaluateByPath(asTag, e, m, 'Model');
		end;

		// evaluate Icon properties
		if (sSignature = 'BSGN') or (sSignature = 'CLAS') or (sSignature = 'LSCR')
		or (sSignature = 'LTEX') or (sSignature = 'REGN') then
			EvaluateByPath(asTag, e, m, 'Icon');

		// evaluate Model properties
		if (sSignature = 'ACTI') or (sSignature = 'DOOR') or (sSignature = 'FLOR')
		or (sSignature = 'FURN') or (sSignature = 'GRAS') or (sSignature = 'STAT') then
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
			if (wbGameMode = gmFO3)
			or (wbGameMode = gmFNV) then
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
			if wbGameMode = gmTES5 then
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

			if wbGameMode = gmTES5 then
			begin
				EvaluateByPath(asTag, e, m, 'NAM8');
				EvaluateByPath(asTag, e, m, 'NAM9');
			end;

			EvaluateByPath(asTag, e, m, 'DATA');
		end;

		// evaluate MGEF properties
		if wbGameMode = gmTES5 then
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
				if (wbGameMode <> gmFO3) or (wbGameMode <> gmTES5) then
					EvaluateByPath(asTag, e, m, 'RNAM'); // FO3 and TESV don't have this element
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
			// TES5
			if wbGameMode = gmTES5 then
				EvaluateByPath(asTag, e, m, 'SNDD');

			// FO3, FNV, TES4
			if wbGameMode <> gmTES5 then
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
		if (sSignature = 'ALCH')
		or (sSignature = 'AMMO')
		or (sSignature = 'APPA')
		or (sSignature = 'ARMO')
		or (sSignature = 'BOOK')
		or (sSignature = 'CLOT')
		or (sSignature = 'INGR')
		or (sSignature = 'KEYM')
		or (sSignature = 'LIGH')
		or (sSignature = 'MISC')
		or (sSignature = 'SGST')
		or (sSignature = 'SLGM')
		or (sSignature = 'WEAP') then
		begin
			EvaluateByPath(asTag, e, m, 'EDID');
			EvaluateByPath(asTag, e, m, 'DATA');

			if (sSignature = 'ARMO')
			or (sSignature = 'WEAP') then
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

// ----------------------------------------------------------------------------
// Delev, Relev
// ----------------------------------------------------------------------------
// Bookmark: Delev, Relev
procedure ProcessDelevRelevTags(e, m: IInterface);
var
	i, j: integer;
	kEntries, kEntriesMaster, kEntry, kEntryMaster: IInterface;
	kCOED, kCOEDMaster: IInterface; // extra data
	sSortKey, sSortKeyMaster, sTag: String; // sortkeys for extra data, sortkey is a compact text representation of element's values
begin
	// nothing to do if already tagged
	if TagExists('Delev') and TagExists('Relev') then
		exit;

	// get Leveled List Entries
	kEntries       := ElementByName(e, 'Leveled List Entries');
	kEntriesMaster := ElementByName(m, 'Leveled List Entries');

	if not Assigned(kEntries)
	or not Assigned(kEntriesMaster) then
		exit;

	// initalize count matched on reference entries
	j := 0;

	// iterate through all entries
	for i := 0 to ElementCount(kEntries) - 1 do
	begin
		kEntry       := ElementByIndex(kEntries, i);
		kEntryMaster := SortedArrayElementByValue(kEntriesMaster, 'LVLO\Reference', GetElementEditValues(kEntry, 'LVLO\Reference'));

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
					kCOED       := ElementBySignature(kEntry,  'COED');
					kCOEDMaster := ElementBySignature(kEntryMaster, 'COED');

					sSortKey := '';
					if Assigned(kCOED) then
						sSortKey := SortKeyEx(kCOED);

					sSortKeyMaster := '';
					if Assigned(kCOEDMaster) then
						sSortKeyMaster := SortKeyEx(kCOEDMaster);

					if sSortKey <> sSortKeyMaster then
					begin
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
			AddLogEntry(sTag, '[ElementCount]', kEntries, kEntriesMaster);
			AddTag(sTag);
			exit;
		end;
	end;
end;

// ----------------------------------------------------------------------------
// Debug Message
// ----------------------------------------------------------------------------
function AddLogEntry(asTag, asTestName: String; e, m: IInterface): String;
var
	sPrefix, sString: String;
begin
	if optionOutputLog = mrNo then
		exit;

	sPrefix := asTestName + ' ' + asTag + ': ';
	sString := sPrefix + TrimLeft(FullPath(e)) + #13#10 + sPrefix + TrimLeft(FullPath(m));
	lsLog.Add(sString);
end;


// ******************************************************************
// PROCESSOR
// ******************************************************************

// ----------------------------------------------------------------------------
// Main
// ----------------------------------------------------------------------------
function Initialize: Integer;
begin
	// clear
	ClearMessages();

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
	lsTags := TStringList.Create;
	lsTags.Delimiter := ','; // separated by comma

	Separator(True);

	if wbGameMode = gmFO3 then
		AddMessage('Using record structure for Fallout 3');
	if wbGameMode = gmFNV then
		AddMessage('Using record structure for Fallout: New Vegas');
	if wbGameMode = gmTES4 then
		AddMessage('Using record structure for The Elder Scrolls IV: Oblivion');
	if wbGameMode = gmTES5 then
		AddMessage('Using record structure for The Elder Scrolls V: Skyrim');

	Separator(False);
end;



// ----------------------------------------------------------------------------
// Process
// ----------------------------------------------------------------------------
function Process(e: IInterface): Integer;
var
	o: IInterface;
	sTag, sSignature: String;
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
	or (sSignature = 'TES4')
	or (ConflictState = caUnknown)
	or (ConflictState = caOnlyOne)
	or (ConflictState = caNoConflict) then
		exit;

	// get file and file name
	if not bRunOnce then
	begin
		f  := GetFile(e);
		fn := GetFileName(f);
		bRunOnce := True;
	end;

	// exit if the record should not be processed
	if (fn = 'Dawnguard.esm') then
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
		if (sSignature = 'CREA')
		or (sSignature = 'NPC_') then
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
	// GROUP: Supported tags exclusive to TES5
	// -------------------------------------------------------------------------------
	if wbGameMode = gmTES5 then
		if sSignature = 'CELL' then
		begin
			ProcessTag('C.Location', e, o);
			ProcessTag('C.Regions', e, o);
			ProcessTag('C.SkyLighting', e, o);
		end;

	// -------------------------------------------------------------------------------
	// GROUP: Supported tags exclusive to FO3 and FNV
	// -------------------------------------------------------------------------------
	if (wbGameMode = gmFO3)
	or (wbGameMode = gmFNV) then
	begin
		sTag := 'Destructible';
		if (sSignature = 'ACTI')
		or (sSignature = 'ALCH')
		or (sSignature = 'AMMO')
		or (sSignature = 'BOOK')
		or (sSignature = 'CONT')
		or (sSignature = 'DOOR')
		or (sSignature = 'FURN')
		or (sSignature = 'IMOD')
		or (sSignature = 'KEYM')
		or (sSignature = 'MISC')
		or (sSignature = 'MSTT')
		or (sSignature = 'PROJ')
		or (sSignature = 'TACT')
		or (sSignature = 'TERM')
		or (sSignature = 'WEAP') then
			ProcessTag(sTag, e, o);

		// special handling for CREA and NPC_ record types
		if (sSignature = 'CREA')
		or (sSignature = 'NPC_') then
			if not CompareFlags(sTag, e, o, 'ACBS\Template Flags', 'Use Model/Animation', False, False) then
				ProcessTag(sTag, e, o);
	end;

	// -------------------------------------------------------------------------------
	// GROUP: Supported tags exclusive to FO3, FNV, and TES4
	// -------------------------------------------------------------------------------
	if (wbGameMode = gmFO3)
	or (wbGameMode = gmFNV)
	or (wbGameMode = gmTES4) then
	begin
		if (sSignature = 'CREA')
		or (sSignature = 'NPC_') then
		begin
			sTag := 'Factions';
			if wbGameMode = gmTES4 then
				ProcessTag(sTag, e, o);
			else
				if not CompareFlags(sTag, e, o, 'ACBS\Template Flags', 'Use Factions', False, False) then
					ProcessTag(sTag, e, o);
		end;

		if sSignature = 'FACT' then
			ProcessTag('Relations', e, o);
	end;

	// -------------------------------------------------------------------------------
	// GROUP: Supported tags exclusive to FO3, FNV, and TES5
	// -------------------------------------------------------------------------------
	if (wbGameMode = gmFO3)
	or (wbGameMode = gmFNV)
	or (wbGameMode = gmTES5) then
	begin
		if (sSignature = 'CREA')
		or (sSignature = 'NPC_') then
		begin
			sTag := 'Actors.ACBS';
			if not CompareFlags(sTag, e, o, 'ACBS\Template Flags', 'Use Stats', False, False) then
				ProcessTag(sTag, e, o);

			sTag := 'Actors.AIData';
			if not CompareFlags(sTag, e, o, 'ACBS\Template Flags', 'Use AI Data', False, False) then
				ProcessTag(sTag, e, o);

			sTag := 'Actors.AIPackages';
			if not CompareFlags(sTag, e, o, 'ACBS\Template Flags', 'Use AI Packages', False, False) then
				ProcessTag(sTag, e, o);

			if sSignature = 'CREA' then
				if not CompareFlags(sTag, e, o, 'ACBS\Template Flags', 'Use Model/Animation', False, False) then
					ProcessTag('Actors.Anims', e, o);

			if not CompareFlags(sTag, e, o, 'ACBS\Template Flags', 'Use Traits', False, False) then
			begin
				ProcessTag('Actors.CombatStyle', e, o);
				ProcessTag('Actors.DeathItem', e, o);
			end;

			sTag := 'Actors.Skeleton';
			if not CompareFlags(sTag, e, o, 'ACBS\Template Flags', 'Use Model/Animation', False, False) then
				ProcessTag(sTag, e, o);

			sTag := 'Actors.Stats';
			if not CompareFlags(sTag, e, o, 'ACBS\Template Flags', 'Use Stats', False, False) then
				ProcessTag(sTag, e, o);

			// TODO: IIM - NOT IMPLEMENTED
			// TODO: MustBeActiveIfImported - NOT IMPLEMENTED

			if sSignature = 'NPC_' then
			begin
				sTag := 'NPC.Class';
				if not CompareFlags(sTag, e, o, 'ACBS\Template Flags', 'Use Traits', False, False) then
					ProcessTag(sTag, e, o);

				sTag := 'NPC.Race';
				if not CompareFlags(sTag, e, o, 'ACBS\Template Flags', 'Use Traits', False, False) then
					ProcessTag(sTag, e, o);

				sTag := 'NpcFaces';
				if not CompareFlags(sTag, e, o, 'ACBS\Template Flags', 'Use Model/Animation', False, False) then
					ProcessTag(sTag, e, o);
			end;

			sTag := 'Scripts';
			if not CompareFlags(sTag, e, o, 'ACBS\Template Flags', 'Use Script', False, False) then
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

		if (sSignature = 'ACTI')
		or (sSignature = 'ALCH')
		or (sSignature = 'ARMO')
		or (sSignature = 'CONT')
		or (sSignature = 'DOOR')
		or (sSignature = 'FLOR')
		or (sSignature = 'FURN')
		or (sSignature = 'INGR')
		or (sSignature = 'KEYM')
		or (sSignature = 'LIGH')
		or (sSignature = 'LVLC')
		or (sSignature = 'MISC')
		or (sSignature = 'QUST')
		or (sSignature = 'WEAP') then
			ProcessTag('Scripts', e, o);
	end; // end game

	// -------------------------------------------------------------------------------
	// GROUP: Supported tags exclusive to FO3, FNV, TES4, and TES5
	// -------------------------------------------------------------------------------
	if (wbGameMode = gmFO3)
	or (wbGameMode = gmFNV)
	or (wbGameMode = gmTES4)
	or (wbGameMode = gmTES5) then
	begin
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
		if (sSignature = 'LVLC')
		or (sSignature = 'LVLI')
		or (sSignature = 'LVLN')
		or (sSignature = 'LVSP') then
			ProcessDelevRelevTags(e, o);

		// TODO: Filter - NOT IMPLEMENTED

		if (sSignature = 'ACTI')
		or (sSignature = 'ALCH')
		or (sSignature = 'AMMO')
		or (sSignature = 'APPA')
		or (sSignature = 'ARMO')
		or (sSignature = 'BOOK')
		or (sSignature = 'BSGN')
		or (sSignature = 'CLAS')
		or (sSignature = 'CLOT')
		or (sSignature = 'DOOR')
		or (sSignature = 'FLOR')
		or (sSignature = 'FURN')
		or (sSignature = 'INGR')
		or (sSignature = 'KEYM')
		or (sSignature = 'LIGH')
		or (sSignature = 'MGEF')
		or (sSignature = 'MISC')
		or (sSignature = 'SGST')
		or (sSignature = 'SLGM')
		or (sSignature = 'WEAP') then
		begin
			ProcessTag('Graphics', e, o);
			ProcessTag('Names', e, o);
			ProcessTag('Stats', e, o);

			if (sSignature = 'ACTI')
			or (sSignature = 'DOOR')
			or (sSignature = 'LIGH')
			or (sSignature = 'MGEF') then
				ProcessTag('Sound', e, o);
		end;


		if (sSignature = 'CREA')
		or (sSignature = 'EFSH')
		or (sSignature = 'GRAS')
		or (sSignature = 'LSCR')
		or (sSignature = 'LTEX')
		or (sSignature = 'REGN')
		or (sSignature = 'STAT')
		or (sSignature = 'TREE') then
			ProcessTag('Graphics', e, o);

		if sSignature = 'CONT' then
		begin
			ProcessTag('Invent', e, o);
			ProcessTag('Names', e, o);
			ProcessTag('Sound', e, o);
		end;

		if (sSignature = 'DIAL')
		or (sSignature = 'ENCH')
		or (sSignature = 'EYES')
		or (sSignature = 'FACT')
		or (sSignature = 'HAIR')
		or (sSignature = 'QUST')
		or (sSignature = 'RACE')
		or (sSignature = 'SPEL')
		or (sSignature = 'WRLD') then
			ProcessTag('Names', e, o);

		// TODO: NoMerge - NOT IMPLEMENTED

		if (sSignature = 'WTHR') then
			ProcessTag('Sound', e, o);

		// special handling for CREA and NPC_
		if (sSignature = 'CREA')
		or (sSignature = 'NPC_') then
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
				if not CompareFlags(sTag, e, o, 'ACBS\Template Flags', 'Use Inventory', False, False) then
					ProcessTag(sTag, e, o);

				// special handling for CREA and NPC_ record types
				sTag := 'Names';
				if not CompareFlags(sTag, e, o, 'ACBS\Template Flags', 'Use Base Data', False, False) then
					ProcessTag(sTag, e, o);

				// special handling for CREA record type
				sTag := 'Sound';
				if sSignature = 'CREA' then
					if not CompareFlags(sTag, e, o, 'ACBS\Template Flags', 'Use Model/Animation', False, False) then
						ProcessTag(sTag, e, o);
			end;
		end;


	end; // end game
end;

// ----------------------------------------------------------------------------
// Finalize
// ----------------------------------------------------------------------------
function Finalize: Integer;
var
	kHeader, kDescription: IInterface;
	sTags, eTags, dTags, bTags, tTags: TSTringList;
	sDescription: String;
begin
	sTags := TStringList.Create;
	eTags := TStringList.Create;
	dTags := TStringList.Create;
	bTags := TStringList.Create;
	tTags := TStringList.Create;

	// exit conditions
	if (optionAddTags = mrAbort)
	or not Assigned(lsTags)
	or not Assigned(fn) then
		exit;

	// sort list of suggested tags
	lsTags.Sort;

	// output file name
	AddMessage(Uppercase(fn));

	// output log
	if optionOutputLog = mrYes then
		if lsLog.Count > 0 then
			AddMessage(lsLog.Text);

	// if any suggested tags were generated
	if lsTags.Count > 0 then
	begin
		kHeader := ElementBySignature(f, 'TES4');

		// determine if the header record exists
		if Assigned(kHeader) then
		begin
			kDescription := ElementBySignature(kHeader, 'SNAM');
			sTags.CommaText := lsTags.CommaText;
			tTags.CommaText := lsTags.CommaText;
			eTags.CommaText := Substring(GetEditValue(kDescription), '{{BASH:', '}}');
			dTags.CommaText := Diff(eTags, sTags);

			// exit if existing and suggested tags are the same
			if (eTags.CommaText = sTags.CommaText) then
			begin
				AddMessage('No tags suggested. Exiting.' + #13#10);
				Separator(False);
				exit;
			end;

		// exit if the header record doesn't exist
		end else begin
			AddMessage('Header record not found. Nothing to do. Exiting.' + #13#10);
			Separator(False);
			exit;
		end;

		// write tags
		if optionAddTags = mrYes then
		begin
			// if the description element doesn't exist, add the element
			kDescription := AddElementByString(kHeader, 'SNAM');

			if eTags <> sTags then
			begin
				// store description
				sDescription := GetEditValue(kDescription);

				// remove existing tags, if any; trim regardless
				sDescription := Trim(RemoveFromEnd(sDescription, Format('{{BASH:%s}}', [eTags.DelimitedText])));

				// write new description
				SetEditValue(kDescription, sDescription + #13#10 + #13#10 + Format('{{BASH:%s}}', [lsTags.DelimitedText]));
			end;

			// output bad tags
			bTags.CommaText := Diff(tTags, eTags);
			FormatTags(bTags, 'bad tag removed:', 'bad tags removed:', 'No bad tags found.');

			// output to log
			FormatTags(dTags, 'tag added to file header:', 'tags added to file header:', 'No tags added.');
		end;

		// suggest tags only and output to log
		if optionAddTags = mrNo then
		begin
			// output existing tags
			FormatTags(eTags, 'existing tag found:', 'existing tags found:', 'No existing tags found.');

			// output bad tags
			bTags.CommaText := Diff(tTags, eTags);
			FormatTags(bTags, 'bad tag found:', 'bad tags found:', 'No bad tags found.');

			// output suggested tags
			FormatTags(dTags, 'suggested tag to add:', 'suggested tags to add:', 'No suggested tags to add.');

			// output all suggested tags
			FormatTags(lsTags, 'suggested tag overall:', 'suggested tags overall:', 'No suggested tags overall.');
		end;
	end;

	lsLog.Free;
	lsTags.Free;
	sTags.Free;
	eTags.Free;
	dTags.Free;
	bTags.Free;
	tTags.Free;
end;

end.