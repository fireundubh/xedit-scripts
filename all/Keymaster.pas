{
	Purpose:
		- Global find and replace keywords
		- Add keywords where record matches criteria
		- Delete keywords where record matches criteria
		- Remove duplicate keywords
	Author: fireundubh <fireundubh@gmail.com>
	Version: 0.1
}

unit UserScript;

uses dubhFunctions;

var
	bReplace, bAdd, bDelete, bNoInput: Boolean;
	sQueryPath: String;
	lsKeywordsToReplace, lsKeywordsToAdd, lsKeywordsToDelete: TStringList;

function Initialize: Integer;
var
	sPathReplace, sPathAdd, sPathDelete: String;
begin
	sPathReplace := ScriptsPath + 'Keymaster\lsKeywordsToReplace.csv';
	sPathAdd := ScriptsPath + 'Keymaster\lsKeywordsToAdd.csv';
	sPathDelete := ScriptsPath + 'Keymaster\lsKeywordsToDelete.csv';

	// create list for replacing keywords
	lsKeywordsToReplace := TStringList.Create;
	lsKeywordsToReplace.NameValueSeparator := '=';
	lsKeywordsToReplace.Duplicates := dupAccept;
	lsKeywordsToReplace.Sorted := False;

	bReplace := True;
	if FileExists(sPathReplace) then
		lsKeywordsToReplace.LoadFromFile(sPathReplace)
	else
		bReplace := False;

	// create list for adding keywords
	lsKeywordsToAdd := TStringList.Create;
	lsKeywordsToAdd.NameValueSeparator := '+';
	lsKeywordsToAdd.Duplicates := dupAccept;
	lsKeywordsToAdd.Sorted := False;

	bAdd := True;
	if FileExists(sPathAdd) then
		lsKeywordsToAdd.LoadFromFile(sPathAdd)
	else
		bAdd := False;

	// create list for deleting keywords
	lsKeywordsToDelete := TStringList.Create;
	lsKeywordsToDelete.NameValueSeparator := '-';
	lsKeywordsToDelete.Duplicates := dupAccept;
	lsKeywordsToDelete.Sorted := False;

	bDelete := True;
	if FileExists(sPathDelete) then
		lsKeywordsToDelete.LoadFromFile(sPathDelete)
	else
		bDelete := False;

	// clear messages tab
	ClearMessages();

	// prompt for path
	if not InputQuery('Keywords Path/Signature', 'Keywords Path/Signature:', sQueryPath) then begin
		bNoInput := True;
		exit;
	end;

	Log(#13#10 + '-------------------------------------------------------------------------------');
	Log('Keymaster by fireundubh <fireundubh@gmail.com>');
	Log('-------------------------------------------------------------------------------' + #13#10);
	Log('Processing... ' + sQueryPath + #13#10);
end;

function Process(e: IInterface): Integer;
var
	aParent: IInterface;
	i: Integer;
begin
	if bNoInput then
		exit;

	Log(SmallNameEx(e));

	aParent := GetElement(e, sQueryPath);

	if bReplace and (lsKeywordsToReplace.Count > 0) then
		for i := 0 to Pred(ElementCount(aParent)) do
			ReplaceKeywordWithNameValuePair(ElementByIndex(aParent, i), lsKeywordsToReplace, True);

	if bAdd and (lsKeywordsToAdd.Count > 0) then
		AddKeywordWithNameValuePair(aParent, lsKeywordsToAdd, True);

	if bDelete and (lsKeywordsToDelete.Count > 0) then
		for i := 0 to Pred(ElementCount(aParent)) do
			DeleteKeywordWithNameValuePair(ElementByIndex(aParent, i), lsKeywordsToDelete, True);

	RemoveDuplicateKeywords(aParent, sQueryPath);

	Log('');
end;

function Finalize: Integer;
begin
	lsKeywordsToReplace.Free;
	lsKeywordsToAdd.Free;
	lsKeywordsToDelete.Free;
end;

// ----------------------------------------------------------------------------------------------------------------------------
// lsKeywordsToReplace.csv Structure: name=value
// - Name: Form ID of keyword to find
// - Value: Form ID of keyword to replace
// ----------------------------------------------------------------------------------------------------------------------------
// NOTE: This function is a global find and replace.
// ----------------------------------------------------------------------------------------------------------------------------
procedure ReplaceKeywordWithNameValuePair(e: IInterface; lsHaystack: TStringList; bCaseSensitive: Boolean);
var
	i: Integer;
begin
	for i := 0 to lsHaystack.Count - 1 do
		if HasString(HexFormID(LinksTo(e)), lsHaystack.Names[i], bCaseSensitive) then begin
			SetEditValue(e, lsHaystack.Values[lsHaystack.Names[i]]);
			Log('  -- Replaced keyword [KYWD:' + lsHaystack.Names[i] + '] with [KYWD:' + lsHaystack.Values[lsHaystack.Names[i]] + ']');
		end;
end;

// ----------------------------------------------------------------------------------------------------------------------------
// lsKeywordsToAdd.csv Structure: name+value
// - Name: Form ID of record to modify
// - Value: Form ID of keyword to add
// ----------------------------------------------------------------------------------------------------------------------------
// NOTE: This function will check whether the current record should be modified.
// ----------------------------------------------------------------------------------------------------------------------------
procedure AddKeywordWithNameValuePair(e: IInterface; lsHaystack: TStringList; bCaseSensitive: Boolean);
var
	i: Integer;
	aKeyword: IInterface;
begin
	for i := 0 to lsHaystack.Count - 1 do begin
		if HasString(HexFormID(ContainingMainRecord(e)), lsHaystack.Names[i], bCaseSensitive) then begin
			aKeyword := ElementAssign(e, HighInteger, nil, False);
			SetEditValue(aKeyword, lsHaystack.Values[lsHaystack.Names[i]]);
			Log('  -- Added keyword: [KYWD:' + lsHaystack.Values[lsHaystack.Names[i]] + ']');
		end;
	end;
end;

// ----------------------------------------------------------------------------------------------------------------------------
// lsKeywordsToDelete.csv Structure: name-value
// - Name: Form ID of record to modify
// - Value: Form ID of record to delete
// ----------------------------------------------------------------------------------------------------------------------------
// NOTE: This function will check whether the current record should be modified.
// ----------------------------------------------------------------------------------------------------------------------------
procedure DeleteKeywordWithNameValuePair(e: IInterface; lsHaystack: TStringList; bCaseSensitive: Boolean);
var
	i: Integer;
begin
	for i := 0 to lsHaystack.Count - 1 do
		if HasString(HexFormID(ContainingMainRecord(e)), lsHaystack.Names[i], bCaseSensitive) then
			if HasString(HexFormID(LinksTo(e)), lsHaystack.Values[lsHaystack.Names[i]], bCaseSensitive) then begin
				Remove(e);
				Log('  -- Deleted keyword: [KYWD:' + lsHaystack.Values[lsHaystack.Names[i]] + ']');
			end;
end;

procedure RemoveDuplicateKeywords(e: IInterface; sParent: String);
var
	i, iKeywordCount: Integer;
	aMaster, aParent, aChild: IInterface;
	lsKeywords: TStringList;
begin
	lsKeywords := TStringList.Create;
	lsKeywords.Duplicates := dupIgnore;
	lsKeywords.Sorted := True;

	iKeywordCount := ElementCount(e);
	for i := 0 to Pred(iKeywordCount) do
		lsKeywords.Add(HexFormID(LinksTo(ElementByIndex(e, i))));

	iKeywordCount := iKeywordCount - lsKeywords.Count;

	if iKeywordCount > 0 then begin
		if iKeywordCount > 1 then
			Log('  -- Removed ' + IntToStr(iKeywordCount) + ' duplicates.')
		else
			Log('  -- Removed ' + IntToStr(iKeywordCount) + ' duplicate.');

		aMaster := ContainingMainRecord(e);
		Remove(e);

		aParent := GetElement(aMaster, sParent);
		if not Assigned(aParent) then
			aParent := AddElementByString(aMaster, sParent);

		for i := 0 to lsKeywords.Count - 1 do begin
			aChild := ElementAssign(aParent, HighInteger, nil, False);
			SetEditValue(aChild, lsKeywords[i]);
		end;
	end;

	lsKeywords.Free;
end;

end.
