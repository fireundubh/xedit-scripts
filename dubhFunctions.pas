{
	Purpose: Extensions for mteFunctions
	Author: fireundubh <fireundubh@gmail.com>
	Version:
		- 0.2: December 1, 2015
		- 0.1: Long, long ago
}

unit dubhFunctions;
uses mteFunctions;

// --------------------------------------------------------------------
// AddMessage
// --------------------------------------------------------------------
function Log(s: string): Integer;
begin
	AddMessage(s);
end;

// --------------------------------------------------------------------
// GetEditValue
// --------------------------------------------------------------------
function gev(s: String): String;
begin
	Result := GetEditValue(s);
end;

// --------------------------------------------------------------------
// SetEditValue
// --------------------------------------------------------------------
function sev(x: IInterface; s: String): Integer;
begin
	SetEditValue(x, s);
end;

// --------------------------------------------------------------------
// Lowercases two strings, compares them, and returns value
// --------------------------------------------------------------------
function CompareStrLower(s1, s2: String): Integer;
begin
	Result := CompareStr(Lowercase(s1), Lowercase(s2));
end;

// --------------------------------------------------------------------
// Returns true/false if substring is in element's substring
// --------------------------------------------------------------------
function HasElementSubstring(s: String; x: IInterface; CaseSensitive: Boolean): Boolean;
begin
	if not CaseSensitive then
		if pos(Lowercase(s), Lowercase(GetEditValue(x))) > 0 then
			Result := true;
	if CaseSensitive then
		if pos(s, GetEditValue(x)) > 0 then
			Result := true;
end;

// --------------------------------------------------------------------
// Returns true/false if element is assigned by element's signature
// --------------------------------------------------------------------
function AssignedBySignature(x: IInterface; s: String): Boolean;
begin
	if Assigned(GetElement(x, s)) then
		Result := true;
end;

// --------------------------------------------------------------------
// Returns the string of an element by the element's signature
// --------------------------------------------------------------------
function ElementStringBySignature(x: IInterface; s: String): String;
begin
	Result := GetEditValue(GetElement(x, s));
end;

// --------------------------------------------------------------------
// Returns true/false if a string is in a TStringList
// --------------------------------------------------------------------
function StringExistsInList(item: String; list: TStringList): Boolean;
begin
	Result := (list.IndexOf(item) <> -1);
end;

// --------------------------------------------------------------------
// Returns the string for a condition type
// --------------------------------------------------------------------
function GetCondType(val: String): String;
var
	operators: String;
	a, b, c, d, e: Boolean;
begin
	if GetChar(val, 1) = '1' then // equal to
		a := true;
	if GetChar(val, 2) = '1' then // greater than
		b := true;
	if GetChar(val, 3) = '1' then // less than
		c := true;
	if GetChar(val, 4) = '1' then // or
		d := true;
	if val = '00000000' then
		e := true;

	if e then
		Result := 'Not equal to';
	if a and not b and not c and not d then
		Result := 'Equal to';
	if b and not a and not c and not d then
		Result := 'Greater than';
	if c and not a and not b and not d then
		Result := 'Less than';

	if a and b and not c and not d then
		Result := 'Greater than or equal to';
	if a and c and not b and not d then
		Result := 'Less than or equal to';

	if a and d and not b and not c then
		Result := 'Equal to / Or';
	if b and d and not a and not c then
		Result := 'Greater than / Or';
	if c and d and not a and not b then
		Result := 'Less than / Or';

	if a and b and d and not c then
		Result := 'Greater than or equal to / Or';
	if a and c and d and not b then
		Result := 'Less than or equal to / Or';
end;

// --------------------------------------------------------------------
// Returns the string of a condition type but with comparison operators
// --------------------------------------------------------------------
function GetCondTypeOperator(val: String): String;
var
	operators: String;
	a, b, c, d, e: Boolean;
begin
	if GetChar(val, 1) = '1' then // equal to
		a := true;
	if GetChar(val, 2) = '1' then // greater than
		b := true;
	if GetChar(val, 3) = '1' then // less than
		c := true;
	if GetChar(val, 4) = '1' then // or
		d := true;
	if val = '00000000' then
		e := true;

	if e then
		Result := '<>';
	if a and not b and not c and not d then
		Result := '==';
	if b and not a and not c and not d then
		Result := '>';
	if c and not a and not b and not d then
		Result := '<';

	if a and b and not c and not d then
		Result := '>=';
	if a and c and not b and not d then
		Result := '<=';

	if a and d and not b and not c then
		Result := '== / Or';
	if b and d and not a and not c then
		Result := '> / Or';
	if c and d and not a and not b then
		Result := '< / Or';

	if a and b and d and not c then
		Result := '>= / Or';
	if a and c and d and not b then
		Result := '<= / Or';
end;

// --------------------------------------------------------------------
// Returns a localized string as a string from a hexadecimal Form ID
// Note: Init LocalizationGetStringsFromFile() for performance gain
// --------------------------------------------------------------------
function LocalizedStringByFormID(hex: String; sl: TStringList): String;
var
	idx: Integer;
begin
	// add LocalizationGetStringsFromFile('fallout4_en.strings', stringListObj);
	// to Initialize
	idx := sl.IndexOfObject(TObject(StrToInt('$' + hex)));
	if idx > -1 then
		Result := sl[idx]
	else
		Result := 'String not found in lookup table';
end;

// --------------------------------------------------------------------
// Converts a string to a hexadecimal value
// --------------------------------------------------------------------
function StringToHex(S: String): string;
var
	I: Integer;
begin
  Result := '';
  for I := 1 to length (S) do
    Result := Result + IntToHex(ord(S[i]), 2);
end;

// --------------------------------------------------------------------
// Converts a hexadecimal value to a string
// --------------------------------------------------------------------
function HexToString(H: String): String;
var
	I: Integer;
begin
  Result := '';
  for I := 1 to length (H) div 2 do
    Result := Result + Char(StrToInt('$' + Copy(H, (I-1)*2+1, 2)));
end;

// --------------------------------------------------------------------
// Reverses a byte array and returns a string
// --------------------------------------------------------------------
function ReverseHex(const S: String): String;
var
	i: Integer;
	slSource, slTarget: TStringList;
begin
	slSource := TStringList.Create;
	slSource.Delimiter := ' ';

	slTarget := TStringList.Create;
	slTarget.Delimiter := ' ';

	slSource.DelimitedText := S;
	for i := slSource.Count - 1 downto 0 do
		slTarget.Add(slSource[i]);

	Result := TrimSpaces(slTarget.DelimitedText);
end;

// --------------------------------------------------------------------
// Return the number of times a character occurs in a string
// --------------------------------------------------------------------
function NumOfChar(const S: String; const C: Char): Integer;
var
  i: Integer;
begin
	Result := 0;
	for i := 1 to Length(S) do
		if S[i] = C then
			inc(Result);
end;

// --------------------------------------------------------------------
// Trim spaces from string and return string
// --------------------------------------------------------------------
function TrimSpaces(const str: String): String;
begin
	Result := StringReplace(str, ' ', '', [rfReplaceAll, rfIgnoreCase]);
end;

// --------------------------------------------------------------------
// Trim dashes from string and return string
// --------------------------------------------------------------------
function TrimDashes(const str: String): String;
begin
	Result := StringReplace(str, '-', '', [rfReplaceAll, rfIgnoreCase]);
end;

// --------------------------------------------------------------------
// Returns True if the x signature is in the y list of signatures
// --------------------------------------------------------------------
function InStringList(x, y: String): Boolean;
var
	idx: Integer;
	l: TStringList;
begin
	l := TStringList.Create;
	l.DelimitedText := y;
	idx := l.IndexOf(x);
	l.Free;
	Result := (idx > -1);
end;

// --------------------------------------------------------------------
// Returns true if the needle is in the haystack
// --------------------------------------------------------------------
function HasString(needle, haystack: String; caseSensitive: Boolean): Boolean;
begin
	if caseSensitive then
		if pos(needle, haystack) > 0 then
			Result := true;

	if not caseSensitive then
		if pos(Lowercase(needle), Lowercase(haystack)) > 0 then
			Result := true;
end;

// --------------------------------------------------------------------
// Returns any element from a string
// --------------------------------------------------------------------
function GetElement(x: IInterface; s: String): IInterface;
begin
	if (pos('[', s) > 0) then
		Result := ElementByIP(x, s)
	else if (pos('\', s) > 0) then
		Result := ElementByPath(x, s)
	else if (s = Uppercase(s)) then
		Result := ElementBySignature(x, s)
	else
		Result := ElementByName(x, s);
end;

// --------------------------------------------------------------------
// Returns a master overriding record. Int parameter should be 1 or 2.
// --------------------------------------------------------------------
function GetLastMaster(x: IInterface; i: integer): IInterface;
function GetLastOverride(x: IInterface; i: integer): IInterface;
function OverrideMaster(x: IInterface; i: integer): IInterface;
var
	o: IInterface;
begin
	o := Master(x);
	if not Assigned(o) then
		o := MasterOrSelf(x);
	if OverrideCount(o) > i - 1 then
		o := OverrideByIndex(o, OverrideCount(o) - i);
	Result := o;
end;

// --------------------------------------------------------------------
// Returns "NAME/EDID [SIG_:00000000]" as a String
// --------------------------------------------------------------------
function SmallNameEx(e: IInterface): string;
begin
	if Signature(e) = 'REFR' then
		Result := GetElementEditValues(e, 'NAME') + ' [' + Signature(e) + ':' + HexFormID(e) + ']'
	else
		Result := GetElementEditValues(e, 'EDID') + ' [' + Signature(e) + ':' + HexFormID(e) + ']';
end;

// --------------------------------------------------------------------
// Returns the sortkey with handling for .nif paths, and unknown/unused
// 	data. Also uses a better delimiter.
// --------------------------------------------------------------------
function SortKeyEx(e: IInterface): string;
var
	i: integer;
begin
	Result := GetEditValue(e);

	// manipulate result for model paths - sometimes the same paths have different cases
	if pos('.nif', Lowercase(Result)) > 0 then
		Result := Lowercase(GetEditValue(e));

	for i := 0 to ElementCount(e) - 1 do begin
		if (pos('unknown', Lowercase(Name(ElementByIndex(e, i)))) > 0)
		or (pos('unused', Lowercase(Name(ElementByIndex(e, i)))) > 0) then
			exit;
		if (Result <> '') then
			Result := Result + ' ' + SortKeyEx(ElementByIndex(e, i))
		else
			Result := SortKeyEx(ElementByIndex(e, i));
	end;
end;

// --------------------------------------------------------------------
// Returns values from a text file as a TStringList
// --------------------------------------------------------------------
function LoadFromCsv(autoSort: boolean; allowDuplicates: boolean; useDelimiter: boolean; delimiter: string = '='): TStringList;
var
	fileObject: TOpenDialog;
	lsLines: TStringList;
begin
	lsLines := TStringList.Create;

	if autoSort then
		lsLines.Sorted;

	if allowDuplicates then
		lsLines.Duplicates := dupIgnore;

	if useDelimiter then
		if delimiter = '' then
			lsLines.NameValueSeparator := ',';

	fileObject := TOpenDialog.Create(nil);

	try
		fileObject.InitialDir := GetCurrentDir;
		fileObject.Options := [ofFileMustExist];
		fileObject.Filter := '*.csv';
		fileObject.FilterIndex := 1;
		if fileObject.Execute then
			lsLines.LoadFromFile(fileObject.FileName);
	finally
		fileObject.Free;
	end;

	Result := lsLines;
end;

// --------------------------------------------------------------------
// Returns a group by signature, or adds the group if needed
// --------------------------------------------------------------------
function AddGroupBySignature(f: IwbFile; s: String): IInterface;
var g: IInterface;
begin
	g := GroupBySignature(f, s);
	if not Assigned(g) then
		Result := Add(f, s, true)
	else
		Result := g;
end;

// --------------------------------------------------------------------
// Adds a new record to a group
// --------------------------------------------------------------------
function AddNewRecordToGroup(g: IInterface; s: String): IInterface;
var
	r: IInterface;
begin
	r := Add(g, s, true);
	if not Assigned(r) then
		r := Add(g, s, true); // tries twice because
	Result := r;
	// TODO: Signature(g) instead of String param would be more efficient but shit doesn't work right
end;

// --------------------------------------------------------------------
// Returns an element by string, or adds the element if needed
// --------------------------------------------------------------------
function AddElementByString(r: IInterface; s: String): IInterface;
var
	e: IInterface;
begin
	e := GetElement(r, s);
	if not Assigned(e) then
		Result := Add(r, s, true)
	else
		Result := e;
end;

// --------------------------------------------------------------------
// Adds a form to a formlist
// --------------------------------------------------------------------
procedure AddRecordToFormList(f, r: IInterface);
var
	l: IInterface;
begin
	l := ElementAssign(f, HighInteger, nil, true);
	SetEditValue(l, SmallNameEx(r));
	// TODO: HexFormID() would probably be faster by a tiny number of time units
end;

// --------------------------------------------------------------------
// Copies a record as an override to a file
// --------------------------------------------------------------------
procedure AddOverrideToFile(targetFile: IwbFile; rec: IInterface);
var
	ovr, ovr_edid, ovr_full: IInterface;
begin
	ovr := wbCopyElementToFile(rec, targetFile, false, true);
	{EDID} ovr_edid := AddElementByString(ovr, 'EDID');
	{FULL} ovr_full := AddElementByString(ovr, 'FULL');
	SetEditValue(ovr_full, GetEditValue(ovr_edid));
	// TODO: string fuckery shouldn't be part of the proc, but what the fuck ever
end;

end.