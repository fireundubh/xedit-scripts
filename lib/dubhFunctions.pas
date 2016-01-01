unit dubhFunctions;

uses mteFunctions;

// --------------------------------------------------------------------
// AddMessage
// --------------------------------------------------------------------
procedure Log(const s: string);
begin
	AddMessage(s);
end;

// --------------------------------------------------------------------
// Return the form version as a string
// --------------------------------------------------------------------
function FormVersion(x: IInterface): String;
var
	v: String;
begin
	v := GetElementEditValues(x, 'Record Header\Form Version');
	if Length(v) = 2 then
		v := '0' + v;
	Result := v;
end;

// --------------------------------------------------------------------
// A shorter way to replace strings without RegEx
// --------------------------------------------------------------------
function StrReplace(this, that, subject: String): String;
begin
	Result := StringReplace(subject, this, that, [rfReplaceAll, rfIgnoreCase]);
end;

// --------------------------------------------------------------------
// Replace a substring with a RegEx pattern and return the new string
// --------------------------------------------------------------------
function RegExReplace(pattern, subject, replacement: String): String;
var
	regex: TPerlRegEx;
	output: String;
begin
	regex := TPerlRegEx.Create;
	try
		regex.RegEx := pattern;
		regex.Options := [];
		regex.Subject := subject;
		regex.Replacement := replacement;
		regex.ReplaceAll;
		output := regex.Subject;
	finally
		regex.Free;
		Result := output;
	end;
end;

// --------------------------------------------------------------------
// Return a substring with a RegEx pattern
// --------------------------------------------------------------------
function RegExMatch(pattern, subject: String): String;
var
	regex: TPerlRegEx;
	output: String;
begin
	regex := TPerlRegEx.Create;
	try
		regex.RegEx := pattern;
		regex.Options := [];
		regex.Subject := subject;
		regex.Match;
		output := regex.MatchedText;
	finally
		regex.Free;
		Result := output;
	end;
end;

// --------------------------------------------------------------------
// Return whether a string matches a RegEx pattern
// --------------------------------------------------------------------
function RegExMatches(pattern, subject: String): Boolean;
var
	regex: TPerlRegEx;
	output: String;
begin
	regex := TPerlRegEx.Create;
	try
		regex.RegEx := pattern;
		regex.Options := [];
		regex.Subject := subject;
		regex.Match;
		output := regex.FoundMatch;
	finally
		regex.Free;
		Result := output;
	end;
end;

// --------------------------------------------------------------------
// Apply Markdown formatting to TStringList
// --------------------------------------------------------------------
function Markdown(const ls: TStringList; const d: Char; const pre: Boolean): TStringList;
var
	mds, mdb, mde, mdd: String;
	i: Integer;
	lsmd: TStringList;
begin
	if pre then begin
		mdb := '<pre>';
		mde := '</pre>';
		mdd := '</pre> | <pre>';
	end else begin
		mdb := '`';
		mde := '`';
		mdd := '` | `';
	end;

	lsmd := TStringList.Create;

	for i := 0 to ls.Count - 1 do begin
		mds := mdb + StringReplace(ls[i], d, mdd, [rfReplaceAll, rfIgnoreCase]) + mde;
		lsmd.Add(mds);
	end;

	Result := lsmd;
end;

// --------------------------------------------------------------------
// GetEditValue
// --------------------------------------------------------------------
function gev(const s: String): String;
begin
	Result := GetEditValue(s);
end;

// --------------------------------------------------------------------
// SetEditValue
// --------------------------------------------------------------------
procedure sev(const x: IInterface; const s: String);
begin
	SetEditValue(x, s);
end;

// --------------------------------------------------------------------
// GetNativeValue
// --------------------------------------------------------------------
function gnv(const x: IInterface): Variant;
begin
	Result := GetNativeValue(x);
end;

// --------------------------------------------------------------------
// SetNativeValue
// --------------------------------------------------------------------
procedure sev(const x: IInterface; const v: Variant);
begin
	SetNativeValue(x, v);
end;

// --------------------------------------------------------------------
// ElementCount - 1
// --------------------------------------------------------------------
function PredCount(const x: IInterface): Integer;
begin
	Result := Pred(ElementCount(x));
end;

// --------------------------------------------------------------------
// Lowercases two strings, compares them, and returns value
// --------------------------------------------------------------------
function CompareStrLower(const s1, s2: String): Integer;
begin
	Result := CompareStr(Lowercase(s1), Lowercase(s2));
end;

// --------------------------------------------------------------------
// Returns true/false if substring is in element's substring
// --------------------------------------------------------------------
function HasElementSubstring(const s: String; const x: IInterface; const CaseSensitive: Boolean): Boolean;
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
function AssignedBySignature(const x: IInterface; const s: String): Boolean;
begin
	if Assigned(GetElement(x, s)) then
		Result := true;
end;

// --------------------------------------------------------------------
// Returns the string of an element by the element's signature
// --------------------------------------------------------------------
function ElementStringBySignature(const x: IInterface; const s: String): String;
begin
	Result := GetEditValue(GetElement(x, s));
end;

// --------------------------------------------------------------------
// Returns true/false if a string is in a TStringList
// --------------------------------------------------------------------
function StringExistsInList(const item: String; const list: TStringList): Boolean;
begin
	Result := (list.IndexOf(item) <> -1);
end;

// --------------------------------------------------------------------
// Returns the string for a condition type
// --------------------------------------------------------------------
function ConditionType(const val: String): String;
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
function ConditionOperator(const val: String): String;
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
function LocalizedStringByFormID(const hex: String; const sl: TStringList): String;
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
function StringToHex(const s: String): string;
var
	i: Integer;
begin
  Result := '';
  for i := 1 to Length(s) do
    Result := Result + IntToHex(Ord(s[i]), 2);
end;

// --------------------------------------------------------------------
// Converts a hexadecimal value to a string
// --------------------------------------------------------------------
function HexToString(h: String): String;
var
	i, j: Integer;
	c: Char;
	s: String;
begin
  Result := '';
  for i := 1 to Length(h) div 2 do begin
  	s := Copy(h, (i-1) * 2+1, 2);
    j := StrToInt('$' + s);
    c := Chr(j);
    Result := Result+c;
	end;
end;

// --------------------------------------------------------------------
// Reverses a byte array and returns a string
// --------------------------------------------------------------------
function ReverseHex(const s: String): String;
var
	i: Integer;
	slSource, slTarget: TStringList;
begin
	slSource := TStringList.Create;
	slSource.Delimiter := #32;

	slTarget := TStringList.Create;
	slTarget.Delimiter := #32;

	slSource.DelimitedText := s;
	for i := slSource.Count - 1 downto 0 do
		slTarget.Add(slSource[i]);

	Result := TrimSpaces(slTarget.DelimitedText);
end;

// --------------------------------------------------------------------
// Return the number of times a character occurs in a string
// --------------------------------------------------------------------
function NumOfChar(const s: String; const c: Char): Integer;
var
  i: Integer;
begin
	Result := 0;
	for i := 1 to Length(s) do
		if s[i] = c then
			Inc(Result);
end;

// --------------------------------------------------------------------
// Trim spaces from string and return string
// --------------------------------------------------------------------
function TrimSpaces(const str: String): String;
begin
	Result := StringReplace(str, #32, '', [rfReplaceAll, rfIgnoreCase]);
end;

// --------------------------------------------------------------------
// Trim dashes from string and return string
// --------------------------------------------------------------------
function TrimDashes(const str: String): String;
begin
	Result := StringReplace(str, #45, '', [rfReplaceAll, rfIgnoreCase]);
end;

// --------------------------------------------------------------------
// Returns True if the x signature is in the y list of signatures
// --------------------------------------------------------------------
function InStringList(const x, y: String): Boolean;
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
function HasString(const needle, haystack: String; const caseSensitive: Boolean): Boolean;
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
function GetElement(const x: IInterface; const s: String): IInterface;
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
function OverrideMaster(const x: IInterface; i: integer): IInterface;
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
function SmallNameEx(const e: IInterface): string;
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
function SortKeyEx(const e: IInterface): string;
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
function LoadFromCsv(const autoSort, allowDuplicates, useDelimiter: Boolean; const del: String = '='): TStringList;
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
		if del = '' then
			lsLines.NameValueSeparator := #44
		else
			lsLines.NameValueSeparator := del;

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
function AddGroupBySignature(const f: IwbFile; const s: String): IInterface;
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
function AddNewRecordToGroup(const g: IInterface; const s: String): IInterface;
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
function AddElementByString(const r: IInterface; const s: String): IInterface;
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
// Returns an element by string, or adds the element if needed
// --------------------------------------------------------------------
function AssignElementByString(const r: IInterface; const s: String): IInterface;
var
	e: IInterface;
begin
	e := GetElement(r, s);
	if Assigned(e) then
		Result := ElementAssign(e, HighInteger, nil, true)
	else
		AddMessage('Failed to assign element to: ' + SmallNameEx(r));
end;

// --------------------------------------------------------------------
// Adds a form to a formlist
// --------------------------------------------------------------------
procedure AddRecordToFormList(const f, r: IInterface);
var
	l: IInterface;
begin
	l := ElementAssign(f, HighInteger, nil, true);
	SetEditValue(l, IntToHex(FixedFormID(r), 8));
end;

// --------------------------------------------------------------------
// Copies a record as an override to a file
// --------------------------------------------------------------------
procedure AddOverrideToFile(const targetFile: IwbFile; const rec: IInterface);
var
	ovr, ovr_edid, ovr_full: IInterface;
begin
	ovr := wbCopyElementToFile(rec, targetFile, false, true);
	{EDID} ovr_edid := AddElementByString(ovr, 'EDID');
	{FULL} ovr_full := AddElementByString(ovr, 'FULL');
	SetEditValue(ovr_full, GetEditValue(ovr_edid));
	// TODO: string fuckery shouldn't be part of the proc, but what the fuck ever
end;

// --------------------------------------------------------------------
// Int to Bin
// --------------------------------------------------------------------
function IntToBin(value: LongInt; size: Integer): String;
var
	i: Integer;
begin
	Result := '';
	for i := size - 1 downto 0 do begin
		if value and (1 shl i) <> 0 then begin
			Result := Result + '1';
		end else begin
			Result := Result + '0';
		end;
	end;
end;

// --------------------------------------------------------------------
// Bin to Int
// --------------------------------------------------------------------
function BinToInt(value: String): LongInt;
var
	i, Size: Integer;
begin
	Result := 0;
	Size := Length (Value);
	for i := Size downto 1 do begin
		if Copy (Value, i, 1) = '1' then
			Result := Result + (1 shl (Size-i));
	end;
end;

// --------------------------------------------------------------------
// Binary representation of float to integer
// --------------------------------------------------------------------
function FloatBinToInt(value: String): Real;
var
	i, Size: Integer;
begin
	Result := 0;
	Size := Length(Value);
	for i := Size downto 1 do begin
		if Copy(Value, i, 1) = '1' then
			Result := Result + 1 / (1 shl i);
	end;
end;

// --------------------------------------------------------------------
// Hex to Bin
// --------------------------------------------------------------------
function HexToBin(h: string): String;
var
	box: Array [0..15] of String;
	i: Integer;
begin
	box[0] := '0000';
	box[1] := '0001';
	box[2] := '0010';
	box[3] := '0011';
	box[4] := '0100';
	box[5] := '0101';
	box[6] := '0110';
	box[7] := '0111';
	box[8] := '1000';
	box[9] := '1001';
	box[10] := '1010';
	box[11] := '1011';
	box[12] := '1100';
	box[13] := '1101';
	box[14] := '1110';
	box[15] := '1111';

	for i := Length(h) downto 1 do
		Result := box[StrToInt('$' + h[i])] + Result;
end;

// --------------------------------------------------------------------
// Hex to Float
// --------------------------------------------------------------------
function HexToFloat(s: String): Real;
var
	b, t: String;
	e: Integer;
	f: Real;
begin
	b := HexToBin(s);
	t := Copy(b, 2,8);
	e := BinToInt(t) - 127;
	t := Copy(b, 10, 23);
	f := 1 + FloatBinToInt(t);
	if (Copy(b, 1,1) = '0') then
		Result :=  Power(2, e) * f
	else
		Result := -Power(2, e) * f;
end;

// --------------------------------------------------------------------
// zilav's hex array to string function - thanks!
// Note: This is safer to use than HexToString()
// --------------------------------------------------------------------
function HexArrayToStr(s: string): string;
var
  i: integer;
  c: char;
  hex: string;
begin
  Result := '';
  i := 1;
  while i < Length(s) do begin
    if s <> ' ' then begin
      c := Chr(StrToInt('$' + Copy(s, i, 2)));
      if c = #0 then
        exit;
      Result := Result + c;
      i := i + 3;
    end else
      i := i + 1;
  end;
end;

end.