unit dubhFunctions;

uses mteFunctions;

// --------------------------------------------------------------------
// AddMessage
// --------------------------------------------------------------------
procedure Log(const s: String);
begin
	AddMessage(s);
end;

// --------------------------------------------------------------------
// Return whether a value is null
// --------------------------------------------------------------------
function IsNull(x: Variant): Boolean;
begin
	if (DefTypeString(x) = 'dtInteger') and not CanContainFormIDs(x) then
		Result := (GetNativeValue(x) = 0)
	else if (DefTypeString(x) = 'dtFloat') and not CanContainFormIDs(x) then
		Result := (GetNativeValue(x) = 0.0)
	else if CanContainFormIDs(x) then
		Result := HasString('00000000', GetEditValue(x), False);
end;

// --------------------------------------------------------------------
// Return a hexadecimal form id with signature
// --------------------------------------------------------------------
function ShortFormID(x: IInterface): String;
begin
	Result := '[' + Signature(x) + ':' + HexFormID(x) + ']';
end;

// --------------------------------------------------------------------
// Return whether a number is divisible by another into a whole number
// --------------------------------------------------------------------
function DivisibleBy(x, y: Real): Boolean;
begin
	Result := (x mod y = 0);
end;

// --------------------------------------------------------------------
// Recursively add value of named field to list
// --------------------------------------------------------------------
procedure RecursiveAddToList(e: IInterface; elementName: String; results: TStringList);
var
	i, c: Integer;
	kFile: IwbFile;
	kMainRecord, kChild: IInterface;
	sFile: String;
begin
	kMainRecord := ContainingMainRecord(e);

	c := ElementCount(e);
	if c > 0 then begin
		for i := 0 to Pred(c) do begin
			kChild := ElementByIndex(e, i);
			if ElementCount(kChild) > 0 then
				RecursiveAddToList(kChild, elementName, results)
			else
				if HasString(elementName, Name(kChild), False) then
					results.Add(SmallNameEx(kMainRecord) + #9 + GetEditValue(kChild));
		end;
	end;
end;

// --------------------------------------------------------------------
// Recursively add key-value pairs to list
// --------------------------------------------------------------------
procedure RecursiveAddPairToList(e: IInterface; keyName, valueName: String; results: TStringList);
var
	i, c: Integer;
	kFile: IwbFile;
	kMainRecord, kChild: IInterface;
	sFile, sTempKey: String;
begin
	results.NameValueSeparator := '=';

	kMainRecord := ContainingMainRecord(e);

	sTempKey := '';

	c := ElementCount(e);
	if c > 0 then begin
		for i := 0 to Pred(c) do begin
			kChild := ElementByIndex(e, i);
			if ElementCount(kChild) > 0 then
				RecursiveAddPairToList(kChild, keyName, valueName, results)
			else
				if HasString(keyName, Name(kChild), False) then
					sTempKey := GetEditValue(kChild);
				if HasString(valueName, Name(kChild), False) then begin
					results.CommaText := '"' + sTempKey + '"=' + GetEditValue(kChild);
					sTempKey := '';
				end;
		end;
	end;
end;

// --------------------------------------------------------------------
// Return comma-separated list of hexadecimal form ids
// --------------------------------------------------------------------
function FlagsToHex(e: IInterface): String;
var
  i: Integer;
  ls: TStringList;
begin
	ls := TStringList.Create;
	ls.Add(IntToHex(GetNativeValue(e), 8));
	for i := 0 to ElementCount(e) - 1 do
		ls.Add(IntToHex(GetNativeValue(ElementByIndex(e, i)), 8));
	Result := ls.CommaText;
end;

// --------------------------------------------------------------------
// Return comma-separated list of element names
// --------------------------------------------------------------------
function FlagsToNames(e: IInterface): String;
var
  i: Integer;
  ls: TStringList;
begin
	if not Assigned(e) then
		Result := '';
	ls := TStringList.Create;
	for i := 0 to ElementCount(e) - 1 do
		ls.Add(Name(ElementByIndex(e, i)));
	Result := ls.CommaText;
end;

// --------------------------------------------------------------------
// Return path without labels
// --------------------------------------------------------------------
function BasePath(x: IInterface): String;
var
	sPath: String;
begin
	//sPath := TrimChar(#32, RegExReplace('\s[-]\s[a-zA-Z0-9 ]+', '', Trim(Path(x))));
	sPath := Path(x);
	sPath := Trim(sPath);
	sPath := RegExReplace('\h+[-]\h+[a-zA-Z0-9 ]+', '', sPath);
	sPath := Trim(sPath);
	sPath := RegExReplace('^[a-zA-Z0-9]{4}', '', sPath);
	sPath := Trim(sPath);
	sPath := RegExReplace('^[\\]', '', sPath);
	sPath := Trim(sPath);
	sPath := RegExReplace('\h*[\\]\h+', '\\', sPath);
	sPath := Trim(sPath);
	sPath := RegExReplace('\h+[-]$', '', sPath);
	sPath := Trim(sPath);
	Result := sPath;

	//Result := TrimChar(#32, RegExReplace('\s[-]\s\w+\s', Trim(Path(x)), ''));
	//Result := TrimChar(#32, RegExReplace('\b[a-zA-Z0-9]+\b\K\h[-]$|\b[a-zA-Z0-9]+\b\K\h[-](.*)$|\b[a-zA-Z0-9]+\b\K\h[-]\h\b[a-zA-Z0-9]+\b$', TrimRight(Path(x)), '#'));
end;

// --------------------------------------------------------------------
// A shorter way to replace strings without RegEx
// --------------------------------------------------------------------
function StrReplace(this, that, subj: String): String;
begin
	Result := StringReplace(subj, this, that, [rfReplaceAll, rfIgnoreCase]);
end;

function EscapeSlashes(s: String): String;
begin
	Result := StrReplace('\', '\\', s);
end;

// --------------------------------------------------------------------
// Replace a substring with a RegEx pattern and return the new string
// --------------------------------------------------------------------
function RegExReplace(const ptrn, repl, subj: String): String;
var
	re: TPerlRegEx;
	output: String;
begin
	re := TPerlRegEx.Create;
	try
		re.RegEx := ptrn;
		re.Options := [];
		re.Subject := subj;
		re.Replacement := repl;
		re.ReplaceAll;
		output := re.Subject;
	finally
		re.Free;
		Result := output;
	end;
end;

// --------------------------------------------------------------------
// Return a substring with a RegEx pattern
// --------------------------------------------------------------------
function RegExMatch(ptrn, subj: String): String;
var
	re: TPerlRegEx;
	output: String;
begin
	output := nil;
	re := TPerlRegEx.Create;
	try
		re.RegEx := ptrn;
		re.Options := [];
		re.Subject := subj;
		if re.Match then
			output := re.MatchedText;
	finally
		re.Free;
		Result := output;
	end;
end;

function RegExMatchAll(ptrn, subj: String): TStringList;
var
	re: TPerlRegEx;
	output: TStringList;
	i: Integer;
begin
	output := TStringList.Create;
	re := TPerlRegEx.Create;
	try
		re.RegEx := ptrn;
		re.Options := [];
		re.Subject := subj;
		if re.Match then begin
			output.Add(re.MatchedText);
			repeat
				output.Add(re.MatchedText);
			until not re.MatchAgain;
		end;
	finally
		re.Free;
		Result := output;
	end;
end;

// --------------------------------------------------------------------
// Return whether a string matches a RegEx pattern
// --------------------------------------------------------------------
function RegExMatches(ptrn, subj: String): Boolean;
var
	re: TPerlRegEx;
	output: String;
begin
	output := False;

	re := TPerlRegEx.Create;
	try
		re.RegEx := ptrn;
		re.Options := [];
		re.Subject := subj;
		if re.Match then
			output := re.FoundMatch;
	finally
		re.Free;
		Result := output;
	end;
end;

function UnCamelCase(s: String): String;
var
	output: String;
begin
		output := RegExReplace('(\B[A-Z])', ' $1', s);
		Result := RegExReplace('(\B[0-9]{2})', ' $1', output);
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
	end;

	if not pre then begin
		mdb := '';
		mde := '';
		mdd := ' | ';
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
procedure snv(const x: IInterface; const v: Variant);
begin
	SetNativeValue(x, v);
end;

// --------------------------------------------------------------------
// Lowercases two strings, compares them, and returns value
// --------------------------------------------------------------------
function StringCompare(const this, that: String; cs: Boolean): Integer;
begin
	if not cs then
		Result := CompareStr(Lowercase(this), Lowercase(that));
	else
		Result := CompareStr(this, that);
end;

// --------------------------------------------------------------------
// Returns true/false if element is assigned by element's signature
// --------------------------------------------------------------------
function AssignedBySignature(const x: IInterface; const s: String): Boolean;
begin
	Result := Assigned(GetElement(x, s));
end;

// --------------------------------------------------------------------
// Returns the string for a condition type
// --------------------------------------------------------------------
function GetConditionType(const val: String): String;
var
	operators: String;
	a, b, c, d, e: Boolean;
begin
	if GetChar(val, 1) = '1' then	a := True; // equal to
	if GetChar(val, 2) = '1' then	b := True; // greater than
	if GetChar(val, 3) = '1' then	c := True; // less than
	if GetChar(val, 4) = '1' then d := True; // or
	if val = '00000000' then e := True;
	if e then	Result := 'Not equal to';
	if a and not b and not c and not d then	Result := 'Equal to';
	if b and not a and not c and not d then	Result := 'Greater than';
	if c and not a and not b and not d then	Result := 'Less than';
	if a and b and not c and not d then	Result := 'Greater than or equal to';
	if a and c and not b and not d then	Result := 'Less than or equal to';
	if a and d and not b and not c then	Result := 'Equal to / Or';
	if b and d and not a and not c then	Result := 'Greater than / Or';
	if c and d and not a and not b then	Result := 'Less than / Or';
	if a and b and d and not c then	Result := 'Greater than or equal to / Or';
	if a and c and d and not b then	Result := 'Less than or equal to / Or';
end;

// --------------------------------------------------------------------
// Returns the string of a condition type but with comparison operators
// --------------------------------------------------------------------
function GetConditionOperator(const val: String): String;
var
	a, b, c, d, e: Boolean;
begin
	if GetChar(val, 1) = '1' then a := True; // equal to
	if GetChar(val, 2) = '1' then b := True; // greater than
	if GetChar(val, 3) = '1' then c := True; // less than
	if GetChar(val, 4) = '1' then d := True; // or
	if val = '00000000' then e := True;
	if e then Result := '<>';
	if a and not b and not c and not d then Result := '==';		// 1000
	if b and not a and not c and not d then	Result := '>';		// 0100
	if c and not a and not b and not d then	Result := '<';		// 0010
	if a and b and not c and not d then	Result := '>=';				// 1100
	if a and c and not b and not d then	Result := '<=';				// 1010
	if a and d and not b and not c then	Result := '== / Or';	// 1001
	if not a and b and not c and d then	Result := '> / Or';		// 0101
	if not a and not b and c and d then	Result := '< / Or';		// 0011
	if a and b and d and not c then	Result := '>= / Or';			// 1101
	if a and c and d and not b then	Result := '<= / Or';			// 1011
end;

// --------------------------------------------------------------------
// Returns a localized string as a string from a hexadecimal Form ID
// Note: Init LocalizationGetStringsFromFile() for performance gain
// --------------------------------------------------------------------
function GetLStringByFormID(const hex: String; const sl: TStringList): String;
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
function StrToHex(const s: String): string;
var
	i: Integer;
begin
  Result := '';
  for i := 1 to Length(s) do
    Result := Result + IntToHex(Ord(s[i]), 2);
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

	Result := TrimChar(' ', slTarget.DelimitedText);
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
		if s[i] = c then Inc(Result);
end;

// --------------------------------------------------------------------
// Trim character from string and return string
// --------------------------------------------------------------------
function TrimChar(const c: Char; const s: String): String;
begin
	Result := StringReplace(s, c, '', [rfReplaceAll, rfIgnoreCase]);
end;

// --------------------------------------------------------------------
// Returns True if the x signature is in the y list of signatures
// --------------------------------------------------------------------
function InDelimitedList(const x, y: String; const z: Char): Boolean;
var
	idx: Integer;
	l: TStringList;
begin
	l := TStringList.Create;
	l.Delimiter := z;
	l.DelimitedText := y;
	idx := l.IndexOf(x);
	l.Free;
	Result := (idx > -1);
end;

// --------------------------------------------------------------------
// Returns true/false if a string is in a TStringList
// --------------------------------------------------------------------
function InStringList(const s: String; const l: TStringList): Boolean;
begin
	Result := (l.IndexOf(s) <> -1);
end;

// --------------------------------------------------------------------
// Returns the number of duplicates of a string in a TStringList
// --------------------------------------------------------------------
function CountDuplicatesInTStringList(s: String; l: TStringList): Integer;
var
	i, j: Integer;
begin
	if not InStringList(s, l) then
		Result := 0;

	j := 0;
	for i := 0 to l.Count - 1 do
		if (s = l[i]) then
			j := j + 1;

	Result := j;
end;

// --------------------------------------------------------------------
// Returns true if the needle is in the haystack
// --------------------------------------------------------------------
function HasString(const asNeedle, asHaystack: String; const abCaseSensitive: Boolean): Boolean;
begin
	if abCaseSensitive then
		if Pos(asNeedle, asHaystack) > 0 then
			Result := True
	else
		if Pos(Lowercase(asNeedle), Lowercase(asHaystack)) > 0 then
			Result := True;
end;

// --------------------------------------------------------------------
// Returns any element from a string
// --------------------------------------------------------------------
function GetElement(const x: IInterface; const s: String): IInterface;
begin
	if Length(s) > 0 then begin
		if Pos('[', s) > 0 then
			Result := ElementByIP(x, s)
		else if Pos('\', s) > 0 then
			Result := ElementByPath(x, s)
		else if s = Uppercase(s) then
			Result := ElementBySignature(x, s)
		else
			Result := ElementByName(x, s);
	end;
end;

// --------------------------------------------------------------------
// Returns a master overriding record. Int parameter should be 1 or 2.
// --------------------------------------------------------------------
function OverrideMaster(const x: IInterface; i: Integer): IInterface;
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
// Returns the last overriding record
// --------------------------------------------------------------------
function LastOverride(const x: IInterface): IInterface;
var
	o: IInterface;
begin
	o := Master(x);
	if not Assigned(o) then
		o := MasterOrSelf(x);
	if OverrideCount(o) > 0 then
		o := OverrideByIndex(o, OverrideCount(o) - 1);
	Result := o;
end;

// --------------------------------------------------------------------
// Returns "NAME/EDID [SIG_:00000000]" as a String
// --------------------------------------------------------------------
function SmallNameEx(const e: IInterface): String;
var
	sig: String;
begin
	if Signature(e) <> 'REFR' then sig := 'EDID' else sig := 'NAME';
	Result := Trim(GetElementEditValues(e, sig) + ' [' + Signature(e) + ':' + HexFormID(e) + ']');
end;

// --------------------------------------------------------------------
// Returns the sortkey with handling for .nif paths, and unknown/unused
// 	data. Also uses a better delimiter.
// --------------------------------------------------------------------
function SortKeyEx(const e: IInterface): String;
var
	i: Integer;
	kElement: IInterface;
begin
	Result := Lowercase(GetEditValue(e));

	for i := 0 to ElementCount(e) - 1 do
	begin
		kElement := ElementByIndex(e, i);

		if (Pos('unknown', Lowercase(Name(kElement))) > 0)
		or (Pos('unused', Lowercase(Name(kElement))) > 0) then
			exit;

		if Result <> '' then
			Result := Result + ' ' + SortKeyEx(kElement)
		else
			Result := SortKeyEx(kElement);
	end;
end;

// --------------------------------------------------------------------
// Returns values from a text file as a TStringList
// --------------------------------------------------------------------
function LoadFromCsv(const bSorted, bDuplicates, bDelimited: Boolean; const d: String = ';'): TStringList;
var
	objFile: TOpenDialog;
	lsLines: TStringList;
begin
	lsLines := TStringList.Create;

	if bSorted then
		lsLines.Sorted;

	if bDuplicates then
		lsLines.Duplicates := dupIgnore;

	if bDelimited then
		if d <> '' then
			lsLines.NameValueSeparator := d
		else
			lsLines.NameValueSeparator := #44;

	objFile := TOpenDialog.Create(nil);

	try
		objFile.InitialDir := GetCurrentDir;
		objFile.Options := [ofFileMustExist];
		objFile.Filter := '*.csv';
		objFile.FilterIndex := 1;
		if objFile.Execute then
			lsLines.LoadFromFile(objFile.FileName);
	finally
		objFile.Free;
	end;

	Result := lsLines;
end;

// --------------------------------------------------------------------
// Returns values from a text file as a TStringList
// --------------------------------------------------------------------
function LoadFromDelimitedList(const sDelimiter: String = ';'): TStringList;
var
	objFile: TOpenDialog;
	lsLines: TStringList;
begin
	lsLines := TStringList.Create;
	lsLines.Delimiter := sDelimiter;

	objFile := TOpenDialog.Create(nil);

	try
		objFile.InitialDir := GetCurrentDir;
		objFile.Options := [ofFileMustExist];
		objFile.Filter := '*.csv';
		objFile.FilterIndex := 1;
		if objFile.Execute then
			lsLines.LoadFromFile(objFile.FileName);
	finally
		objFile.Free;
	end;

	Result := lsLines;
end;

// --------------------------------------------------------------------
// Converts a Formlist or Leveled List to a TStringList
// --------------------------------------------------------------------
function ListObjectToTStringList(e: IInterface): TStringList;
var
	aParent, aChild: IINterface;
	lsResults: TStringList;
	i: Integer;
begin
	lsResults := TStringList.Create;
	lsResults.Sorted := False;
	lsResults.Duplicates := dupAccept;

	aParent := ElementByName(e, 'Leveled List Entries');
	if not Assigned(aParent) then
		aParent := ElementByName(e, 'FormIDs');

	if Name(aParent) = 'Leveled List Entries' then begin
		for i := 0 to ElementCount(aParent) - 1 do begin
			aChild := ElementBySignature(ElementByIndex(aParent, i), 'LVLO');
			lsResults.Add(GetEditValue(ElementByIndex(aChild, 0)) + ',' + HexFormID(LinksTo(ElementByIndex(aChild, 2))) + ',' + GetEditValue(ElementByIndex(aChild, 3)));
		end;
	end else begin
		for i := 0 to ElementCount(aParent) - 1 do begin
			aChild := ElementByIndex(aParent, i);
			lsResults.Add(GetEditValue(aChild));
		end;
	end;

	Result := lsResults;
end;

// --------------------------------------------------------------------
// Returns a group by signature, or adds the group if needed
// --------------------------------------------------------------------
function AddGroupBySignature(const f: IwbFile; const s: String): IInterface;
begin
	Result := GroupBySignature(f, s);
	if not Assigned(Result) then
		Result := Add(f, s, True);
end;

// --------------------------------------------------------------------
// Adds a new record to a group
// --------------------------------------------------------------------
function AddNewRecordToGroup(const g: IInterface; const s: String): IInterface;
begin
	Result := Add(g, s, True);
	if not Assigned(Result) then
		Result := Add(g, s, True); // tries twice because
end;

// --------------------------------------------------------------------
// Returns an element by string, or adds the element if needed
// --------------------------------------------------------------------
function AddElementByString(const r: IInterface; const s: String): IInterface;
begin
	Result := GetElement(r, s);
	if not Assigned(Result) then
		Result := Add(r, s, True);
end;

// --------------------------------------------------------------------
// Returns an element by string, or adds the element if needed
// --------------------------------------------------------------------
function AssignElementByString(const r: IInterface; const s: String): IInterface;
begin
	Result := ElementAssign(GetElement(r, s), HighInteger, nil, False);
end;

function AppendElementByString(const r: IInterface; const s: String): IInterface;
begin
	Result := ElementAssign(GetElement(r, s), LowInteger, nil, False);
end;

// --------------------------------------------------------------------
// Adds a form to a formlist
// --------------------------------------------------------------------
procedure AddRecordToFormList(const f, r: IInterface);
var
	l: IInterface;
begin
	l := ElementAssign(f, HighInteger, nil, True);
	SetEditValue(l, IntToHex(FixedFormID(r), 8));
end;

// --------------------------------------------------------------------
// Int to Bin
// --------------------------------------------------------------------
function IntToBin(value: LongInt; sz: Integer): String;
var
	i: Integer;
begin
	Result := '';
	for i := sz - 1 downto 0 do
		if value and (1 shl i) <> 0 then
			Result := Result + '1';
		else
			Result := Result + '0';
end;

// --------------------------------------------------------------------
// Bin to Int
// --------------------------------------------------------------------
function BinToInt(value: String): LongInt;
var
	i, sz: Integer;
begin
	Result := 0;
	sz := Length(value);
	for i := sz downto 1 do
		if Copy(value, i, 1) = '1' then
			Result := Result + (1 shl (sz - i));
end;

// --------------------------------------------------------------------
// Binary representation of float to integer
// --------------------------------------------------------------------
function FloatBinToInt(value: String): Real;
var
	i, sz: Integer;
begin
	Result := 0;
	sz := Length(Value);
	for i := sz downto 1 do
		if Copy(value, i, 1) = '1' then
			Result := Result + 1 / (1 shl i);
end;

// --------------------------------------------------------------------
// Hex to Bin
// --------------------------------------------------------------------
function HexToBin(h: String): String;
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
	e: Integer;
	f: Real;
	b: String;
begin
	b := HexToBin(s);
	e := BinToInt(Copy(b, 2,8)) - 127;
	f := 1 + FloatBinToInt(Copy(b, 10, 23));
	if Copy(b, 1,1) <> '0' then
		Result := -Power(2, e) * f
	else
		Result := Power(2, e) * f;
end;

// --------------------------------------------------------------------
// zilav's hex array to string function
// --------------------------------------------------------------------
function HexToString(s: String): String;
var
  c: Char;
  i: Integer;
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
