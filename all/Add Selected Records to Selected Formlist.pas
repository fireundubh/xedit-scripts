unit UserScript;

// globally scoped variables
var
	formlistCount: Integer;
	kFormlist: IInterface;
	lsForms, lsItems: TStringList;

function Initialize: Integer;
begin
	// create string lists
	lsForms := TStringList.Create;
	lsItems := TStringList.Create;

	// initialize selected FLST record count
	formlistCount := 0;
end;

function Process(e: IInterface): Integer;
begin
	// process loops through selected records
	// - when FLST found, make record globally referenceable
	// - when all other records found, add Form ID string to string list
	// - if more than one FLST record was found, throw exception
	if Signature(e) = 'FLST' then begin
		kFormlist := e;
		Inc(formlistCount);
		AssertEqual(formlistCount, 1, '[FATAL] Cannot have more than one FLST record selected');
	end else
		lsForms.Add(IntToHex(FormID(e), 8));
end;

function Finalize: Integer;
var
	i: Integer;
	kParent, kChild, kRecord: IInterface;
begin	
	AssertNotEqual(formlistCount, 0, '[FATAL] Cannot populate formlist because no FLST record was selected');
	
	AssertNotEqual(lsForms.Count, 0, '[FATAL] Cannot populate formlist because no records were selected');

	try
		// return 'FormIDs' element, or create the 'FormIDs' element if needed
		kParent := ElementByPath(kFormlist, 'FormIDs');
		if not Assigned(kParent) then
			kParent := Add(kFormlist, 'FormIDs', false);

		// loop through FLST and initialize temporary list of FormID strings
		// we'll use this list to prevent duplicates from being added
		for i := 0 to Pred(ElementCount(kParent)) do begin
			kChild := ElementByIndex(kParent, i);

			// do not add null items
			if pos('NULL', GetEditValue(kChild)) > 0 then
				continue;

			lsItems.Add(IntToHex(FormID(LinksTo(kChild)), 8));
		end;

		// loop through selected records
		for i := 0 to Pred(lsForms.Count) do begin
			// do not add duplicates
			if lsItems.IndexOf(lsForms[i]) > -1 then
				continue;

			// assign item element and set value to FormID string
			kChild := ElementAssign(kParent, HighInteger, nil, True);
			SetEditValue(kChild, lsForms[i]);

			// retrieve the added record so we can print the log message
			kRecord := RecordByFormID(GetFile(kParent), StrToInt('$' + lsForms[i]), false);
			AddMessage('[INFO] Added item to formlist: [' + Signature(kRecord) + ':' + lsForms[i] + '] ' + EditorID(kRecord));
		end;

		// reinitialize changed FormIDs element
		kParent := ElementByPath(kFormlist, 'FormIDs');

		// loop through formlist and remove NULL items
		for i := 0 to Pred(ElementCount(kParent)) do begin
			kChild := ElementByIndex(kParent, i);
			if pos('NULL', GetEditValue(kChild)) > 0 then
				Remove(kChild);
		end;
	except
		on e : Exception do
			raise Exception.Create('[FATAL] Could not populate formlist because: ' + e.Message);
	end;
end;

// unit test procedures
procedure AssertEqual(a: Variant; b: Variant; s: String = 'FAIL');
begin
	if a <> b then
		raise Exception.Create(s);
end;

procedure AssertNotEqual(a: Variant; b: Variant; s: String = 'FAIL');
begin
	if a = b then
		raise Exception.Create(s);
end;

end.
