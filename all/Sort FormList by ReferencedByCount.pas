unit UserScript;

var
	i: Integer;
	sortedList, unsortedList: TStringList;

// function ripped from mator's library

function HexFormID(e: IInterface): String;
var
  s: String;
begin
  s := GetElementEditValues(e, 'Record Header\FormID');

  if SameText(Signature(e), '') then
    Result := '00000000'
  else
    Result := Copy(s, Pos('[' + Signature(e) + ':', s) + Length(Signature(e)) + 2, 8);
end;

function Initialize: Integer;
begin
	sortedList := TStringList.Create;
	sortedList.Sorted := True;

	unsortedList := TStringList.Create;
	unsortedList.Sorted := False;
end;

function Process(e: IInterface): Integer;
var
	kParent, kChild, kReference: IInterface;
	s: String;
begin
	if Signature(e) <> 'FLST' then
		exit;

	kParent := ElementByName(e, 'FormIDs');

	// create a delimited list of ref counts and Form IDs by iterating through
	// lnams, counting refs, and sorting by number of refs.
	// the list is sorted automatically.

	for i := 0 to ElementCount(kParent) - 1 do begin
		kChild := ElementByIndex(kParent, i);
		kReference := LinksTo(kChild);

		// call to format() pads ref count with up to 5 leading zeroes
		// so we can sort numbers as strings correctly
		s := Format('%.*d', [5, ReferencedByCount(kReference)]) + ',' + HexFormID(kReference);

		sortedList.Add(s);
	end;

	// create a new explicitly unsorted list of Form IDs by removing padded ref
	// counts from all items in the previous list.

	for i := sortedList.Count - 1 downto 0 do begin
		s := sortedList[i];

		// remove chars at indices 1 through 6 (padded number + comma)
		Delete(s, 1, 6);

		unsortedList.Add(s);
	end;

	// iterate through lnams again, from the bottom up, overwriting each lnam
	// with the values from the unsorted list. we go bottom up because the
	// original list is sorted in ascending order, but we want descending order
	// for the formlist, so that the most common item is first.

	kParent := ElementByName(e, 'FormIDs');

	for i := ElementCount(kParent) - 1 downto 0 do begin
		kChild := ElementByIndex(kParent, i);

		SetEditValue(kChild, unsortedList[i]);
	end;

	sortedList.Clear;
	unsortedList.Clear;
end;

function Finalize: Integer;
begin
	sortedList.Free;
	unsortedList.Free;
end;

end.
