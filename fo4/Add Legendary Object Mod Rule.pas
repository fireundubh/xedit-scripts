unit UserScript;
uses dubhFunctions;

var
	esp, esm: IwbFile;
	csv, masters: TStringList;

function Initialize: Integer;
var
	i: Integer;
	ovr, qst, vmad: IInterface;
begin
	// create list of masters
  masters := TStringList.Create;
  masters.Duplicates := dupIgnore;
  masters.Add('Fallout4.esm');

	// prompt user to select a target file
	esp := FileSelect('Select the target file:');
	if not Assigned(esp) then
		Result := 1;

	// add masters in case user selects 'create new file'
  AddMastersToFile(esp, masters, false);

	// prompt user to load rules from csv
	csv := TStringList.Create;
	csv := LoadFromCsv(false, false, true, ';');

	// get the LegendaryItemQuest record from the master file
	esm := FileByName('Fallout4.esm');
	qst := RecordByFormID(esm, $001CCDA5, true);

	// copy quest as override to target file
	qst := wbCopyElementToFile(qst, esp, false, true);

	// NOTE: xEdit gives no fucks if the QUST override already exists, and
	// since there's no check for duplicates, you could end up with duplicate
	// rules if you're not careful. Best thing to do is fill up your rules
	// document with all of the rules you need, delete the LegendaryItemQuest
	// record if it exists, and then run the script. Repeat each time you
	// want to add more rules to the quest.

	// select the vmad element
	vmad := ElementBySignature(qst, 'VMAD');

	// add rules from csv
	for i := 0 to csv.Count - 1 do
		AddRule(vmad, csv, i);
end;

function Finalize: Integer;
begin
	csv.Free;
end;

// ----------------------------------------------------------------------------
// Adds a two-member rule (LegendaryObjectMod, AllowedKeywords) to rules array
// NOTE: A three-member rule would require a more complex CSV or XML parser,
// but there's only a few that add DisallowedKeywords as the third member.
// These are special cases, so if you need a special case, use your hands.
// ----------------------------------------------------------------------------
procedure AddRule(el: IInterface; csv: TStringList; idx: Integer);
var
	rules, rule, mbr0, mbr1: IInterface;
begin
	// add masters if missing
	masters.Add(GetFileName(GetFile(RecordByHexFormID(GetTextIn(csv.Names[idx], ':', ']')))));
	masters.Add(GetFileName(GetFile(RecordByHexFormID(GetTextIn(csv.ValueFromIndex[idx], ':', ']')))));
	AddMastersToFile(esp, masters, true);

	// select the rules array
	rules := ElementByIP(el, 'Data\Quest VMAD\Scripts\Script\Properties\[1]\Value\Array of Struct');

	// add a rule
	rule := ElementAssign(rules, HighInteger, nil, true);

	// add first rule member
	mbr0 := ElementAssign(rule, HighInteger, nil, true);

	// add second rule member
	mbr1 := ElementAssign(rule, HighInteger, nil, true);

	// configure first rule member
	seev(mbr0, 'memberName', 'LegendaryObjectMod');
	senv(mbr0, 'Type', 1); // Object
	senv(mbr0, 'Flags', 1); // Edited
	seev(mbr0, 'Value\Object Union\Object v2\FormID', csv.Names[idx]);
	seev(mbr0, 'Value\Object Union\Object v2\Alias', 'None');

	// configure second rule member
	seev(mbr1, 'memberName', 'AllowedKeywords');
	senv(mbr1, 'Type', 1); // Object
	senv(mbr1, 'Flags', 1); // Edited
	seev(mbr1, 'Value\Object Union\Object v2\FormID', csv.ValueFromIndex[idx]);
	seev(mbr1, 'Value\Object Union\Object v2\Alias', 'None');

	// tell me about it
	Log('Added rule: ' + csv.Names[idx] + ';' + csv.ValueFromIndex[idx]);
end;

// ----------------------------------------------------------------------------
// GetTextIn truncates substrings at wrong starting index, so lifted this fx
// from mteFunctions and applied a fix. TODO: Remove when fixed at root.
// ----------------------------------------------------------------------------
function GetTextIn(str: string; open, close: char): string;
var
  i, openIndex: integer;
  bOpen: boolean;
begin
  Result := '';
  bOpen := false;
  for i := 0 to Length(str) do begin
    if not bOpen and (GetChar(str, i) = open) then begin
      openIndex := i;
      bOpen := true;
    end;
    if bOpen and (GetChar(str, i) = close) then begin
      Result := CopyFromTo(str, openIndex + 1, i - 1);
      break;
    end;
  end;
end;

end.
