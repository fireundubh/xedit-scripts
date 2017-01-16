unit UserScript;

var 
	lsFlags: TStringList;
	resetLevel: Integer;

function Initialize: Integer;
begin
	// default level to which leveled list entries are reset 
	resetLevel := 1;
	
	// you can force the script to apply to only records with these flags
	// by uncommenting the lsFlags.Add() lines, or adding your own
	lsFlags := TStringList.Create;
	
	//lsFlags.Add('Calculate from all levels <= player''s level');
	//lsFlags.Add('Calculate for each item in count');
end;

function Process(e: IInterface): Integer;
var
	i, matches: Integer;
	flag, flags, entry, entries, level, reference: IInterface;
begin
	// if there are no Leveled List Entries, exit
	entries := ElementByName(e, 'Leveled List Entries');
	if not Assigned(entries) then
		exit;

	if lsFlags.Count > 0 then begin
		// if there are no Flags, exit
		flags := ElementBySignature(e, 'LVLF');
		if not Assigned(flags) then 
			exit;
			
		if ElementCount(flags) <= 0 then
			exit;
	
		// if existing flags match lsFlags
		matches := 0;
		for i := 0 to ElementCount(flags) - 1 do begin
			flag := ElementByIndex(flags, i);
			if lsFlags.IndexOf(Name(flag)) <> -1 then 
				matches := matches + 1;
		end;
		
		if matches <> lsFlags.Count then
			exit;
	end;

	// iterate through leveled list entries and reset entry levels to resetLevel
	for i := 0 to ElementCount(entries) - 1 do begin
		entry := ElementBySignature(ElementByIndex(entries, i), 'LVLO');
		reference := ElementByName(entry, 'Reference');
		level := ElementByName(entry, 'Level');

		if GetNativeValue(level) <> resetLevel then begin
			AddMessage('Resetting level for: ' + GetEditValue(reference));
			SetNativeValue(level, resetLevel);
		end;
	end;
end;

function Finalize: Integer;
begin
	lsFlags.Free; 
end;

end.
