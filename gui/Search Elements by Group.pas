{
	Search Elements by Group
	Hotkey: Ctrl+G
}

unit UserScript;

var
	sGroupSignature, sQueryNeedle, sQueryPath: String;
	bCaseSensitive: Boolean;

	// form variables
	SearchForm: TForm;
	lblGroupSignature: TLabel;
	lblQueryPath: TLabel;
	lblQueryNeedle: TLabel;
	edGroupSignature: TEdit;
	edQueryPath: TEdit;
	edQueryNeedle: TEdit;
	btnSearch: TButton;
	btnCancel: TButton;

function Initialize: Integer;
begin
	ShowSearchForm;

	if Length(edGroupSignature.Text) < 4 then
		SearchForm.ModalResult := mrCancel;

	if Length(edQueryPath.Text) = 0 then
		SearchForm.ModalResult := mrCancel;

	if Length(edQueryNeedle.Text) = 0 then
		SearchForm.ModalResult := mrCancel;

	if SearchForm.ModalResult = mrCancel then
		exit;

	sGroupSignature := UpperCase(edGroupSignature.Text);
	sQueryPath := edQueryPath.Text;
	sQueryNeedle := edQueryNeedle.Text;

	bCaseSensitive := CompareStr(sQueryNeedle, LowerCase(sQueryNeedle)) <> 0;
end;

function Finalize: Integer;
var
	slResults: TStringList;
	f: IwbFile;
	g: IwbGroupRecord;
	i, j: Integer;
begin
	if SearchForm.ModalResult = mrCancel then begin
		SearchForm.Free;
		exit;
	end;

	SearchForm.Free;

	slResults := TStringList.Create;
	slResults.Sorted := True;
	slResults.Duplicates := dupIgnore;

	for i := 0 to 9999 do begin
		try
			f := FileByIndex(i);
		except
			break;
		end;

		g := GroupBySignature(f, sGroupSignature);
		if not Assigned(g) then
			continue;

		for j := 0 to Pred(ElementCount(g)) do
			FindMatch(ElementByIndex(g, j), sQueryPath, sQueryNeedle, slResults);
	end;

	slResults.Sort();
	AddMessage(slResults.Text);
	slResults.Free;
end;

procedure FindMatch(aRecord: IInterface; aQueryPath: string; aQueryNeedle: string; aResults: TStringList);
var
	kQueryPath: IInterface;
	sFormID, sQueryHaystack: String;
	bMatched: Boolean;
begin
	kQueryPath := GetElement(aRecord, aQueryPath);
	if not Assigned(kQueryPath) then
		exit;

	sQueryHaystack := GetEditValue(kQueryPath);
	if sQueryHaystack = '' then
		exit;

	sFormID := GetEditValue(GetElement(aRecord, 'Record Header\FormID'));

	if bCaseSensitive then
		bMatched := ContainsStr(sQueryHaystack, aQueryNeedle)
	else
		bMatched := ContainsText(sQueryHaystack, aQueryNeedle);

	if bMatched then
		aResults.Add(GetFileName(GetFile(aRecord)) + #9 + sFormID);
end;

function GetElement(const aElement: IInterface; const aPath: String): IInterface;
begin
	if Pos('[', aPath) > 0 then
		Result := ElementByIP(aElement, aPath)
	else if Pos('\', aPath) > 0 then
		Result := ElementByPath(aElement, aPath)
	else if CompareStr(aPath, Uppercase(aPath)) = 0 then
		Result := ElementBySignature(aElement, aPath)
	else
		Result := ElementByName(aElement, aPath);
end;

function ElementByIP(aElement: IInterface; aIndexedPath: String): IInterface;
var
	i, index, startPos: integer;
	path: TStringList;
begin
	aIndexedPath := StringReplace(aIndexedPath, '/', '\', [rfReplaceAll]);

	path := TStringList.Create;
	path.Delimiter := '\';
	path.StrictDelimiter := true;
	path.DelimitedText := aIndexedPath;

	for i := 0 to Pred(path.count) do begin
		startPos := Pos('[', path[i]);

		if not (startPos > 0) then begin
			aElement := ElementByPath(aElement, path[i]);
			continue;
		end;

		index := StrToInt(MidStr(path[i], startPos+1, Pos(']', path[i])-2));

		aElement := ElementByIndex(aElement, index);
	end;

	Result := aElement;
end;

procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    TForm(Sender).ModalResult := mrCancel
  else if Key = VK_RETURN then
  	TForm(Sender).ModalResult := mrOk;
end;

procedure ShowSearchForm;
var
	scaleFactor: Double;
begin
	SearchForm := TForm.Create(nil);

	scaleFactor := Screen.PixelsPerInch / 96;

	lblGroupSignature := TLabel.Create(SearchForm);
	lblQueryPath := TLabel.Create(SearchForm);
	lblQueryNeedle := TLabel.Create(SearchForm);
	edGroupSignature := TEdit.Create(SearchForm);
	edQueryPath := TEdit.Create(SearchForm);
	edQueryNeedle := TEdit.Create(SearchForm);
	btnSearch := TButton.Create(SearchForm);
	btnCancel := TButton.Create(SearchForm);

	SearchForm.Name := 'SearchForm';
	SearchForm.BorderStyle := bsDialog;
	SearchForm.Caption := 'Search Elements by Group';
	SearchForm.ClientHeight := 138 * scaleFactor;
	SearchForm.ClientWidth := 280 * scaleFactor;
	SearchForm.Color := clBtnFace;
	SearchForm.Position := poScreenCenter;
	SearchForm.KeyPreview := True;
	SearchForm.OnKeyDown := FormKeyDown;

	lblGroupSignature.Name := 'lblGroupSignature';
	lblGroupSignature.Parent := SearchForm;
	lblGroupSignature.Left := 8 * scaleFactor;
	lblGroupSignature.Top := 8 * scaleFactor;
	lblGroupSignature.Width := 78 * scaleFactor;
	lblGroupSignature.Height := 13 * scaleFactor;
	lblGroupSignature.Alignment := taRightJustify;
	lblGroupSignature.Caption := 'Group Signature';

	lblQueryPath.Name := 'lblQueryPath';
	lblQueryPath.Parent := SearchForm;
	lblQueryPath.Left := 31 * scaleFactor;
	lblQueryPath.Top := 40 * scaleFactor;
	lblQueryPath.Width := 55 * scaleFactor;
	lblQueryPath.Height := 13 * scaleFactor;
	lblQueryPath.Alignment := taRightJustify;
	lblQueryPath.Caption := 'Query Path';

	lblQueryNeedle.Name := 'lblQueryNeedle';
	lblQueryNeedle.Parent := SearchForm;
	lblQueryNeedle.Left := 21 * scaleFactor;
	lblQueryNeedle.Top := 72 * scaleFactor;
	lblQueryNeedle.Width := 65 * scaleFactor;
	lblQueryNeedle.Height := 13 * scaleFactor;
	lblQueryNeedle.Alignment := taRightJustify;
	lblQueryNeedle.Caption := 'Search Terms';

	edGroupSignature.Name := 'edGroupSignature';
	edGroupSignature.Parent := SearchForm;
	edGroupSignature.Left := 92 * scaleFactor;
	edGroupSignature.Top := 5 * scaleFactor;
	edGroupSignature.Width := 180 * scaleFactor;
	edGroupSignature.Height := 21 * scaleFactor;
	edGroupSignature.MaxLength := 4;
	edGroupSignature.TabOrder := 0;
	edGroupSignature.Text := 'ARMO';

	edQueryPath.Name := 'edQueryPath';
	edQueryPath.Parent := SearchForm;
	edQueryPath.Left := 92 * scaleFactor;
	edQueryPath.Top := 37 * scaleFactor;
	edQueryPath.Width := 180 * scaleFactor;
	edQueryPath.Height := 21 * scaleFactor;
	edQueryPath.TabOrder := 1;
	edQueryPath.Text := 'FULL';

	edQueryNeedle.Name := 'edQueryNeedle';
	edQueryNeedle.Parent := SearchForm;
	edQueryNeedle.Left := 92 * scaleFactor;
	edQueryNeedle.Top := 69 * scaleFactor;
	edQueryNeedle.Width := 180 * scaleFactor;
	edQueryNeedle.Height := 21 * scaleFactor;
	edQueryNeedle.TabOrder := 2;
	edQueryNeedle.Text := 'leather';

	btnSearch.Name := 'btnSearch';
	btnSearch.Parent := SearchForm;
	btnSearch.Left := 197 * scaleFactor;
	btnSearch.Top := 104 * scaleFactor;
	btnSearch.Width := 75 * scaleFactor;
	btnSearch.Height := 25 * scaleFactor;
	btnSearch.Caption := 'Search';
	btnSearch.TabOrder := 3;
	btnSearch.ModalResult := mrOk;

	btnCancel.Name := 'btnCancel';
	btnCancel.Parent := SearchForm;
	btnCancel.Left := 116 * scaleFactor;
	btnCancel.Top := 105 * scaleFactor;
	btnCancel.Width := 75 * scaleFactor;
	btnCancel.Height := 25 * scaleFactor;
	btnCancel.Caption := 'Cancel';
	btnCancel.TabOrder := 4;
	btnCancel.ModalResult := mrCancel;

	SearchForm.ShowModal;
end;

end.
