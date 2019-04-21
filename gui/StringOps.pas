{
	StringOps
	Hotkey: Ctrl+R
}

unit UserScript;

var
	frmSearch: TForm;
	sElementPath: String;
	bProperCase: Boolean;
	bSentenceCase: Boolean;
	bModify: Boolean;
	sQuerySearch: String;
	sQueryReplace: String;
	sQueryPrefix: String;
	sQuerySuffix: String;
	bCaseSensitive: Boolean;
	frmModalResult: TModalResult;
	lblModifyOptions: TLabel;
	edElementPath: TEdit;
	edQuerySearch: TEdit;
	edQueryReplace: TEdit;
	rbProperCase: TRadioButton;
	rbSentenceCase: TRadioButton;
	rbModify: TRadioButton;
	edQueryPrefix: TEdit;
	edQuerySuffix: TEdit;
	btnExecute: TButton;
	btnClose: TButton;

function Initialize: Integer;
begin
	ShowSearchForm;

	if frmSearch.ModalResult = mrCancel then
		exit;
end;

function Process(e: IInterface): Integer;
var
	x: IInterface;
	i: Integer;
	sRecordID: String;
	sElementPathTemplate: String;
begin
	if frmSearch.ModalResult = mrCancel then
		exit;

	sRecordID := GetEditValue(GetElement(e, 'Record Header\FormID'));

	if ContainsText(sElementPath, '[*]') then begin
		sElementPathTemplate := sElementPath;

		// set up first child element
		sElementPath := StringReplace(sElementPathTemplate, '[*]', '[0]', [rfReplaceAll]);
		x := GetElement(e, sElementPath);

		if not Assigned(x) or not IsEditable(x) then
			exit;

		i := 0;
		repeat
			ExecuteExecuteExecute(x);
			AddMessage('Processed: ' + sRecordID + ' @ ' + sElementPath);

			// set up next child element
			Inc(i);
			sElementPath := StringReplace(sElementPathTemplate, '[*]', '[' + IntToStr(i) + ']', [rfReplaceAll]);
			x := GetElement(e, sElementPath);
		until not Assigned(x) or not IsEditable(x);

		sElementPath := sElementPathTemplate;
		exit;
	end;

	x := GetElement(e, sElementPath);

	if not Assigned(x) or not IsEditable(x) then
		exit;

	ExecuteExecuteExecute(x);
	AddMessage('Processed: ' + sRecordID);
end;

function Finalize: Integer;
begin
	frmSearch.Free;
end;

function ExecuteExecuteExecute(aElement: IInterface): Integer;
var
	sHaystack: String;
	bMatchFound: Boolean;
	bAddPrefix: Boolean;
	bAddSuffix: Boolean;
begin
	sHaystack := GetEditValue(aElement);

	if not (Length(sHaystack) > 0) then
		exit;

	if bProperCase then begin
		SetEditValue(aElement, ProperCase(sHaystack));
		exit;
	end;

	if bSentenceCase then begin
		SetEditValue(aElement, SentenceCase(sHaystack));
		exit;
	end;

	if not bModify then
		exit;

	if Length(sQuerySearch) > 0 then begin
		if bCaseSensitive then
			bMatchFound := ContainsStr(sHaystack, sQuerySearch)
		else
			bMatchFound := ContainsText(sHaystack, sQuerySearch);

		if bMatchFound then begin
			sHaystack := GetEditValue(aElement);

			if bCaseSensitive then
				sHaystack := StringReplace(sHaystack, sQuerySearch, sQueryReplace, [rfReplaceAll])
			else
				sHaystack := StringReplace(sHaystack, sQuerySearch, sQueryReplace, [rfReplaceAll, rfIgnoreCase]);

			// handle special tokens
			sHaystack := StringReplace(sHaystack, '#13', #13, [rfReplaceAll]);
			sHaystack := StringReplace(sHaystack, '#10', #10, [rfReplaceAll]);
			sHaystack := StringReplace(sHaystack, '#9', #9, [rfReplaceAll]);

			SetEditValue(aElement, sHaystack);
		end;
	end;

	bAddPrefix := Length(sQueryPrefix) > 0;
	bAddSuffix := Length(sQuerySuffix) > 0;

	if bAddPrefix or bAddSuffix then begin
		// ensure we're working with latest haystack
		sHaystack := GetEditValue(aElement);

		// we don't care about prefix/suffix case sensitivity
		if bAddPrefix then
			if not StartsText(sQueryPrefix, sHayStack) then
				sHaystack := Insert(sQueryPrefix, sHaystack, 0);

		if bAddSuffix then
			if not EndsText(sQuerySuffix, sHaystack) then
				sHaystack := Insert(sQuerySuffix, sHaystack, Length(sHaystack) + 1);

		SetEditValue(aElement, sHaystack);
	end;
end;

function GetChar(const aText: String; aPosition: Integer): Char;
begin
	Result := Copy(aText, aPosition, 1);
end;

procedure SetChar(var aText: String; aPosition: Integer; aChar: Char);
var
	sHead, sTail: String;
begin
	sHead := Copy(aText, 1, aPosition - 1);
	sTail := Copy(aText, aPosition + 1, Length(aText));
	aText := sHead + aChar + sTail;
end;

function InStringList(const aText: String; const aList: TStringList): Boolean;
begin
	Result := (aList.IndexOf(aText) <> -1);
end;

function SentenceCase(const aText: String): String;
begin
	Result := '';
	if aText <> '' then
		Result := UpCase(aText[1]) + Copy(LowerCase(aText), 2, Length(aText));
end;

function ProperCase(const aText: String): String;
var
	slResults: TStringList;
	slLowerCase: TStringList;
	slUpperCase: TStringList;
	i, dp: Integer;
	rs, tmp: String;
begin
	slLowerCase := TStringList.Create;
	slUpperCase := TStringList.Create;

	slLowerCase.CommaText := ' a , an , the , and , but , or , nor , at , by , for , from , in , into , of , off , on , onto , out , over , up , with , to , as ';
	slUpperCase.CommaText := ' fx , npc ';

	slResults := TStringList.Create;
	slResults.Delimiter := ' ';
	slResults.DelimitedText := aText;

	for i := 0 to Pred(slResults.Count) do begin
		tmp := slResults[i];

		tmp := SentenceCase(tmp);

		if InStringList(tmp, slLowerCase) and i <> 0 then
			tmp := LowerCase(tmp);

		if InStringList(tmp, slUpperCase) and i <> 0 then
			tmp := UpperCase(tmp);

		if GetChar(tmp, 1) = '(' then
			SetChar(tmp, 2, UpperCase(GetChar(tmp, 2)));

		if GetChar(tmp, 1) = '<' then
			SetChar(tmp, 2, UpperCase(GetChar(tmp, 2)));

		if GetChar(tmp, 1) = '=' then
			SetChar(tmp, 2, UpperCase(GetChar(tmp, 2)));

		if Pos('-', tmp) > 0 then begin
			dp := Pos('-', tmp);
			if GetChar(tmp, dp + 1) <> ' ' then
				SetChar(tmp, dp + 1, UpperCase(GetChar(tmp, dp + 1)));
		end;

		slResults[i] := tmp;
	end;

	Result := slResults.DelimitedText;

	slResults.Free;
	slLowerCase.Free;
	slUpperCase.Free;
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
	i, index, startPos: Integer;
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
		btnClose.Click
	else if Key = VK_RETURN then
		btnExecute.Click;
end;

procedure btnExecuteClick(Sender: TObject);
begin
	sElementPath := edElementPath.Text;
	sQuerySearch := edQuerySearch.Text;
	sQueryReplace := edQueryReplace.Text;
	sQueryPrefix := edQueryPrefix.Text;
	sQuerySuffix := edQuerySuffix.Text;
	bProperCase := rbProperCase.Checked;
	bSentenceCase := rbSentenceCase.Checked;
	bModify := rbModify.Checked;
	bCaseSensitive := CompareStr(sQuerySearch, LowerCase(sQueryReplace)) <> 0;
end;

procedure rbProperCaseClick(Sender: TObject);
begin
	rbSentenceCase.Checked := False;
	rbModify.Checked := False;

	edQuerySearch.Enabled := False;
	edQueryReplace.Enabled := False;
	edQueryPrefix.Enabled := False;
	edQuerySuffix.Enabled := False;

	lblModifyOptions.Caption := 'Modify Options (disabled)';
end;

procedure rbSentenceCaseClick(Sender: TObject);
begin
	rbProperCase.Checked := False;
	rbModify.Checked := False;

	edQuerySearch.Enabled := False;
	edQueryReplace.Enabled := False;
	edQueryPrefix.Enabled := False;
	edQuerySuffix.Enabled := False;

	lblModifyOptions.Caption := 'Modify Options (disabled)';
end;

procedure rbModifyClick(Sender: TObject);
begin
	rbProperCase.Checked := False;
	rbSentenceCase.Checked := False;

	edQuerySearch.Enabled := True;
	edQueryReplace.Enabled := True;
	edQueryPrefix.Enabled := True;
	edQuerySuffix.Enabled := True;

	lblModifyOptions.Caption := 'Modify Options (enabled)';
end;

procedure ShowSearchForm;
var
	lblElementPath: TLabel;
	lblBatchOperation: TLabel;
	lblQuerySearch: TLabel;
	lblQueryReplace: TLabel;
	lblQueryPrefix: TLabel;
	lblQuerySuffix: TLabel;
	ini: TMemIniFile;
	iniDefaultElementPath: String;
	iniDefaultQuerySearch: String;
	iniDefaultQueryReplace: String;
	iniDefaultQueryPrefix: String;
	iniDefaultQuerySuffix: String;
	iniDefaultProperCase: Boolean;
	iniDefaultSentenceCase: Boolean;
	iniDefaultModify: Boolean;
	scaleFactor: Double;
begin
	ini := TMemIniFile.Create(wbScriptsPath + 'StringOps.ini');
	iniDefaultElementPath := ini.ReadString('Settings', 'ElementPath', 'FULL');
	iniDefaultQuerySearch := ini.ReadString('Settings', 'QuerySearch', '');
	iniDefaultQueryReplace := ini.ReadString('Settings', 'QueryReplace', '');
	iniDefaultQueryPrefix := ini.ReadString('Settings', 'QueryPrefix', '');
	iniDefaultQuerySuffix := ini.ReadString('Settings', 'QuerySuffix', '');
	iniDefaultProperCase := ini.ReadBool('Settings', 'ProperCase', False);
	iniDefaultSentenceCase := ini.ReadBool('Settings', 'SentenceCase', False);
	iniDefaultModify := ini.ReadBool('Settings', 'Modify', True);

	scaleFactor := Screen.PixelsPerInch / 96;

	frmSearch := TForm.Create(nil);

	try
		lblElementPath := TLabel.Create(frmSearch);
		lblQuerySearch := TLabel.Create(frmSearch);
		lblQueryReplace := TLabel.Create(frmSearch);
		lblBatchOperation := TLabel.Create(frmSearch);
		lblModifyOptions := TLabel.Create(frmSearch);
		lblQueryPrefix := TLabel.Create(frmSearch);
		lblQuerySuffix := TLabel.Create(frmSearch);
		edElementPath := TEdit.Create(frmSearch);
		edQuerySearch := TEdit.Create(frmSearch);
		edQueryReplace := TEdit.Create(frmSearch);
		btnExecute := TButton.Create(frmSearch);
		btnClose := TButton.Create(frmSearch);
		rbProperCase := TRadioButton.Create(frmSearch);
		rbSentenceCase := TRadioButton.Create(frmSearch);
		rbModify := TRadioButton.Create(frmSearch);
		edQueryPrefix := TEdit.Create(frmSearch);
		edQuerySuffix := TEdit.Create(frmSearch);

		frmSearch.Name := 'frmSearch';
		frmSearch.BorderStyle := bsDialog;
		frmSearch.Caption := 'StringOps by fireundubh';
		frmSearch.ClientHeight := 284 * scaleFactor;
		frmSearch.ClientWidth := 274 * scaleFactor;
		frmSearch.Color := clBtnFace;
		frmSearch.KeyPreview := True;
		frmSearch.OnKeyDown := FormKeyDown;
		frmSearch.Position := poScreenCenter;

		lblElementPath.Name := 'lblElementPath';
		lblElementPath.Parent := frmSearch;
		lblElementPath.Left := 16 * scaleFactor;
		lblElementPath.Top := 12 * scaleFactor;
		lblElementPath.Width := 63 * scaleFactor;
		lblElementPath.Height := 13 * scaleFactor;
		lblElementPath.Alignment := taRightJustify;
		lblElementPath.Caption := 'Element path';

		lblQuerySearch.Name := 'lblQuerySearch';
		lblQuerySearch.Parent := frmSearch;
		lblQuerySearch.Left := 32 * scaleFactor;
		lblQuerySearch.Top := 124 * scaleFactor;
		lblQuerySearch.Width := 47 * scaleFactor;
		lblQuerySearch.Height := 13 * scaleFactor;
		lblQuerySearch.Alignment := taRightJustify;
		lblQuerySearch.Caption := 'Find what';

		lblQueryReplace.Name := 'lblQueryReplace';
		lblQueryReplace.Parent := frmSearch;
		lblQueryReplace.Left := 18 * scaleFactor;
		lblQueryReplace.Top := 156 * scaleFactor;
		lblQueryReplace.Width := 61 * scaleFactor;
		lblQueryReplace.Height := 13 * scaleFactor;
		lblQueryReplace.Alignment := taRightJustify;
		lblQueryReplace.Caption := 'Replace with';

		lblBatchOperation.Name := 'lblBatchOperation';
		lblBatchOperation.Parent := frmSearch;
		lblBatchOperation.Left := 8 * scaleFactor;
		lblBatchOperation.Top := 40 * scaleFactor;
		lblBatchOperation.Width := 94 * scaleFactor;
		lblBatchOperation.Height := 13 * scaleFactor;
		lblBatchOperation.Caption := 'Select Operation';

		lblModifyOptions.Name := 'lblModifyOptions';
		lblModifyOptions.Parent := frmSearch;
		lblModifyOptions.Left := 8 * scaleFactor;
		lblModifyOptions.Top := 96 * scaleFactor;
		lblModifyOptions.Width := 159 * scaleFactor;
		lblModifyOptions.Height := 13 * scaleFactor;
		lblModifyOptions.Caption := 'Modify Options (enabled)';

		lblQueryPrefix.Name := 'lblQueryPrefix';
		lblQueryPrefix.Parent := frmSearch;
		lblQueryPrefix.Left := 29 * scaleFactor;
		lblQueryPrefix.Top := 187 * scaleFactor;
		lblQueryPrefix.Width := 50 * scaleFactor;
		lblQueryPrefix.Height := 13 * scaleFactor;
		lblQueryPrefix.Alignment := taRightJustify;
		lblQueryPrefix.Caption := 'Add prefix';

		lblQuerySuffix.Name := 'lblQuerySuffix';
		lblQuerySuffix.Parent := frmSearch;
		lblQuerySuffix.Left := 30 * scaleFactor;
		lblQuerySuffix.Top := 220 * scaleFactor;
		lblQuerySuffix.Width := 49 * scaleFactor;
		lblQuerySuffix.Height := 13 * scaleFactor;
		lblQuerySuffix.Alignment := taRightJustify;
		lblQuerySuffix.Caption := 'Add suffix';

		edElementPath.Name := 'edElementPath';
		edElementPath.Parent := frmSearch;
		edElementPath.Left := 85 * scaleFactor;
		edElementPath.Top := 8 * scaleFactor;
		edElementPath.Width := 180 * scaleFactor;
		edElementPath.Height := 21 * scaleFactor;
		edElementPath.TabOrder := 0;
		edElementPath.Text := iniDefaultElementPath;

		rbProperCase.Name := 'rbProperCase';
		rbProperCase.Parent := frmSearch;
		rbProperCase.Left := 8 * scaleFactor;
		rbProperCase.Top := 60 * scaleFactor;
		rbProperCase.Width := 80 * scaleFactor;
		rbProperCase.Height := 17 * scaleFactor;
		rbProperCase.Caption := 'Proper Case';
		rbProperCase.Checked := iniDefaultProperCase;
		rbProperCase.TabOrder := 1;
		rbProperCase.OnClick := rbProperCaseClick;

		rbSentenceCase.Name := 'rbSentenceCase';
		rbSentenceCase.Parent := frmSearch;
		rbSentenceCase.Left := 102 * scaleFactor;
		rbSentenceCase.Top := 60 * scaleFactor;
		rbSentenceCase.Width := 91 * scaleFactor;
		rbSentenceCase.Height := 17 * scaleFactor;
		rbSentenceCase.Caption := 'Sentence Case';
		rbSentenceCase.Checked := iniDefaultSentenceCase;
		rbSentenceCase.TabOrder := 2;
		rbSentenceCase.OnClick := rbSentenceCaseClick;

		rbModify.Name := 'rbModify';
		rbModify.Parent := frmSearch;
		rbModify.Left := 210 * scaleFactor;
		rbModify.Top := 60 * scaleFactor;
		rbModify.Width := 49 * scaleFactor;
		rbModify.Height := 17 * scaleFactor;
		rbModify.Caption := 'Modify';
		rbModify.Checked := iniDefaultModify;
		rbModify.TabOrder := 3;
		rbModify.OnClick := rbModifyClick;

		edQuerySearch.Name := 'edQuerySearch';
		edQuerySearch.Parent := frmSearch;
		edQuerySearch.Left := 85 * scaleFactor;
		edQuerySearch.Top := 120 * scaleFactor;
		edQuerySearch.Width := 180 * scaleFactor;
		edQuerySearch.Height := 21 * scaleFactor;
		edQuerySearch.TabOrder := 4;
		edQuerySearch.Text := iniDefaultQuerySearch;

		edQueryReplace.Name := 'edQueryReplace';
		edQueryReplace.Parent := frmSearch;
		edQueryReplace.Left := 85 * scaleFactor;
		edQueryReplace.Top := 152 * scaleFactor;
		edQueryReplace.Width := 180 * scaleFactor;
		edQueryReplace.Height := 21 * scaleFactor;
		edQueryReplace.TabOrder := 5;
		edQueryReplace.Text := iniDefaultQueryReplace;

		edQueryPrefix.Name := 'edQueryPrefix';
		edQueryPrefix.Parent := frmSearch;
		edQueryPrefix.Left := 85 * scaleFactor;
		edQueryPrefix.Top := 184 * scaleFactor;
		edQueryPrefix.Width := 180 * scaleFactor;
		edQueryPrefix.Height := 21 * scaleFactor;
		edQueryPrefix.TabOrder := 6;
		edQueryPrefix.Text := iniDefaultQueryPrefix;

		edQuerySuffix.Name := 'edQuerySuffix';
		edQuerySuffix.Parent := frmSearch;
		edQuerySuffix.Left := 85 * scaleFactor;
		edQuerySuffix.Top := 216 * scaleFactor;
		edQuerySuffix.Width := 180 * scaleFactor;
		edQuerySuffix.Height := 21 * scaleFactor;
		edQuerySuffix.TabOrder := 7;
		edQuerySuffix.Text := iniDefaultQuerySuffix;

		btnExecute.Name := 'btnExecute';
		btnExecute.Parent := frmSearch;
		btnExecute.Left := 191 * scaleFactor;
		btnExecute.Top := 251 * scaleFactor;
		btnExecute.Width := 75 * scaleFactor;
		btnExecute.Height := 25 * scaleFactor;
		btnExecute.Caption := 'Execute';
		btnExecute.TabOrder := 8;
		btnExecute.OnClick := btnExecuteClick;
		btnExecute.ModalResult := mrOk;

		btnClose.Name := 'btnClose';
		btnClose.Parent := frmSearch;
		btnClose.Left := 110 * scaleFactor;
		btnClose.Top := 252 * scaleFactor;
		btnClose.Width := 75 * scaleFactor;
		btnClose.Height := 25 * scaleFactor;
		btnClose.Caption := 'Close';
		btnClose.TabOrder := 9;
		btnClose.ModalResult := mrCancel;

		frmSearch.ShowModal;
	finally
		ini.WriteString('Settings', 'ElementPath', edElementPath.Text);
		ini.WriteString('Settings', 'QuerySearch', edQuerySearch.Text);
		ini.WriteString('Settings', 'QueryReplace', edQueryReplace.Text);
		ini.WriteString('Settings', 'QueryPrefix', edQueryPrefix.Text);
		ini.WriteString('Settings', 'QuerySuffix', edQuerySuffix.Text);
		ini.WriteBool('Settings', 'ProperCase', rbProperCase.Checked);
		ini.WriteBool('Settings', 'SentenceCase', rbSentenceCase.Checked);
		ini.WriteBool('Settings', 'Modify', rbModify.Checked);

		try
			ini.UpdateFile;
		except
			AddMessage('Cannot save settings, no write access to ' + ini.FileName);
		end;

		ini.Free;
	end;
end;

end.
