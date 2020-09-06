{
  Purpose: Generates bash tags for a selected plugin automatically
  Games:   FO3/FNV/FO4/TES4/TES5/SSE/Enderal
  Author:  fireundubh <fireundubh@gmail.com>
}

unit GenerateWryeBashTags;


const
  scaleFactor = Screen.PixelsPerInch / 96;


var
  kFile           : IwbFile;
  slBadTags       : TStringList;
  slDifferentTags : TStringList;
  slExistingTags  : TStringList;
  slLog           : TStringList;
  slMasterPlugins : TStringList;
  slSuggestedTags : TStringList;
  sFileName       : String;
  sTag            : String;
  sScriptName     : String;
  sScriptVersion  : String;
  sScriptAuthor   : String;
  sScriptEmail    : String;
  optionAddTags   : Integer;
  optionOutputLog : Integer;
  bQuickExit      : Boolean;
  bFirstProcess   : Boolean;
  sGuiPlugin      : String;


function wbIsOblivion: Boolean;
begin
  Result := wbGameMode = gmTES4;
end;


function wbIsSkyrim: Boolean;
begin
  Result := (wbGameMode = gmTES5) or (wbGameMode = gmEnderal) or (wbGameMode = gmTES5VR) or (wbGameMode = gmSSE);
end;


function wbIsSkyrimSE: Boolean;
begin
  Result := wbGameMode = gmSSE;
end;


function wbIsFallout3: Boolean;
begin
  Result := wbGameMode = gmFO3;
end;


function wbIsFalloutNV: Boolean;
begin
  Result := wbGameMode = gmFNV;
end;


function wbIsFallout4: Boolean;
begin
  Result := (wbGameMode = gmFO4) or (wbGameMode = gmFO4VR);
end;


function wbIsFallout76: Boolean;
begin
  Result := wbGameMode = gmFO76;
end;


procedure BuildMasterPluginsList(f: IwbFile; output: TStringList);
var
  m: IwbFile;
  i: Integer;
begin
  output.Add(GetFileName(f));

  for i := 0 to Pred(MasterCount(f)) do
  begin
    m := MasterByIndex(f, i);
    output.Add(GetFileName(m));
    BuildMasterPluginsList(m, output);
  end;
end;


procedure LogInfo(s: String);
begin
  AddMessage('[INFO] ' + s);
end;


procedure LogWarn(s: String);
begin
  AddMessage('[WARN] ' + s);
end;


procedure LogError(s: String);
begin
  AddMessage('[ERRO] ' + s);
end;


function Initialize: Integer;
begin
  sScriptName    := 'GenerateWryeBashTags'; // working name
  sScriptVersion := '1.6.3.0';
  sScriptAuthor  := 'fireundubh';
  sScriptEmail   := 'fireundubh@gmail.com';
  bFirstProcess  := true;
  sGuiPlugin     := '';

  Result := 0;

  // clear
  ClearMessages();

  // show script header
  AddMessage(#10);
  LogInfo(sScriptName + ' v' + sScriptVersion + ' by ' + sScriptAuthor + ' <' + sScriptEmail + '>');
  AddMessage(#10);
end;

function Process(e: IInterface): integer;
begin
  if bFirstProcess then
  begin
    bFirstProcess := false;
	sGuiPlugin := GetFileName(e);
    Result := Work();
  end
end;
  
function Work: Integer;
var
  kDescription : IInterface;
  kHeader      : IInterface;
  sBashTags    : String;
  sDescription : String;
  sMasterName  : String;
  r            : IwbMainRecord;
  i            : Integer;
begin

  optionAddTags   := mrNo;
  optionOutputLog := mrYes;

  kFile := Configure(sScriptName + ' v' + sScriptVersion);
  if not Assigned(kFile) then
    exit;

  sFileName := GetFileName(kFile);

  // create list of log entries
  slLog := TStringList.Create;
  slLog.Sorted := False;
  slLog.Duplicates := dupAccept;

  // create list of tags
  slSuggestedTags := TStringList.Create;
  slSuggestedTags.Sorted := True;
  slSuggestedTags.Duplicates := dupIgnore;
  slSuggestedTags.Delimiter := ','; // separated by comma

  slExistingTags  := TStringList.Create; // existing tags
  slDifferentTags := TStringList.Create; // different tags
  slBadTags       := TStringList.Create; // bad tags

  if wbIsFallout3 then
    LogInfo('Using game mode: Fallout 3');
  if wbIsFalloutNV then
    LogInfo('Using game mode: Fallout: New Vegas');
  if wbIsFallout4 then
    LogInfo('Using game mode: Fallout 4');
  if wbIsFallout76 then
  begin
    LogError('Fallout 76 is not supported by CBash.');
    exit;
  end;
  if wbIsOblivion then
    LogInfo('Using game mode: Oblivion');
  if wbIsSkyrim and not wbIsSkyrimSE then
    LogInfo('Using game mode: Skyrim');
  if wbIsSkyrimSE then
    LogInfo('Using game mode: Skyrim Special Edition');

  slMasterPlugins := TStringList.Create;
  slMasterPlugins.Duplicates := dupIgnore;
  slMasterPlugins.Sorted := True;

  AddMessage(#10);

  BuildMasterPluginsList(kFile, slMasterPlugins);

  LogInfo('Processing... Please wait. This could take a while.');

  for i := 0 to Pred(RecordCount(kFile)) do
    ProcessRecord(RecordByIndex(kFile, i));

  AddMessage(#10);

  // exit conditions
  if not Assigned(sFileName) then
    exit;

  // output file name
  LogInfo(Uppercase(sFileName));

  AddMessage(#10);

  // output log
  if optionOutputLog = mrYes then
    for i := 0 to Pred(slLog.Count) do
      LogInfo(slLog[i]);

  if (optionOutputLog = mrYes) and (slLog.Count > 0) then
    AddMessage(#10);

  // if any suggested tags were generated
  if slSuggestedTags.Count > 0 then
  begin
    kHeader := ElementBySignature(kFile, 'TES4');

    // determine if the header record exists
    if Assigned(kHeader) then
    begin
      kDescription := ElementBySignature(kHeader, 'SNAM');
      sDescription := GetEditValue(kDescription);

      // categorize tag list
      sBashTags := RegExMatch('(?:[{]{2}BASH:).*?[}]{2}', sDescription);
      if Length(sBashTags) > 0 then
      begin
        sBashTags := Trim(MidStr(sBashTags, 8, Length(sBashTags) - 9));
        slExistingTags.CommaText := sBashTags;
      end else
        slExistingTags.CommaText := '';

      slDifferentTags := Diff(slSuggestedTags, slExistingTags);
      slBadTags := Diff(slExistingTags, slSuggestedTags);
      slSuggestedTags.AddStrings(slDifferentTags);

      // exit if existing and suggested tags are the same
      if SameText(slExistingTags.CommaText, slSuggestedTags.CommaText) then
      begin
        LogInfo(FormatTags(slExistingTags, 'existing tag found:', 'existing tags found:', 'No existing tags found.'));
        LogInfo(FormatTags(slSuggestedTags, 'suggested tag:', 'suggested tags:', 'No suggested tags.'));
        LogWarn('No tags to add.' + #13#10);
        exit;
      end;

    // exit if the header record doesn't exist
    end else begin
      LogError('Header record not found. Nothing to do. Exiting.' + #13#10);
      exit;
    end;

    // write tags
    if optionAddTags = mrYes then
    begin
      // if the description element doesn't exist, add the element
      kDescription := ElementBySignature(kHeader, 'SNAM');
      if not Assigned(kDescription) then
        kDescription := Add(kHeader, 'SNAM', True);

      if not SameText(slExistingTags.CommaText, slSuggestedTags.CommaText) then
      begin
        sDescription := GetEditValue(kDescription);
        sDescription := Trim(RemoveFromEnd(sDescription, Format('{{BASH:%s}}', [slExistingTags.DelimitedText])));
        SetEditValue(kDescription, sDescription + #13#10 + #13#10 + Format('{{BASH:%s}}', [slSuggestedTags.DelimitedText]));
      end;

      LogInfo(FormatTags(slBadTags,       'bad tag removed:',          'bad tags removed:',          'No bad tags found.'));
      LogInfo(FormatTags(slDifferentTags, 'tag added to file header:', 'tags added to file header:', 'No tags added.'));
    end;

    // suggest tags only and output to log
    if optionAddTags = mrNo then
    begin
      LogInfo(FormatTags(slExistingTags,  'existing tag found:',    'existing tags found:',    'No existing tags found.'));
      LogInfo(FormatTags(slBadTags,       'bad tag found:',         'bad tags found:',         'No bad tags found.'));
      LogInfo(FormatTags(slDifferentTags, 'suggested tag to add:',  'suggested tags to add:',  'No suggested tags to add.'));
      LogInfo(FormatTags(slSuggestedTags, 'suggested tag overall:', 'suggested tags overall:', 'No suggested tags overall.'));
    end;
  end else
    LogInfo('No tags are suggested for this plugin.');

  AddMessage(#10);
end;


function ProcessRecord(e: IInterface): Integer;
var
  o             : IInterface;
  sTag          : String;
  sSignature    : String;
  ConflictState : TConflictThis;
  iFormID       : Integer;
begin
  // exit conditions
  ConflictState := ConflictAllForMainRecord(e);

  // get record signature
  sSignature := Signature(e);

  if (ConflictState = caUnknown)
  or (ConflictState = caOnlyOne)
  or (ConflictState = caNoConflict) then
    exit;

  // exit if the record should not be processed
  if CompareText(sFileName, 'Dawnguard.esm') = 0 then
  begin
    iFormID := FileFormID(e);
    if (iFormID = $00016BCF)
    or (iFormID = $0001EE6D)
    or (iFormID = $0001FA4C)
    or (iFormID = $00039F67)
    or (iFormID = $0006C3B6) then
      exit;
  end;

  // get master record if record is an override
  o := Master(e);

  if not Assigned(o) then
    exit;

  // if record overrides several masters, then get the last one
  o := HighestOverrideOrSelfInList(o, OverrideCount(o), slMasterPlugins);

  // stop processing deleted records to avoid errors
  if GetIsDeleted(e)
  or GetIsDeleted(o) then
    exit;

  // -------------------------------------------------------------------------------
  // GROUP: Supported tags exclusive to FNV
  // -------------------------------------------------------------------------------
  if wbIsFalloutNV then
    if sSignature = 'WEAP' then
      ProcessTag('WeaponMods', e, o);

  // -------------------------------------------------------------------------------
  // GROUP: Supported tags exclusive to TES4
  // -------------------------------------------------------------------------------
  if wbIsOblivion then
  begin
    if InDelimitedList(sSignature, 'CREA NPC_', ' ') then
    begin
      ProcessTag('Actors.Spells', e, o);

      if sSignature = 'CREA' then
        ProcessTag('Creatures.Blood', e, o);
    end;

    // TODO: Npc.EyesOnly - NOT IMPLEMENTED
    // TODO: Npc.HairOnly - NOT IMPLEMENTED
    // TODO: R.AddSpells - NOT IMPLEMENTED

    if sSignature = 'RACE' then
    begin
      ProcessTag('R.ChangeSpells', e, o);
      ProcessTag('R.Attributes-F', e, o);
      ProcessTag('R.Attributes-M', e, o);
    end;

    if sSignature = 'ROAD' then
      ProcessTag('Roads', e, o);

    if sSignature = 'SPEL' then
      ProcessTag('SpellStats', e, o);
  end;

  // -------------------------------------------------------------------------------
  // GROUP: Supported tags exclusive to TES5 and SSE
  // -------------------------------------------------------------------------------
  if wbIsSkyrim then
  begin
    if sSignature = 'CELL' then
    begin
      ProcessTag('C.Location', e, o);
      ProcessTag('C.LockList', e, o);
      ProcessTag('C.Regions', e, o);
      ProcessTag('C.SkyLighting', e, o);
    end;

    if InDelimitedList(sSignature, 'ACTI ALCH AMMO ARMO BOOK FLOR FURN INGR KEYM LCTN MGEF MISC NPC_ SCRL SLGM SPEL TACT WEAP', ' ') then
      ProcessTag('Keywords', e, o);
  end;

  // -------------------------------------------------------------------------------
  // GROUP: Supported tags exclusive to FO3 and FNV
  // -------------------------------------------------------------------------------
  if wbIsFallout3 or wbIsFalloutNV then
  begin
    sTag := 'Destructible';
    if InDelimitedList(sSignature, 'ACTI ALCH AMMO BOOK CONT DOOR FURN IMOD KEYM MISC MSTT PROJ TACT TERM WEAP', ' ') then
      ProcessTag(sTag, e, o);

    // special handling for CREA and NPC_ record types
    if InDelimitedList(sSignature, 'CREA NPC_', ' ') then
      if not CompareFlags(sTag, e, o, 'ACBS\Template Flags', 'Use Model/Animation', False, False) then
        ProcessTag(sTag, e, o);
  end;

  // -------------------------------------------------------------------------------
  // GROUP: Supported tags exclusive to FO3, FNV, and TES4
  // -------------------------------------------------------------------------------
  if wbIsOblivion or wbIsFallout3 or wbIsFalloutNV then
  begin
    if InDelimitedList(sSignature, 'CREA NPC_', ' ') then
    begin
      sTag := 'Factions';
      if wbIsOblivion then
        ProcessTag(sTag, e, o)
      else
        if not CompareFlags(sTag, e, o, 'ACBS\Template Flags', 'Use Factions', False, False) then
          ProcessTag(sTag, e, o);
    end;

    if sSignature = 'FACT' then
      ProcessTag('Relations', e, o);
  end;

  // -------------------------------------------------------------------------------
  // GROUP: Supported tags exclusive to FO3, FNV, TES5, and SSE
  // -------------------------------------------------------------------------------
  if not wbIsOblivion and not wbIsFallout4 then
  begin
    if InDelimitedList(sSignature, 'CREA NPC_', ' ') then
    begin
      sTag := 'Actors.ACBS';
      if not CompareFlags(sTag, e, o, 'ACBS\Template Flags', 'Use Stats', False, False) then
        ProcessTag(sTag, e, o);

      sTag := 'Actors.AIData';
      if not CompareFlags(sTag, e, o, 'ACBS\Template Flags', 'Use AI Data', False, False) then
        ProcessTag(sTag, e, o);

      sTag := 'Actors.AIPackages';
      if not CompareFlags(sTag, e, o, 'ACBS\Template Flags', 'Use AI Packages', False, False) then
        ProcessTag(sTag, e, o);

      if sSignature = 'CREA' then
        if not CompareFlags(sTag, e, o, 'ACBS\Template Flags', 'Use Model/Animation', False, False) then
          ProcessTag('Actors.Anims', e, o);

      if not CompareFlags(sTag, e, o, 'ACBS\Template Flags', 'Use Traits', False, False) then
      begin
        ProcessTag('Actors.CombatStyle', e, o);
        ProcessTag('Actors.DeathItem', e, o);
      end;

      sTag := 'Actors.Skeleton';
      if not CompareFlags(sTag, e, o, 'ACBS\Template Flags', 'Use Model/Animation', False, False) then
        ProcessTag(sTag, e, o);

      sTag := 'Actors.Stats';
      if not CompareFlags(sTag, e, o, 'ACBS\Template Flags', 'Use Stats', False, False) then
        ProcessTag(sTag, e, o);

      // TODO: IIM - NOT IMPLEMENTED
      // TODO: MustBeActiveIfImported - NOT IMPLEMENTED

      if sSignature = 'NPC_' then
      begin
        sTag := 'NPC.Class';
        if not CompareFlags(sTag, e, o, 'ACBS\Template Flags', 'Use Traits', False, False) then
          ProcessTag(sTag, e, o);

        sTag := 'NPC.Race';
        if not CompareFlags(sTag, e, o, 'ACBS\Template Flags', 'Use Traits', False, False) then
          ProcessTag(sTag, e, o);

        sTag := 'NpcFaces';
        if not CompareFlags(sTag, e, o, 'ACBS\Template Flags', 'Use Model/Animation', False, False) then
          ProcessTag(sTag, e, o);
      end;

      sTag := 'Scripts';
      if not CompareFlags(sTag, e, o, 'ACBS\Template Flags', 'Use Script', False, False) then
        ProcessTag(sTag, e, o);
    end;

    if sSignature = 'CELL' then
    begin
      ProcessTag('C.Acoustic', e, o);
      ProcessTag('C.Encounter', e, o);
      ProcessTag('C.ImageSpace', e, o);
    end;

    if sSignature = 'RACE' then
    begin
      ProcessTag('Body-F', e, o);
      ProcessTag('Body-M', e, o);
      ProcessTag('Body-Size-F', e, o);
      ProcessTag('Body-Size-M', e, o);
      ProcessTag('Eyes', e, o);
      ProcessTag('Hair', e, o);
      ProcessTag('R.Description', e, o);
      ProcessTag('R.Ears', e, o);
      ProcessTag('R.Head', e, o);
      ProcessTag('R.Mouth', e, o);
      ProcessTag('R.Relations', e, o);
      ProcessTag('R.Skills', e, o);
      ProcessTag('R.Teeth', e, o);
      ProcessTag('Voice-F', e, o);
      ProcessTag('Voice-M', e, o);
    end;

    // TODO: ScriptContents - SHOULD NOT BE IMPLEMENTED
    // -- According to the Wrye Bash Readme: "Should not be used. Can cause serious issues."

    if InDelimitedList(sSignature, 'ACTI ALCH ARMO CONT DOOR FLOR FURN INGR KEYM LIGH LVLC MISC QUST WEAP', ' ') then
      ProcessTag('Scripts', e, o);
  end;

  // -------------------------------------------------------------------------------
  // GROUP: Supported tags exclusive to FO3, FNV, TES4, TES5, and SSE
  // -------------------------------------------------------------------------------

  if not wbIsFallout4 then
  begin
    if sSignature = 'CELL' then
    begin
      ProcessTag('C.Climate', e, o);
      ProcessTag('C.Light', e, o);
      ProcessTag('C.Music', e, o);
      ProcessTag('C.Name', e, o);
      ProcessTag('C.Owner', e, o);
      ProcessTag('C.RecordFlags', e, o);
      ProcessTag('C.Water', e, o);
    end;

    // TODO: Deactivate - NOT IMPLEMENTED

    // TAG: Delev, Relev
    if InDelimitedList(sSignature, 'LVLC LVLI LVLN LVSP', ' ') then
      ProcessDelevRelevTags(e, o);

    // TODO: Filter - NOT IMPLEMENTED

    if InDelimitedList(sSignature, 'ACTI ALCH AMMO APPA ARMO BOOK BSGN CLAS CLOT DOOR FLOR FURN INGR KEYM LIGH MGEF MISC SGST SLGM WEAP', ' ') then
    begin
      ProcessTag('Graphics', e, o);
      ProcessTag('Names', e, o);
      ProcessTag('Stats', e, o);

      if InDelimitedList(sSignature, 'ACTI DOOR LIGH MGEF', ' ') then
        ProcessTag('Sound', e, o);
    end;

    if InDelimitedList(sSignature, 'CREA EFSH GRAS LSCR LTEX REGN STAT TREE', ' ') then
      ProcessTag('Graphics', e, o);

    if sSignature = 'CONT' then
    begin
      ProcessTag('Invent', e, o);
      ProcessTag('Names', e, o);
      ProcessTag('Sound', e, o);
    end;

    if InDelimitedList(sSignature, 'DIAL ENCH EYES FACT HAIR QUST RACE SPEL WRLD', ' ') then
      ProcessTag('Names', e, o);

    // TODO: NoMerge - NOT IMPLEMENTED

    if (sSignature = 'WTHR') then
      ProcessTag('Sound', e, o);

    // special handling for CREA and NPC_
    if InDelimitedList(sSignature, 'CREA NPC_', ' ') then
    begin
      if wbIsOblivion then
      begin
        ProcessTag('Invent', e, o);
        ProcessTag('Names', e, o);

        if sSignature = 'CREA' then
          ProcessTag('Sound', e, o);
      end;

      if not wbIsOblivion then
      begin
        sTag := 'Invent';
        if not CompareFlags(sTag, e, o, 'ACBS\Template Flags', 'Use Inventory', False, False) then
          ProcessTag(sTag, e, o);

        // special handling for CREA and NPC_ record types
        sTag := 'Names';
        if not CompareFlags(sTag, e, o, 'ACBS\Template Flags', 'Use Base Data', False, False) then
          ProcessTag(sTag, e, o);

        // special handling for CREA record type
        sTag := 'Sound';
        if sSignature = 'CREA' then
          if not CompareFlags(sTag, e, o, 'ACBS\Template Flags', 'Use Model/Animation', False, False) then
            ProcessTag(sTag, e, o);
      end;
    end;
  end;

  // ObjectBounds
  sTag := 'ObjectBounds';

  if wbIsFallout3 then
    if InDelimitedList(sSignature, 'ACTI ADDN ALCH AMMO ARMA ARMO ASPC BOOK COBJ CONT CREA DOOR EXPL FURN GRAS IDLM INGR KEYM LIGH LVLC LVLI LVLN MISC MSTT NOTE NPC_ PROJ PWAT SCOL SOUN STAT TACT TERM TREE TXST WEAP', ' ') then
      ProcessTag(sTag, e, o);

  if wbIsFalloutNV then
    if InDelimitedList(sSignature, 'ACTI ADDN ALCH AMMO ARMA ARMO ASPC BOOK CCRD CHIP CMNY COBJ CONT CREA DOOR EXPL FURN GRAS IDLM IMOD INGR KEYM LIGH LVLC LVLI LVLN MISC MSTT NOTE NPC_ PROJ PWAT SCOL SOUN STAT TACT TERM TREE TXST WEAP', ' ') then
      ProcessTag(sTag, e, o);

  if wbIsSkyrim then
    if InDelimitedList(sSignature, 'ACTI ADDN ALCH AMMO APPA ARMO ARTO ASPC BOOK CONT DOOR DUAL ENCH EXPL FLOR FURN GRAS HAZD IDLM INGR KEYM LIGH LVLI LVLN LVSP MISC MSTT NPC_ PROJ SCRL SLGM SOUN SPEL STAT TACT TREE TXST WEAP', ' ') then
      ProcessTag(sTag, e, o);

  if wbIsFallout4 then
    if InDelimitedList(sSignature, 'LVLI LVLN', ' ') then
      ProcessTag(sTag, e, o);

  // Text
  if not wbIsFallout4 then
  begin
    sTag := 'Text';

    if wbIsOblivion then
      if InDelimitedList(sSignature, 'BOOK BSGN CLAS LSCR MGEF SKIL', ' ') then
        ProcessTag(sTag, e, o);

    if wbIsFallout3 then
      if InDelimitedList(sSignature, 'AVIF BOOK CLAS LSCR MESG MGEF NOTE PERK TERM', ' ') then
        ProcessTag(sTag, e, o);

    if wbIsFalloutNV then
      if InDelimitedList(sSignature, 'AVIF BOOK CHAL CLAS IMOD LSCR MESG MGEF NOTE PERK TERM', ' ') then
        ProcessTag(sTag, e, o);

    if wbIsSkyrim then
      if InDelimitedList(sSignature, 'ALCH AMMO APPA ARMO AVIF BOOK CLAS LSCR MESG MGEF SCRL SHOU SPEL WEAP', ' ') then
        ProcessTag(sTag, e, o);
  end;
end;

function Finalize: Integer;
begin
  if not Assigned(kFile) then
  begin
    LogInfo('Script execution was aborted.' + #13#10);
    exit;
  end;

  slLog.Free;
  slSuggestedTags.Free;
  slExistingTags.Free;
  slDifferentTags.Free;
  slBadTags.Free;
  slMasterPlugins.Free;
end;


function StrToBool(s: String): Boolean;
begin
  if (s <> '0') and (s <> '1') then
    Result := nil
  else
    if (s = '1') then
      Result := True
    else
      Result := False;
end;


function InDelimitedList(asNeedle: String; asHaystack: String; asDelimiter: String): Boolean;
var
  a: TStringDynArray;
  i: Integer;
begin
  Result := False;

  a := SplitString(asHaystack, asDelimiter);
  for i := 0 to Pred(Length(a)) do
    if SameText(asNeedle, a[i]) then
    begin
      Result := True;
      break;
    end;
end;


function RegExMatch(asPattern: String; asSubject: String): String;
var
  re: TPerlRegEx;
begin
  Result := nil;
  re := TPerlRegEx.Create;
  try
    re.RegEx := asPattern;
    re.Options := [];
    re.Subject := asSubject;
    if re.Match then
      Result := re.MatchedText;
  finally
    re.Free;
  end;
end;


function RemoveFromEnd(asSource: String; asSubstring: String): String;
begin
  Result := asSource;
  if EndsText(asSource, asSubstring) then
    Result := Copy(asSource, 1, Length(asSource) - Length(asSubstring));
end;


function SortKeyEx(const akElement : IInterface) : string;
var
  kElement: IInterface;
  i: Integer;
begin
  Result := GetEditValue(akElement);

  for i := 0 to Pred(ElementCount(akElement)) do
  begin
    kElement := ElementByIndex(akElement, i);

    if SameText(Name(kElement), 'unknown') or SameText(Name(kElement), 'unused') then
      continue;

    if Result <> '' then
      Result := Result + ' ' + SortKeyEx(kElement)
    else
      Result := SortKeyEx(kElement);
  end;
end;


function CompareAssignment(asTag: String; e, m: IInterface): Boolean;
var
  bAssignedE : Boolean;
  bAssignedM : Boolean;
begin
  if TagExists(asTag) then
    exit;

  Result := False;

  bAssignedE := Assigned(e);
  bAssignedM := Assigned(m);

  if (not bAssignedE and not bAssignedM)
  or (bAssignedE and bAssignedM) then
    exit;

  if bAssignedE <> bAssignedM then
  begin
    if optionOutputLog = mrYes then
      AddLogEntry(asTag, 'Assigned', e, m);
    AddTag(asTag);
    Result := True;
  end;
end;


function CompareElementCount(asTag: String; e, m: IInterface): Boolean;
var
  iCountE : Integer;
  iCountM : Integer;
begin
  if TagExists(asTag) then
    exit;

  Result := False;

  iCountE := ElementCount(e);
  iCountM := ElementCount(m);

  if iCountE = iCountM then
    exit;

  if iCountE <> iCountM then
  begin
    if optionOutputLog = mrYes then
      AddLogEntry(asTag, 'ElementCount', e, m);
    AddTag(asTag);
    Result := True;
  end;
end;


function CompareEditValue(asTag: String; e, m: IInterface): Boolean;
var
  sValueE : String;
  sValueM : String;
begin
  if TagExists(asTag) then
    exit;

  Result := False;

  sValueE := GetEditValue(e);
  sValueM := GetEditValue(m);

  if SameText(sValueE, sValueM) then
    exit;

  if not SameText(sValueE, sValueM) then
  begin
    if optionOutputLog = mrYes then
      AddLogEntry(asTag, 'GetEditValue', e, m);
    AddTag(asTag);
    Result := True;
  end;
end;


function CompareFlags(asTag: String; e, m: IInterface; asPath, asFlagName: String; bAddTag, bOperation: Boolean): Boolean;
var
  x         : IInterface;
  y         : IInterface;
  a         : IInterface;
  b         : IInterface;
  sa        : String;
  sb        : String;
  sTestName : String;
  bResult   : Boolean;
begin
  if TagExists(asTag) then
    exit;

  Result := False;

  // flags arrays
  x := ElementByPath(e, asPath);
  y := ElementByPath(m, asPath);

  // individual flags
  a := ElementByName(x, asFlagName);
  b := ElementByName(y, asFlagName);

  // individual flag edit values
  sa := GetEditValue(a);
  sb := GetEditValue(b);

  if bOperation then
    bResult := not SameText(sa, sb)  // only used for Behave Like Exterior, Use Sky Lighting, and Has Water
  else
    bResult := StrToBool(sa) or StrToBool(sb);

  if bAddTag and bResult then
  begin
    if bOperation then
      sTestName := 'CompareFlags:NOT'
    else
      sTestName := 'CompareFlags:OR';

    if optionOutputLog = mrYes then
      AddLogEntry(asTag, sTestName, x, y);
    AddTag(asTag);
  end;

  Result := bResult;
end;


function CompareKeys(asTag: String; e, m: IInterface): Boolean;
var
  bResult       : Boolean;
  sKeyE         : String;
  sKeyM         : String;
  ConflictState : TConflictThis;
begin
  if TagExists(asTag) then
    exit;

  Result := False;

  ConflictState := ConflictAllForMainRecord(ContainingMainRecord(e));

  if (ConflictState = caUnknown)
  or (ConflictState = caOnlyOne)
  or (ConflictState = caNoConflict) then
    exit;

  sKeyE := SortKeyEx(e);
  sKeyM := SortKeyEx(m);

  // empty check
  if (IsEmptyKey(sKeyE) and IsEmptyKey(sKeyM))
  or SameText(sKeyE, sKeyM) then
    exit;

  // case sensitive comparison
  if not SameText(sKeyE, sKeyM) then
  begin
    if optionOutputLog = mrYes then
      AddLogEntry(asTag, 'CompareKeys', e, m);
    AddTag(asTag);
    Result := True;
  end;
end;


function CompareNativeValues(asTag: String; e, m: IInterface; asPath: String): Boolean;
var
  x : IInterface;
  y : IInterface;
begin
  if TagExists(asTag) then
    exit;

  Result := False;

  x := ElementByPath(e, asPath);
  y := ElementByPath(m, asPath);

  if GetNativeValue(x) = GetNativeValue(y) then
    exit;

  if GetNativeValue(x) <> GetNativeValue(y) then
  begin
    if optionOutputLog = mrYes then
      AddLogEntry(asTag, 'CompareNativeValues', e, m);
    AddTag(asTag);
    Result := True;
  end;
end;


function HighestOverrideOrSelfInList(akMainRecord: IInterface; aiMaxLoadOrder: Integer; aslMasterPlugins: TStringList): IInterface;
var
  kMaster   : IwbMainRecord;
  kFile     : IwbFile;
  sFileName : String;
  i         : Integer;
begin
  Result := akMainRecord;

  kMaster := MasterOrSelf(akMainRecord);

  for i := Pred(OverrideCount(akMainRecord)) downto 0 do
  begin
    kFile := GetFile(OverrideByIndex(kMaster, i));
    sFileName := GetFileName(kFile);

    // skip plugins that aren't masters because we don't care about them
    if aslMasterPlugins.IndexOf(sFileName) = -1 then
      continue;

    if GetLoadOrder(kFile) <= aiMaxLoadOrder then
    begin
      Result := OverrideByIndex(kMaster, i);
      exit;
    end;
  end;
end;


function SortedArrayElementByValue(e: IInterface; sPath, sValue: String): IInterface;
var
  i: Integer;
  kEntry: IInterface;
begin
  Result := nil;
  for i := 0 to ElementCount(e) - 1 do
  begin
    kEntry := ElementByIndex(e, i);
    if SameText(GetElementEditValues(kEntry, sPath), sValue) then
    begin
      Result := kEntry;
      exit;
    end;
  end;
end;


function Diff(lsList1, lsList2: TStringList): TStringList;
var
  i   : Integer;
  tmp : TStringList;
begin
  tmp := TStringList.Create;
  for i := 0 to lsList1.Count - 1 do
    if lsList2.IndexOf(lsList1[i]) < 0 then
      tmp.Add(lsList1[i]);
  Result := tmp;
end;


// TODO: speed this up!
function IsEmptyKey(asSortKey: String): Boolean;
var
  i : Integer;
begin
  Result := True;
  for i := 1 to Length(asSortKey) do
    if asSortKey[i] = '1' then
    begin
      Result := False;
      exit;
    end;
end;


function FormatTags(aslTags: TStringList; asSingular, asPlural, asNull: String): String;
var
  iTagCount : Integer;
  sTagCount : String;
begin
  iTagCount := aslTags.Count;
  sTagCount := IntToStr(iTagCount);

  if iTagCount = 1 then
    Result := sTagCount + ' ' + asSingular + #13#10#32#32#32#32#32#32
  else
  if iTagCount > 1 then
    Result := sTagCount + ' ' + asPlural + #13#10#32#32#32#32#32#32;

  if iTagCount > 0 then
    Result := Result + Format(' {{BASH:%s}}', [aslTags.DelimitedText])
  else
    Result := asNull;
end;


function TagExists(asTag: String): Boolean;
begin
  Result := (slSuggestedTags.IndexOf(asTag) <> -1);
end;


procedure AddTag(asTag: String);
begin
  if not TagExists(asTag) then
    slSuggestedTags.Add(asTag);
end;


procedure Evaluate(asTag: String; e, m: IInterface);
begin
  // exit if the tag already exists
  if TagExists(asTag) then
    exit;

  // Suggest tag if one element exists while the other does not
  if CompareAssignment(asTag, e, m) then
    exit;

  // exit if the first element does not exist
  if not Assigned(e) then
    exit;

  // suggest tag if the two elements are different
  if CompareElementCount(asTag, e, m) then
    exit;

  // suggest tag if the edit values of the two elements are different
  if CompareEditValue(asTag, e, m) then
    exit;

  // compare any number of elements with CompareKeys
  if CompareKeys(asTag, e, m) then
    exit;
end;

procedure EvaluateByPath(asTag: String; e, m: IInterface; asPath: String);
var
  x : IInterface;
  y : IInterface;
begin
  x := ElementByPath(e, asPath);
  y := ElementByPath(m, asPath);

  Evaluate(asTag, x, y);
end;

procedure ProcessTag(asTag: String; e, m: IInterface);
var
  x          : IInterface;
  y          : IInterface;
  a          : IInterface;
  b          : IInterface;
  j          : IInterface;
  k          : IInterface;
  sElement   : String;
  sSignature : String;
begin
  if TagExists(asTag) then
    exit;

  sSignature := Signature(e);

  // Bookmark: Actors.ACBS
  if asTag = 'Actors.ACBS' then
  begin
    // assign ACBS elements
    sElement := 'ACBS';
    x := ElementBySignature(e, sElement);
    y := ElementBySignature(m, sElement);

    // evaluate Flags if the Use Base Data flag is not set
    sElement := 'Flags';
    a := ElementByName(x, sElement);
    b := ElementByName(y, sElement);

    if wbIsOblivion then
      if CompareKeys(asTag, a, b) then
        exit;

    if not wbIsOblivion then
      if not CompareFlags(asTag, x, y, 'Template Flags', 'Use Base Data', False, False) then
        if CompareKeys(asTag, a, b) then
          exit;

    // evaluate properties
    EvaluateByPath(asTag, x, y, 'Fatigue');
    EvaluateByPath(asTag, x, y, 'Level');
    EvaluateByPath(asTag, x, y, 'Calc min');
    EvaluateByPath(asTag, x, y, 'Calc max');
    EvaluateByPath(asTag, x, y, 'Speed Multiplier');
    EvaluateByPath(asTag, e, m, 'DATA\Base Health');

    // evaluate Barter Gold if the Use AI Data flag is not set
    sElement := 'Barter gold';
    if wbIsOblivion then
      EvaluateByPath(asTag, x, y, sElement)
    else
      if not CompareFlags(asTag, x, y, 'Template Flags', 'Use AI Data', False, False) then
        EvaluateByPath(asTag, x, y, sElement);
  end;

  // Bookmark: Actors.AIData
  if asTag = 'Actors.AIData' then
  begin
    // assign AIDT elements
    sElement := 'AIDT';
    x := ElementBySignature(e, sElement);
    y := ElementBySignature(m, sElement);

    // evaluate AIDT properties
    EvaluateByPath(asTag, x, y, 'Aggression');
    EvaluateByPath(asTag, x, y, 'Confidence');
    EvaluateByPath(asTag, x, y, 'Energy level');
    EvaluateByPath(asTag, x, y, 'Responsibility');
    EvaluateByPath(asTag, x, y, 'Teaches');
    EvaluateByPath(asTag, x, y, 'Maximum training level');

    // check flags for Buys/Sells and Services
    if CompareNativeValues(asTag, x, y, 'Buys/Sells and Services') then
      exit;
  end;

  // Bookmark: Actors.AIPackages
  if asTag = 'Actors.AIPackages' then
    EvaluateByPath(asTag, e, m, 'Packages');

  // Bookmark: Actors.Anims
  if asTag = 'Actors.Anims' then
    EvaluateByPath(asTag, e, m, 'KFFZ');

  // Bookmark: Actors.CombatStyle
  if asTag = 'Actors.CombatStyle' then
    EvaluateByPath(asTag, e, m, 'ZNAM');

  // Bookmark: Actors.DeathItem
  if asTag = 'Actors.DeathItem' then
    EvaluateByPath(asTag, e, m, 'INAM');

  // Bookmark: Actors.Skeleton
  if asTag = 'Actors.Skeleton' then
  begin
    // assign Model elements
    sElement := 'Model';
    x := ElementByName(e, sElement);
    y := ElementByName(m, sElement);

    // exit if the Model property does not exist in the control record
    if not Assigned(x) then
      exit;

    // evaluate properties
    EvaluateByPath(asTag, x, y, 'MODL');
    EvaluateByPath(asTag, x, y, 'MODB');
    EvaluateByPath(asTag, x, y, 'MODT');
  end;

  // Bookmark: Actors.Spells
  if asTag = 'Actors.Spells' then
    EvaluateByPath(asTag, e, m, 'Spells');

  // Bookmark: Actors.Stats
  if asTag = 'Actors.Stats' then
  begin
    // assign DATA elements
    sElement := 'DATA';
    x := ElementBySignature(e, sElement);
    y := ElementBySignature(m, sElement);

    // evaluate CREA properties
    if sSignature = 'CREA' then
    begin
      EvaluateByPath(asTag, x, y, 'Health');
      EvaluateByPath(asTag, x, y, 'Combat Skill');
      EvaluateByPath(asTag, x, y, 'Magic Skill');
      EvaluateByPath(asTag, x, y, 'Stealth Skill');
      EvaluateByPath(asTag, x, y, 'Attributes');
    end;

    // evaluate NPC_ properties
    if sSignature = 'NPC_' then
    begin
      EvaluateByPath(asTag, x, y, 'Base Health');
      EvaluateByPath(asTag, x, y, 'Attributes');
      EvaluateByPath(asTag, e, m, 'DNAM\Skill Values');
      EvaluateByPath(asTag, e, m, 'DNAM\Skill Offsets');
    end;
  end;

  // Bookmark: Body-F
  if asTag = 'Body-F' then
    EvaluateByPath(asTag, e, m, 'Body Data\Female Body Data\Parts');

  // Bookmark: Body-M
  if asTag = 'Body-M' then
    EvaluateByPath(asTag, e, m, 'Body Data\Male Body Data\Parts');

  // Bookmark: Body-Size-F
  if asTag = 'Body-Size-F' then
  begin
    EvaluateByPath(asTag, e, m, 'DATA\Female Height');
    EvaluateByPath(asTag, e, m, 'DATA\Female Weight');
  end;

  // Bookmark: Body-Size-M
  if asTag = 'Body-Size-M' then
  begin
    EvaluateByPath(asTag, e, m, 'DATA\Male Height');
    EvaluateByPath(asTag, e, m, 'DATA\Male Weight');
  end;

  // Bookmark: C.Acoustic
  if asTag = 'C.Acoustic' then
    EvaluateByPath(asTag, e, m, 'XCAS');

  // Bookmark: C.Climate
  if asTag = 'C.Climate' then
  begin
    // add tag if the Behave Like Exterior flag is set ine one record but not the other
    if CompareFlags(asTag, e, m, 'DATA', 'Behave Like Exterior', True, True) then
      exit;

    // evaluate additional property
    EvaluateByPath(asTag, e, m, 'XCCM');
  end;

  // Bookmark: C.Encounter
  if asTag = 'C.Encounter' then
    EvaluateByPath(asTag, e, m, 'XEZN');

  // Bookmark: C.ImageSpace
  if asTag = 'C.ImageSpace' then
    EvaluateByPath(asTag, e, m, 'XCIM');

  // Bookmark: C.Light
  if asTag = 'C.Light' then
    EvaluateByPath(asTag, e, m, 'XCLL');

  // Bookmark: C.Location
  if asTag = 'C.Location' then
    EvaluateByPath(asTag, e, m, 'XLCN');

  // Bookmark: C.LockList
  if asTag = 'C.LockList' then
    EvaluateByPath(asTag, e, m, 'XILL');

  // Bookmark: C.Music
  if asTag = 'C.Music' then
    EvaluateByPath(asTag, e, m, 'XCMO');

  // Bookmark: FULL (C.Name, Names, SpellStats)
  if (asTag = 'C.Name') or (asTag = 'Names') or (asTag = 'SpellStats') then
    EvaluateByPath(asTag, e, m, 'FULL');

  // Bookmark: C.Owner
  if asTag = 'C.Owner' then
    EvaluateByPath(asTag, e, m, 'Ownership');

  // Bookmark: C.RecordFlags
  if asTag = 'C.RecordFlags' then
  begin
    // store Record Flags elements
    sElement := 'Record Header\Record Flags';
    x := ElementByPath(e, sElement);
    y := ElementByPath(m, sElement);

    // compare Record Flags elements
    if CompareKeys(asTag, x, y) then
      exit;
  end;

  // Bookmark: C.Regions
  if asTag = 'C.Regions' then
    EvaluateByPath(asTag, e, m, 'XCLR');

  // Bookmark: C.SkyLighting
  if asTag = 'C.SkyLighting' then
    // add tag if the Behave Like Exterior flag is set ine one record but not the other
    if CompareFlags(asTag, e, m, 'DATA', 'Use Sky Lighting', True, True) then
      exit;

  // Bookmark: C.Water
  if asTag = 'C.Water' then
  begin
    // add tag if Has Water flag is set in one record but not the other
    if CompareFlags(asTag, e, m, 'DATA', 'Has Water', True, True) then
      exit;

    // exit if Is Interior Cell is set in either record
    if CompareFlags(asTag, e, m, 'DATA', 'Is Interior Cell', False, False) then
      exit;

    // evaluate properties
    EvaluateByPath(asTag, e, m, 'XCLW');
    EvaluateByPath(asTag, e, m, 'XCWT');
  end;

  // Bookmark: Creatures.Blood
  if asTag = 'Creatures.Blood' then
  begin
    EvaluateByPath(asTag, e, m, 'NAM0');
    EvaluateByPath(asTag, e, m, 'NAM1');
  end;

  // Bookmark: Destructible
  if asTag = 'Destructible' then
  begin
    // assign Destructable elements
    sElement := 'Destructable';
    x := ElementByName(e, sElement);
    y := ElementByName(m, sElement);

    if CompareAssignment(asTag, x, y) then
      exit;

    sElement := 'DEST';
    a := ElementBySignature(x, sElement);
    b := ElementBySignature(y, sElement);

    // evaluate Destructable properties
    EvaluateByPath(asTag, a, b, 'Health');
    EvaluateByPath(asTag, a, b, 'Count');
    EvaluateByPath(asTag, x, y, 'Stages');

    // assign Destructable flags
    if not wbIsSkyrim then
    begin
      sElement := 'Flags';
      j := ElementByName(a, sElement);
      k := ElementByName(b, sElement);

      if Assigned(j) or Assigned(k) then
      begin
        // add tag if Destructable flags exist in one record
        if CompareAssignment(asTag, j, k) then
          exit;

        // evaluate Destructable flags
        if CompareKeys(asTag, j, k) then
          exit;
      end;
    end;
  end;

  // Bookmark: Eyes
  if asTag = 'Eyes' then
    EvaluateByPath(asTag, e, m, 'ENAM');

  // Bookmark: Factions
  if asTag = 'Factions' then
  begin
    // assign Factions properties
    x := ElementByName(e, asTag);
    y := ElementByName(m, asTag);

    // add tag if the Factions properties differ
    if CompareAssignment(asTag, x, y) then
      exit;

    // exit if the Factions property in the control record does not exist
    if not Assigned(x) then
      exit;

    // evaluate Factions properties
    if CompareKeys(asTag, x, y) then
      exit;
  end;

  // Bookmark: Graphics
  if asTag = 'Graphics' then
  begin
    // evaluate Icon and Model properties
    if InDelimitedList(sSignature, 'ALCH AMMO APPA BOOK INGR KEYM LIGH MGEF MISC SGST SLGM TREE WEAP', ' ') then
    begin
      EvaluateByPath(asTag, e, m, 'Icon');
      EvaluateByPath(asTag, e, m, 'Model');
    end;

    // evaluate Icon properties
    if InDelimitedList(sSignature, 'BSGN CLAS LSCR LTEX REGN', ' ') then
      EvaluateByPath(asTag, e, m, 'Icon');

    // evaluate Model properties
    if InDelimitedList(sSignature, 'ACTI DOOR FLOR FURN GRAS STAT', ' ') then
      EvaluateByPath(asTag, e, m, 'Model');

    // evaluate ARMO properties
    if sSignature = 'ARMO' then
    begin
      // Shared
      EvaluateByPath(asTag, e, m, 'Male world model');
      EvaluateByPath(asTag, e, m, 'Female world model');

      // ARMO - Oblivion
      if wbIsOblivion then
      begin
        // evaluate Icon properties
        EvaluateByPath(asTag, e, m, 'Icon');
        EvaluateByPath(asTag, e, m, 'Icon 2 (female)');

        // assign First Person Flags elements
        sElement := 'BODT\First Person Flags';

        x := ElementByPath(e, sElement);
        if not Assigned(x) then
          exit;

        y := ElementByPath(m, sElement);

        // evaluate First Person Flags
        if CompareKeys(asTag, x, y) then
          exit;

        // assign General Flags elements
        sElement := 'BODT\General Flags';

        x := ElementByPath(e, sElement);
        if not Assigned(x) then
          exit;

        y := ElementByPath(m, sElement);

        // evaluate General Flags
        if CompareKeys(asTag, x, y) then
          exit;
      end;

      // ARMO - FO3, FNV
      if wbIsFallout3 or wbIsFalloutNV then
      begin
        // evaluate Icon properties
        EvaluateByPath(asTag, e, m, 'ICON');
        EvaluateByPath(asTag, e, m, 'ICO2');

        // assign First Person Flags elements
        sElement := 'BMDT\Biped Flags';

        x := ElementByPath(e, sElement);
        if not Assigned(x) then
          exit;

        y := ElementByPath(m, sElement);

        // evaluate First Person Flags
        if CompareKeys(asTag, x, y) then
          exit;

        // assign General Flags elements
        sElement := 'BMDT\General Flags';

        x := ElementByPath(e, sElement);
        if not Assigned(x) then
          exit;

        y := ElementByPath(m, sElement);

        // evaluate General Flags
        if CompareKeys(asTag, x, y) then
          exit;
      end;

      // ARMO - TES5
      if wbIsSkyrim then
      begin
        // evaluate Icon properties
        EvaluateByPath(asTag, e, m, 'Icon');
        EvaluateByPath(asTag, e, m, 'Icon 2 (female)');

        // evaluate Biped Model properties
        EvaluateByPath(asTag, e, m, 'Male world model');
        EvaluateByPath(asTag, e, m, 'Female world model');

        // assign First Person Flags elements
        sElement := 'BOD2\First Person Flags';

        x  := ElementByPath(e, sElement);
        if not Assigned(x) then
          exit;

        y := ElementByPath(m, sElement);

        // evaluate First Person Flags
        if CompareKeys(asTag, x, y) then
          exit;

        // assign General Flags elements
        sElement := 'BOD2\General Flags';

        x := ElementByPath(e, sElement);
        if not Assigned(x) then
          exit;

        y := ElementByPath(m, sElement);

        // evaluate General Flags
        if CompareKeys(asTag, x, y) then
          exit;
      end;
    end;

    // evaluate CREA properties
    if sSignature = 'CREA' then
    begin
      EvaluateByPath(asTag, e, m, 'NIFZ');
      EvaluateByPath(asTag, e, m, 'NIFT');
    end;

    // evaluate EFSH properties
    if sSignature = 'EFSH' then
    begin
      // evaluate Record Flags
      sElement := 'Record Header\Record Flags';

      x := ElementByPath(e, sElement);
      y := ElementByPath(m, sElement);

      if CompareKeys(asTag, x, y) then
        exit;

      // evaluate Icon properties
      EvaluateByPath(asTag, e, m, 'ICON');
      EvaluateByPath(asTag, e, m, 'ICO2');

      // evaluate other properties
      EvaluateByPath(asTag, e, m, 'NAM7');

      if wbIsSkyrim then
      begin
        EvaluateByPath(asTag, e, m, 'NAM8');
        EvaluateByPath(asTag, e, m, 'NAM9');
      end;

      EvaluateByPath(asTag, e, m, 'DATA');
    end;

    // evaluate MGEF properties
    if wbIsSkyrim then
      if sSignature = 'MGEF' then
      begin
        EvaluateByPath(asTag, e, m, 'Magic Effect Data\DATA\Casting Light');
        EvaluateByPath(asTag, e, m, 'Magic Effect Data\DATA\Hit Shader');
        EvaluateByPath(asTag, e, m, 'Magic Effect Data\DATA\Enchant Shader');
      end;

    // evaluate Material property
    if sSignature = 'STAT' then
      EvaluateByPath(asTag, e, m, 'DNAM\Material');
  end;

  // Bookmark: Hair
  if asTag = 'Hair' then
    EvaluateByPath(asTag, e, m, 'HNAM');

  // Bookmark: Invent
  if asTag = 'Invent' then
  begin
    // assign Items properties
    sElement := 'Items';

    x := ElementByName(e, sElement);
    y := ElementByName(m, sElement);

    // add tag if Items properties exist in one record but not the other
    if CompareAssignment(asTag, x, y) then
      exit;

    // exit if Items property does not exist in control record
    if not Assigned(x) then
      exit;

    // Items are sorted, so we don't need to compare by individual item
    // SortKey combines all the items data
    if CompareKeys(asTag, x, y) then
      exit;
  end;

  // Bookmark: Keywords
  if asTag = 'Keywords' then
  begin
    x := ElementBySignature(e, 'KWDA');
    y := ElementBySignature(m, 'KWDA');

    if CompareAssignment(asTag, x, y) then
      exit;

    if CompareElementCount(asTag, x, y) then
      exit;

    x := ElementBySignature(e, 'KSIZ');
    y := ElementBySignature(m, 'KSIZ');

    if CompareAssignment(asTag, x, y) then
      exit;

    if CompareEditValue(asTag, x, y) then
      exit;
  end;

  // Bookmark: NPC.Class
  if asTag = 'NPC.Class' then
    EvaluateByPath(asTag, e, m, 'CNAM');

  // Bookmark: NPC.Race
  if asTag = 'NPC.Race' then
    EvaluateByPath(asTag, e, m, 'RNAM');

  // Bookmark: NpcFaces
  if asTag = 'NpcFaces' then
  begin
    EvaluateByPath(asTag, e, m, 'HNAM');
    EvaluateByPath(asTag, e, m, 'LNAM');
    EvaluateByPath(asTag, e, m, 'ENAM');
    EvaluateByPath(asTag, e, m, 'HCLR');
    EvaluateByPath(asTag, e, m, 'FaceGen Data');
  end;

  // Bookmark: ObjectBounds
  if asTag = 'ObjectBounds' then
    EvaluateByPath(asTag, e, m, 'OBND');

  // Bookmark: R.Attributes-F
  if asTag = 'R.Attributes-F' then
    EvaluateByPath(asTag, e, m, 'ATTR\Female');

  // Bookmark: R.Attributes-M
  if asTag = 'R.Attributes-M' then
    EvaluateByPath(asTag, e, m, 'ATTR\Male');

  // Bookmark: R.ChangeSpells
  if asTag = 'R.ChangeSpells' then
    EvaluateByPath(asTag, e, m, 'Spells');

  // Bookmark: R.Description
  if asTag = 'R.Description' then
    EvaluateByPath(asTag, e, m, 'DESC');

  // Bookmark: R.Ears
  if asTag = 'R.Ears' then
  begin
    EvaluateByPath(asTag, e, m, 'Head Data\Male Head Data\Parts\[1]');
    EvaluateByPath(asTag, e, m, 'Head Data\Female Head Data\Parts\[1]');
  end;

  // Bookmark: R.Head
  if asTag = 'R.Head' then
  begin
    EvaluateByPath(asTag, e, m, 'Head Data\Male Head Data\Parts\[0]');
    EvaluateByPath(asTag, e, m, 'Head Data\Female Head Data\Parts\[0]');
    EvaluateByPath(asTag, e, m, 'FaceGen Data');
  end;

  // Bookmark: R.Mouth
  if asTag = 'R.Mouth' then
  begin
    EvaluateByPath(asTag, e, m, 'Head Data\Male Head Data\Parts\[2]');
    EvaluateByPath(asTag, e, m, 'Head Data\Female Head Data\Parts\[2]');
  end;

  // Bookmark: R.Relations
  if asTag = 'R.Relations' then
    EvaluateByPath(asTag, e, m, 'Relations');

  // Bookmark: R.Skills
  if asTag = 'R.Skills' then
    EvaluateByPath(asTag, e, m, 'DATA\Skill Boosts');

  // Bookmark: R.Teeth
  if asTag = 'R.Teeth' then
  begin
    EvaluateByPath(asTag, e, m, 'Head Data\Male Head Data\Parts\[3]');
    EvaluateByPath(asTag, e, m, 'Head Data\Female Head Data\Parts\[3]');

    // FO3
    if wbIsFallout3 then
    begin
      EvaluateByPath(asTag, e, m, 'Head Data\Male Head Data\Parts\[4]');
      EvaluateByPath(asTag, e, m, 'Head Data\Female Head Data\Parts\[4]');
    end;
  end;

  // Bookmark: Relations
  if asTag = 'Relations' then
    EvaluateByPath(asTag, e, m, 'Relations');

  // Bookmark: Roads
  if asTag = 'Roads' then
    EvaluateByPath(asTag, e, m, 'PGRP');

  // Bookmark: Scripts
  if asTag = 'Scripts' then
    EvaluateByPath(asTag, e, m, 'SCRI');

  // Bookmark: Sound
  if asTag = 'Sound' then
  begin
    // Activators, Containers, Doors, and Lights
    if (sSignature = 'ACTI')
    or (sSignature = 'CONT')
    or (sSignature = 'DOOR')
    or (sSignature = 'LIGH') then
    begin
      EvaluateByPath(asTag, e, m, 'SNAM');

      // Activators
      if sSignature = 'ACTI' then
        EvaluateByPath(asTag, e, m, 'VNAM');

      // Containers
      if sSignature = 'CONT' then
      begin
        EvaluateByPath(asTag, e, m, 'QNAM');
        if not wbIsSkyrim and not wbIsFallout3 then
          EvaluateByPath(asTag, e, m, 'RNAM'); // FO3, TESV, and SSE don't have this element
      end;

      // Doors
      if sSignature = 'DOOR' then
      begin
        EvaluateByPath(asTag, e, m, 'ANAM');
        EvaluateByPath(asTag, e, m, 'BNAM');
      end;
    end;

    // Creatures
    if sSignature = 'CREA' then
    begin
      EvaluateByPath(asTag, e, m, 'WNAM');
      EvaluateByPath(asTag, e, m, 'CSCR');
      EvaluateByPath(asTag, e, m, 'Sound Types');
    end;

    // Magic Effects
    if sSignature = 'MGEF' then
    begin
      // TES5, SSE
      if wbIsSkyrim then
        EvaluateByPath(asTag, e, m, 'SNDD');

      // FO3, FNV, TES4
      if not wbIsSkyrim then
      begin
        EvaluateByPath(asTag, e, m, 'DATA\Effect sound');
        EvaluateByPath(asTag, e, m, 'DATA\Bolt sound');
        EvaluateByPath(asTag, e, m, 'DATA\Hit sound');
        EvaluateByPath(asTag, e, m, 'DATA\Area sound');
      end;
    end;

    // Weather
    if sSignature = 'WTHR' then
      EvaluateByPath(asTag, e, m, 'Sounds');
  end;

  // Bookmark: SpellStats
  if asTag = 'SpellStats' then
    EvaluateByPath(asTag, e, m, 'SPIT');

  // Bookmark: Stats
  if asTag = 'Stats' then
  begin
    if InDelimitedList(sSignature, 'ALCH AMMO APPA ARMO BOOK CLOT INGR KEYM LIGH MISC SGST SLGM WEAP', ' ') then
    begin
      EvaluateByPath(asTag, e, m, 'EDID');
      EvaluateByPath(asTag, e, m, 'DATA');

      if InDelimitedList(sSignature, 'ARMO WEAP', ' ') then
        EvaluateByPath(asTag, e, m, 'DNAM');

      if sSignature = 'WEAP' then
        EvaluateByPath(asTag, e, m, 'CRDT');
    end;

    if sSignature = 'ARMA' then
      EvaluateByPath(asTag, e, m, 'DNAM');
  end;

  // Bookmark: Text
  if asTag = 'Text' then
  begin
    if InDelimitedList(sSignature, 'ALCH AMMO APPA ARMO AVIF BOOK BSGN CHAL CLAS IMOD LSCR MESG MGEF PERK SCRL SHOU SKIL SPEL TERM WEAP', ' ') then
      EvaluateByPath(asTag, e, m, 'DESC');

    if not wbIsOblivion then begin
      if sSignature = 'BOOK' then
        EvaluateByPath(asTag, e, m, 'CNAM');

      if sSignature = 'MGEF' then
        EvaluateByPath(asTag, e, m, 'DNAM');

      if sSignature = 'NOTE' then
        EvaluateByPath(asTag, e, m, 'TNAM');
    end;
  end;

  // Bookmark: Voice-F
  if asTag = 'Voice-F' then
    EvaluateByPath(asTag, e, m, 'VTCK\Voice #1 (Female)');

  // Bookmark: Voice-M
  if asTag = 'Voice-M' then
    EvaluateByPath(asTag, e, m, 'VTCK\Voice #0 (Male)');

  // Bookmark: WeaponMods
  if asTag = 'WeaponMods' then
    EvaluateByPath(asTag, e, m, 'Weapon Mods');
end;

// Bookmark: Delev, Relev
procedure ProcessDelevRelevTags(e, m: IInterface);
var
  kEntries       : IInterface;
  kEntriesMaster : IInterface;
  kEntry         : IInterface;
  kEntryMaster   : IInterface;
  kCOED          : IInterface; // extra data
  kCOEDMaster    : IInterface; // extra data
  sElement       : String;
  sSortKey       : String;
  sSortKeyMaster : String;
  sTag           : String;
  i              : Integer;
  j              : Integer;
begin
  // nothing to do if already tagged
  if TagExists('Delev') and TagExists('Relev') then
    exit;

  // get Leveled List Entries
  sElement := 'Leveled List Entries';
  kEntries := ElementByName(e, sElement);
  kEntriesMaster := ElementByName(m, sElement);

  if not Assigned(kEntries)
  or not Assigned(kEntriesMaster) then
    exit;

  // initalize count matched on reference entries
  j := 0;

  // iterate through all entries
  for i := 0 to Pred(ElementCount(kEntries)) do
  begin
    sElement := 'LVLO\Reference';
    kEntry := ElementByIndex(kEntries, i);
    kEntryMaster := SortedArrayElementByValue(kEntriesMaster, sElement, GetElementEditValues(kEntry, sElement));

    if Assigned(kEntryMaster) then
    begin
      Inc(j);

      sTag := 'Relev';
      if not TagExists(sTag) then
      begin
        if CompareNativeValues(sTag, kEntry, kEntryMaster, 'LVLO\Level')
        or CompareNativeValues(sTag, kEntry, kEntryMaster, 'LVLO\Count') then
          exit;

        // Relev check for changed level, count, extra data
        if not wbIsOblivion then
        begin
          sElement := 'COED';
          kCOED := ElementBySignature(kEntry, sElement);
          kCOEDMaster := ElementBySignature(kEntryMaster, sElement);

          sSortKey := SortKeyEx(kCOED);
          sSortKeyMaster := SortKeyEx(kCOEDMaster);

          if not SameText(sSortKey, sSortKeyMaster) then
          begin
            if optionOutputLog = mrYes then
              AddLogEntry(sTag, 'Assigned', sSortKey, sSortKeyMaster);
            AddTag(sTag);
            exit;
          end;
        end;
      end;
    end;
  end;

  // if number of matched entries less than in master list
  sTag := 'Delev';
  if not TagExists(sTag) then
    if j < ElementCount(kEntriesMaster) then
    begin
      if optionOutputLog = mrYes then
        AddLogEntry(sTag, 'ElementCount', kEntries, kEntriesMaster);
      AddTag(sTag);
      exit;
    end;
end;

function AddLogEntry(asTag: String; asTestName: String; e, m: IInterface): String;
var
  sPrefix : String;
  sString : String;
begin
  if optionOutputLog = mrNo then
    exit;

  sPrefix := '(' + asTestName + ' Test)' + ' ' + asTag + ': ';

  slLog.Add(sPrefix + PathName(m));
  slLog.Add(sPrefix + PathName(e));
end;


function FileByName(asFileName: String): IwbFile;
var
  kFile : IwbFile;
  i     : Integer;
begin
  Result := nil;

  for i := 0 to Pred(FileCount) do
  begin
    kFile := FileByIndex(i);
    if asFileName = GetFileName(kFile) then
    begin
      Result := kFile;
      exit;
    end;
  end;
end;


procedure EscKeyHandler(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = 27 then
    Sender.Close;
end;


procedure chkAddTagsClick(Sender: TObject);
begin
  if optionAddTags = mrYes then
    optionAddTags := mrNo
  else
    optionAddTags := mrYes;
end;


procedure chkLoggingClick(Sender: TObject);
begin
  if optionOutputLog = mrYes then
    optionOutputLog := mrNo
  else
    optionOutputLog := mrYes;
end;


function Configure(asCaption: String): IwbFile;
var
  frm        : TForm;
  lblPlugins : TLabel;
  chkAddTags : TCheckBox;
  chkLogging : TCheckBox;
  cbbPlugins : TComboBox;
  btnCancel  : TButton;
  btnOk      : TButton;
  i          : Integer;
  iGuiPlugin : Integer;

  kFile      : IwbFile;

  sName : String;

begin

  Result := nil;

  frm := TForm.Create(TForm(frmMain));

  try
    frm.Caption      := asCaption;
    frm.BorderStyle  := bsToolWindow;
    frm.ClientWidth  := 234 * scaleFactor;
    frm.ClientHeight := 154 * scaleFactor;
    frm.Position     := poScreenCenter;
    frm.KeyPreview   := True;
    frm.OnKeyDown    := EscKeyHandler;

    lblPlugins := TLabel.Create(frm);
    lblPlugins.Parent   := frm;
    lblPlugins.Left     := 16 * scaleFactor;
    lblPlugins.Top      := 64 * scaleFactor;
    lblPlugins.Width    := 200 * scaleFactor;
    lblPlugins.Height   := 16 * scaleFactor;
    lblPlugins.Caption  := 'Select file to analyze:';
    lblPlugins.AutoSize := False;

    chkAddTags := TCheckBox.Create(frm);
    chkAddTags.Parent   := frm;
    chkAddTags.Left     := 16 * scaleFactor;
    chkAddTags.Top      := 16 * scaleFactor;
    chkAddTags.Width    := 185 * scaleFactor;
    chkAddTags.Height   := 16 * scaleFactor;
    chkAddTags.Caption  := 'Write suggested tags to header';
    chkAddTags.Checked  := False;
    chkAddTags.OnClick  := chkAddTagsClick;
    chkAddTags.TabOrder := 0;

    chkLogging := TCheckBox.Create(frm);
    chkLogging.Parent   := frm;
    chkLogging.Left     := 16 * scaleFactor;
    chkLogging.Top      := 39 * scaleFactor;
    chkLogging.Width    := 185 * scaleFactor;
    chkLogging.Height   := 16 * scaleFactor;
    chkLogging.Caption  := 'Log test results to Messages tab';
    chkLogging.Checked  := True;
    chkLogging.OnClick  := chkLoggingClick;
    chkLogging.TabOrder := 1;

    cbbPlugins := TComboBox.Create(frm);
    cbbPlugins.Parent         := frm;
    cbbPlugins.Left           := 16 * scaleFactor;
    cbbPlugins.Top            := 85 * scaleFactor;
    cbbPlugins.Width          := 200 * scaleFactor;
    cbbPlugins.Height         := 21 * scaleFactor;
    cbbPlugins.Style          := csDropDownList;
    cbbPlugins.DoubleBuffered := True;
    cbbPlugins.TabOrder       := 2;

    iGuiPlugin := -1;
    for i := 0 to Pred(FileCount) do
    begin
      kFile := FileByIndex(i);
      if IsEditable(kFile) then
      begin
        sName := GetFileName(kFile);
        cbbPlugins.Items.Add(sName);
        if CompareText(sName, sGuiPlugin) = 0 then
          iGuiPlugin := Pred(cbbPlugins.Items.Count);
      end;
    end;

    if iGuiPlugin = -1 then
    begin
      cbbPlugins.ItemIndex      := Pred(cbbPlugins.Items.Count);
    end else begin
      cbbPlugins.ItemIndex      := iGuiPlugin;
    end

    btnOk := TButton.Create(frm);
    btnOk.Parent              := frm;
    btnOk.Left                := 62 * scaleFactor;
    btnOk.Top                 := 120 * scaleFactor;
    btnOk.Width               := 75 * scaleFactor;
    btnOk.Height              := 25 * scaleFactor;
    btnOk.Caption             := 'Run';
    btnOk.Default             := True;
    btnOk.ModalResult         := mrOk;
    btnOk.TabOrder            := 3;

    btnCancel := TButton.Create(frm);
    btnCancel.Parent          := frm;
    btnCancel.Left            := 143 * scaleFactor;
    btnCancel.Top             := 120 * scaleFactor;
    btnCancel.Width           := 75 * scaleFactor;
    btnCancel.Height          := 25 * scaleFactor;
    btnCancel.Caption         := 'Abort';
    btnCancel.ModalResult     := mrAbort;
    btnCancel.TabOrder        := 4;

    if frm.ShowModal = mrOk then
      Result := FileByName(cbbPlugins.Text);
  finally
    frm.Free;
  end;
end;

end.
