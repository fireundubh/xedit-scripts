# Fallout 4

## `Add Legendary Object Mod Rules.pas`

Automatically add object mod rules to `LegendaryItemQuest [QUST:001CCDA5]` from a delimited text file.

**IMPORTANT: You must have the latest build of xEdit and Simple Records must be unchecked in the Options!**

Before you do anything in xEdit, create a text file with a list of OMOD/FLST pairs formatted like this:

```
mod_Legendary_Weapon_Sadistic [OMOD:01000800];LegendaryModRule_AllowedKeywords_WeaponTypeGun [FLST:001CCDA7]
mod_Legendary_Weapon_Sadistic [OMOD:01000800];LegendaryModRule_AllowedKeywords_WeaponTypeMeleeAndHandToHand [FLST:001EC03A]
```

Or this:

```
[OMOD:01000800];[FLST:001CCDA7]
[OMOD:01000800];[FLST:001EC03A]
```

Save the text file with the `.csv` extension.

1. Load the plugins that contain the OMOD records listed in the text file.
2. Right-click anywhere in the tree control and apply the script.
3. Select the target plugin from the dropdown menu. This is where your new rules will be written.
4. Click OK.
5. Select the rules file you created.
6. Click Open.


## `Generate Loose Mods for Selected OMOD Records.pas`

Automatically generate and assign MISC loose mods for the selected OMOD records.

1. Ensure that the sEditorPrefix is desired.
2. Select one or many OMOD records, and apply the script.
3. Select the file where the new MISC records should be saved.


## `Generate Scrap Recipes for Selected Object Records.pas`

Automatically generate COBJ and FLST records to make any object scrappable.

1. Create a new plugin, or load an editable plugin.
2. Under Fallout4.esm, find the records you want to make scrappable.
3. Select a single record, or hold shift and select multiple records.
4. Right-click any selected record and apply the script.
5. Select the target plugin from the dropdown menu.
6. Click OK.
7. Type the object name using alphanumeric characters. Spaces will be removed. You are advised to use the generic version of an object's Editor ID (e.g., Bramble) for organizational purposes.
8. Click OK.

If there are no errors, the script will create COBJ and FLST records in the target plugin, as well as named overrides of the scrappable object records.


## `Restore Material Swap Fields.pas`

An older build of xEdit did not copy over MODS fields when copying records as new or overrides. This script restores those fields, if the Editor IDs of the selected records match the Editor IDs of the source records. If the EDID fields do not match, the MODS fields will have to be restored manually.

1. Load the plugin you want to fix.
2. Select a single record, or hold shift and select multiple records.
3. Right-click any selected record and apply the script.
4. Select the source plugin from the dropdown menu.
5. Click OK.


## `Unlevel Perks.pas`

Generates a new "unleveled perks" mod using your load order

1. Copy `UnleveledPerks.esp` to your `Data` or `overwrite` folder.
2. Reorder `UnleveledPerks.esp` so that the plugin is last in your load order.
3. Load all plugins in xEdit, including `UnleveledPerks.esp`.
4. Expand `UnleveledPerks.esp`.
5. Expand the FormID List category.
6. Right-click the `dubhFirstLevelPerks` record and apply the script.

The script will copy the last overriding version of each record in the formlist, and unlevel each record accordingly.

By default, records that would be "identical to master" are not copied.