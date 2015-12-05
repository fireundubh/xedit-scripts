# fireundubh's xEdit Scripts

This is a repository of fireundubh's "released" scripts for xEdit.

Other scripts can be found [here](https://drive.google.com/folderview?id=0B0o3cG8Q52tpeTd1VjZ0QU5TN1E&usp=sharing#list).

## Libraries

### dubhFunctions.pas

A collection of functions and procedures that make my life easier.

All of my scripts require dubhFunctions, which in turn requires [mteFunctions](https://github.com/matortheeternal/TES5EditScripts/blob/master/Edit%20Scripts/mteFunctions.pas).

## Fallout 4

### Generate Scrap Recipes for Selected Object Records.pas

Automatically generate COBJ and FLST records to make any object scrappable.

1. Create a new plugin, or load an editable plugin.
2. Under Fallout4.esm, find the records you want to make scrappable.
3. Select a single record, or hold shift and select multiple records.
4. Right-click any selected record and apply the script.
5. Select the target plugin from the dropdown menu.
6. Click OK.
7. Type the object name using alphanumeric characters. Spaces will be removed. You are advised to use the generic version of an object's Editor ID (e.g., Bramble) for organizational purposes.
8. Click OK.

If there are no errors, the script will create COBJ and FLST records in the
target plugin, as well as named overrides of the scrappable object records.

### Restore Material Swap Fields

An older build of xEdit did not copy over MODS fields when copying records as new or overrides. This script restores those fields, if the Editor IDs of the selected records match the Editor IDs of the source records. If the EDID fields do not match, the MODS fields will have to be restored manually.

1. Load the plugin you want to fix.
2. Select a single record, or hold shift and select multiple records.
3. Right-click any selected record and apply the script.
4. Select the source plugin from the dropdown menu.
5. Click OK.

## All Games

### Generate Tags for Wrye Bash.pas

Automatically generate tags for Wrye Bash by processing all records in any plugin for all Bethesda Softworks games.

1. Load only the plugin for which you want to generate tags.
2. Right-click the name of the plugin and apply the script.
3. Choose Yes, No, or Abort on the subsequent prompt.
4. Wait until the script finishes executing.

Support for Fallout 4 is not yet implemented.
