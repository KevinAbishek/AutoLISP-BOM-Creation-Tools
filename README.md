# AutoLISP BOM Creation Tools for AutoCAD
A lightweight AutoLISP utility for generating a consolidated Bill of Materials (BOM) directly inside base AutoCAD by reading block attributes and grouping related blocks into assemblies.

Designed for electrical, pneumatic, and P&ID-style schematics where components are represented as blocks and need to be counted and summarized without using specialized AutoCAD toolsets.

## What this does
- Creates a formatted BOM TABLE entity in AutoCAD
- Reads block attributes from a user-defined selection
- Groups multiple blocks into a single assembly using a parent–child block relationship
- Merges repeated assemblies and increments quantities automatically
- Writes results back into the drawing while preserving table formatting

## Files in this repo
1. GolbalVars.lsp
  Defines global attribute tags and loads COM support.

2. CreateBomTable.lsp
  Command: CreateBomTable
  Inserts a formatted BOM table (title + headers).

3. RebuildBomTable.lsp
  Command: RebuildBomTable
  Scans selected blocks, builds the BOM internally, and appends rows into the table.

## Block attribute conventions
### Parent blocks (assemblies)
Required attributes:
1. SYMBOL – assembly identifier (e.g. Connector number 4 in the main control unit may have the symbol "MCU-CONN4". No formatting needed, symbols are treated as string identifiers.)
2. PARTNUM
3. DESCRIPTION

### Child blocks (components)
Required attributes:
1. PARENT_SYMBOL – must match the parent’s SYMBOL
2. PARTNUM
3. DESCRIPTION

Standalone parts are simply parent blocks with no matching children.

## How assemblies are detected
- Blocks with SYMBOL are treated as parents
- Blocks with PARENT_SYMBOL are treated as children
- Children are grouped under parents when PARENT_SYMBOL == SYMBOL
- A unique “assembly fingerprint” (based on part numbers) is used to detect duplicates
- Duplicate assemblies increment quantities instead of creating new rows

## Output format
The BOM table columns are:

| SYMBOL | PART NUMBER | DESCRIPTION | QUANTITY |
|---------|----------|----------|----------|

First row is title of BOM table, columns merged. Text style, height, and alignment are copied from an existing table row so appended rows match the drawing’s formatting.

## Customization
All attribute tag names and part-number prefix logic can be modified in: GolbalVars.lsp

Sorting logic assumes part numbers contain a numeric suffix.
If your numbering scheme differs, update the sorting helpers in RebuildBomTable.lsp.
