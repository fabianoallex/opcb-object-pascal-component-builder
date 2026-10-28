# Changelog

## v0.2.0 - 2026-10-28

### Added

- **Per-class default properties: `SetDefaultProp`/`UnsetDefaultProps`.**
  `SetDefaultProp(AClass, APropName, AValue)` registers a property value applied
  to every control of that class or a descendant added afterwards, unless its
  builder sets that property itself (through a typed method like `WithHeight` or
  through `WithProp`). When defaults for several classes match, the closest class
  wins; for the same class, the last one registered wins. Defaults belong to the
  current level: nested `SubLevel`s inherit them, and ones set inside a `SubLevel`
  don't leak back after `SuperLevel`. `UnsetDefaultProps(AClass)` removes the
  defaults registered for exactly that class.
- `Name`, `Tag`, `Owner`, `Parent` and `OnClick` are never applied as defaults. In
  a grid, `Width`, `Height`, `Top`, `Left` and `Align` are skipped too, because the
  grid computes them.
- `SetControlHeight`/`SetControlWidth` still win over a `SetDefaultProp` default
  for `Height`/`Width`.

### Tests

- 22 new tests in both suites (DUnitX for the VCL, FPCUnit for the LCL).

## v0.1.0 - 2026-10-28

First tagged version. Everything up to commit `b61c993` plus the two fixes below,
both found by pascal-agenda-faa (a desktop client whose screens are built in code
with OPCB, one source for the VCL and the LCL).

### Fixed

- **A control without a height in a grid cell was placed as if it were 0 high.**
  With the default cell position (`cpCenter`) and no vertical stretch, its top
  went to the middle of the cell and its height to 0; the widgetset then gave it
  its own height (a `TEdit` 23), so it overflowed the cell, the last row went past
  the grid's bottom, and what followed the grid started over its last row. The
  same for the width.

  For each dimension a control in a cell now takes, in this order: the cell's size
  when the grid stretches that dimension; the builder's size; the level's default
  size (`SetControlHeight`/`SetControlWidth`); the control's own size once it's
  built. It is then positioned in the cell (`GridCellPosition`) from that size.

  Both options considered are used, as a chain, because each covers a case the
  other doesn't: the default size, applied before the cell rect is computed, is
  what a form usually wants (every edit the same height) and gives an exact layout;
  the control's own size covers a control with neither (its class default or its
  `AutoSize`, which differ between the VCL and the LCL, so OPCB can't guess them
  up front and reads them after building the control).

  Related changes:
  - The default size no longer resizes a control after it was placed in a cell, and
    never overrides a stretched dimension. In a cell the builder's size wins over
    the default (outside a grid the default still replaces it, as before).
  - The default size set on the level `GridInit` is called on now reaches the grid's
    controls (the grid's internal level used to start without one).
  - After `GridFinish`, what follows in the outer level (after a `Break`/`BreakLine`
    too) starts past the grid's occupied cells. It used to follow the grid's
    controls measured from the first one's top, which put the next line over the
    grid when the controls didn't fill their cells.

- **`RecalcParentHeight`/`RecalcParentWidth`/`RecalcParentSize` set the parent's
  outer size.** The controls' coordinates are relative to the client area, but a
  VCL form's `Height`/`Width` include its caption and borders: a `bsDialog` form
  came out 39 pixels short on Windows 11, cutting the bottom row of buttons. A
  panel with a window border (`BorderStyle = bsSingle`) had the same problem on the
  VCL and the LCL. They now set `ClientHeight`/`ClientWidth` (VCL and LCL). FMX is
  unchanged: there a level's parent is a `TControl`, with no non-client area.

### Tests

- New tests in both suites (DUnitX for the VCL, FPCUnit for the LCL) for both fixes.
- The Delphi test project now finds the OPCB sources through its own search path
  (`..\..\..\src`) instead of depending on the IDE's library path.
- The Lazarus console runner initializes the LCL widgetset, so tests that need a
  window handle run there too.
