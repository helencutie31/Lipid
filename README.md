# lipidflowshiny

Web interface for the LipidFlow lipidomics ecosystem. Built in the order
decided along the way: navigation shell + Home, then Quantification
(fastest to wire up since lipidflow already had it), then Peak Picking
(`massprocesser`). Lipid Annotation (`metid` + MS-DIAL's MS/MS libraries)
is the one remaining stub - see "Next module" below.

## Current state

| Tab | Status |
|---|---|
| Home | Done - overview, module status, links into Quantification |
| Peak Picking | Functional - wraps `massprocesser::process_data()` |
| Lipid Annotation | Stub, with the decided approach documented in-app - see below |
| Quantification | Functional - wraps `lipidflow::get_lipid_absolute_quantification()` |

The Peak Picking and Quantification "Data folder on the server" fields are
now synced (`shared_data_path` in `app_server.R`) - type it in one tab, it
appears in the other, since both operate on the same `path/POS,NEG/...`
folder.

## Decision log

**Build order (Home + Quantification, then Peak Picking, Annotation last):**
validated in practice, not just in theory - `massprocesser` turned out to
expect the exact same `path/POS/<group>/*.mzXML` layout Quantification
already used, confirmed by reading its source rather than assumed. Building
Quantification first happened to lock in the right shared convention before
Peak Picking existed to get it wrong independently.

**Tool choice for Peak Picking - `massprocesser`:** straightforward yes,
matches what was already planned before any outside input, for the reasons
in the "How this maps" section for lipidflow: it's the TidyMass ecosystem's
own xcms wrapper, already something of a known quantity here.

**Tool choice for Annotation - `metid` + MS-DIAL's public MS/MS lipid
libraries (instead of the originally-planned Bioconductor
`MetaboAnnotation` + LIPID MAPS route):** the better call, for a concrete
reason beyond familiarity - `metid` is built by the same author/ecosystem as
`massdataset`/`massprocesser`, so it consumes the `mass_dataset` object
Peak Picking produces with no object-model conversion layer, unlike
`MetaboAnnotation`'s `Spectra`/`MsExperiment` model. MS2 spectral matching
against a library that large and lipid-specific also beats MS1-only m/z
matching on precision - lipid isomers/isobars are exactly where m/z-only
matching is weakest. Two open items to resolve before building it:
1. This path is MS2-dependent - features without triggered fragmentation
   get no annotation this way. A lightweight MS1-only fallback (even just
   `MetaboAnnotation::matchMz()` against LIPID MAPS as a secondary pass)
   is probably still worth adding later for full feature coverage.
2. Confirm MS-DIAL's MSP library terms cover reuse as an input reference
   inside a separate redistributed tool, not just use within MS-DIAL
   itself, before shipping it as a dependency.

## How massprocesser maps to this app

Same approach as lipidflow: read the actual source
(`tidymass/massprocesser`) before building against it.

- `process_data()` handles **one polarity per call** - unlike lipidflow's
  wrapper, it doesn't take POS and NEG together. `mod_peak_picking.R` calls
  it twice inside one background job: once with `path = <data>/POS,
  polarity = "positive"`, once with `path = <data>/NEG, polarity =
  "negative"`.
- Sample "group" comes from the same convention as lipidflow: the immediate
  subfolder name holding each raw file. No separate sample sheet.
- Output lands in `<data>/POS/Result/` and `<data>/NEG/Result/` respectively:
  `Peak_table.csv`, `Peak_table_for_cleaning.csv`, optional `TIC.pdf` /
  `BPC.pdf` / `RT correction plot.pdf`, and critically an `object` file - a
  saved `mass_dataset` (from `massdataset`) that the Annotation module will
  eventually consume directly.
- `process_data()` caches intermediate state
  (`Result/intermediate_data/{raw_data,xdata,xdata2,xdata3}`) and silently
  reuses it on a re-run - there's no `rerun=` flag like lipidflow has. The
  "Clear cached intermediate data" checkbox in the UI deletes that folder
  before launching, since that's the only way to force a genuine redo.

## How this maps to the real lipidflow package

Before writing any UI I cloned `jaspershen/lipidflow` and read the actual
source rather than guessing the interface. Worth knowing, since it shaped
every design decision below:

- `get_lipid_absolute_quantification()` is a single function that runs all
  4 internal steps (IS retention time -> relative quant -> absolute quant ->
  output/plots) for you. The UI calls this one function; it does not
  orchestrate the 4 steps separately.
- It expects a specific folder layout under one `path`:
  ```
  path/
    POS/
      IS_information.xlsx
      lipid_annotation_table_pos.xlsx
      <group>/*.mzXML        e.g. POS/D25/D25_1.mzXML
      <group>/*.mzXML        e.g. POS/M19/M19_1.mzXML
    NEG/
      ... same idea
  ```
  The "group" is literally the subfolder name holding each sample's mzXML
  file - lipidflow parses `group` and `sample.name` straight out of that
  path, so the folder structure **is** the sample sheet. There's no
  separate sample-info upload.
- `match_item_pos` / `match_item_neg` are R named lists mapping lipid class
  -> internal standard name(s) (e.g. `"PC" = "15:0-18:1(d7) PC"`). This
  isn't a file, it's a parameter - see "match_item builder" below for how
  the UI constructs it.
- The only currently-required annotation source is a LipidSearch-style
  export. There is no open annotation step in lipidflow itself - that's
  exactly the gap the (future) Annotation tab is meant to fill.

## Key design decisions (and why)

**Why a server path instead of uploading raw data.** Raw LC-MS batches run
hundreds of MB to multiple GB - routing that through a browser upload to
Shiny is slow and fights `shiny.maxRequestSize` for no benefit, especially
given you already place large files on the server via `rsync`/`scp` (same
as the MTBLS/MassIVE downloads and the mapa embedding database). So: raw
data lives at a server path you type in, and only the small xlsx config
files go through the browser. If you'd rather support drag-and-drop of a
zipped POS/NEG tree for smaller test datasets, that's a contained addition
to `mod_quantification.R`'s upload section - the rest of the pipeline
doesn't change.

**Why the lipid-class column is picked, not assumed.** LipidSearch export
schemas vary by version/settings, and I didn't want to hardcode a column
name I hadn't verified against your actual files. The UI reads the
uploaded annotation table's column names and asks you to confirm which one
is the class column, then reads unique values from it. Once you know your
schema is stable, you can simplify this by hardcoding the column name in
`mod_quantification.R` (`detect_classes_btn` handler).

**The match_item builder.** After you pick the class column and click
"Detect lipid classes," the UI renders one multi-select per detected class
(POS and NEG separately), pre-filled from the same defaults baked into
`get_lipid_absolute_quantification()`'s own signature, filtered to
whichever of those names actually exist in your uploaded IS table. Check
these before running - auto-fill is a starting point, not a guarantee your
class names match the defaults.

**Validation before running.** `.lfs_validate_path_structure()` in
`R/utils_quant.R` mirrors the exact pre-flight checks
`get_lipid_absolute_quantification()` runs internally (POS/NEG folders,
config files, mzXML presence, the RT-confirmation group folder). Running
these client-side means you get a readable checklist immediately instead of
an R `stop()` after a long background job. The Run button stays disabled
(`shinyjs::toggleState`) until validation passes.

**Background execution.** A real run can take minutes to hours. `Run`
launches `get_lipid_absolute_quantification()` in a separate process via
`callr::r_bg()` so the Shiny session stays responsive; stdout/stderr are
redirected to a log file that gets tailed into the UI every 1.5s
(`shiny::reactiveTimer`). `supervise = TRUE` so an orphaned job gets
cleaned up if the session dies - worth keeping on a shared server.

**Per-session resource paths.** The class-plot gallery serves images via
`shiny::addResourcePath()`, which is app-wide, not per-session. The resource
prefix is keyed on `session$token` specifically so two people running
different jobs at the same time on the shared server never resolve each
other's plot files under the same URL.

## Running locally

```r
# from the package root
devtools::load_all(".")
lipidflowshiny::run_lipidflow_shiny()
```

You'll need `lipidflow` and `massprocesser` installed for Quantification and
Peak Picking to actually run (Home/Annotation stub work without either):

```r
remotes::install_github("jaspershen/lipidflow")
remotes::install_github("tidymass/massprocesser")
```

Both pull in `xcms` and `MSnbase` from Bioconductor - the same heavy
install you've presumably already dealt with elsewhere in the tidymass
ecosystem, but budget time for it if this is a fresh server.

## Deploying

Same pattern as your other Shiny apps (chico-shiny, mapa-shiny):

1. `R CMD INSTALL` this package (or `devtools::install()`) on the server,
   or install straight from a git remote once you push it somewhere.
2. Copy `deploy/app.R` to `/srv/shiny-server/lipidflow-shiny/app.R`.
3. `sudo chown -R shiny:shiny /srv/shiny-server/lipidflow-shiny`
4. Add the Nginx `location` block + Cloudflare DNS record + Certbot cert,
   same steps as your `featuremsea.jaspershenlab.com` setup.

## Testing against real data found 2 real bugs - both fixed

Fabricated folder trees (empty placeholder files) validate structure and
reactive logic, but say nothing about whether the app handles what a real
LipidSearch export actually looks like. So before writing this section,
`lipidflow`'s own bundled demo annotation tables
(`inst/POS/lipid_annotation_table_pos.xlsx` etc.) were read directly and
inspected with `readxl` - not assumed. Two real problems turned up:

1. **LipidSearch's export format has a 26-row metadata preamble** before
   the real header (`#[c-1]:Blank1.raw`, `#[c-2]:D25_1.raw`, ... then the
   real header row: `LipidIon, Class, FattyAcid, ...`). A plain
   `read_xlsx()` - what the class-column picker originally did - reads row
   1 as the header and gets meaningless `...2, ...3, ...` names instead of
   `Class`, silently breaking class detection against every real
   LipidSearch file. Fixed in `.lfs_detect_header_skip()`
   (`R/utils_quant.R`): reads column 1 only, finds the first row that
   isn't `#`-prefixed, uses that as the real header row. Confirmed against
   both the POS and NEG demo files (26 rows skipped in each, `Class`
   correctly recovered, 17 real class values extracted). Returns 0 for a
   plain single-header-row file, so a hand-built annotation table is
   unaffected.
2. **The demo `IS_information.xlsx` has trailing non-breaking spaces**
   (U+00A0) on several `name` values, e.g. `"15:0-18:1(d7) DAG\u00a0"`.
   Exact-string matching against the plain-space defaults in
   `.lfs_default_match_item_pos()` returned nothing for those classes - no
   error, the auto pre-fill just silently selected nothing. Fixed with
   `.lfs_normalize_ws()`: comparison happens on whitespace-normalized
   strings, but the real (un-normalized) value from the IS table is still
   what gets selected and submitted, since that's the exact string
   lipidflow needs downstream.

Both fixes were verified against the actual demo files, not re-fabricated
data - `.lfs_detect_header_skip()` correctly returns 26 for both real
annotation tables and 0 for the (unaffected) `IS_information.xlsx`, and the
match-item pre-fill now correctly resolves `DG -> 15:0-18:1(d7) DAG ` (with
the real trailing NBSP) instead of silently matching nothing.

## Walkthrough: testing Quantification with the bundled demo data

```r
# 1. Get lipidflow's own demo POS/NEG data into a folder shaped the way
#    the app expects (this is the same system.file() pattern from your
#    earlier question about copying lipidflow's package data)
library(lipidflow)
path <- file.path(tempdir(), "lipidflow_demo")
dir.create(path)
file.copy(system.file("POS", package = "lipidflow"), path, recursive = TRUE)
file.copy(system.file("NEG", package = "lipidflow"), path, recursive = TRUE)
cat(path, "\n")  # note this path, you'll paste it into the app
```

The demo data already contains `IS_information.xlsx` and
`lipid_annotation_table_pos.xlsx`/`lipid_annotation_table_neg.xlsx` inside
`POS/` and `NEG/` respectively - matching the app's default expected
filenames exactly, so **no file uploads are needed to test this**. Groups
are `D25` and `M19` (not `QC`).

```r
# 2. Run the app locally
devtools::load_all(".")
lipidflowshiny::run_lipidflow_shiny()
```

In the Quantification tab:
1. Paste the path from step 1 into "Data folder on the server", click
   **Scan folder** - should report `Found groups: D25, M19`.
2. Set "Group used to confirm IS retention time" to **D25** (this is what
   lipidflow's own vignette uses for this dataset; the app's fallback
   logic happens to already default here since "D25" sorts first
   alphabetically, but worth confirming explicitly).
3. Leave the 4 file upload fields empty - the config files are already in
   place.
4. Set Class column (POS) and (NEG) to **Class** (now correctly detected
   as an option, post-fix), click **Detect lipid classes** - should find
   17 POS classes (AEA, CL, Cer, ChE, DG, ...).
5. Click **Validate structure** - all checks should pass.
6. Review the auto-filled match_item selections, then **Run
   quantification**. Expect real wall-clock time here (minutes, not
   seconds) - the log panel tails real `xcms`/lipidflow progress messages.

## How this was tested (no live R in the build sandbox, so this mattered)

This sandbox had no R installation to start with, so rather than ship code
written purely by inspection, `r-base-core` plus every real dependency
(`shiny`, `bslib`, `DT`, `callr`, `readxl`, `readr`, `openxlsx`, `zip`,
`shinyjs`) were installed via `apt` (Ubuntu ships CRAN-mirrored
`r-cran-*` packages) specifically to test against. Every `.R` file is
parse-checked, the full `app_ui()` and `shinyApp(ui, server)` are
constructed end to end, and the reactive logic (scan/validate/shared-path
sync) is exercised with `shiny::testServer()` against fabricated POS/NEG
folder trees, happy-path and failure-path both. `lipidflow` and
`massprocesser` themselves aren't installed (their Bioconductor
dependencies aren't reachable from this sandbox), so the actual
`process_data()` / `get_lipid_absolute_quantification()` calls remain
untested at that final layer - treat your first real run as a genuine
debugging pass there specifically.

One gotcha worth recording since it cost real debugging time: testing the
`shared_data_path` cross-module sync (the `observeEvent(input$x, ...,
ignoreInit = TRUE)` pattern in both `mod_quantification.R` and
`mod_peak_picking.R`) initially looked broken under `testServer()` -
`ignoreInit` swallowed the very first `session$setInputs()` call, because
`testServer()` doesn't run the automatic "startup" reactive flush a live
browser session gets for free before any user interaction. Fixing it was
test-only: seed a baseline `session$setInputs(data_path = "")` before the
real one you want to assert on. The application code itself was correct
the whole time - worth remembering if you extend this sync pattern to a
third tab and write tests for it.

## Known gaps / things to check before trusting this beyond a demo

- **NAMESPACE is hand-written**, not roxygen-generated. Add proper
  `@export` roxygen tags (a few functions already have doc comments
  started) and run `devtools::document()` once you have R + roxygen2
  locally to regenerate it correctly.
- Quantification and Peak Picking parameter defaults (`chol_rt`, `ppm`,
  `rt.tolerance`/`peakwidth`/`snthresh`/etc., `threads`) mirror each
  underlying function's own defaults - change them in the relevant
  `mod_*.R` if your lab's usual values differ.
- Both "Everything" download tabs zip whole `Result/` folders on demand;
  for large runs with many EIC/QC plots this could be a sizeable download -
  worth a size warning if that turns out to be common in practice.
- Peak Picking runs POS then NEG **sequentially** in one background job for
  simplicity/lower risk, not concurrently - an easy later optimization if
  wall-clock time on real batches turns out to matter, at the cost of
  tracking two processes/logs instead of one.

## Next module

Just Annotation left. Same shape as the other two: a new `mod_annotation.R`
(UI + server) swapped in for `mod_placeholder_ui("annotation", ...)` /
`mod_placeholder_server("annotation")` in `R/app_ui.R` / `R/app_server.R`.
Per the decision log above: `metid`, matched against MS-DIAL's public lipid
MS/MS libraries, consuming the `mass_dataset` `object` file Peak Picking
now produces at `<data>/POS/Result/object` and `<data>/NEG/Result/object`.
Worth prototyping directly against a real Peak Picking run's output before
wiring up UI, same way Quantification and Peak Picking were each built
against their real upstream function signatures rather than assumed ones.

Once Annotation exists, Quantification's manual annotation-table upload
can be replaced with "use output from the Annotation tab," closing the
loop into one pipeline instead of three separate tabs.
