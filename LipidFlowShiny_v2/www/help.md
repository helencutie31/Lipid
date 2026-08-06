# LipidFlow — Documentation

LipidFlow is a browser-based pipeline for untargeted lipidomics: it takes raw LC-MS files through internal-standard peak extraction, chromatographic peak picking, MS-DIAL-referenced lipid annotation, and internal-standard-based absolute quantification, with every intermediate table available for download.

## Quick Start

1. **Small Tools → Peak Extraction.** Upload a single QC raw file (`.mzXML`/`.mzML`) and an internal-standard (IS) CSV. Pick candidate adducts and an m/z tolerance, then run **Extract Peaks**. Inspect the EIC for each IS and confirm the adduct with the cleanest, highest-intensity peak. Click **Use for Quantification (Step 3)** — this produces `Y_IS_opt`, the reference table Step 3 needs later.
2. **lipidAnalysis → Data Import.** This is the single place to stage every file for the 3-step pipeline: raw sample files (Option A: new analysis, or Option B: load an existing result), MS2 spectra, a spectral database (the bundled MS-DIAL database is selected by default), and the IS table (`.xlsx`).
3. **Step 1: Peak Picking.** Click **Run**. Advanced settings (ppm, peak width, S/N threshold, …) have defaults suited to most LC-MS/MS runs; only change them if you know why. Produces one feature table (m/z, RT, peak area per sample) per ion mode.
4. **Step 2: Lipid Annotation.** Choose the chromatography column type (`rp` or `hilic`) and click **Annotate**. Every detected feature is matched against the spectral database by MS1 m/z (and, where MS2 spectra are supplied, fragment matching) within the configured ppm tolerance.
5. **Step 3: Quantification.** Requires Steps 1–2 *and* a completed Small Tools run. Select which internal standards to quantify with, then click **Quantify** — LipidFlow converts each matched lipid feature's peak area into an absolute concentration.
6. **Save Results.** Download the most advanced table produced so far, or bundle everything generated in the session into one archive.

> **Tip.** Run Small Tools → Peak Extraction *before* Step 3, ideally right after loading your raw data — it only needs to be done once per QC batch, not once per sample.

## Data Input Specs

LipidFlow expects the following files. All uploads are validated on arrival; a missing or misnamed column is reported immediately rather than surfacing as a cryptic failure mid-run.

### Raw MS data

| File | Format | Used by |
|---|---|---|
| QC raw file | `.mzXML` or `.mzML` | Small Tools → Peak Extraction |
| Sample raw files | `.mzXML` (converted from vendor formats via [ProteoWizard/msConvert](http://proteowizard.sourceforge.net/)) | Step 1: Peak Picking |
| MS2 spectra | `.mzXML`, `.mzML`, `.mgf`, or `.msp` | Step 2: Lipid Annotation |

Sample files are grouped automatically from their filename (the trailing `_<replicate number>` is stripped, e.g. `D25_1.mzXML` and `D25_2.mzXML` both become group `D25`). Files that don't follow a `<group>_<n>` naming convention are each treated as a single-sample group.

### Internal Standard CSV (Small Tools → Peak Extraction)

Exactly 4 columns, in any order:

| Column | Type | Meaning |
|---|---|---|
| `ID` | text, unique | Internal identifier for the standard |
| `Name` | text | Display name |
| `Formula` | text | Molecular formula (reference only) |
| `Accurate_Mass` | numeric, > 0 | Monoisotopic neutral mass |

### Internal Standard table (Data Import, `.xlsx`)

Exactly 5 columns:

| Column | Meaning |
|---|---|
| `name` | Internal standard name — must match names used elsewhere (Small Tools IS CSV, match-item tables) |
| `exact.mass` | Monoisotopic exact mass |
| `formula` | Molecular formula |
| `ug_ml` | Known concentration, µg/mL |
| `um` | Known concentration, µmol (despite the column name, the unit is µmol, **not** µm) |

### Spectral database

LipidFlow ships with two MS-DIAL-derived lipid databases (`data/msdial_lipid_pos_db.rda`, `data/msdial_lipid_neg_db.rda`), pre-formatted for the `metID` package and selected by default. A custom `.rda` database (built offline in the same `metID` format) can be uploaded instead if needed.

## Pipeline Algorithms

### Peak Extraction (Small Tools)

For each internal standard and each candidate adduct, the theoretical target m/z is:

```
m/z_target = (Accurate_Mass + Δ_adduct) / |z|
```

with the following adduct mass shifts (monoisotopic, `z = 1` for all):

| Mode | Adduct | Δ_adduct (Da) |
|---|---|---|
| Positive | `+H` | +1.007276 |
| Positive | `+Na` | +22.989218 |
| Positive | `+NH4` | +18.033823 |
| Negative | `-H` | −1.007276 |
| Negative | `+Cl` | +34.969402 |
| Negative | `+HCOO` | +44.998201 |

An extracted-ion chromatogram (EIC) is read from the QC raw file within `m/z_target ± ppm` and, optionally, an RT window. Each EIC is scored on two criteria: **apex intensity** and a **symmetry score** (a proxy for how Gaussian/chromatographically "clean" the peak is, derived from the half-height peak widths on either side of the apex). The adduct that best balances both is selected automatically per standard, producing `Y_IS_opt`: one row per IS with its selected adduct, target m/z, measured RT, and peak area (via trapezoidal integration).

### Step 1 — Peak Picking

Raw files are processed with `massprocesser`, which wraps the CentWave algorithm (`xcms`) for feature detection, followed by retention-time correction and cross-sample alignment. Key parameters (all adjustable under *Advanced settings*):

- `ppm` — instrument mass accuracy (default 15)
- `peakwidth` — expected chromatographic peak width range, in seconds (default 5–30)
- `snthresh` — minimum signal-to-noise ratio (default 10)
- `noise` — intensity cutoff below which signal is treated as noise (default 500)
- `min_fraction` — minimum fraction of samples a feature must appear in to be kept (default 0.5)

Output is one feature table per ion mode: `variable_id` (Peak ID), `mz`, `rt`, and one peak-area column per sample.

### Step 2 — Annotation via MS-DIAL

Detected features are matched against the selected spectral database using `metID::annotate_metabolites_mass_dataset()`. A feature is accepted as a candidate match only if its **MS1 mass error**, in ppm, is within tolerance:

```
Error (ppm) = |m/z_measured − m/z_theoretical| / m/z_theoretical × 10^6
```

Default MS1 tolerance is 5–10 ppm depending on context (Small Tools defaults to 15 ppm search windows; annotation defaults to 10 ppm matching, tightened further where MS2 spectra are available). Where MS2 spectral files were supplied in Data Import, fragment-level matching (`ms2.match.ppm`, `ms2.match.tol`) refines candidate ranking further, and up to `candidate.num` top candidates are retained per feature before the best match is reported.

### Step 3 — Internal-Standard Quantification

Each annotated lipid feature is assigned to a lipid class (parsed from its name, e.g. `PC(34:1)` → class `PC`), and each class is mapped to the internal standard that normalizes it. The final concentration is:

```
Final_Calculated = (Peak_Area_Sample / Peak_Area_IS_opt) × Known_Conc_IS
```

- `Peak_Area_Sample` — the annotated feature's peak area in a given sample (from Step 2)
- `Peak_Area_IS_opt` — the matched internal standard's QC peak area, measured **once** from the Small Tools run, not per sample
- `Known_Conc_IS` — the internal standard's known concentration (`ug_ml` column of the IS table)

Because `Peak_Area_IS_opt` is fixed per IS rather than recomputed per sample, a completed Small Tools → Peak Extraction run is a hard prerequisite for this step.

## Troubleshooting

| Symptom | Likely cause | Resolution |
|---|---|---|
| Status shows *Error* after clicking Run/Annotate/Extract | A required input is missing | Check the log/message panel directly below the button — it names exactly what's missing |
| IS table (`.xlsx`) upload is rejected | Wrong or missing column names | Confirm the file has exactly the 5 columns listed under *Data Input Specs → Internal Standard table* |
| IS CSV (Small Tools) upload is rejected | Wrong or missing column names, non-numeric/non-positive `Accurate_Mass`, or duplicate `ID` values | Confirm the file has exactly the 4 columns listed under *Data Input Specs → Internal Standard CSV* |
| Step 2/3 shows "No data yet" | An upstream step hasn't finished | Complete the previous step first — each step's summary line states what it's waiting on |
| Step 3 (Quantification) stays blocked even after annotation finishes | Small Tools → Peak Extraction hasn't been run/confirmed yet | Go to Small Tools → Peak Extraction, run it, and click *Use for Quantification (Step 3)* |
| QC plots aren't shown for Step 1 | Result was loaded via Option B (existing result), which has no linked run folder | Expected — QC plots are only generated for a fresh Option A run |
| A lipid feature's `Final_Calculated` is blank/NA | Its parsed lipid class has no matching internal standard, or the matched IS wasn't included in the selected IS subset | Confirm the lipid class is covered by the default class→IS mapping, or select the relevant IS in Step 3 |

---

*For pipeline internals beyond this document (function-level parameters, package versions), see the R source under `R/` — each `utils_*.R` file documents the exact formula and validation rules it implements.*
