# Helper functions for mod_import.R - organizing uploaded/staged files into
# the layout massprocesser expects, inferring sample groups from filenames.
# Split out of the original utils_peak_picking.R (moved here verbatim, not rewritten)
# as part of the mod_/utils_ file reorganization.

# ---------------------------------------------------------------------------
# Infer a sample "group" from an original filename, e.g. "D25_1.mzXML" -> "D25",
# "M19_2.mzXML" -> "M19" - strips the file extension then a trailing
# "_<number>" or "-<number>" (the replicate index). Falls back to the full
# stem if no such pattern is found (each file becomes its own group of one).
# This only exists because the new upload-based flow has no folder structure
# to read a group from (a browser file picker flattens that) - so the group
# has to come from *somewhere*, and the filename convention already used in
# lipidflow's own demo data (D25_1, D25_2, M19_1, M19_2) is the least-new-
# concept way to get it. Real user files that don't follow this pattern will
# each land in their own single-sample group - noted in the UI.
# ---------------------------------------------------------------------------
.lfs_infer_group <- function(filename) {
  stem <- tools::file_path_sans_ext(filename)
  group <- sub("[_-]?[0-9]+$", "", stem)
  if (!nzchar(group)) stem else group
}

# ---------------------------------------------------------------------------
# Build a "columns = group, rows = sample name" preview table from a vector
# of raw filenames - lets the user visually confirm the auto-grouping (by
# filename) is correct before running, instead of it being a silent,
# unverifiable inference.
# ---------------------------------------------------------------------------
.lfs_build_group_preview <- function(filenames) {
  if (length(filenames) == 0) return(NULL)
  groups <- vapply(filenames, .lfs_infer_group, character(1))
  sample_names <- tools::file_path_sans_ext(filenames)
  split_names <- split(sample_names, groups)
  max_n <- max(lengths(split_names))
  cols <- lapply(split_names, function(x) { length(x) <- max_n; x })
  as.data.frame(cols, check.names = FALSE)
}

# ---------------------------------------------------------------------------
# Given a shiny fileInput() data.frame (columns: name, datapath, ...) for one
# polarity, copy each uploaded file into <root>/<mode>/<inferred group>/<original name>
# - the exact layout massprocesser::process_data() expects. Returns the
# inferred group -> file count mapping for display.
# ---------------------------------------------------------------------------
.lfs_organize_uploads <- function(files_df, root, mode) {
  groups <- character(nrow(files_df))
  for (i in seq_len(nrow(files_df))) {
    grp <- .lfs_infer_group(files_df$name[i])
    groups[i] <- grp
    dest_dir <- file.path(root, mode, grp)
    dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
    file.copy(files_df$datapath[i], file.path(dest_dir, files_df$name[i]), overwrite = TRUE)
  }
  table(groups)
}

# Simple flat-folder version of .lfs_organize_uploads() - no group inference,
# just copies every uploaded file into one folder under its original name.
# Used for MS2 file uploads (massdataset::mutate_ms2() just wants a folder).
.lfs_organize_uploads_flat <- function(files_df, dest_dir) {
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  for (i in seq_len(nrow(files_df))) {
    file.copy(files_df$datapath[i], file.path(dest_dir, files_df$name[i]), overwrite = TRUE)
  }
  nrow(files_df)
}

# ---------------------------------------------------------------------------
# xlsx reading/validation helpers used by mod_import.R - moved here verbatim
# from utils_quant.R as part of the mod_/utils_ file reorganization.
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# LipidSearch's native xlsx export prepends a metadata block before the real
# header row - one row per raw file, formatted like "#[c-1]:D25_1.raw".
# Confirmed directly against lipidflow's own bundled demo annotation tables
# (inst/POS/lipid_annotation_table_pos.xlsx: 26 such rows, real header
# "LipidIon, Class, FattyAcid, ..." at row 27; NEG file: same, 26 rows).
# Without accounting for this, reading row 1 as the header - which is what
# a plain read_xlsx() does - produces meaningless "...2, ...3, ..." column
# names instead of "Class", breaking the class-column picker entirely
# against real LipidSearch output, which is the primary annotation-table
# source lipidflow expects. Detected generically (read column 1 only, find
# the first row that isn't "#"-prefixed) so it works for any LipidSearch
# export, and correctly returns 0 (no skip) for a plain single-header-row
# file, e.g. one you built by hand rather than exported from LipidSearch.
# ---------------------------------------------------------------------------
.lfs_detect_header_skip <- function(filepath) {
  if (is.null(filepath) || !file.exists(filepath)) return(0)
  col1 <- tryCatch(
    suppressMessages(readxl::read_xlsx(filepath, col_names = FALSE))[[1]],
    error = function(e) NULL
  )
  if (is.null(col1)) return(0)
  hdr_row <- which(!startsWith(trimws(as.character(col1)), "#"))[1]
  if (is.na(hdr_row)) return(0)
  hdr_row - 1
}

# ---------------------------------------------------------------------------
# Read just the column names of an uploaded xlsx (fast, header-only) so the
# UI can ask "which column holds the lipid class?" instead of assuming a
# fixed schema - annotation tables commonly come from LipidSearch exports,
# whose exact column naming varies by version/export settings.
# ---------------------------------------------------------------------------
.lfs_xlsx_colnames <- function(filepath) {
  if (is.null(filepath) || !file.exists(filepath)) return(character())
  skip <- .lfs_detect_header_skip(filepath)
  df <- suppressMessages(readxl::read_xlsx(filepath, skip = skip, n_max = 0))
  colnames(df)
}

.lfs_unique_column_values <- function(filepath, column) {
  if (is.null(filepath) || !file.exists(filepath) || !nzchar(column)) return(character())
  skip <- .lfs_detect_header_skip(filepath)
  df <- suppressMessages(readxl::read_xlsx(filepath, skip = skip))
  if (!column %in% colnames(df)) return(character())
  sort(unique(stats::na.omit(as.character(df[[column]]))))
}

# lipidflow's own bundled demo IS_information.xlsx has trailing non-breaking
# spaces (U+00A0) on several `name` values, e.g. "15:0-18:1(d7) DAG\u00a0" -
# confirmed by inspecting the real file, not assumed. Exact-string matching
# against the plain-space defaults in .lfs_default_match_item_pos()/_neg()
# silently returns nothing for those classes otherwise (intersect() with no
# error, just an empty pre-fill - easy to miss). Used only for comparison;
# the real (un-normalized) string from is_names is still what gets selected
# and submitted, since that's what lipidflow needs to match internally.
.lfs_normalize_ws <- function(x) trimws(gsub("[\u00a0[:space:]]+", " ", x))

.lfs_is_names <- function(filepath) {
  if (is.null(filepath) || !file.exists(filepath)) return(character())
  df <- readxl::read_xlsx(filepath)
  if (!"name" %in% colnames(df)) return(character())
  sort(unique(stats::na.omit(as.character(df[["name"]]))))
}

# ---------------------------------------------------------------------------
# Validate an uploaded Internal Standard table has the 5 required columns
# (name, exact.mass, formula, ug_ml, um) - report exactly which ones are
# missing rather than a generic error, so the user can fix the file quickly.
# ---------------------------------------------------------------------------
.lfs_validate_is_table <- function(filepath) {
  required_cols <- c("name", "exact.mass", "formula", "ug_ml", "um")
  if (is.null(filepath) || !file.exists(filepath)) {
    return(list(ok = FALSE, message = "File not found."))
  }
  cols <- tryCatch(colnames(readxl::read_xlsx(filepath, n_max = 0)),
                   error = function(e) character())
  missing <- setdiff(required_cols, cols)
  if (length(missing) > 0) {
    return(list(ok = FALSE,
                message = paste0("Missing required column(s): ", paste(missing, collapse = ", "),
                                 ". Expected exactly: ", paste(required_cols, collapse = ", "), ".")))
  }
  list(ok = TRUE, message = NULL)
}