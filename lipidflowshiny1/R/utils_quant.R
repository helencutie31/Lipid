# Helper functions used by quantification.R.
#
# These are deliberately NOT exported - they're internal plumbing. Kept in
# their own file so the module file itself stays readable.

# ---------------------------------------------------------------------------
# Default IS <-> lipid class matches, copied verbatim from the defaults baked
# into lipidflow::get_lipid_absolute_quantification()'s own function
# signature (see jaspershen/lipidflow, R/get_lipid_absolute_quantification.R).
# Used to pre-fill the match_item builder UI so users aren't starting from a
# blank form for classes that follow the lab's usual internal standard mix.
# ---------------------------------------------------------------------------
.lfs_default_match_item_pos <- function() {
  list(
    "Cer"  = "d18:1 (d7)-15:0 Cer",
    "ChE"  = c("18:1(d7) Chol Ester", "Cholesterol (d7)"),
    "Chol" = "Cholesterol (d7)",
    "DG"   = "15:0-18:1(d7) DAG",
    "LPC"  = "18:1(d7) Lyso PC",
    "LPE"  = "18:1(d7) Lyso PE",
    "MG"   = "18:1 (d7) MG",
    "PA"   = "15:0-18:1(d7) PA (Na Salt)",
    "PC"   = "15:0-18:1(d7) PC",
    "PE"   = "15:0-18:1(d7) PE",
    "PG"   = "15:0-18:1(d7) PG (Na Salt)",
    "PI"   = "15:0-18:1(d7) PI (NH4 Salt)",
    "PPE"  = "C18(Plasm)-18:1(d9) PE",
    "PS"   = "15:0-18:1(d7) PS (Na Salt)",
    "SM"   = "d18:1-18:1(d9) SM",
    "TG"   = "15:0-18:1(d7)-15:0 TAG"
  )
}

.lfs_default_match_item_neg <- function() {
  list(
    "Cer"  = "d18:1 (d7)-15:0 Cer",
    "Chol" = "Cholesterol (d7)",
    "ChE"  = c("18:1(d7) Chol Ester", "Cholesterol (d7)"),
    "LPC"  = "18:1(d7) Lyso PC",
    "LPE"  = "18:1(d7) Lyso PE",
    "PC"   = "15:0-18:1(d7) PC",
    "PE"   = "15:0-18:1(d7) PE",
    "PG"   = "15:0-18:1(d7) PG (Na Salt)",
    "PI"   = "15:0-18:1(d7) PI (NH4 Salt)",
    "PPE"  = "C18(Plasm)-18:1(d9) PE",
    "PS"   = "15:0-18:1(d7) PS (Na Salt)",
    "SM"   = "d18:1-18:1(d9) SM"
  )
}

# ---------------------------------------------------------------------------
# Pre-flight structure check, mirroring the exact checks
# get_lipid_absolute_quantification() runs internally before doing any real
# work (POS/NEG folders, is_info/annotation files, mzXML presence, the RT
# confirmation group folder). Running these client-side first means the user
# gets a fast, readable checklist instead of an R stop() after minutes of
# waiting on a background job that could never have succeeded.
#
# Returns a data.frame with columns: check, status ("ok"/"fail"), message
# ---------------------------------------------------------------------------
.lfs_validate_path_structure <- function(path,
                                          is_info_name_pos,
                                          is_info_name_neg,
                                          lipid_annotation_table_pos,
                                          lipid_annotation_table_neg,
                                          which_group_for_rt_confirm) {

  add <- function(rows, check, ok, message) {
    rbind(rows, data.frame(check = check, status = ifelse(ok, "ok", "fail"),
                            message = message, stringsAsFactors = FALSE))
  }

  rows <- data.frame(check = character(), status = character(),
                      message = character(), stringsAsFactors = FALSE)

  if (!nzchar(path) || !dir.exists(path)) {
    rows <- add(rows, "Data folder exists", FALSE,
                paste0("Folder not found on the server: ", path))
    return(rows)
  }
  rows <- add(rows, "Data folder exists", TRUE, path)

  pos_dir <- file.path(path, "POS")
  neg_dir <- file.path(path, "NEG")

  rows <- add(rows, "POS/ subfolder", dir.exists(pos_dir),
              if (dir.exists(pos_dir)) "found" else "lipidflow requires a POS/ subfolder, even if you only ran one mode")
  rows <- add(rows, "NEG/ subfolder", dir.exists(neg_dir),
              if (dir.exists(neg_dir)) "found" else "lipidflow requires a NEG/ subfolder, even if you only ran one mode")

  check_mode <- function(rows, mode_dir, mode_label, is_info_name, annot_name) {
    if (!dir.exists(mode_dir)) return(rows)

    is_ok <- file.exists(file.path(mode_dir, is_info_name))
    rows <- add(rows, paste0(mode_label, ": ", is_info_name), is_ok,
                if (is_ok) "found" else paste("expected at", file.path(mode_dir, is_info_name)))

    annot_ok <- file.exists(file.path(mode_dir, annot_name))
    rows <- add(rows, paste0(mode_label, ": ", annot_name), annot_ok,
                if (annot_ok) "found" else paste("expected at", file.path(mode_dir, annot_name)))

    mzxml <- list.files(mode_dir, pattern = "mzXML$", recursive = TRUE, ignore.case = TRUE)
    rows <- add(rows, paste0(mode_label, ": .mzXML files"), length(mzxml) > 0,
                if (length(mzxml) > 0) paste(length(mzxml), "file(s) found") else "no .mzXML files found anywhere under this folder")

    group_ok <- dir.exists(file.path(mode_dir, which_group_for_rt_confirm))
    rows <- add(rows, paste0(mode_label, ": '", which_group_for_rt_confirm, "' group folder"), group_ok,
                if (group_ok) "found" else paste0("no subfolder named '", which_group_for_rt_confirm, "' under ", mode_dir))

    rows
  }

  rows <- check_mode(rows, pos_dir, "POS", is_info_name_pos, lipid_annotation_table_pos)
  rows <- check_mode(rows, neg_dir, "NEG", is_info_name_neg, lipid_annotation_table_neg)

  rows
}

# ---------------------------------------------------------------------------
# List immediate subfolder names under path/POS or path/NEG - these are the
# "group" names lipidflow parses out of the mzXML folder structure, and one
# of them must be picked as which_group_for_rt_confirm.
# ---------------------------------------------------------------------------
.lfs_list_groups <- function(path, mode = c("POS", "NEG")) {
  mode <- match.arg(mode)
  mode_dir <- file.path(path, mode)
  if (!dir.exists(mode_dir)) return(character())
  list.dirs(mode_dir, full.names = FALSE, recursive = FALSE)
}

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
# Launch get_lipid_absolute_quantification() in a background R process via
# callr, so the Shiny session stays responsive while a run - which can take
# anywhere from minutes to a couple of hours depending on sample count and
# threads - is in progress. stdout/stderr are redirected to log_path so the
# UI can tail real progress (the underlying function already emits cat()
# progress messages at each pipeline stage).
# ---------------------------------------------------------------------------
.lfs_run_quant_job <- function(args_list, log_path) {
  callr::r_bg(
    func = function(args_list) {
      do.call(lipidflow::get_lipid_absolute_quantification, args_list)
    },
    args = list(args_list = args_list),
    stdout = log_path,
    stderr = log_path,
    supervise = TRUE
  )
}
