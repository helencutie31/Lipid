# Helper functions used by mod_quantification.R.
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

# ---------------------------------------------------------------------------
# Giai doan 3 (Quantification) helpers - the real math mod_quantification.R
# was still missing a working implementation of:
#   Final_Calculated = (Peak_Area_Sample / Peak_Area_IS_opt) * Known_Conc_IS
# where Peak_Area_IS_opt and its RT come from Small Tools -> Peak Extraction
# (Y_IS_opt, utils_small_tools.R), not from the sample run itself.
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# Extract the leading alphabetic lipid-class prefix from a lipid name, e.g.
# "PC(15:0_18:1)" -> "PC", "TG(52:2)" -> "TG", "Cer(d18:1/24:0)" -> "Cer".
# Used to look up which internal standard normalizes a given lipid feature
# via match_item (see .lfs_default_match_item_pos()/_neg() above). Returns
# NA for names with no leading letters (garbage/empty input) rather than
# erroring - a single unparsable name shouldn't abort the whole table.
# ---------------------------------------------------------------------------
# ---------------------------------------------------------------------------
# Heuristically pick the lipid-name column out of an annotation table whose
# exact column names depend on what metid::annotate_metabolites_mass_dataset()
# / massdataset::extract_annotation_table() produced. Tries an exact
# "Lipid_Name" match first (the name this pipeline's own docs use), then
# common metid/LipidSearch-style alternatives, then falls back to the first
# character column found. Returns NA if nothing usable is found.
# ---------------------------------------------------------------------------
.lfs_detect_lipid_name_col <- function(df) {
  if (!is.data.frame(df) || ncol(df) == 0) return(NA_character_)
  candidates <- c("Lipid_Name", "Compound.name", "compound_name", "Compound_name", "name", "Name")
  hit <- candidates[candidates %in% colnames(df)]
  if (length(hit) > 0) return(hit[1])
  char_cols <- colnames(df)[vapply(df, is.character, logical(1))]
  if (length(char_cols) > 0) return(char_cols[1])
  NA_character_
}

.lfs_parse_lipid_class <- function(lipid_name) {
  lipid_name <- as.character(lipid_name)
  out <- rep(NA_character_, length(lipid_name))
  trimmed <- trimws(lipid_name)
  has_prefix <- !is.na(lipid_name) & nzchar(trimmed) & grepl("^[A-Za-z]+", trimmed)
  out[has_prefix] <- sub("^([A-Za-z]+).*$", "\\1", trimmed[has_prefix])
  out
}

# ---------------------------------------------------------------------------
# Pure vectorized implementation of the Giai doan 3 formula. Recycles
# length-1 arguments against the others; a ratio of Inf (division by a zero
# IS peak area) is reported as NA rather than propagating Inf into a report.
# ---------------------------------------------------------------------------
.lfs_calc_final_concentration <- function(peak_area_sample, peak_area_is, known_conc_is) {
  if (!is.numeric(peak_area_sample) || !is.numeric(peak_area_is) || !is.numeric(known_conc_is)) {
    stop(".lfs_calc_final_concentration: all arguments must be numeric.")
  }
  n <- max(length(peak_area_sample), length(peak_area_is), length(known_conc_is))
  recyclable <- function(x) length(x) == n || length(x) == 1
  if (!(recyclable(peak_area_sample) && recyclable(peak_area_is) && recyclable(known_conc_is))) {
    stop(".lfs_calc_final_concentration: arguments are not recyclable to a common length.")
  }
  ratio <- peak_area_sample / peak_area_is
  ratio[is.infinite(ratio)] <- NA_real_
  ratio * known_conc_is
}

# ---------------------------------------------------------------------------
# Full Giai doan 3 pipeline: for every (lipid feature, sample) pair in
# annotation_table, look up its lipid class, the internal standard that
# normalizes that class (match_item), that IS's measured QC peak area
# (y_is_opt, from Small Tools), and its known concentration (is_table),
# then apply .lfs_calc_final_concentration(). Vectorized throughout via
# dplyr/tidyr joins - no manual per-row loop. Returns a new long-format
# data.frame; none of the 3 input tables are mutated.
# ---------------------------------------------------------------------------
.lfs_quantify_lipids <- function(annotation_table, y_is_opt, is_table, match_item,
                                  lipid_name_col = "Lipid_Name", sample_cols = NULL,
                                  conc_col = "ug_ml", is_name_col = "name") {
  if (!is.data.frame(annotation_table)) stop(".lfs_quantify_lipids: 'annotation_table' must be a data.frame.")
  if (!is.data.frame(y_is_opt)) stop(".lfs_quantify_lipids: 'y_is_opt' must be a data.frame.")
  if (!is.data.frame(is_table)) stop(".lfs_quantify_lipids: 'is_table' must be a data.frame.")
  if (!is.list(match_item) || length(match_item) == 0) {
    stop(".lfs_quantify_lipids: 'match_item' must be a non-empty named list (class -> IS name(s)).")
  }
  if (!lipid_name_col %in% colnames(annotation_table)) {
    stop(".lfs_quantify_lipids: annotation_table is missing column '", lipid_name_col, "'.")
  }
  required_is_opt_cols <- c("IS_Name", "Peak_Area")
  if (!all(required_is_opt_cols %in% colnames(y_is_opt))) {
    stop(".lfs_quantify_lipids: y_is_opt is missing column(s): ",
         paste(setdiff(required_is_opt_cols, colnames(y_is_opt)), collapse = ", "))
  }
  if (!all(c(is_name_col, conc_col) %in% colnames(is_table))) {
    stop(".lfs_quantify_lipids: is_table is missing column '", is_name_col, "' or '", conc_col, "'.")
  }

  if (is.null(sample_cols)) {
    numeric_cols <- colnames(annotation_table)[vapply(annotation_table, is.numeric, logical(1))]
    sample_cols <- setdiff(numeric_cols, lipid_name_col)
  }
  if (length(sample_cols) == 0) {
    stop(".lfs_quantify_lipids: no numeric sample columns found/specified in annotation_table.")
  }
  missing_sample_cols <- setdiff(sample_cols, colnames(annotation_table))
  if (length(missing_sample_cols) > 0) {
    stop(".lfs_quantify_lipids: annotation_table is missing sample column(s): ",
         paste(missing_sample_cols, collapse = ", "))
  }

  class_to_is <- data.frame(
    Class = names(match_item),
    Matched_IS = vapply(match_item, function(x) as.character(x[1]), character(1)),
    stringsAsFactors = FALSE
  )

  base_tbl <- data.frame(
    Lipid_Name = as.character(annotation_table[[lipid_name_col]]),
    Class = .lfs_parse_lipid_class(annotation_table[[lipid_name_col]]),
    stringsAsFactors = FALSE
  )
  base_tbl <- cbind(base_tbl, annotation_table[, sample_cols, drop = FALSE])

  long_tbl <- tidyr::pivot_longer(
    base_tbl, cols = dplyr::all_of(sample_cols),
    names_to = "Sample", values_to = "Peak_Area_Sample"
  )
  long_tbl <- as.data.frame(long_tbl)

  long_tbl <- dplyr::left_join(long_tbl, class_to_is, by = "Class")

  is_opt_small <- y_is_opt[, c("IS_Name", "Peak_Area"), drop = FALSE]
  colnames(is_opt_small) <- c("Matched_IS", "Peak_Area_IS")
  long_tbl <- dplyr::left_join(long_tbl, is_opt_small, by = "Matched_IS")

  is_conc <- is_table[, c(is_name_col, conc_col), drop = FALSE]
  colnames(is_conc) <- c("Matched_IS", "Known_Conc_IS")
  long_tbl <- dplyr::left_join(long_tbl, is_conc, by = "Matched_IS")

  long_tbl$Final_Calculated <- .lfs_calc_final_concentration(
    long_tbl$Peak_Area_Sample, long_tbl$Peak_Area_IS, long_tbl$Known_Conc_IS
  )

  long_tbl[, c("Lipid_Name", "Class", "Sample", "Peak_Area_Sample", "Matched_IS",
               "Peak_Area_IS", "Known_Conc_IS", "Final_Calculated"), drop = FALSE]
}
