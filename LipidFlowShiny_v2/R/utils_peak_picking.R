# Helper functions for mod_peak_picking.R.
#
# Deliberately kept separate from utils_quant.R even though there's some
# overlap with .lfs_validate_path_structure() (both check POS/NEG + mzXML
# presence) - massprocesser needs no config xlsx files, so unifying the two
# checkers would mean threading a bunch of "is this check relevant here"
# flags through one function. Some duplication, but each function stays
# readable. Worth revisiting once a third module needs the same checks.

# ---------------------------------------------------------------------------
# Pre-flight check mirroring what massprocesser::process_data() actually
# needs: POS/NEG folders, each with at least one .mzXML/.mzML file somewhere
# under it. Unlike lipidflow's quantification step, there's no required
# config file and no hard requirement on group_for_figure existing -
# process_data() degrades gracefully (picks a substitute group) if the
# requested one isn't found, so that's surfaced as an informational note
# in the UI rather than a blocking check here.
# ---------------------------------------------------------------------------
.lfs_validate_peak_picking_structure <- function(path) {
  add <- function(rows, check, ok, message) {
    rbind(rows, data.frame(check = check, status = ifelse(ok, "ok", "fail"),
                           message = message, stringsAsFactors = FALSE))
  }
  rows <- data.frame(check = character(), status = character(),
                     message = character(), stringsAsFactors = FALSE)
  
  if (!nzchar(path) || !dir.exists(path)) {
    rows <- add(rows, "Data folder exists", FALSE, paste0("Folder not found on the server: ", path))
    return(rows)
  }
  rows <- add(rows, "Data folder exists", TRUE, path)
  
  check_mode <- function(rows, mode) {
    mode_dir <- file.path(path, mode)
    ok_dir <- dir.exists(mode_dir)
    rows <- add(rows, paste0(mode, "/ subfolder"), ok_dir,
                if (ok_dir) "found" else paste0("massprocesser needs a ", mode, "/ subfolder"))
    if (ok_dir) {
      raw_files <- list.files(mode_dir, pattern = "\\.(mzXML|mzML|cdf|mgf)$",
                              recursive = TRUE, ignore.case = TRUE)
      rows <- add(rows, paste0(mode, ": raw files"), length(raw_files) > 0,
                  if (length(raw_files) > 0) paste(length(raw_files), "file(s) found")
                  else "no .mzXML/.mzML/.cdf files found anywhere under this folder")
    }
    rows
  }
  
  rows <- check_mode(rows, "POS")
  rows <- check_mode(rows, "NEG")
  rows
}

# ---------------------------------------------------------------------------
# Load two saved mass_dataset "object" files (POS + NEG), merge them, and
# populate the shared pipeline state - shared by both Option A (after a
# fresh massprocesser run finishes) and Option B (loading previously-saved
# results directly, no re-run). Kept as one function so both paths can't
# drift out of sync on how the merge/summary logic works.
# ---------------------------------------------------------------------------
.lfs_load_object_file <- function(path) {
  e <- new.env()
  load(path, envir = e)
  get(ls(e)[1], envir = e)
}

# ---------------------------------------------------------------------------
# Pure, testable core of .lfs_peak_table() below: merge a variable_info
# data.frame (must have a 'variable_id' column - Peak_ID/m/z_mean/RT_peak in
# the pipeline's terms) with an expression_data matrix/data.frame (one row
# per variable_id, one column per sample - Peak_Area). Row order is matched
# on variable_id, never assumed - see Chunk 1 discussion in project notes.
# Split out from .lfs_peak_table() so it can be unit-tested against plain
# data.frames/matrices, without needing a real S4 mass_dataset object.
# Vectorized (match() + cbind()); never mutates variable_info/expression_data,
# always returns a new data.frame.
# ---------------------------------------------------------------------------
.lfs_merge_variable_expression <- function(variable_info, expression_data) {
  if (is.null(variable_info) || !is.data.frame(variable_info)) {
    stop(".lfs_merge_variable_expression: 'variable_info' must be a data.frame.")
  }
  if (!"variable_id" %in% colnames(variable_info)) {
    stop(".lfs_merge_variable_expression: 'variable_info' must contain a 'variable_id' column.")
  }
  if (is.null(expression_data)) {
    stop(".lfs_merge_variable_expression: 'expression_data' must not be NULL.")
  }

  expr <- as.data.frame(expression_data, check.names = FALSE)
  if (is.null(rownames(expr)) || !any(nzchar(rownames(expr)))) {
    stop(".lfs_merge_variable_expression: 'expression_data' must have row names matching 'variable_id'.")
  }

  missing_ids <- setdiff(variable_info$variable_id, rownames(expr))
  if (length(missing_ids) > 0) {
    stop(".lfs_merge_variable_expression: expression_data is missing row(s) for variable_id(s): ",
         paste(utils::head(missing_ids, 5), collapse = ", "),
         if (length(missing_ids) > 5) ", ..." else "")
  }

  expr_ordered <- expr[match(variable_info$variable_id, rownames(expr)), , drop = FALSE]
  cbind(variable_info, expr_ordered)
}

# Merge variable_info + expression_data into 1 flat table (match() by
# variable_id, not row order - see Chunk 1 discussion). This is the ONLY
# place this merge should happen - every UI location that shows a peak
# table calls this, so display and download can never drift apart.
.lfs_peak_table <- function(mass_dataset_obj) {
  tryCatch({
    var_info <- mass_dataset_obj@variable_info
    expr_data <- mass_dataset_obj@expression_data
    .lfs_merge_variable_expression(var_info, expr_data)
  }, error = function(e) {
    message("[utils_peak_picking] .lfs_peak_table failed: ", conditionMessage(e))
    stop(e)
  })
}

.lfs_finalize_peak_picking <- function(pipeline_state, pos_obj_path, neg_obj_path) {
  md_pos <- .lfs_load_object_file(pos_obj_path)
  md_neg <- .lfs_load_object_file(neg_obj_path)
  
  # POS and NEG features can't false-collide here: merge_mass_dataset's
  # default join key is variable_id + mz + rt together, and positive vs
  # negative ions never share both mz and rt by coincidence - confirmed by
  # reading massdataset's own merge_mass_dataset() source rather than
  # assumed. sample_direction="full" keeps every sample even if it only
  # ran in one mode; variable_direction="full" keeps every feature from
  # both modes (a union, since real matches across modes are essentially
  # impossible on that join key).
  merged <- massdataset::merge_mass_dataset(
    x = md_pos, y = md_neg,
    sample_direction = "full", variable_direction = "full"
  )
  
  pipeline_state$mass_dataset <- merged
  pipeline_state$mass_dataset_pos <- md_pos
  pipeline_state$mass_dataset_neg <- md_neg
  pipeline_state$pp_summary <- data.frame(
    mode = c("POS", "NEG", "Merged"),
    n_features = c(tryCatch(nrow(md_pos@variable_info), error = function(e) NA),
                   tryCatch(nrow(md_neg@variable_info), error = function(e) NA),
                   tryCatch(nrow(merged@variable_info), error = function(e) NA)),
    n_samples = c(tryCatch(nrow(md_pos@sample_info), error = function(e) NA),
                  tryCatch(nrow(md_neg@sample_info), error = function(e) NA),
                  tryCatch(nrow(merged@sample_info), error = function(e) NA))
  )
  pipeline_state$pp_done <- TRUE
  invisible(TRUE)
}

# ---------------------------------------------------------------------------
# Generic background-job launcher - same shape as .lfs_run_quant_job() in
# utils_quant.R, kept as its own small function here so this file has no
# dependency ordering requirement on utils_quant.R being sourced first.
# ---------------------------------------------------------------------------
.lfs_run_bg_job <- function(func, args, log_path) {
  callr::r_bg(func = func, args = args, stdout = log_path, stderr = log_path,
              supervise = TRUE, libpath = .libPaths())
}

# Deletes massprocesser's cache folders so a run is genuinely redone rather
# than silently reusing old raw_data/xdata/xdata2/xdata3 - process_data()
# has no rerun= flag of its own, it just reuses whatever it finds in
# Result/intermediate_data.
.lfs_clear_peak_picking_cache <- function(path) {
  for (mode in c("POS", "NEG")) {
    cache_dir <- file.path(path, mode, "Result", "intermediate_data")
    if (dir.exists(cache_dir)) unlink(cache_dir, recursive = TRUE)
  }
}