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
      raw_files <- list.files(mode_dir, pattern = "\\.(mzXML|mzML|cdf)$",
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
# Generic background-job launcher - same shape as .lfs_run_quant_job() in
# utils_quant.R, kept as its own small function here so this file has no
# dependency ordering requirement on utils_quant.R being sourced first.
# ---------------------------------------------------------------------------
.lfs_run_bg_job <- function(func, args, log_path) {
  callr::r_bg(func = func, args = args, stdout = log_path, stderr = log_path, supervise = TRUE)
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
