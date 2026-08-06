# MULTI-MODE LIPIDOMICS PIPELINE (WITH RDA + MS2 MGF MATCHING)
options(stringsAsFactors = FALSE)

# ---- 1. CONFIGURE YOUR DATA --------------------------------------------------
mode_dirs <- list(
  positive = "C:/Users/LENOVO/Downloads/LipidFlowShiny/demo/POS_mzXML/D25",
  negative = "C:/Users/LENOVO/Downloads/LipidFlowShiny/demo/NEG_mzXML/D25"
)

# File database .rda chuẩn MS2 đã convert thành công
rda_files <- list(
  positive = "C:/Users/LENOVO/Downloads/LipidFlowShiny/msdial_lipid_pos_db.rda",
  negative = "C:/Users/LENOVO/Downloads/LipidFlowShiny/msdial_lipid_neg_db.rda"
)

# Đã cập nhật đường dẫn chính xác tới 2 thư mục MGF theo ảnh image_c4df42.png
ms2_dirs <- list(
  positive = "C:/Users/LENOVO/Downloads/LipidFlowShiny/demo/D25_POS_mgf",
  negative = "C:/Users/LENOVO/Downloads/LipidFlowShiny/demo/D25_NEG_mgf"
)

run_massprocesser_if_needed <- TRUE
peak_params <- list(ppm = 15, peakwidth = c(10, 60), snthresh = 5,
                    noise = 500, min_fraction = 0.5, fill_peaks = FALSE)
column_type <- "rp"

# Giữ threads = 1L để đảm bảo không bị lỗi bplapply trên Windows
threads <- 1L 

annotation_params <- list(
  ms1_ppm = 15, 
  ms2_ppm = 20, 
  ms2_tol = 0.02,
  ms1_ms2_ppm = 10, 
  ms1_ms2_rt_seconds = 20,
  candidate_num = 3L
)

run_lipidflow_targeted_extraction <- FALSE

# ---- 2. VALIDATE CONFIGURATION ----------------------------------------------
active_modes <- intersect(names(mode_dirs), c("positive", "negative"))
if (!length(active_modes)) stop("Set at least one of mode_dirs$positive or mode_dirs$negative.")
if (!all(active_modes %in% names(rda_files))) stop("Every configured mode needs a matching RDA file path.")

needed <- c("massprocesser", "massdataset", "metid", "openxlsx")
if (run_lipidflow_targeted_extraction) needed <- c(needed, "lipidflow", "readxl")
missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Install missing package(s): ", paste(missing, collapse = ", "))

suppressPackageStartupMessages(library(metid))
tryCatch(
  metid::load_adduct_table(polarity = active_modes[1], column = column_type),
  error = function(e) stop(
    "MetID's built-in adduct table could not be loaded. Original error: ", conditionMessage(e), call. = FALSE
  )
)

for (mode in active_modes) {
  if (!dir.exists(mode_dirs[[mode]])) stop("Mode directory not found for ", mode, ": ", mode_dirs[[mode]])
  if (!file.exists(rda_files[[mode]]) || dir.exists(rda_files[[mode]]))
    stop("RDA path for ", mode, " must be one existing .rda file: ", rda_files[[mode]])
}

# ---- 3. HELPERS --------------------------------------------------------------
find_object <- function(path) {
  options <- c(file.path(path, "Result", "object"), file.path(path, "Results", "object"))
  options <- options[file.exists(options)]
  if (length(options)) return(options[1])
  found <- list.files(path, pattern = "^object$", recursive = TRUE, full.names = TRUE)
  if (length(found)) found[1] else NA_character_
}

load_object <- function(path) {
  object_path <- find_object(path)
  if (is.na(object_path)) stop("No massprocesser object under: ", path)
  env <- new.env(parent = emptyenv()); names_loaded <- load(object_path, envir = env)
  object <- if ("object" %in% names_loaded) env[["object"]] else if (length(names_loaded) == 1) env[[names_loaded]] else stop("Object file has multiple objects and none is named 'object'.")
  massdataset::check_object_class(object, class = "mass_dataset")
  object
}

find_col <- function(x, choices) {
  index <- match(tolower(choices), tolower(names(x)), nomatch = 0L)
  index <- index[index > 0L]
  if (length(index)) names(x)[index[1]] else NA_character_
}

add_feature_coordinates <- function(ann, object) {
  annotation_id <- find_col(ann, c("variable_id", "Variable.ID", "variable", "variableID"))
  var_info <- massdataset::extract_variable_info(object)
  variable_id <- find_col(var_info, c("variable_id", "Variable.ID", "variable", "variableID"))
  mz_col <- find_col(var_info, c("mz", "MZ", "mass_to_charge"))
  rt_col <- find_col(var_info, c("rt", "RT", "retention_time", "retention time"))
  if (is.na(annotation_id) || is.na(variable_id) || is.na(mz_col) || is.na(rt_col)) {
    stop("Could not find feature ID, mz, or rt in the massdataset variable information.")
  }
  
  feature_coordinates <- data.frame(
    feature_id = var_info[[variable_id]], 
    mz = suppressWarnings(as.numeric(var_info[[mz_col]])), 
    rt = suppressWarnings(as.numeric(var_info[[rt_col]])), 
    stringsAsFactors = FALSE
  )
  names(feature_coordinates)[1] <- annotation_id
  
  for (coordinate in c("mz", "rt")) {
    old <- find_col(ann, coordinate)
    if (!is.na(old)) names(ann)[match(old, names(ann))] <- paste0("library_", coordinate)
  }
  merge(ann, feature_coordinates, by = annotation_id, all.x = TRUE, sort = FALSE)
}

best_candidate_per_feature <- function(x) {
  feature_id <- find_col(x, c("variable_id", "Variable.ID", "variable", "variableID"))
  if (is.na(feature_id)) return(x)
  score_col <- find_col(x, c("total.score", "Total.Score", "score", "Score", "ms2.score", "MS2.Score"))
  if (is.na(score_col)) return(x[!duplicated(x[[feature_id]]), , drop = FALSE])
  score <- suppressWarnings(as.numeric(x[[score_col]]))
  score[is.na(score)] <- -Inf
  x <- x[order(x[[feature_id]], -score), , drop = FALSE]
  x[!duplicated(x[[feature_id]]), , drop = FALSE]
}

write_annotation_workbook <- function(x, file, sheet) {
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, sheet)
  openxlsx::writeDataTable(wb, sheet, x, withFilter = TRUE)
  openxlsx::freezePane(wb, sheet, firstRow = TRUE)
  openxlsx::setColWidths(wb, sheet, cols = seq_len(ncol(x)), widths = "auto")
  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
}

# ---- 4. PROCESS EVERY CONFIGURED MODE ---------------------------------------
results <- list()
for (mode in active_modes) {
  mode_dir <- mode_dirs[[mode]]
  output_dir <- file.path(mode_dir, "MetID_Output")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  message("\n==================================================")
  message("PROCESSING MODE: ", toupper(mode))
  message("==================================================")
  
  # 1. Peak Picking / Load Mass Dataset Object
  if (is.na(find_object(mode_dir))) {
    if (!run_massprocesser_if_needed) stop("No Result/object for ", mode, ". Set run_massprocesser_if_needed = TRUE after arranging input data.")
    do.call(massprocesser::process_data, c(list(
      path = mode_dir, polarity = mode, threads = threads, output_tic = TRUE, output_bpc = TRUE, output_rt_correction_plot = TRUE
    ), peak_params))
  }
  object <- load_object(mode_dir)
  
  # 2. Attach MS2 MGF Data
  mgf_dir <- ms2_dirs[[mode]]
  mgfs <- if (!is.null(mgf_dir) && dir.exists(mgf_dir)) list.files(mgf_dir, pattern = "\\.mgf$", recursive = TRUE, ignore.case = TRUE, full.names = TRUE) else character()
  if (length(mgfs)) {
    object <- massdataset::mutate_ms2(object, column = column_type, polarity = mode,
                                      ms1.ms2.match.mz.tol = annotation_params$ms1_ms2_ppm,
                                      ms1.ms2.match.rt.tol = annotation_params$ms1_ms2_rt_seconds, path = mgf_dir)
    message("Attached MS2 from ", length(mgfs), " MGF file(s).")
  } else message("No MGF MS2 files found in: ", mgf_dir)
  
  # 3. Load RDA Database
  message("Loading Database for ", mode, "...")
  db_env <- new.env(parent = emptyenv())
  loaded_names <- load(rda_files[[mode]], envir = db_env)
  database <- db_env[[loaded_names[1]]] 
  
  # Validate Database Structure
  metid::check_database(database) 
  message("Database loaded and validated successfully.")
  
  # 4. Perform Annotation (MS1 + MS2)
  annotated <- metid::annotate_metabolites_mass_dataset(
    object = object, database = database, polarity = mode, column = column_type,
    ms1.match.ppm = annotation_params$ms1_ppm, ms2.match.ppm = annotation_params$ms2_ppm,
    ms2.match.tol = annotation_params$ms2_tol, rt.match.tol = NA,
    candidate.num = annotation_params$candidate_num, threads = threads
  )
  saveRDS(annotated, file.path(output_dir, "annotated_mass_dataset.rds"), compress = FALSE)
  ann <- massdataset::extract_annotation_table(annotated)
  
  # 5. Export Results
  if (nrow(ann)) {
    all_annotations <- add_feature_coordinates(ann, annotated)
    review_targets <- best_candidate_per_feature(all_annotations)
    
    write_annotation_workbook(all_annotations, file.path(output_dir, "metid_all_candidates.xlsx"), "All candidates")
    write_annotation_workbook(review_targets, file.path(output_dir, "metid_targets_FOR_REVIEW.xlsx"), "For review")
  } else {
    message("No MetID annotations were produced for ", mode, ".")
  }
  
  results[[mode]] <- list(output_dir = output_dir, annotation_rows = nrow(ann))
  message("MetID rows: ", nrow(ann), ". Review output saved in: ", output_dir)
}

message("\n==================================================")
message("ALL MODES FINISHED SUCCESSFULLY WITH MS2 MATCHING: ", paste(names(results), collapse = ", "))
message("==================================================")

