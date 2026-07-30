# Cau noi giua Annotation (metid) va Quantification that (lipidflow), thay
# the nhanh "lipid" cua lipidflow::get_relative_quantification() - nhanh do
# khoa cung vao tidy_lipidsearch_data(from="lipidsearch"), khong dung duoc
# voi output cua metid. Cac ham o day GOI THANG cac ham that, export cong
# khai cua lipidflow (extract_targeted_peaks, combine_quantification_data_
# and_feature_info, get_IS_RT, get_relative_quantification voi type="is",
# get_absolute_quantification, combine_pos_neg_quantification, output_result)
# - khong sua/fork package lipidflow, chi bo qua doan parse LipidSearch.

# ---------------------------------------------------------------------------
# Dung feature_table.xlsx (schema toi thieu ma extract_targeted_peaks() can:
# name, mz, rt, adduct, + compound_name de dedup) tu annotation_result cua
# Step 2. LUU Y DON VI: rt o day lay thang tu mass_dataset (xcms/massprocesser
# dung don vi GIAY), khong nhan lai *60 nhu code goc cua lipidflow (cai do
# *60 vi RT cua LipidSearch la PHUT - truong hop cua ta khac).
# ---------------------------------------------------------------------------
.lfs_build_feature_table <- function(annotation_result) {
  if (!"Compound.name" %in% colnames(annotation_result)) {
    stop("annotation_result thieu cot 'Compound.name' - kiem tra lai output ",
         "cua metid::annotate_metabolites_mass_dataset().")
  }
  if (!"variable_id" %in% colnames(annotation_result)) {
    stop("annotation_result thieu cot 'variable_id'.")
  }
  if (!"mz" %in% colnames(annotation_result)) stop("annotation_result thieu cot 'mz'.")
  if (!"rt" %in% colnames(annotation_result)) stop("annotation_result thieu cot 'rt'.")
  
  adduct_col <- grep("adduct", colnames(annotation_result), ignore.case = TRUE, value = TRUE)
  if (length(adduct_col) == 0) {
    stop("Khong tim thay cot adduct trong annotation_result - chay ",
         "colnames(annotation_result) de xem ten cot that va sua lai ham nay.")
  }
  adduct_col <- adduct_col[1]
  
  out <- data.frame(
    name = annotation_result$variable_id,
    mz = annotation_result$mz,
    rt = annotation_result$rt,
    adduct = annotation_result[[adduct_col]],
    Class = vapply(annotation_result$Compound.name, .lfs_class_from_name, character(1)),
    compound_name = annotation_result$Compound.name,
    stringsAsFactors = FALSE
  )
  
  out[!is.na(out$compound_name) & !is.na(out$Class), , drop = FALSE]
}

# ---------------------------------------------------------------------------
# Tai hien phan con lai cua get_relative_quantification(type="lipid") SAU
# doan tidy_lipidsearch_data/clean_lipid_data (bo qua 2 ham do).
# ---------------------------------------------------------------------------
.lfs_run_lipid_relative_quant <- function(path, output_path_name, annotation_result,
                                          fit.gaussian = TRUE, integrate_xcms = TRUE,
                                          output_eic = TRUE, output_integrate = TRUE,
                                          ppm = 40, rt.tolerance = 180, threads = 3) {
  
  feature_table <- .lfs_build_feature_table(annotation_result)
  openxlsx::write.xlsx(feature_table, file.path(path, "feature_table.xlsx"), asTable = TRUE)
  
  lipidflow::extract_targeted_peaks(
    path = path,
    output_path_name = output_path_name,
    targeted_targeted_peak_table_name = "feature_table.xlsx",
    forced_targeted_peak_table_name = NULL,
    from_lipid_search = TRUE,
    fit.gaussian = fit.gaussian,
    integrate_xcms = integrate_xcms,
    output_eic = output_eic,
    output_integrate = output_integrate,
    ppm = ppm,
    rt.tolerance = rt.tolerance,
    threads = threads,
    facet = TRUE
  )
  
  output_path <- file.path(path, output_path_name)
  quantification_data <- readxl::read_xlsx(file.path(output_path, "quantification_table.xlsx"))
  quantification_data[is.na(quantification_data)] <- 0
  
  feature_info <- feature_table
  colnames(feature_info)[colnames(feature_info) == "name"] <- "peak_name"
  feature_info$mean.int <- NA_real_   # gia tri that se bi combine_quantification_data_and_feature_info() ghi de
  
  quantification_data <- lipidflow::combine_quantification_data_and_feature_info(
    quantification_data = quantification_data,
    feature_info = feature_info,
    targeted_table_type = "lipid"
  )
  
  openxlsx::write.xlsx(
    quantification_data,
    file = file.path(output_path, "lipid_quantification_table.xlsx"),
    asTable = TRUE
  )
  
  unlink(file.path(path, "feature_table.xlsx"))
  quantification_data
}

# ---------------------------------------------------------------------------
# Thay the lipidflow::get_lipid_absolute_quantification() - giu nguyen cau
# truc/thu tu goi ham that, chi swap nhanh lipid.
# ---------------------------------------------------------------------------
.lfs_run_lipid_absolute_quantification <- function(path,
                                                   annotation_result,
                                                   is_info_name = "IS_information.xlsx",
                                                   chol_rt = 1169,
                                                   ppm = 40,
                                                   rt.tolerance = 180,
                                                   threads = 3,
                                                   which_group_for_rt_confirm = "QC",
                                                   match_item_pos = .lfs_default_match_item_pos(),
                                                   match_item_neg = .lfs_default_match_item_neg()) {
  
  if (!"polarity" %in% colnames(annotation_result)) {
    stop("annotation_result thieu cot 'polarity' - can duoc gan trong ",
         "R/annotation.R (md_pos@variable_info$polarity <- 'positive') truoc khi merge.")
  }
  annotation_result_pos <- annotation_result[annotation_result$polarity == "positive", , drop = FALSE]
  annotation_result_neg <- annotation_result[annotation_result$polarity == "negative", , drop = FALSE]
  
  cat("Getting IS retention times (positive)...\n")
  is_info_table_pos <- readxl::read_xlsx(file.path(path, "POS", is_info_name))
  is_info_table_new_pos <- lipidflow::get_IS_RT(
    path = file.path(path, "POS", which_group_for_rt_confirm),
    is_info_table = is_info_table_pos,
    polarity = "positive",
    threads = threads,
    rerun = FALSE,
    output_eic = TRUE
  )
  openxlsx::write.xlsx(is_info_table_new_pos, file.path(path, "POS", "IS_info_new.xlsx"), asTable = TRUE)
  
  cat("Getting IS retention times (negative)...\n")
  is_info_table_neg <- readxl::read_xlsx(file.path(path, "NEG", is_info_name))
  is_info_table_new_neg <- lipidflow::get_IS_RT(
    path = file.path(path, "NEG", which_group_for_rt_confirm),
    is_info_table = is_info_table_neg,
    polarity = "negative",
    threads = threads,
    rerun = FALSE,
    output_eic = TRUE
  )
  openxlsx::write.xlsx(is_info_table_new_neg, file.path(path, "NEG", "IS_info_new.xlsx"), asTable = TRUE)
  
  dir.create(file.path(path, "Result"), showWarnings = FALSE)
  
  build_sample_info <- function(mode_dir) {
    mzxml <- list.files(mode_dir, pattern = "mzXML", recursive = TRUE)
    info <- as.data.frame(do.call(rbind, strsplit(mzxml, "/")), stringsAsFactors = FALSE)
    colnames(info) <- c("group", "sample.name")
    info <- info[, c("sample.name", "group")]
    info$sample.name <- sub("\\.mzXML$", "", info$sample.name)
    info
  }
  sample_info_pos <- build_sample_info(file.path(path, "POS"))
  sample_info_neg <- build_sample_info(file.path(path, "NEG"))
  
  cat("Internal standard - positive mode...\n")
  lipidflow::get_relative_quantification(
    path = file.path(path, "POS"), output_path_name = "is_relative_quantification",
    targeted_table_name = "IS_info_new.xlsx", sample_info = sample_info_pos,
    targeted_table_type = "is", polarity = "positive", chol_rt = chol_rt,
    ppm = ppm, rt.tolerance = rt.tolerance, threads = threads, rerun = FALSE
  )
  
  cat("Lipid - positive mode (metid bridge)...\n")
  .lfs_run_lipid_relative_quant(
    path = file.path(path, "POS"), output_path_name = "lipid_relative_quantification",
    annotation_result = annotation_result_pos,
    ppm = ppm, rt.tolerance = rt.tolerance, threads = threads
  )
  
  cat("Internal standard - negative mode...\n")
  lipidflow::get_relative_quantification(
    path = file.path(path, "NEG"), output_path_name = "is_relative_quantification",
    targeted_table_name = "IS_info_new.xlsx", sample_info = sample_info_neg,
    targeted_table_type = "is", polarity = "negative", chol_rt = chol_rt,
    ppm = ppm, rt.tolerance = rt.tolerance, threads = threads, rerun = FALSE
  )
  
  cat("Lipid - negative mode (metid bridge)...\n")
  .lfs_run_lipid_relative_quant(
    path = file.path(path, "NEG"), output_path_name = "lipid_relative_quantification",
    annotation_result = annotation_result_neg,
    ppm = ppm, rt.tolerance = rt.tolerance, threads = threads
  )
  
  cat("Absolute quantification...\n")
  is_quant_pos <- readxl::read_xlsx(file.path(path, "POS/is_relative_quantification/is_quantification_table.xlsx"))
  lipid_quant_pos <- readxl::read_xlsx(file.path(path, "POS/lipid_relative_quantification/lipid_quantification_table.xlsx"))
  absolute_data_pos <- lipidflow::get_absolute_quantification(
    path = file.path(path, "POS"), is_quantification_table = is_quant_pos,
    lipid_quantification_table = lipid_quant_pos, sample_info = sample_info_pos,
    match_item = match_item_pos
  )
  
  is_quant_neg <- readxl::read_xlsx(file.path(path, "NEG/is_relative_quantification/is_quantification_table.xlsx"))
  lipid_quant_neg <- readxl::read_xlsx(file.path(path, "NEG/lipid_relative_quantification/lipid_quantification_table.xlsx"))
  absolute_data_neg <- lipidflow::get_absolute_quantification(
    path = file.path(path, "NEG"), is_quantification_table = is_quant_neg,
    lipid_quantification_table = lipid_quant_neg, sample_info = sample_info_neg,
    match_item = match_item_neg
  )
  
  lipidflow::combine_pos_neg_quantification(
    path = file.path(path, "Result"),
    express_data_abs_ug_ml_pos = absolute_data_pos$express_data_abs_ug_ml,
    express_data_abs_um_pos = absolute_data_pos$express_data_abs_um,
    variable_info_abs_pos = absolute_data_pos$variable_info_abs,
    express_data_abs_ug_ml_neg = absolute_data_neg$express_data_abs_ug_ml,
    express_data_abs_um_neg = absolute_data_neg$express_data_abs_um,
    variable_info_abs_neg = absolute_data_neg$variable_info_abs
  )
  
  cat("Output results...\n")
  lipidflow::output_result(path = path, match_item_pos = match_item_pos, match_item_neg = match_item_neg)
  cat("All done.\n")
}