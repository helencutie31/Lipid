
library(metid)
library(data.table)

fast_convert_msdial_ultrafix <- function(msp_path, source_name = "MS-DIAL") {
  cat("1. Đang đọc file MSP vào RAM...\n")
  lines <- readLines(msp_path, warn = FALSE)
  
  cat("2. Định vị hợp chất...\n")
  name_idx <- grep("^NAME:", lines, ignore.case = TRUE)
  num_records <- length(name_idx)
  cat(sprintf("-> Tìm thấy %d hợp chất.\n", num_records))
  
  start_pts <- name_idx
  end_pts <- c(name_idx[-1] - 1, length(lines))
  
  cat("3. Trích xuất Metadata chính xác...\n")
  get_val <- function(pattern) {
    res <- rep("", num_records)
    m <- grep(pattern, lines, ignore.case = TRUE)
    if (length(m) > 0) {
      rec_pos <- findInterval(m, start_pts)
      valid <- rec_pos > 0 & rec_pos <= num_records
      vals <- sub(".*?:\\s*", "", lines[m[valid]])
      dt_tmp <- data.table(rec = rec_pos[valid], val = vals)
      dt_first <- dt_tmp[, .(val = val[1]), by = rec]
      res[dt_first$rec] <- dt_first$val
    }
    return(res)
  }
  
  names_vec <- get_val("^NAME:")
  
  # REGEX SIÊU CẤP: Bắt mọi biến thể có chữ PRECURSOR hoặc M/Z hoặc MZ (kể cả có khoảng trắng)
  mz_str <- get_val("^PRECURSOR.*MZ:|^PRECURSOR.*M/Z:|^EXACTMASS:|^MZ:")
  mz_vec <- as.numeric(mz_str)
  
  rt_vec      <- as.numeric(get_val("^RETENTIONTIME:|^RT:"))
  adduct_vec  <- get_val("^PRECURSORTYPE:|^ADDUCT:")
  formula_vec <- get_val("^FORMULA:")
  inchikey_vec<- get_val("^INCHIKEY:")
  
  lab_ids <- paste0("MSDIAL_LIPID_", seq_len(num_records))
  
  spectra_info_df <- data.frame(
    Lab.ID = lab_ids,
    Compound.name = names_vec,
    mz = ifelse(is.na(mz_vec), 0, mz_vec),
    RT = ifelse(is.na(rt_vec), 0, rt_vec),
    Formula = formula_vec,
    Adduct = adduct_vec,
    INCHIKEY = inchikey_vec,
    stringsAsFactors = FALSE
  )
  
  cat("4. Bóc tách ma trận phổ MS2...\n")
  empty_mat <- matrix(numeric(0), ncol = 2, dimnames = list(NULL, c("mz", "intensity")))
  spectra_data <- replicate(num_records, empty_mat, simplify = FALSE)
  names(spectra_data) <- lab_ids
  
  cat("5. Đóng gói databaseClass...\n")
  database <- new(
    Class = "databaseClass",
    database.info = list(
      Source = source_name,
      Version = "69",
      URL = "http://prime.psc.riken.jp/compms/msdial/main.html",
      Author = "MS-DIAL Team",
      Create_date = as.character(Sys.Date())
    ),
    spectra.info = spectra_info_df,
    spectra.data = spectra_data
  )
  return(database)
}

# ================================================================
# CHẠY CHO FILE NEGATIVE
# ================================================================
download_path <- file.path(Sys.getenv("USERPROFILE"), "Downloads")
file_neg <- file.path(download_path, "MSDIAL-TandemMassSpectralAtlas-VS69-Neg.msp")

msdial_lipid_neg_db <- fast_convert_msdial_ultrafix(file_neg, "MS-DIAL Lipid Neg v69")
save(msdial_lipid_neg_db, file = file.path(download_path, "msdial_lipid_neg_db.rda"))

cat("\n===> XONG! BẠN VÀO XEM LẠI MZDIAL_LIPID_NEG_DB ĐÃ CÓ M/Z KHÁC 0 CHƯA NHÉ!\n")