# Helper functions cho R/annotation.R (Step 2 - Lipid annotation).
#
# metid::annotate_metabolites_mass_dataset() bat buoc tham so `database` phai
# la S4 object class "databaseClass" (kiem tra cung: is(database,
# "databaseClass")). construct_database() cua metid KHONG dung duoc cho viec
# nay - no danh cho quy trinh tu do chuan noi bo (in-house standards), doc
# raw mzXML/MGF cua tung chat chuan. Thu vien MS-DIAL la 1 file MSP co san
# (hang nghin entry ly thuyet), nen ta tu dung databaseClass truc tiep tu
# file MSP do, khong qua construct_database().
#
# Cung KHONG dung metid::read_msp_database() de parse - ham do tu dong doan
# field mz/rt bang regex va co nhanh fallback "MetAnalyzer" de xu ly khi
# khong thay field RT, de bi kich hoat sai voi thu vien ly thuyet (khong co
# RT that). Viet parser rieng, bam theo dung field chuan cua MS-DIAL
# (NAME / PRECURSORMZ / PRECURSORTYPE / Num Peaks) de tranh phu thuoc vao
# hanh vi tu dong do.
#
# CAN KIEM TRA: field name o tren la quy uoc chuan cua MS-DIAL, nhung chua
# doi chieu voi file that ban dang dung - neu file cua ban dung ten field
# khac, sua trong .lfs_parse_msdial_msp().

.lfs_lipid_class_whitelist <- function() {
  c("PPC", "PPE", "LPC", "LPE", "ChE", "TG", "DG", "MG",
    "PA", "PC", "PE", "PG", "PI", "PS", "SM", "Cer", "Chol")
}

.lfs_class_from_name <- function(name, whitelist = .lfs_lipid_class_whitelist()) {
  ordered <- whitelist[order(-nchar(whitelist))]
  pattern <- paste0("^(", paste(ordered, collapse = "|"), ")(?=[\\s\\(:]|$)")
  m <- stringr::str_extract(name, stringr::regex(pattern))
  ifelse(is.na(m), NA_character_, m)
}

# ---------------------------------------------------------------------------
# Parser MSP rieng cho dung format cua MS-DIAL - moi block cach nhau boi dong
# trong, header dang "KEY: value" ket thuc bang dong "Num Peaks: N", theo sau
# la N dong "mz intensity".
# ---------------------------------------------------------------------------
.lfs_parse_msdial_msp <- function(file_path) {
  lines <- readr::read_lines(file_path)
  lines <- lines[!is.na(lines)]
  
  blank_idx <- which(trimws(lines) == "")
  block_start <- c(1, blank_idx + 1)
  block_end <- c(blank_idx - 1, length(lines))
  keep <- block_start <= block_end
  block_start <- block_start[keep]
  block_end <- block_end[keep]
  
  records <- purrr::map(seq_along(block_start), function(i) {
    block <- lines[block_start[i]:block_end[i]]
    num_peaks_idx <- grep("^Num Peaks", block, ignore.case = TRUE)
    if (length(num_peaks_idx) == 0) return(NULL)
    
    header <- block[seq_len(num_peaks_idx - 1)]
    peak_lines <- block[(num_peaks_idx + 1):length(block)]
    peak_lines <- peak_lines[nzchar(trimws(peak_lines))]
    
    kv <- strsplit(header, ":", fixed = TRUE)
    keys <- toupper(trimws(vapply(kv, function(x) x[1], character(1))))
    vals <- trimws(vapply(kv, function(x) paste(x[-1], collapse = ":"), character(1)))
    names(vals) <- keys
    
    get_field <- function(...) {
      for (k in c(...)) {
        if (k %in% names(vals) && nzchar(vals[[k]])) return(unname(vals[[k]]))
      }
      NA_character_
    }
    
    name <- get_field("NAME")
    precursor_mz <- suppressWarnings(as.numeric(get_field("PRECURSORMZ", "EXACTMASS")))
    adduct <- get_field("PRECURSORTYPE", "ADDUCT", "ADDUCTIONNAME")
    
    # Khong dung magrittr::`%>%` (goi khong namespaced se loi "could not find
    # function" neu package khong co san pipe global) - viet bang base R.
    peak_split <- lapply(strsplit(peak_lines, "[\\s\\t]+"),
                         function(x) suppressWarnings(as.numeric(x[1:2])))
    spec <- as.data.frame(do.call(rbind, peak_split))
    if (nrow(spec) == 0) return(NULL)
    colnames(spec) <- c("mz", "intensity")
    spec <- spec[!is.na(spec$mz) & !is.na(spec$intensity), , drop = FALSE]
    
    if (is.na(name) || is.na(precursor_mz) || nrow(spec) == 0) return(NULL)
    
    list(name = name, precursor_mz = precursor_mz, adduct = adduct, spec = spec)
  })
  
  purrr::compact(records)
}

# ---------------------------------------------------------------------------
# Dung 1 databaseClass tu file MSP cua MS-DIAL cho 1 polarity. RT = FALSE
# trong database.info vi thu vien la ly thuyet (in-silico), khong co RT thuc
# nghiem - .lfs_run_annotation() se tat diem RT khi match vi ly do nay.
# ---------------------------------------------------------------------------
.lfs_build_msdial_database <- function(msp_path,
                                       polarity = c("positive", "negative"),
                                       version = "1.0.0") {
  polarity <- match.arg(polarity)
  records <- .lfs_parse_msdial_msp(msp_path)
  if (length(records) == 0) {
    stop("Khong doc duoc entry nao tu file MSP: ", msp_path,
         " - kiem tra lai field NAME / PRECURSORMZ / Num Peaks co dung ",
         "format MS-DIAL khong (xem .lfs_parse_msdial_msp()).")
  }
  
  lab_id <- sprintf("MSDIAL_%06d", seq_along(records))
  compound_name <- vapply(records, `[[`, character(1), "name")
  mz <- vapply(records, `[[`, numeric(1), "precursor_mz")
  adduct <- vapply(records, function(x) if (is.na(x$adduct)) "" else x$adduct, character(1))
  lipid_class <- vapply(compound_name, .lfs_class_from_name, character(1))
  
  spectra_info <- data.frame(
    Lab.ID = lab_id,
    Compound.name = compound_name,
    mz = mz,
    RT = NA_real_,
    Adduct = adduct,
    Class = lipid_class,
    stringsAsFactors = FALSE
  )
  
  spec_list <- purrr::map(records, function(x) list(CE0 = x$spec))
  names(spec_list) <- lab_id
  
  spectra_data <- list(Spectra.positive = list(), Spectra.negative = list())
  if (polarity == "positive") {
    spectra_data$Spectra.positive <- spec_list
  } else {
    spectra_data$Spectra.negative <- spec_list
  }
  
  methods::new(
    "databaseClass",
    database.info = list(
      Version = version,
      Source = "MS-DIAL",
      Link = "https://systemsomicslab.github.io/compms/msdial/main.html",
      Creater = "LipidFlow Shiny",
      Email = "",
      RT = FALSE
    ),
    spectra.info = spectra_info,
    spectra.data = spectra_data
  )
}

# ---------------------------------------------------------------------------
# Chay metid::annotate_metabolites_mass_dataset() voi database MS-DIAL da
# dung. rt.match.weight = 0 va rt.match.tol rat lon vi thu vien khong co RT
# that - tu tay tat diem RT thay vi trong doi metid tu dong lam dieu do chi
# vi database.info$RT = FALSE (chua xac nhan hanh vi tu dong nay co that hay
# khong, nen chu dong dat weight = 0 cho chac).
# ---------------------------------------------------------------------------
.lfs_run_annotation <- function(object, database, polarity,
                                ms1.match.ppm = 25, ms2.match.tol = 0.5,
                                candidate.num = 3, threads = 3) {
  metid::annotate_metabolites_mass_dataset(
    object = object,
    database = database,
    polarity = polarity,
    column = "rp",
    ms1.match.ppm = ms1.match.ppm,
    ms2.match.tol = ms2.match.tol,
    rt.match.tol = 1e6,
    ms1.match.weight = 0.4,
    rt.match.weight = 0,
    ms2.match.weight = 0.6,
    candidate.num = candidate.num,
    threads = threads
  )
}
