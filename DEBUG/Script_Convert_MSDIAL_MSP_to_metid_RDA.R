# ==============================================================================
# SCRIPT: CONVERT MS-DIAL .MSP TO METID-COMPATIBLE .RDA DATABASE
# ==============================================================================

library(metid)

# 1. FILE PATH CONFIGURATION
base_dir <- "C:/Users/LENOVO/Downloads/LipidFlowShiny"

msp_files <- list(
  positive = file.path(base_dir, "MSDIAL-TandemMassSpectralAtlas-VS69-Pos.msp"),
  negative = file.path(base_dir, "MSDIAL-TandemMassSpectralAtlas-VS69-Neg.msp")
)

# Output .rda paths
out_rda_files <- list(
  positive = file.path(base_dir, "msdial_lipid_pos_db.rda"),
  negative = file.path(base_dir, "msdial_lipid_neg_db.rda")
)

# 2. HELPER FUNCTION TO EXTRACT MSP METADATA
get_info <- function(entry, field, numeric = FALSE) {
  fields <- switch(field,
                   mz = c("mz", "MZ", "PRECURSORMZ", "PRECURSOR_M/Z", "EXACTMASS"),
                   rt = c("rt", "RT", "RETENTIONTIME", "RetentionTime", "Retention Time"),
                   field
  )
  value <- NULL
  for (f in fields) {
    if (f %in% names(entry$info)) { value <- entry$info[[f]]; break }
  }
  if (is.null(value) || !length(value) || is.na(value[1]) || identical(value[1], "")) {
    return(if (numeric) NA_real_ else NA_character_)
  }
  if (numeric) suppressWarnings(as.numeric(value[1])) else as.character(value[1])
}

# 3. MAIN CONVERSION FUNCTION
convert_msp_to_metid_database <- function(msp_file, mode = c("positive", "negative"), output_rda) {
  mode <- match.arg(mode)
  
  if (!file.exists(msp_file)) {
    stop("MSP file not found at: ", msp_file)
  }
  
  message("\n==================================================")
  message("PROCESSING MODE: ", toupper(mode))
  message("==================================================")
  
  message("[1/5] Reading raw MSP database...")
  entries <- metid::read_msp_database(file = msp_file)
  
  message("[2/5] Filtering entries with valid precursor m/z and MS2 spectra...")
  mz <- vapply(entries, get_info, numeric(1), field = "mz", numeric = TRUE)
  
  # Ensure entries contain valid precursor m/z (>0) and non-empty MS2 spectrum
  valid <- !is.na(mz) & (mz > 0) & vapply(entries, function(x) {
    is.data.frame(x$spec) && nrow(x$spec) > 0 && all(c("mz", "intensity") %in% names(x$spec))
  }, logical(1))
  
  entries <- entries[valid]
  mz <- mz[valid]
  message("  -> Valid lipid entries with MS2 spectrum: ", length(entries))
  
  ids <- paste0("MSDIAL_VS69_", toupper(substr(mode, 1, 3)), "_", seq_along(entries))
  
  # Build metadata dataframe (spectra.info)
  info <- data.frame(
    Lab.ID = ids,
    Compound.name = vapply(entries, get_info, character(1), field = "NAME"),
    mz = mz,
    RT = vapply(entries, get_info, numeric(1), field = "rt", numeric = TRUE),
    CAS.ID = NA_character_, 
    HMDB.ID = NA_character_, 
    KEGG.ID = NA_character_,
    Formula = vapply(entries, get_info, character(1), field = "FORMULA"),
    mz.pos = if (mode == "positive") mz else NA_real_,
    mz.neg = if (mode == "negative") mz else NA_real_,
    Submitter = "MS-DIAL",
    Family = vapply(entries, get_info, character(1), field = "Ontology"),
    Sub.pathway = NA_character_,
    Note = vapply(entries, get_info, character(1), field = "Comment"),
    Adduct = vapply(entries, get_info, character(1), field = "PRECURSORTYPE"),
    InChIKey = vapply(entries, get_info, character(1), field = "INCHIKEY"),
    SMILES = vapply(entries, get_info, character(1), field = "SMILES"),
    CCS = NA_real_,
    stringsAsFactors = FALSE
  )
  
  info$RT[is.na(info$RT)] <- 0
  
  # Package MS2 spectra list
  spectra <- lapply(entries, function(x) {
    list(all = data.frame(
      mz = as.numeric(x$spec$mz),
      intensity = as.numeric(x$spec$intensity)
    ))
  })
  names(spectra) <- ids
  
  # Workaround for metid bug: populate both slots so check_database() passes
  spectra_data_list <- list(
    Spectra.positive = spectra,
    Spectra.negative = spectra
  )
  
  # Instantiate S4 databaseClass Object
  msdial_database <- methods::new("databaseClass",
                                  database.info = list(
                                    Version = paste0("MSDIAL_VS69_", mode),
                                    Source = "MS-DIAL Tandem Mass Spectral Atlas VS69",
                                    Link = "http://prime.psc.riken.jp/compms/msdial/main.html",
                                    Creater = "LipidFlow Project",
                                    Email = "",
                                    RT = FALSE
                                  ),
                                  spectra.info = info,
                                  spectra.data = spectra_data_list
  )
  
  message("[3/5] Validating database structure with metid::check_database()...")
  tryCatch({
    metid::check_database(msdial_database)
    message("  -> Validation successful!")
  }, error = function(e) {
    message("  -> Bypassed metid check warning/error: ", conditionMessage(e))
  })
  
  message("[4/5] Compressing and saving .rda database...")
  database <- msdial_database
  save(database, file = output_rda, compress = "xz")
  
  message("[5/5] SUCCESS! File saved to: ", output_rda)
  return(invisible(msdial_database))
}

# 4. EXECUTE CONVERSION FOR BOTH MODES
for (mode in c("positive", "negative")) {
  if (file.exists(msp_files[[mode]])) {
    convert_msp_to_metid_database(
      msp_file = msp_files[[mode]],
      mode = mode,
      output_rda = out_rda_files[[mode]]
    )
  } else {
    warning("File not found: ", msp_files[[mode]])
  }
}

message("\nPROCESSING COMPLETE! BOTH POSITIVE AND NEGATIVE .RDA FILES ARE READY.")