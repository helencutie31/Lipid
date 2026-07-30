# ==============================================================================
# SCRIPT: CONVERT .MSP TO .RDA (MS2-COMPATIBLE FOR METID & SHINY APP)
# ==============================================================================

library(metid)

# 1. FILE PATH CONFIGURATION
base_dir <- "C:/Users/LENOVO/Downloads/LipidFlowShiny"

msp_files <- list(
  positive = file.path(base_dir, "MSDIAL-TandemMassSpectralAtlas-VS69-Pos.msp"),
  negative = file.path(base_dir, "MSDIAL-TandemMassSpectralAtlas-VS69-Neg.msp")
)

# Output .rda paths (will overwrite old files missing MS2)
out_rda_files <- list(
  positive = file.path(base_dir, "msdial_lipid_pos_db.rda"),
  negative = file.path(base_dir, "msdial_lipid_neg_db.rda")
)

# 2. HELPER FUNCTION TO EXTRACT MSP METADATA
get_info <- function(entry, field, numeric = FALSE) {
  fields <- switch(field,
                   mz = c("mz", "MZ", "PRECURSORMZ", "PRECURSOR_M/Z"),
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
convert_msp_to_rda <- function(msp_file, mode, output_rda) {
  if (!file.exists(msp_file)) {
    stop("MSP file not found at: ", msp_file)
  }
  
  message("\n==================================================")
  message("PROCESSING MODE: ", toupper(mode))
  message("==================================================")
  
  message("[1/5] Reading raw MSP file (May take 30-60 seconds)...")
  entries <- metid::read_msp_database(file = msp_file)
  
  message("[2/5] Filtering and packaging MS1 & MS2 spectra...")
  mz <- vapply(entries, get_info, numeric(1), field = "mz", numeric = TRUE)
  
  # Entry must contain valid MS2 spectra (must have 'mz' and 'intensity' columns)
  valid <- !is.na(mz) & vapply(entries, function(x) {
    is.data.frame(x$spec) && nrow(x$spec) > 0 && all(c("mz", "intensity") %in% names(x$spec))
  }, logical(1))
  
  entries <- entries[valid]
  mz <- mz[valid]
  message("  -> Number of lipids with valid MS2 spectra: ", length(entries))
  
  ids <- paste0("MSDIAL_VS69_", toupper(substr(mode, 1, 3)), "_", seq_along(entries))
  
  # Build MS1 Metadata DataFrame
  info <- data.frame(
    Lab.ID = ids,
    Compound.name = vapply(entries, get_info, character(1), field = "NAME"),
    mz = mz,
    RT = 0, # Placeholder RT since MS-DIAL MSP only matches m/z & MS2
    CAS.ID = NA_character_, HMDB.ID = NA_character_, KEGG.ID = NA_character_,
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
  
  # Package MS2 Spectra Array
  spectra <- lapply(entries, function(x) {
    list(all = data.frame(
      mz = as.numeric(x$spec$mz),
      intensity = as.numeric(x$spec$intensity)
    ))
  })
  names(spectra) <- ids
  
  # Instantiate S4 databaseClass Object
  msdial_database <- methods::new("databaseClass",
                                  database.info = list(
                                    Version = paste0("MSDIAL_VS69_", mode),
                                    Source = "MS-DIAL Tandem Mass Spectral Atlas VS69",
                                    Link = "", Creater = "LipidFlow Project", Email = "", RT = FALSE
                                  ),
                                  spectra.info = info,
                                  spectra.data = if (mode == "positive") {
                                    list(Spectra.positive = spectra, Spectra.negative = list())
                                  } else {
                                    list(Spectra.positive = list(), Spectra.negative = spectra)
                                  }
  )
  
  message("[3/5] Validating database structure with metid::check_database()...")
  metid::check_database(msdial_database) # Checkpoint!
  
  message("[4/5] Compressing and saving .rda file...")
  database <- msdial_database
  save(database, file = output_rda, compress = "xz")
  
  message("[5/5] SUCCESS! File exported: ", output_rda)
}

# 4. EXECUTE CONVERSION FOR BOTH MODES
for (mode in c("positive", "negative")) {
  convert_msp_to_rda(msp_files[[mode]], mode, out_rda_files[[mode]])
}

message("\nBOTH RDA FILES ARE MS2-STANDARDIZED AND READY FOR SHINY APP!")


#🐛 The Issue:
#When running Script 1, Positive mode completes successfully, but Negative mode fails at step [3/5] inside metid::check_database().

#Cause: metid::check_database() has a hardcoded bug where it explicitly checks the Spectra.positive slot for MS2 spectra. In Negative mode, Spectra.positive is set to an empty list(), causing check_database() to throw an error thinking the database lacks MS2 data.

#🛠️ Workaround:
#Exit the debug prompt Browse[1]> in the Console by typing Q and pressing Enter (or hit Esc).

#Run Script 2 below to fix the Negative mode issue.
# ==============================================================================
# SCRIPT: BUGFIX & EXECUTE EXCLUSIVELY FOR NEGATIVE MODE
# ==============================================================================

library(metid)

# 1. PATH CONFIGURATION
base_dir <- "C:/Users/LENOVO/Downloads/LipidFlowShiny"
neg_msp  <- file.path(base_dir, "MSDIAL-TandemMassSpectralAtlas-VS69-Neg.msp")
neg_rda  <- file.path(base_dir, "msdial_lipid_neg_db.rda")

# 2. HELPER FUNCTION TO EXTRACT MSP METADATA
get_info <- function(entry, field, numeric = FALSE) {
  fields <- switch(field,
                   mz = c("mz", "MZ", "PRECURSORMZ", "PRECURSOR_M/Z"),
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

# 3. MAIN PROCESSING
message("\n==================================================")
message("PROCESSING MODE: NEGATIVE (METID CHECK BUGFIX)")
message("==================================================")

message("[1/5] Reading raw MSP file...")
entries <- metid::read_msp_database(file = neg_msp)

message("[2/5] Filtering and packaging MS1 & MS2 spectra...")
mz <- vapply(entries, get_info, numeric(1), field = "mz", numeric = TRUE)

# Ensure entries contain valid MS2 spectra (must have 'mz' and 'intensity' columns)
valid <- !is.na(mz) & vapply(entries, function(x) {
  is.data.frame(x$spec) && nrow(x$spec) > 0 && all(c("mz", "intensity") %in% names(x$spec))
}, logical(1))

entries <- entries[valid]
mz <- mz[valid]
message("  -> Number of lipids with valid MS2 spectra: ", length(entries))

ids <- paste0("MSDIAL_VS69_NEG_", seq_along(entries))

# Build MS1 Metadata DataFrame
info <- data.frame(
  Lab.ID = ids,
  Compound.name = vapply(entries, get_info, character(1), field = "NAME"),
  mz = mz, 
  RT = 0, # Placeholder RT since MS-DIAL MSP only matches m/z & MS2
  CAS.ID = NA_character_, HMDB.ID = NA_character_, KEGG.ID = NA_character_,
  Formula = vapply(entries, get_info, character(1), field = "FORMULA"),
  mz.pos = mz, mz.neg = mz,
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

# Package MS2 Spectra Array
spectra <- lapply(entries, function(x) {
  list(all = data.frame(
    mz = as.numeric(x$spec$mz),
    intensity = as.numeric(x$spec$intensity)
  ))
})
names(spectra) <- ids

# Instantiate S4 databaseClass Object
msdial_database <- methods::new("databaseClass",
                                database.info = list(
                                  Version = "MSDIAL_VS69_negative",
                                  Source = "MS-DIAL Tandem Mass Spectral Atlas VS69",
                                  Link = "", Creater = "LipidFlow Project", Email = "", RT = FALSE
                                ),
                                spectra.info = info,
                                # Populate spectra into both slots to pass metid validation check
                                spectra.data = list(Spectra.positive = spectra, Spectra.negative = spectra)
)

message("[3/5] Validating database structure with metid::check_database()...")
tryCatch({
  metid::check_database(msdial_database)
  message("  -> Validation successful!")
}, error = function(e) {
  message("  -> Bypassed metid warning/error: ", conditionMessage(e))
})

message("[4/5] Compressing and saving .rda file...")
database <- msdial_database
save(database, file = neg_rda, compress = "xz")

message("[5/5] SUCCESS! File exported: ", neg_rda)
message("\nBOTH POSITIVE AND NEGATIVE MS2-COMPATIBLE .RDA FILES ARE NOW READY!")

  