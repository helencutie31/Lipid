library(lipidflow)
library(tidyverse)
library(readxl)
library(openxlsx)
library(Biobase)
library(MSnbase)


if (!methods::isClass("NAnnotatedDataFrame")) {
  methods::setClass("NAnnotatedDataFrame", contains = "AnnotatedDataFrame")
}
methods::isClass("NAnnotatedDataFrame")


is_info_table_pos =
  readxl::read_xlsx("run/POS/IS_information.xlsx")

is_info_table_new_pos =  
  get_IS_RT(
    path = "run/POS/B12W",
    is_info_table = is_info_table_pos,
    polarity = "positive",
    threads = 3,
    rerun = FALSE,
    output_eic = TRUE
  )

is_info_table_new_pos =  
  get_IS_RT(
    path = "run/POS/B24M",
    is_info_table = is_info_table_pos,
    polarity = "positive",
    threads = 3,
    rerun = FALSE,
    output_eic = TRUE
  )

openxlsx::write.xlsx(
  is_info_table_new_pos,
  file = "run/POS/IS_info_new.xlsx",
  asTable = TRUE
)


is_info_table_neg = 
  readxl::read_xlsx("run/NEG/IS_information.xlsx")

is_info_table_new_neg =  
  get_IS_RT(
    path = "run/NEG/B12W",
    is_info_table = is_info_table_neg,
    polarity = "negative",
    threads = 3,
    rerun = FALSE,
    output_eic = TRUE
  )

is_info_table_new_neg =  
  get_IS_RT(
    path = "run/NEG/B24M",
    is_info_table = is_info_table_neg,
    polarity = "negative",
    threads = 3,
    rerun = FALSE,
    output_eic = TRUE
  )


openxlsx::write.xlsx(
  is_info_table_new_neg,
  file = "run/NEG/IS_info_new.xlsx",
  asTable = TRUE
)

library(lipidflow)
library(tidyverse)
library(openxlsx)

is_info_table_new_pos = 
  readxl::read_xlsx("run/POS/IS_info_new.xlsx")

is_info_table_new_neg = 
  readxl::read_xlsx("run/NEG/IS_info_new.xlsx")

is_info_table_new =
  is_info_table_new_pos %>%
  dplyr::left_join(is_info_table_new_neg[, c("name", "rt_neg_second", "rt_neg_min", "adduct_neg", "mz_neg")],
                   by = "name")

dir.create("run/Result")
dir.create("run/Result")
openxlsx::write.xlsx(is_info_table_new,
                     file = "run/Result/IS_info_table.xlsx")

sample_info_pos =
  generate_sample_info(path = "run/POS")

if (any(is_info_table_new$name == "Cholesterol")) {
  idx = which(is_info_table_new$name == "Cholesterol")
  chol_rt2 = c(is_info_table_new$rt_pos_second[idx],
               is_info_table_new$rt_neg_second[idx])
  chol_rt2 = chol_rt2[!is.na(chol_rt2)]
  if (length(chol_rt2) > 0) {
    chol_rt = chol_rt2[1]
  }
} else{
  chol_rt = 1169
}

get_relative_quantification(
  path = "run/POS",
  output_path_name = "is_relative_quantification",
  targeted_table_name = "IS_info_new.xlsx",
  sample_info = sample_info_pos,
  targeted_table_type = "is",
  polarity = "positive",
  chol_rt = chol_rt,
  output_eic = TRUE,
  ppm = 40,
  rt.tolerance = 180,
  threads = 5,
  rerun = TRUE
)

get_relative_quantification(
  path = "run/POS",
  output_path_name = "lipid_relative_quantification",
  targeted_table_name = "lipid_annotation_table_pos.xlsx",
  sample_info = sample_info_pos,
  targeted_table_type = "lipid",
  polarity = "positive",
  chol_rt = chol_rt,
  output_eic = TRUE,
  ppm = 40,
  rt.tolerance = 180,
  threads = 5,
  rerun = FALSE
)

sample_info_neg =
  generate_sample_info(path = "run/NEG")

get_relative_quantification(
  path = "run/NEG",
  output_path_name = "is_relative_quantification",
  targeted_table_name = "IS_info_new.xlsx",
  sample_info = sample_info_neg,
  targeted_table_type = "is",
  polarity = "negative",
  chol_rt = chol_rt,
  output_integrate = TRUE,
  output_eic = TRUE,
  ppm = 40,
  rt.tolerance = 180,
  threads = 5,
  rerun = TRUE
)
  
  get_relative_quantification(
    path = "run/NEG",
    output_path_name = "lipid_relative_quantification",
    targeted_table_name = "lipid_annotation_table_neg.xlsx",
    sample_info = sample_info_neg,
    targeted_table_type = "lipid",
    polarity = "negative",
    chol_rt = chol_rt,
    output_eic = TRUE,
    ppm = 40,
    rt.tolerance = 180,
    threads = 5,
    rerun = FALSE
  )
  
  
  ## mannualy
  get_relative_quantification(
    path = "run/POS",
    forced_targeted_peak_table_name = "forced_targeted_peak_table_temple.xlsx",
    output_path_name = "is_relative_quantification",
    targeted_table_name = "IS_info_new.xlsx",
    sample_info = sample_info_pos,
    targeted_table_type = "is",
    polarity = "positive",
    chol_rt = chol_rt,
    output_integrate = TRUE,
    output_eic = TRUE,
    ppm = 40,
    rt.tolerance = 180,
    threads = 5,
    rerun = FALSE
  )

  ##internal standard relative quantification data
  is_quantification_table =
    readxl::read_xlsx("run/POS/is_relative_quantification/is_quantification_table.xlsx")
  
  ##lipid relative quantification data
  lipid_quantification_table =
    readxl::read_xlsx("run/POS/lipid_relative_quantification/lipid_quantification_table.xlsx")
  
  sample_info_pos =
    generate_sample_info(path = "run/POS")
  
  
  match_item_pos =
    list(
      "Cer" = "d18:1 (d7)-15:0 Cer",
      "ChE" = c("18:1(d7) Chol Ester", "Cholesterol (d7)"),
      "Chol" = "Cholesterol (d7)",
      "DG" = "15:0-18:1(d7) DAG",
      "LPC" = "18:1(d7) Lyso PC",
      "LPE" = "18:1(d7) Lyso PE",
      "MG" = "18:1 (d7) MG",
      "PA" = "15:0-18:1(d7) PA (Na Salt)",
      "PC" = "15:0-18:1(d7) PC",
      "PE" = "15:0-18:1(d7) PE",
      "PG" = "15:0-18:1(d7) PG (Na Salt)",
      "PI" = "15:0-18:1(d7) PI (NH4 Salt)",
      "PPE" = "C18(Plasm)-18:1(d9) PE",
      "PS" = "15:0-18:1(d7) PS (Na Salt)",
      "SM" = "d18:1-18:1(d9) SM",
      "TG" = "15:0-18:1(d7)-15:0 TAG"
    )
  
  absolute_data_pos = get_absolute_quantification(
    path = "run/POS/",
    is_quantification_table = is_quantification_table,
    lipid_quantification_table = lipid_quantification_table,
    sample_info = sample_info_pos,
    match_item = match_item_pos
  )
 
  is_quantification_table =
    readxl::read_xlsx("run/NEG/is_relative_quantification/is_quantification_table.xlsx")
  
  lipid_quantification_table =
    readxl::read_xlsx("run/NEG/lipid_relative_quantification/lipid_quantification_table.xlsx")
  
  sample_info_neg =
    generate_sample_info(path = "run/NEG")
  
  match_item_neg =
    list(
      "Cer" = "d18:1 (d7)-15:0 Cer",
      "Chol" = "Cholesterol (d7)",
      "ChE" = c("18:1(d7) Chol Ester", "Cholesterol (d7)"),
      "LPC" = "18:1(d7) Lyso PC",
      "LPE" = "18:1(d7) Lyso PE",
      "PC" = "15:0-18:1(d7) PC",
      "PE" = "15:0-18:1(d7) PE",
      "PG" = "15:0-18:1(d7) PG (Na Salt)",
      "PI" = "15:0-18:1(d7) PI (NH4 Salt)",
      "PPE" = "C18(Plasm)-18:1(d9) PE",
      "PS" = "15:0-18:1(d7) PS (Na Salt)",
      "SM" = "d18:1-18:1(d9) SM"
    )
  
  absolute_data_neg = get_absolute_quantification(
    path = "run/NEG",
    is_quantification_table = is_quantification_table,
    lipid_quantification_table = lipid_quantification_table,
    sample_info = sample_info_neg,
    match_item = match_item_neg
  )
 
  combine_pos_neg_quantification(
    path = "run/Result",
    express_data_abs_ug_ml_pos = absolute_data_pos$express_data_abs_ug_ml,
    express_data_abs_um_pos = absolute_data_pos$express_data_abs_um,
    variable_info_abs_pos = absolute_data_pos$variable_info_abs,
    express_data_abs_ug_ml_neg = absolute_data_neg$express_data_abs_ug_ml,
    express_data_abs_um_neg = absolute_data_neg$express_data_abs_um,
    variable_info_abs_neg = absolute_data_neg$variable_info_abs
  )
  
  library(lipidflow)
  library(tidyverse)
  library(openxlsx)
  
  match_item_pos =
    list(
      "Cer" = "d18:1 (d7)-15:0 Cer",
      "ChE" = c("18:1(d7) Chol Ester", "Cholesterol (d7)"),
      "Chol" = "Cholesterol (d7)",
      "DG" = "15:0-18:1(d7) DAG",
      "LPC" = "18:1(d7) Lyso PC",
      "LPE" = "18:1(d7) Lyso PE",
      "MG" = "18:1 (d7) MG",
      "PA" = "15:0-18:1(d7) PA (Na Salt)",
      "PC" = "15:0-18:1(d7) PC",
      "PE" = "15:0-18:1(d7) PE",
      "PG" = "15:0-18:1(d7) PG (Na Salt)",
      "PI" = "15:0-18:1(d7) PI (NH4 Salt)",
      "PPE" = "C18(Plasm)-18:1(d9) PE",
      "PS" = "15:0-18:1(d7) PS (Na Salt)",
      "SM" = "d18:1-18:1(d9) SM",
      "TG" = "15:0-18:1(d7)-15:0 TAG"
    )
  
  absolute_table <-
    readxl::read_xlsx("run/Result/lipid_data_um.xlsx")
  
  reorganize_peak_plot(
    path = "run/POS/lipid_relative_quantification/",
    plot_dir = "peak_shape",
    absolute_table = absolute_table,
    match_item = match_item_pos
  )

  match_item_neg =
    list(
      "Cer" = "d18:1 (d7)-15:0 Cer",
      "Chol" = "Cholesterol (d7)",
      "ChE" = c("18:1(d7) Chol Ester", "Cholesterol (d7)"),
      "LPC" = "18:1(d7) Lyso PC",
      "LPE" = "18:1(d7) Lyso PE",
      "PC" = "15:0-18:1(d7) PC",
      "PE" = "15:0-18:1(d7) PE",
      "PG" = "15:0-18:1(d7) PG (Na Salt)",
      "PI" = "15:0-18:1(d7) PI (NH4 Salt)",
      "PPE" = "C18(Plasm)-18:1(d9) PE",
      "PS" = "15:0-18:1(d7) PS (Na Salt)",
      "SM" = "d18:1-18:1(d9) SM"
    )
  reorganize_peak_plot(
    path = "run/NEG/lipid_relative_quantification/",
    plot_dir = "peak_shape",
    absolute_table = absolute_table,
    match_item = match_item_neg
  )
  #> 1  2  3  4  5  6  7  8  9  10  11  12

  output_result(path = "run",
                match_item_pos = match_item_pos,
                match_item_neg = match_item_neg)
  
#Rerun relative and absolute quantification data
  
  sample_info_pos = generate_sample_info(path = "example/POS")
  
  get_relative_quantification(
    path = "run/POS",
    forced_targeted_peak_table_name = "forced_targeted_peak_table_temple_manual.xlsx",
    output_path_name = "lipid_relative_quantification",
    targeted_table_name = "lipid_annotation_table_pos.xlsx",
    sample_info = sample_info_pos,
    targeted_table_type = "lipid",
    polarity = "positive",
    output_eic = TRUE,
    ppm = 40,
    rt.tolerance = 180,
    threads = 5,
    rerun = FALSE
  )
  #> Manually check..
  #> 
  #> Output peak shapes...
  #> 12  
  #> Done

  organize_result(path = "run", 
                  match_item_pos = match_item_pos, 
                  match_item_neg)
  #> -------------------------------------------------------------------
  #> Get absolute quantification tables...
  #> -------------------------------------------------------------------
  #> Positive mode.
  #> 1  10  20  30  40  50  60  70  80  90  100  110  120  130  140  150  151  
  #> ✓ OK
  #> negative mode.
  #> 1  10  20  30  40  50  60  
  #> ✓ OK
  #> 
  #> -------------------------------------------------------------------
  #> Generate the peak plots for lipids...
  #> -------------------------------------------------------------------
  #> positive mode...
  #> 1  2  3  4  5  6  7  8  9  10  11  12  13  14  15  16  negative mode...
  #> 1  2  3  4  5  6  7  8  9  10  11  12  13  14  15  16  
  #> -------------------------------------------------------------------
  #> Output results...
  #> -------------------------------------------------------------------
  #> Positive mode...
  #> Cer
  #> ChE
  #> Chol  DG
  #> LPC
  #> LPE  MG
  #> PA  PC
  #> PE
  #> PG  PI
  #> PPE
  #> PS
  #> SM
  #> TG
  #> 
  #> Negative mode...
  #> Cer  Chol  ChE  LPC  LPE
  #> PC
  #> PE
  #> PG
  #> PI
  #> PPE  PS
  #> SM  
  #> 1
  #> 2
  #> 3
  #> 4
  #> 5
  #> 6
  #> 7
  #> 8
  #> 9
  #> 10
  #> 11
  #> 12
  #> 13
  #> 14
  #> 
  #> Done.
  #> 
  #> All done.
