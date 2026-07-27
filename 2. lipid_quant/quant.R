library(shiny)
library(bslib)

quantify_lipids <- function(object, is_map, qc_group = "QC") {
  # is_map: named vector, lipid_class -> internal_standard_feature_id
  expr <- massdataset::extract_expression_data(object)
  ann  <- massdataset::extract_annotation_table(object)
  
  normalized <- expr
  for (cls in names(is_map)) {
    is_row <- expr[is_map[[cls]], ]
    cls_features <- ann$variable_id[ann$lipid_class == cls]
    normalized[cls_features, ] <- sweep(expr[cls_features, , drop = FALSE],
                                        2, as.numeric(is_row), "/")
  }
  
  qc_cols <- massdataset::extract_sample_info(object)$sample_id[
    massdataset::extract_sample_info(object)$group == qc_group
  ]
  order <- seq_along(colnames(normalized))
  corrected <- t(apply(normalized, 1, function(row) {
    fit <- loess(row[qc_cols] ~ order[colnames(normalized) %in% qc_cols])
    row / predict(fit, newdata = order)
  }))
  
  massdataset::mutate_expression_data(object, as.data.frame(corrected))
}

