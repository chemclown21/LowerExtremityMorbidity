#=======================================
#LE Trauma Vulnerability Project
#Step 2: Decode DCodes and ECodes for stitched dataset
#Code written by Vitto Resnick, 10/08/25
#=======================================

#========= UTILITIES =========
rm(list = ls(all.names = TRUE))

library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(stringr)
library(cobalt)
library(WeightIt)
library(survey)


#Tim's Cleaning Function for parsing ICD code punctuation
norm_code <- function(x) {
  x <- as.character(x)
  x <- sub("^\\s+|\\s+$", "", x)                         # trim
  x <- sub("^([0-9]+\\.?[0-9]*).*", "\\1", x)            # keep leading numeric part
  sub("\\.?0+$", "", x)                                  # drop trailing .0/ .00
}


LE_10_fracture_map <- c(
  "S32.4"  = "acetabulum",
  "S32.5"  = "pubis",
  "S32.3"  = "ilium",
  "S32.6"  = "ischium",
  "S32.81" = "pelvis_w_dis",
  "S32.82" = "pelvis_wo_dis",
  "S32.88" = "other_pelvis",
  "S32.89" = "other_pelvis",
  "S32.9"  = "other_pelvis",
  "S32.8"  = "other_pelvis",
  "S72.00" = "femoral_neck",
  "S72.01" = "femoral_neck",
  "S72.02" = "femoral_neck",
  "S72.03" = "femoral_neck",
  "S72.04" = "femoral_neck",
  "S72.05" = "other_femur",
  "S72.06" = "other_femur",
  "S72.08" = "other_femur",
  "S72.09" = "other_femur",
  "S72.1"  = "other_femur",
  "S72.2"  = "other_femur",
  "S72.4"  = "other_femur",
  "S72.8"  = "other_femur",
  "S72.9"  = "other_femur",
  "S82.0"  = "patella",
  "S82.1"  = "tibia_fibula",
  "S82142A"= "tibia_fibula",
  "S82.2"  = "tibia_fibula",
  "S82.3"  = "tibia_fibula",
  "S89.0"  = "tibia_fibula",
  "S89.1"  = "tibia_fibula",
  "S89.2"  = "tibia_fibula",
  "S89.3"  = "tibia_fibula",
  "S82.87" = "tibia_fibula",
  "S82.4"  = "tibia_fibula",
  "S82.81" = "tibia_fibula",
  "S82.82" = "tibia_fibula",
  "S82.83" = "tibia_fibula",
  "S82.86" = "tibia_fibula",
  "S82.5"  = "ankle",
  "S82.6"  = "ankle",
  "S82.84" = "ankle",
  "S82.85" = "ankle",
  "S82.88" = "other_LE",
  "S82.89" = "other_LE",
  "S82.9"  = "other_LE",
  "S92"    = "other_LE",
  "S89.8"  = "other_LE",
  "S89.9"  = "other_LE",
  "S72.3"  = "femoral_shaft",
  "S42.0"  = "clavicle",
  "S42.1"  = "scapula",
  "S42.2"  = "humerus",
  "S42.3"  = "humerus",
  "S42.4"  = "humerus",
  "S52"    = "radius_ulna",
  "S62"    = "carpal",
  "S42.9" = "other_UE"
)


map_injury_location <- function(code, map = LE_10_fracture_map) {
  
  code <- as.character(code)
  if (is.na(code) || !nzchar(code)) return(NA_character_)
  
  if (startsWith(code, "S")) {
    codes <- names(map)
    ord <- order(nchar(codes), decreasing = TRUE)
    codes <- codes[ord]
    labels <- unname(map[ord])
    
    out <- rep(NA_character_, length(code))
    # vectorized longest-prefix fill
    for (i in seq_along(codes)) {
      m <- is.na(out) & startsWith(code, codes[i])
      if (any(m)) out[m] <- labels[i]
      if (all(!is.na(out))) break
    }
    out
    
  } else {
    code_num <- as.numeric(code)
    cc <- if (!is.na(code)) norm_code(code) else NA_character_
    cn <- suppressWarnings(as.numeric(cc))                 # numeric view from char
    
    if (is.na(code_num)) code_num <- cn                    # prefer provided numeric, else from char
    
    # --- Pelvis block: any 808.00–808.999... is pelvis ---
    if (!is.na(code_num) && code_num >= 808 & code_num < 809) {
      # Specific pelvis subcodes:
      if (!is.na(cc)) {
        if (cc %in% c("808","808.0","808.1")) return("acetabulum")
        if (cc %in% c("808.2","808.3")) return("pubis")
        if (cc %in% c("808.41","808.51")) return("ilium")
        if (cc %in% c("808.42","808.52")) return("ischium")
        if (cc %in% c("808.43","808.53")) return("pelvis_w_dis")
        if (cc %in% c("808.44","808.54")) return("pelvis_wo_dis")
      }
      # Any other 808.* that didn't match specifics:
      return("other_pelvis")
    }
    
    # --- Other numeric ranges ---
    if (!is.na(code_num) && code_num >= 810 & code_num < 811) return("clavicle")
    if (!is.na(code_num) && code_num >= 811 & code_num < 812) return("scapula")
    if (!is.na(code_num) && code_num >= 812 & code_num < 813) return("humerus")
    if (!is.na(code_num) && code_num >= 813 & code_num < 814) return("radius_ulna")
    if (!is.na(code_num) && code_num >= 814 & code_num < 815) return("carpal")
    if (!is.na(code_num) && code_num >= 815 & code_num < 819.9) return("other_UE")
    if (!is.na(code_num) && code_num >= 820 & code_num < 821) return("femoral_neck")
    if (!is.na(code_num) && code_num %in% c(821.01,821.11)) return("femoral_shaft")
    if (!is.na(code_num) && code_num >= 821 & code_num < 822) return("other_femur")
    if (!is.na(code_num) && code_num >= 822 & code_num < 823) return("patella")
    if (!is.na(code_num) && code_num >= 823 & code_num < 824) return("tibia_fibula")
    if (!is.na(code_num) && code_num >= 824 & code_num < 825) return("ankle")
    if (!is.na(code_num) && code_num >= 825 & code_num < 829.99) return("other_LE")
    
    NA_character_
  }
}


#========= LOADING DATA =========
# Read in your file (assume you named it classified_data.csv)
files_directory = '/Users/vresnick/Documents/GitHub/LowerExtremityMorbidity'
setwd(files_directory)
outdir = "outputs"

dt <- readRDS("outputs/1_stitched_Pre_Post_251009.rds")

#========= TIM CODE: INJ TYPE =========


#print number of 809 codes (fracture of the trunk -> these should be removed)
n_809_total <- dt[, {
  le <- unlist(tstrsplit(LE_Dcode, ",", fixed = TRUE, type.convert = TRUE))
  ue <- unlist(tstrsplit(UE_Dcode, ",", fixed = TRUE, type.convert = TRUE))
  vals <- c(le, ue)
  sum(!is.na(vals) & vals >= 809 & vals < 810)
}]
print(n_809_total) #0

#Remove Spine Injury from New Dataset S32
dt <- dt[ is.na(LE_Dcode) | !startsWith(as.character(LE_Dcode), "S32") ]

# Split LE_Dcode
le_split <- dt[, tstrsplit(LE_Dcode, ",", fixed=TRUE, type.convert=TRUE)]
setnames(le_split, paste0("LE_Dcode", seq_len(ncol(le_split))))

# Split UE_Dcode
ue_split <- dt[, tstrsplit(UE_Dcode, ",", fixed=TRUE, type.convert=TRUE)]
setnames(ue_split, paste0("UE_Dcode", seq_len(ncol(ue_split))))

# Combine all: remove original LE_Dcode and UE_Dcode columns, then add split columns
dt_out <- cbind(
  dt[, !c("LE_Dcode", "UE_Dcode"), with=FALSE], # all other columns
  le_split,
  ue_split
)



setDT(dt_out)
# 1) Counts (your existing lines)
dt_out[, n_LE := rowSums(!is.na(.SD)), .SDcols = patterns("^LE_Dcode")]
dt_out[, n_UE := rowSums(!is.na(.SD)), .SDcols = patterns("^UE_Dcode")]

# 2) Flag any 827.xx in the row
#    (search all UE/LE code columns; if you want to restrict to LE only, use .SDcols = patterns("^LE_Dcode"))
code_cols <- grep("^(UE|LE)_Dcode", names(dt_out), value = TRUE)
dt_out[, has_827 := Reduce(`|`, lapply(.SD, function(x) {
  # keep leading numeric part, drop trailing .0s, then to numeric
  v <- as.numeric(sub("\\.?0+$", "", sub("^([0-9]+\\.?[0-9]*).*", "\\1", as.character(x))))
  !is.na(v) & v >= 827 & v < 828
})), .SDcols = code_cols]

# 3) Reclassify (UE present -> ULE; otherwise 827 bumps to MLE)
dt_out[, InjType :=
         fcase(
           n_LE >= 1 & n_UE >= 1,            "ULE",  # any UE + any LE
           (n_LE > 1 | has_827) & n_UE == 0, "MLE",  # 827 -> MLE even if n_LE == 1
           n_LE == 1 & n_UE == 0,            "ILE",
           default = NA_character_
         )
]

# 4) Clean up (optional)
dt_out[, c("n_LE", "n_UE", "has_827") := NULL]

# Preview
print(dt_out[, .(LE_Dcode1, LE_Dcode2, UE_Dcode1, InjType)])

dt_out[, .N, by = InjType]
table(dt_out$InjType)


#========= TIM CODE: INJ TYPE VALIDATION (PRE-DB) =========

#count number of discrepancies between old classifcation and new_classifications

# 1) Normalize for fair comparison (trim + lowercase)
dt_out[Guidelines=="Pre", `:=`(
  frac_norm = tolower(trimws(`Fracture.Type`)),
  cls_norm  = tolower(trimws(InjType))
)]

# 2) Flag discrepancies (treat both NA as a match)
dt_out[Guidelines=="Pre", discrepancy := fifelse(
  (is.na(frac_norm) & is.na(cls_norm)) | (frac_norm == cls_norm),
  0L, 1L
)]

# 3) Count discrepancies and show percent
n_total <- nrow(dt_out[Guidelines=="Pre"])
n_disc  <- dt_out[Guidelines=="Pre", sum(discrepancy)]
pct_disc <- round(100 * n_disc / n_total, 2)

cat("Discrepancies:", n_disc, "of", n_total, "rows (", pct_disc, "%)\n", sep=" ")

# 4) Confusion matrix (who mismatches with what)
conf_mat <- dt_out[Guidelines=="Pre", .N, by = .(frac_norm, cls_norm)][order(-N)]
print(conf_mat)

# 5) show a few mismatched rows for review
dt_out[discrepancy == 1,
       .(inc_key, `Fracture.Type`, InjType)][1:50]

#6) print out codes to see reason for discrepency 
# all code columns
code_cols <- grep("^(LE|UE)_Dcode", names(dt_out), value = TRUE)

# change this with the codes you see in step 5
ids <- c(9093838, 9235692, 10222010, 10385384, 11343957, 12293034,
         13167012, 13247486, 14441530, 140237448, 140766957)

# wide view (all code columns side-by-side)
dt_out[inc_key %in% ids, c("inc_key", "Fracture.Type", "InjType", code_cols), with = FALSE]
#all of these are 827 codes -> relate to multiple fractures, our new_classifications is correct

#========= TIM CODE: ICD 9 DECODING (PRE-DB) =========

# 1. Find UE/LE code columns
ue_cols <- grep("^UE_Dcode", names(dt_out), value = TRUE)
le_cols <- grep("^LE_Dcode", names(dt_out), value = TRUE)
all_code_cols <- c(ue_cols, le_cols)

# 2. Apply your mapping function rowwise across all those columns
dt_out <- dt_out %>%
  rowwise() %>%
  mutate(
    injury_location = {
      codes <- c_across(all_of(all_code_cols))
      locs  <- sapply(codes, map_injury_location, USE.NAMES = FALSE)
      # Collapse multiple matches into a single string (comma-separated)
      paste0(unique(na.omit(locs)), collapse = ",")
    }
    
  ) %>%
  ungroup()

#check to see if we have any na location codes
na_before <- dt_out %>%
  mutate(row_id = row_number(),
         inj_loc_clean = na_if(trimws(injury_location), "")) %>%
  filter(is.na(inj_loc_clean))
# How many?
nrow(na_before)
# Print IDs + the injury_location as stored
na_before %>%
  select(row_id, inc_key, injury_location) %>%
  print(n = 100)   # increase/decrease n as needed

# If you want to inspect the code columns for these rows too:
na_before %>%
  select(row_id, inc_key, all_of(all_code_cols), injury_location) %>%
  print(n = 50)    # adjust n
#we are good 


# explode -> wide 0/1 
# 1) Derive all distinct labels present (lowercased, trimmed)
setDT(dt_out)

# Tokenize once per row (lowercase, trimmed, drop empties)
tokens <- strsplit(tolower(fifelse(is.na(dt_out$injury_location), "", dt_out$injury_location)),
                   "\\s*,\\s*")
tokens <- lapply(tokens, function(v) { v <- trimws(v); v[v != ""] })

name_map <- c(
  # pelvis & hip
  acetabulum    = "LE_acetabulum",
  pubis         = "LE_pubis",
  ilium         = "LE_ilium",
  ischium       = "LE_ischium",
  pelvis_w_dis  = "LE_pelvis_w_dis",
  pelvis_wo_dis = "LE_pelvis_wo_dis",
  other_pelvis  = "LE_other_pelvis",
  femoral_neck  = "LE_femoral_neck",
  femoral_shaft = "LE_femoral_shaft",
  other_femur   = "LE_other_femur",
  
  # knee/leg/ankle/foot
  patella       = "LE_patella",
  tibia_fibula  = "LE_tibia_fibula",
  ankle         = "LE_ankle",
  other_LE      = "LE_other",
  
  # upper extremity (if you keep UE tokens in the same field)
  clavicle      = "UE_clavicle",
  scapula       = "UE_scapula",
  humerus       = "UE_humerus",
  radius_ulna   = "UE_radius_ulna",
  carpal        = "UE_carpal",
  other_UE      = "UE_other"
)

# OPTIONAL: avoid clobbering existing columns with same names
new_cols <- setdiff(unname(name_map), names(dt_out))
for (col in new_cols) set(dt_out, j = col, value = 0L)

# Fill each indicator column in one vectorized pass
for (reg in names(name_map)) {
  col <- name_map[[reg]]
  set(dt_out, j = col,
      value = as.integer(vapply(tokens, function(x) reg %chin% x, logical(1))))
}

write.csv(dt_out, "classified_data_cleaned_8_20_2025.csv", row.names = FALSE, na = "")

#========= EXPORT =========
fname_csv <- sprintf("2_decoded_Pre_Post_%s.csv", format(Sys.Date(), "%y%m%d"))
fname_rds <- sprintf("2_decoded_Pre_Post_%s.rds", format(Sys.Date(), "%y%m%d"))

fwrite(dt_out, file.path(outdir, fname_csv))
saveRDS(dt_out, file.path(outdir, fname_rds))
