#' Check if any value has the string UNDEFINED
#'
#' In the process of harmonization, values which were not defined,
#' receive the prefix "UNDEFINED: ".
#' This function checks if there are any values with "UNDEFINED" and
#' raises a warning and prints the undefined values.
#'
#' @param df data frame to be checked
#' @param newvar string new variable name with values which have the prefix "UNDEFINED" are expected
#' @param oldvar string old variable name before merging
#'
#' @return raises warning if appropriate.
#' @export
detectUndefinedValue <- function(df, newvar, oldvar) {
  # Store in data frame all values with UNDEFINED prefix in new variable.
  undef <- df %>%
    filter(stringr::str_detect(eval(parse(text = newvar)), "UNDEFINED"))

  # check if undef has entries and raise warning if so
  if (nrow(undef) != 0) {
    warning(paste("Undefined ", newvar, " detected: "), immediate. = TRUE)
    # print undefined values retrieved with old variable name which doesn't have
    # UNDEFINED as prefix
    print(unique(undef[[oldvar]]))
  }
}

#' Harmonize IA locations by vessel to a unified standard
#'
#' The unified standard was selected by a least common denominator over all data sources.
#' For details see: (DataDescription_ISGC_Phenotypes_BayesianModelling)[https://zhaw.sharepoint.com/:x:/s/Aneurysm/EeALEWUBLvRKnnNxZo2PO4MBF37FnRa_7BrJu_aRiLtVFw?e=AIlewJ]
#'
#'
#' @param IAloc data frame column of IA locations by vessels from study raw data
#'
#' @return data frame column of IA locations by vessels ISGC
#'
#' @importFrom dplyr case_when
#'
#' @examples
#' \dontrun{
#' dfnantes <- read_xlsx(system.file("extdata/nantes/ICAN clinical database for ISGC.xlsx", package = "ExplorDataISGC"))
#' # Create new features
#' dfnantes_raw <- dfnantes %>%
#'   # Create ID2 as unique patient identifier (see email from Romain Bourcier on 20.5.22)
#'     mutate(ID2 = 1:n()) %>%
#'   # Create age at diagnosis in years
#'     mutate(ageatdiagy =trunc(lubridate::time_length(
#'       lubridate::interval(
#'         start = lubridate::as_date(age),
#'         end = lubridate::as_date(`age at time of intracranial aneurysm (IA) diagnosis or at time of subarachnoid hemorrhage (SAH)`)),
#'         unit = "year"))) %>%
#'   # translate locations from numbers to names
#'     mutate(across(contains("Location"),
#'                   .fns = nanteslocnumber2locname,
#'                   .names = "vessel_{.col}")) %>%
#'   # harmonize locations to isgc groups
#'     mutate(across(contains("vessel_"),
#'                   .fns = harmonize_loc,
#'                   .names = "isgc_{.col}")) %>%
#'   # group locations by risk
#'     mutate(across(contains("isgc_vessel_"),
#'                   .fns = harmonize_locbyrisk,
#'                   .names = "locrisk_{.col}"))
#' }
harmonize_loc <- function(IAloc){
  case_when(
    IAloc == "ACom" ~ "Acom",
    IAloc == "Acom" ~ "Acom",
    IAloc == "ACOM" ~ "Acom",
    IAloc == "L AComm" ~ "Acom",
    IAloc == "R AComm" ~ "Acom",
    IAloc == "Ant Communicating artery" ~ "Acom",
    IAloc == "anterior communicating artery" ~ "Acom",
    IAloc == "Comm anterior CoA forward" ~ "Acom",
    IAloc == "Comm anterior CoA upward" ~ "Acom",
    IAloc == "Comm anterior CoA backward" ~ "Acom",
    IAloc == "Comm anterior CoA forward-upward" ~ "Acom",
    IAloc == "Comm anterior CoA down" ~ "Acom",
    IAloc == "Ant communicating artery" ~ "Acom",
    IAloc == "anterior communicating midline" ~ "Acom",
    IAloc == "anterior cerebral at anterior communicating junction" ~ "Acom",
    IAloc == "anterior cerebral proximal to anterior communicating" ~ "Acom",

    IAloc == "MCA" ~ "MCA",
    IAloc == "MCA (" ~ "MCA",
    IAloc == "MCA li" ~ "MCA",
    IAloc == "MCA re" ~ "MCA",
    IAloc == "MCA bds" ~ "MCA",
    IAloc == "middle cerebral artery" ~ "MCA",
    IAloc == "L MCA" ~ "MCA",
    IAloc == "R MCA" ~ "MCA",
    IAloc == "Sylvian bifurcation" ~ "MCA",
    IAloc == "M1 segment middle cerebral artery" ~ "MCA",
    IAloc == "M1 perforator artery," ~ "MCA",
    IAloc == "Middle cerebral bifurcation" ~ "MCA",
    IAloc == "Middle cerebral MCA main trunk" ~ "MCA",
    IAloc == "middle cerebral proximal to first main branching" ~ "MCA",
    IAloc == "middle cerebral at main branchings (trifurcation)" ~ "MCA",
    IAloc == "M1 segment middle cerebral artery" ~ "MCA",
    IAloc == "M1" ~ "MCA",
    IAloc == "M 1" ~ "MCA",
    IAloc == "M2" ~ "MCA",

    IAloc == "Pcom" ~ "Pcom",
    IAloc == "PCom" ~ "Pcom",
    IAloc == "PComA" ~ "Pcom",
    IAloc == "PcomA" ~ "Pcom",
    IAloc == "L PComm" ~ "Pcom",
    IAloc == "R PComm" ~ "Pcom",
    IAloc == "Posterior Comm" ~ "Pcom",
    IAloc == "posterior communicating artery" ~ "Pcom",
    IAloc == "Posterior Comm Segment - Inferior wall" ~ "Pcom",
    IAloc == "posterior communicating (distinct from internal carotid junction)" ~ "Pcom",
    IAloc == "internal carotid posterior communicating junction" ~ "Pcom",
    IAloc == "Lateral wall ICA CoP" ~ "Pcom",


    IAloc == "ICA" ~ "ICA",
    IAloc == "internal carotid artery" ~ "ICA",
    IAloc == "L ICA" ~ "ICA",
    IAloc == "R ICA" ~ "ICA",
    IAloc == "Carotid bifurcation" ~ "ICA",
    IAloc == "Ant Choroidal segment carotid" ~ "ICA",
    IAloc == "R Anterior Choroidal" ~ "ICA",
    IAloc == "L Anterior Choroidal" ~ "ICA",
    IAloc == "Carotid bifurcation ICA bif" ~ "ICA",
    IAloc == "Lateral wall ICA ChA" ~ "ICA",
    IAloc == "Carotid Bifurcation" ~ "ICA",
    IAloc == "Ant Choroidal Segment - Inferior wall" ~ "ICA",
    IAloc == "internal carotid supra clinoid at bifurcation" ~ "ICA",
    IAloc == "internal carotid other" ~ "ICA",
    IAloc == "Medial wall ICA distal" ~ "ICA",
    IAloc == "Ant Choroidal segment carotid" ~ "ICA",
    IAloc == "ICA Bif" ~ "ICA",


    IAloc == "OphtICA" ~ "OphtICA",
    IAloc == "OpthICA" ~ "OphtICA",
    IAloc == "OptICA" ~ "OphtICA",
    IAloc == "OphthICA" ~ "OphtICA",
    IAloc == "Ophthalmic artery" ~ "OphtICA",
    IAloc == "opthalmic artery" ~ "OphtICA",
    IAloc == "Ophthalmic segment carotid" ~ "OphtICA",
    IAloc == "Medial wall carotid" ~ "OphtICA",
    IAloc == "Medial wall ICA ophtalmic" ~ "OphtICA",
    IAloc == "Inferior wall ICA" ~ "OphtICA",
    IAloc == "Ophtalmic Segment - Medial wall" ~ "OphtICA",
    IAloc == "Ophtalmic Segment - Lateral wall" ~ "OphtICA",
    IAloc == "internal carotid ophtalmic region" ~ "OphtICA",

    IAloc == "Basilar" ~ "Basilar",
    IAloc == "Basliar" ~ "Basilar",
    IAloc == "Basillaris" ~ "Basilar",
    IAloc == "basilar artery" ~ "Basilar",
    IAloc == "Basilar Tip" ~ "Basilar",
    IAloc == "Basilar tip" ~ "Basilar",
    IAloc == "basilar artery bifurcation" ~ "Basilar",
    IAloc == "Basilar bifurcation" ~ "Basilar",
    IAloc == "P 1 posterior cerebral" ~ "Basilar",
    IAloc == "P1 Posterior cerebral artery" ~ "Basilar",
    IAloc == "basilar termination" ~ "Basilar",
    IAloc == "P1" ~ "Basilar",
    IAloc == "BA" ~ "Basilar",

    IAloc == "VB" ~ "V-B",
    IAloc == "V-B" ~ "V-B",
    IAloc == "V-Bis" ~ "V-B",
    IAloc == "AICA" ~ "V-B",
    IAloc == "anterior inferior cerebellar artery" ~ "V-B",
    IAloc == "PICA" ~ "V-B",
    IAloc == "posterior inferior cerebellar artery" ~ "V-B",
    IAloc == "Superior cerebellar artery" ~ "V-B",
    IAloc == "superior cerebellar artery" ~ "V-B",
    IAloc == "Superior Cerebellar artery  (SCA)" ~ "V-B",
    IAloc == "Superior Cerebellar artery (SCA)" ~ "V-B",
    IAloc == "V4 segment vertebral artery" ~ "V-B",
    IAloc == "Basilar trunk" ~ "V-B",
    IAloc == "L AICA" ~ "V-B",
    IAloc == "R AICA" ~ "V-B",
    IAloc == "L PICA" ~ "V-B",
    IAloc == "R PICA" ~ "V-B",
    IAloc == "Vertebral" ~ "V-B",
    IAloc == "VA" ~ "V-B",
    IAloc == "vertebral artery" ~ "V-B",
    IAloc == "Vertebral trunk" ~ "V-B",
    IAloc == "Basilar others" ~ "V-B",
    IAloc == "Basilar SCA" ~ "V-B",
    IAloc == "Vertebral = PICA origin" ~ "V-B",
    IAloc == "PICA distal" ~ "V-B",
    IAloc == "SCA Segment of Basilar Trunk" ~ "V-B",
    IAloc == "L SCA" ~ "V-B",
    IAloc == "R SCA" ~ "V-B",
    IAloc == "SUCA" ~ "V-B",
    IAloc == "SCA" ~ "V-B",
    IAloc == "PICA Segment of Vertebral Artery" ~ "V-B",
    IAloc == "AICA Segment of Basilar Trunk" ~ "V-B",
    IAloc == "others other (vertebral basilar)" ~ "V-B",
    IAloc == "basilar trunk" ~ "V-B",
    IAloc == "others anterior inferior cerebellar" ~ "V-B",
    IAloc == "others superior cerebellar" ~ "V-B",
    IAloc == "others posterior inferior cerebellar" ~ "V-B",
    IAloc == "posterior cerebral trunk" ~ "V-B",
    IAloc == "posterior cerebral at basilar" ~ "V-B",


    IAloc == "A2" ~ "A2",
    IAloc == "A2 segment ant" ~ "A2",
    IAloc == "A2 segment ant cerebral artery" ~ "A2",
    IAloc == "Pericallosal cerebral artery" ~ "A2",
    IAloc == "Pericallosal proximal" ~ "A2",
    IAloc == "Pericallosal typical" ~ "A2",
    IAloc == "Pericallosal and A2" ~ "A2",
    IAloc == "pericallosal artery" ~ "A2",

    IAloc == "A1 segment ant" ~ "A1 segment ant", # A1 are very rare...
    IAloc == "A1 segment ant cerebral artery" ~ "A1 segment ant",
    IAloc == "A1" ~ "A1 segment ant",

    IAloc == "PC" ~ "PC",
    IAloc == "posterior cerebral artery" ~ "PC",
    IAloc == "L PCA" ~ "PC",
    IAloc == "R PCA" ~ "PC",
    IAloc == "PCA" ~ "PC",
    IAloc == "P1-P2 junction posterior cerebral artery" ~ "PC",
    IAloc == "P2 posterior cerebral artery" ~ "PC",
    IAloc == "P 2 posterior cerebral" ~ "PC",
    IAloc == "P 1/2 posterior cerebral" ~ "PC",
    IAloc == "P2" ~ "PC",

    IAloc == "Other" ~ "Other",
    IAloc == "Distal posterior cerebral artery" ~ "Other",
    IAloc == "L ACA" ~ "Other",
    IAloc == "R ACA" ~ "Other",
    IAloc == "anterior cerebral artery" ~ "Other", # CHECK Utrecht
    IAloc == "Distal to sylvian bifurcation" ~ "Other",
    IAloc == "Distal ant cerebral artery" ~ "Other",
    IAloc == "Middle cerebral peripheral" ~ "Other",
    IAloc == "Pericallosal distal" ~ "Other",
    IAloc == "P 3 posterior cerebral distal" ~ "Other",
    IAloc == "Ant Choroidal Segment - Superior wall" ~ "Other",
    IAloc == "Posterior Comm Segment - Superior wall" ~ "Other",
    IAloc == "middle cerebral distal to main branchings" ~ "Other",
    IAloc == "anterior cerebral other" ~ "Other",
    IAloc == "posterior cerebral other" ~ "Other",
    IAloc == "anterior cerebral distal to anterior communicating" ~ "Other", # might be dirty. Incl. fusiform aneurysms
    IAloc == "Anterior and superior wall carotid" ~ "Other", # consider to remove cause very different behaviour from others (small and deadly).
    IAloc == "Superior wall ICA" ~ "Other", # consider to remove cause very different behaviour from others (small and deadly).
    IAloc == "middle cerebral other" ~ "Other",

    # dirty group... not everyone includes these IAs as intracranial IA. They could also be opthalmic
    IAloc == "Cavernous ICA" ~ "CavICA",
    IAloc == "CavICA" ~ "CavICA",
    IAloc == "CavICa" ~ "CavICA",
    IAloc == "Intracavernous internal carotid" ~ "CavICA",
    IAloc == "Intracavernous Internal carotid" ~ "CavICA",
    IAloc == "internal carotid sublinoid" ~ "CavICA",

    IAloc == "Infundibulum" ~ "Infundibulum",

    # Outside of head
    IAloc == "Cervical internal carotid" ~ "Removed",
    IAloc == "Petrous pyramidal" ~ "Removed",

    is.na(IAloc) ~ NA_character_,

    TRUE ~ paste("UNDEFINED:", IAloc)
  )
}

#' Group IA (ISGC) locations by their risk of rupture
#'
#'
#' @param IAlocation data frame column of IA locations as ISGC-vessels
#'
#' @return data frame column of IA locations by risk of rupture (3 = high risk)
#'
#' @importFrom dplyr if_else
#'
#' @examples
#' \dontrun{
#' dfnantes <- read_xlsx(system.file("extdata/nantes/ICAN clinical database for ISGC.xlsx", package = "ExplorDataISGC"))
#' # Create new features
#' dfnantes_raw <- dfnantes %>%
#'   # Create ID2 as unique patient identifier (see email from Romain Bourcier on 20.5.22)
#'     mutate(ID2 = 1:n()) %>%
#'   # Create age at diagnosis in years
#'     mutate(ageatdiagy =trunc(lubridate::time_length(
#'       lubridate::interval(
#'         start = lubridate::as_date(age),
#'         end = lubridate::as_date(`age at time of intracranial aneurysm (IA) diagnosis or at time of subarachnoid hemorrhage (SAH)`)),
#'         unit = "year"))) %>%
#'   # translate locations from numbers to names
#'     mutate(across(contains("Location"),
#'                   .fns = nanteslocnumber2locname,
#'                   .names = "vessel_{.col}")) %>%
#'   # harmonize locations to isgc groups
#'     mutate(across(contains("vessel_"),
#'                   .fns = harmonize_loc,
#'                   .names = "isgc_{.col}")) %>%
#'   # group locations by risk
#'     mutate(across(contains("isgc_vessel_"),
#'                   .fns = harmonize_locbyrisk,
#'                   .names = "locrisk_{.col}"))
#' }
harmonize_locbyrisk <- function(IAlocation){
  IAlocation <- as.character(IAlocation)

  # Construct location by risk
  if_else(IAlocation == "Acom" |
            IAlocation == "Pcom" |
            IAlocation == "V-B" |
            IAlocation == "A2" |
            IAlocation == "PC", 3,
          if_else(IAlocation == "MCA" |
                    IAlocation == "ICA" |
                    IAlocation == "Basilar" |
                    IAlocation == "Other" |
                    IAlocation == "A1 segment ant", 2,
                  if_else(IAlocation == "CavICA" |
                            IAlocation == "OphtICA", 1,
                          NA_real_)))
}

#' Dictionary translating IA location numbers to vessel names
#'
#' See email from Romain Bourcier on 20.5.22
#'
#' @param loc data frame column of IA locations in numbers
#'
#' @return data frame column of IA locations as vessel names
#'
#' @importFrom dplyr case_when
#'
#' @examples
#' \dontrun{
#' dfnantes <- read_xlsx(system.file("extdata/nantes/ICAN clinical database for ISGC.xlsx", package = "ExplorDataISGC"))
#' # Create new features
#' dfnantes_raw <- dfnantes %>%
#'   # Create ID2 as unique patient identifier (see email from Romain Bourcier on 20.5.22)
#'     mutate(ID2 = 1:n()) %>%
#'   # Create age at diagnosis in years
#'     mutate(ageatdiagy =trunc(lubridate::time_length(
#'       lubridate::interval(
#'         start = lubridate::as_date(age),
#'         end = lubridate::as_date(`age at time of intracranial aneurysm (IA) diagnosis or at time of subarachnoid hemorrhage (SAH)`)),
#'         unit = "year"))) %>%
#'   # translate locations from numbers to names
#'     mutate(across(contains("Location"),
#'                   .fns = nanteslocnumber2locname,
#'                   .names = "vessel_{.col}")) %>%
#'   # harmonize locations to isgc groups
#'     mutate(across(contains("vessel_"),
#'                   .fns = harmonize_loc,
#'                   .names = "isgc_{.col}")) %>%
#'   # group locations by risk
#'     mutate(across(contains("isgc_vessel_"),
#'                   .fns = harmonize_locbyrisk,
#'                   .names = "locrisk_{.col}"))
#' }
nanteslocnumber2locname <- function(loc){
  case_when(
    (loc == 2) | (loc == 102) ~ "external carotid",

    (loc == 4) | (loc == 104) ~ "internal carotid sublinoid",
    (loc == 5) | (loc == 105) ~ "internal carotid ophtalmic region",
    (loc == 6) | (loc == 106) ~ "internal carotid posterior communicating junction",
    (loc == 7) | (loc == 107) ~ "internal carotid supra clinoid at bifurcation",
    (loc == 8) | (loc == 108) ~ "internal carotid other",

    (loc == 9) | (loc == 109) ~ "anterior cerebral proximal to anterior communicating",
    (loc == 11) | (loc == 111) ~ "anterior cerebral at anterior communicating junction",
    (loc == 12) | (loc == 112) ~ "anterior cerebral distal to anterior communicating",
    (loc == 13) | (loc == 113) ~ "anterior cerebral other",

    (loc == 10) ~ "anterior communicating midline",

    (loc == 17) | (loc == 117) ~ "middle cerebral proximal to first main branching",
    (loc == 18) | (loc == 118) ~ "middle cerebral at main branchings (trifurcation)",
    (loc == 19) | (loc == 119) ~ "middle cerebral distal to main branchings",
    (loc == 15) | (loc == 115) ~ "middle cerebral other",

    (loc == 21) | (loc == 121) ~ "posterior communicating (distinct from internal carotid junction)",

    (loc == 23) | (loc == 123) ~ "posterior cerebral trunk",
    (loc == 24) | (loc == 124) ~ "posterior cerebral at basilar",
    (loc == 25) | (loc == 125) ~ "posterior cerebral other",

    (loc == 20) ~ "basilar trunk",
    (loc == 30) ~ "basilar termination",

    (loc == 32) | (loc == 132) ~ "others posterior inferior cerebellar",
    (loc == 33) | (loc == 133) ~ "others anterior inferior cerebellar",
    (loc == 34) | (loc == 134) ~ "others internal meditory",
    (loc == 35) | (loc == 135) ~ "others superior cerebellar",
    (loc == 36) | (loc == 136) ~ "others other (vertebral basilar)"
  )
}

harmonize_bor <- function(basis_of_recruitment){
  case_when(
    basis_of_recruitment %in% c("Retrospective Symptomatic Intracranial Aneurysm") ~ "retro_sympt_IA",
    basis_of_recruitment %in% c("Symptomatic Intracranial Aneurysm", "Symptomatic unruptured IA", "Compression or stroke") ~ "sympt_IA",
    basis_of_recruitment %in% c("Retrospective Incidental Intracranial Aneurysm") ~ "retro_IIA",
    basis_of_recruitment %in% c("Incidental Intracranial Aneurysm", "Incidental intracranial aneurysm", "Incidental unruptured IA", "UIA", "Sporadic UIA", "Fortuite") ~ "IIA",
    basis_of_recruitment %in% c("Retrospective Sub Arachnoid Hemorrhage") ~ "retro_SAH",
    basis_of_recruitment %in% c("Sub Arachnoid Hemorrhage", "Subarachnoid Haemorrhage", "aSAH", "IA rupture") ~ "SAH",
    basis_of_recruitment %in% c("Genetically linked family member", "Screening for Familial IAs", "Familial UIA", "Familial screening") ~ "gen_link_fam",
    basis_of_recruitment %in% c("Case", "case") ~ "case",
    basis_of_recruitment %in% c("Control", "control") ~ "control",
    is.na(basis_of_recruitment) ~ NA_character_,
    TRUE ~ paste("UNDEFINED:", basis_of_recruitment)
  )
}

harmonize_gender <- function(gender){
  case_when(
    gender %in% c("male", "Male", "m", "M") ~ "male",
    gender %in% c("female", "Female", "f", "F", "women", "Women", "w", "W") ~ "female",
    is.na(gender) ~ NA_character_,
    TRUE ~ paste("UNDEFINED:", gender)
  )
}

harmonize_age <- function(age){
  case_when(
    !is.na(age) ~ as.character(age),
    is.na(age) ~ NA_character_,
    TRUE ~ paste("UNDEFINED:", age)
  )
}

harmonize_posfamhist <- function(posfamhist){
  case_when(
    posfamhist %in% c(0, "0", "no", "FALSCH", "No") ~ "no",
    posfamhist %in% c("probably") ~ "probably",
    posfamhist %in% c(1, "1", "yes", "WAHR", "Yes") ~ "yes",
    (posfamhist == "NA") | is.na(posfamhist) ~ NA_character_,
    TRUE ~ paste("UNDEFINED:", posfamhist)
  )
}

#' Harmonize Smoking (three levels) values
#'
#' See also \code{\link[=harmonize_smokingCN]{harmonize_smokingCN()}}.
#'
#' @param smoking as character string with three possible levels: current, former, none smoker
#'
#' @return character string
#' @export
harmonize_smokingCFN <- function(smoking){
  case_when(
    smoking %in% c("Current", "current", "CurSmoker", "Yes", "active smoking", "Yes: still smoking") ~ "current",
    smoking %in% c("Former", "former", "exSmoker", "Quit smoking < 3y", "Quit smoking > 3y",  "Yes: quit smoking") ~ "former",
    smoking %in% c("No", "no", "NonSmoker", "never smoking") ~ "no",
    (smoking == "Unknown") |is.na(smoking) ~ NA_character_,
    TRUE ~ paste("UNDEFINED:", smoking)
  )
}

#' Harmonize Smoking (two levels) values
#'
#' See also \code{\link[=harmonize_smokingCFN]{harmonize_smokingCFN()}}.
#'
#' @param smoking as character string with two possible levels: current, none smoker
#'
#' @return character string
#' @export
harmonize_smokingCN <- function(smoking){
  case_when(
    smoking %in% c("1", "Current", "current", "CurSmoker", "Yes", "active smoking", "Yes: still smoking") ~ "current",
    smoking %in% c("0", "No", "no", "NonSmoker", "never smoking", "not current", "Not Current", "Not current") ~ "not_current",
    (smoking == "Unknown") |is.na(smoking) ~ NA_character_,
    TRUE ~ paste("UNDEFINED:", smoking)
  )
}

harmonize_IAnum <- function(IAnum){
  case_when(
    !is.na(IAnum) ~ as.character(IAnum),
    is.na(IAnum) ~ NA_character_,
    TRUE ~ paste("UNDEFINED:", IAnum)
  )
}

harmonize_IAmult <- function(IAmult){
  case_when(
    IAmult %in% c("No", "FALSE", "0") ~ "no",
    IAmult %in% c("Yes", "TRUE", "1") ~ "yes",
    is.na(IAmult) ~ NA_character_,
    TRUE ~ paste("UNDEFINED:", IAmult)
  )
}

harmonize_hbp <- function(hbp){
  case_when(
    hbp %in% c("No", "no", "no HBP", "Never", "0") ~ "no",
    hbp %in% c("Yes", "yes", "non-treated HBP", "pregnancy HBP", "treated HBP", "Yes - Treated and Controlled", "Yes - Treated and Poorly Controlled", "Yes - Not Treated", "AnyType", "1") ~ "yes",
    (hbp == "NA") | is.na(hbp) ~ NA_character_,
    TRUE ~ paste("UNDEFINED:", hbp)
  )
}

harmonize_hbptreat <- function(hbptreat){
  case_when(
    hbptreat %in% c("No", "no", "FALSE", "non-treated HBP", "Yes - Not Treated") ~ "no",
    hbptreat %in% c("Yes", "yes", "TRUE", "treated HBP", "pregnancy HBP", "Yes - Treated and Controlled", "Yes - Treated and Poorly Controlled") ~ "yes",
    (hbptreat == "no HBP") | is.na(hbptreat) ~ NA_character_,
    TRUE ~ paste("UNDEFINED:", hbptreat)
  )
}

harmonize_IArupt <- function(IArupt){
  case_when(
    IArupt %in% c("ruptured", "R", 1, "O", "Yes", "Ruptured", "Symptomatic", "Fully treated") ~ "yes",
    IArupt %in% c("unruptured", "U", 0, "N", "No", "Unruptured") ~ "no",
    is.na(IArupt) ~ NA_character_,
    TRUE ~ paste("UNDEFINED:", IArupt)
  )
}

harmonize_IAsize <- function(IAsize){
  case_when(
    !is.na(IAsize) ~ as.character(IAsize),
    is.na(IAsize) ~ NA_character_,
    TRUE ~ paste("UNDEFINED:", IAsize)
  )
}

harmonize_mrs <- function(mrs){
  case_when(
    !is.na(mrs) ~ as.character(mrs),
    is.na(mrs) ~ NA_character_,
    TRUE ~ paste("UNDEFINED:", mrs)
  )
}

harmonize_gos <- function(gos){
  case_when(
    !is.na(gos) ~ as.character(gos),
    is.na(gos) ~ NA_character_,
    TRUE ~ paste("UNDEFINED:", gos)
  )
}

harmonize_maxFUtime <- function(maxFUtime){
  case_when(
    !is.na(maxFUtime) ~ as.character(maxFUtime),
    is.na(maxFUtime) ~ NA_character_,
    TRUE ~ paste("UNDEFINED:", maxFUtime)
  )
}

harmonize_termreas <- function(termreas){
  case_when(
    termreas %in% c("IAex") ~ "IAex",
    termreas %in% c("aSAH") ~ "aSAH",
    termreas %in% c("lossFU", "ended", "Ended") ~ "lossFU",
    termreas %in% c("deathOC", "DeathOc", "DeathOC") ~ "deathOC",
    (termreas == "NA") | is.na(termreas) ~ NA_character_,
    TRUE ~ paste("UNDEFINED:", termreas)
  )
}

wrap_bor <- function(basis_of_recruitment){
  case_when(
    basis_of_recruitment %in% c("control", "gen_link_fam") ~ "control",
    TRUE ~ "case"
  )
}

wrap_smokingCFN <- function(smokingCFN){
  case_when(
    smokingCFN == "current" ~ "current",
    smokingCFN %in% c("former", "no") ~ "not_current",
    (smokingCFN == "NA") | is.na(smokingCFN) ~ NA_character_,
    TRUE ~ paste("UNDEFINED:", smokingCFN)
  )
}

wrap_IAnum <- function(IAnum){
  # make numeric
  IAnum <- as.numeric(IAnum)

  # Group in multiple IAs Y/N
  case_when(
    IAnum > 1 ~ "yes",
    IAnum == 1 ~ "no",
    is.na(IAnum) ~ NA_character_,
    TRUE ~ paste("UNDEFINED:", IAnum)
  )
}

wrap_IAsize <- function(IAsize){
  # make numeric
  IAsize <- as.numeric(IAsize)
  IAsize <- round(IAsize, digits = 0) # required to avoid misclassifications due to different levels of significance

  # Group IA size
  case_when(
    IAsize <= 7 ~ "A",
    IAsize > 7 &
      IAsize <= 12 ~ "B",
    IAsize > 12 &
      IAsize <= 25 ~ "C",
    IAsize >= 12 ~ "D",
    is.na(IAsize) ~ "E",
    TRUE ~ paste("UNDEFINED:", IAsize)
  )
}

wrap_age <- function(age){
  age <- as.numeric(age)
  age <- round(age, digits = 0) # required to avoid misclassifications due to different levels of significance

  # Discretise age in bins of A=20-39, B=40-44, C=45-49, D=50-54, E=55-59, F=60-64, G=65-93 years
  age_grouped <- cut(
    age,
    breaks = c(0,
               seq(40, 65, 5),
               max(age, na.rm = TRUE)),
    include.lowest = TRUE,
    right = FALSE
  )

  return(factor(age_grouped, labels = LETTERS[1:length(levels(age_grouped))]))

}

#' Harmonize the data from any study centre to an ISGC data set
#'
#' Harmonize individual data structures to the least common denominator.
#'
#' @param df data.frame of raw data set.
#' @param IDcol1 character string with individual patient identifier or label for link with genomes of the respective study source
#' @param IDcol2 optional other label to identify cases in original study
#' @param gendcol character string of column name for biological sex
#' @param borcol character string of column name for reason of patient's recruitment
#' @param agecol character string of column name for age of patient at time of intracranial aneurysm diagnosis: if Ruptured_IA = Yes -> "Age" refers to the patient age at time of rupture. If Ruptured_IA = No -> "Age" refers to the patient age at the time of her first diagnosed IA (oldest IA).
#' @param posfamhistcol character string of column name for positive familial history
#' @param smokingcol character string of column name for smoking behavior in three levels (current, former, none smoker)
#' @param smokingCNcol character string of column name for smoking behavior (current, not current)
#' @param IAnumcol character string of column name for total number of diagnosed IAs per patient
#' @param multIAcol character string of column name for wrapper of number of IAs.
#' @param hbpcol character string of column name for hypertension y/n
#' @param hbptreatcol character string of column name for type of hypertension treatment
#' @param IAloccol character string of column name for IA location by vessel
#' @param IAruptcol character string of column name for IA rupture status
#' @param IAsizecol character string of column name for IA size
#' @param mRScol character string of column name for for modified ranking score
#' @param goscol character string of column name for Glasgow outcome scale
#' @param maxFUtimecol character string of column name for max. follow-up time
#' @param termreascol character string of column name for follow-up termination reason
#' @param source Study source abbreviation / Description
#' @param returnall if TRUE the original variables remain in the returned data frame.
#'
#' @return data.frame with ISGC harmonized features.
#'
#' @importFrom dplyr mutate across select rename
#' @importFrom tidyselect vars_select_helpers
#' @export
#'
#' @examples
#' \dontrun{
#' dfkuopio_geno_2_smokHBP <- read_xlsx(system.file("extdata/kuopio/Kuopio_ISGC_221021_genotyped_110222_withSmokingAndHyperntension.xlsx", package = "ExplorDataISGC"))
#'
#' kuopio_isgc_geno <- kuopio2isgc(dfkuopio_geno_2_smokHBP,
#' IDcol1="FamilyID_plink",
#' IDcol2="ID",
#' gendcol="Gender",
#' borcol="BasisOfRecruitment",
#' agecol = "AgeatdgSAH",
#' posfamhistcol="PositiveFamilyHistory",
#' smokingcol = "...17",
#' IAnumcol="NumberofIAs",
#' multIAcol=NULL,
#' hbpcol="...18",
#' hbptreatcol=NULL,
#' IAloccol = "LocationOnIA",
#' IAruptcol="SAHstatus",
#' IAsizecol="Size",
#' mRScol=NULL,
#' goscol="GlasgowOutcomeScale",
#' maxFUtimecol="MAxFUtime",
#' termreascol="termination_reason",
#' source="kuopio")
#' str(kuopio_isgc_geno)
#' }
harmonize2isgc <- function(df,
                        IDcol1,
                        IDcol2= NULL,
                        gendcol,
                        borcol,
                        agecol,
                        posfamhistcol,
                        smokingcol = NULL,
                        smokingCNcol = NULL,
                        IAnumcol,
                        multIAcol=NULL,
                        hbpcol,
                        hbptreatcol=NULL,
                        IAloccol,
                        IAruptcol,
                        IAsizecol,
                        mRScol=NULL,
                        goscol=NULL,
                        maxFUtimecol=NULL,
                        termreascol=NULL,
                        source,
                        returnall=FALSE){
  ### rename columns for down-stream consistency. If name of column is NA build an empty (NA) column
  # Required variables
  dftemp <- df %>%
    mutate(ID_1 = .data[[IDcol1]],
           source = source,
           gender = .data[[gendcol]],
           basis_of_recruitment = .data[[borcol]],
           age = .data[[agecol]],
           posfamhist = .data[[posfamhistcol]],
           # smoking = .data[[smokingcol]],
           IAnum = .data[[IAnumcol]],
           hbp = .data[[hbpcol]],
           IAloc = .data[[IAloccol]],
           IArupt = .data[[IAruptcol]],
           IAsize = .data[[IAsizecol]]
    )

  # Optional or wrapper variables
  if(!is.null(IDcol2)){
    dftemp <- dftemp %>%
      mutate(ID_2 = .data[[IDcol2]])
  } else {
    dftemp <- dftemp %>%
      mutate(ID_2 = NA_character_)
  }

  if((!is.null(smokingcol)) & (!is.null(smokingCNcol))){
    warning(simpleWarning("Two smoking variables provided. Don't know which one to keep. Please provide only one of them."))
    stop(simpleError("Can not guarantee coherence between Smoking_current_former_no and smoking_curent_former."))
  } else if(!is.null(smokingcol)){
    dftemp <- dftemp %>%
      mutate(smoking = .data[[smokingcol]])
  } else if(!is.null(smokingCNcol)){
    dftemp <- dftemp %>%
      mutate(smoking = .data[[smokingCNcol]])
  } else if((is.null(smokingcol)) & (is.null(smokingCNcol))){
    warning(simpleWarning("No smoking variable provided. Please provide one of them.
                          Meanwhile, I mark all entries as missing (NAs)."))
    dftemp <- dftemp %>%
      mutate(smoking =  NA_character_)
  }

  if(!is.null(multIAcol)){
    dftemp <- dftemp %>%
      mutate(IAmult = .data[[multIAcol]])
  } else {
    dftemp <- dftemp %>%
      mutate(IAmult = NA_character_)
  }

  if(!is.null(hbptreatcol)){
    dftemp <- dftemp %>%
      mutate(hbptreat = .data[[hbptreatcol]])
  } else {
    dftemp <- dftemp %>%
      mutate(hbptreat = NA_character_)
  }

  if(!is.null(mRScol)){
    dftemp <- dftemp %>%
      mutate(mrs = as.integer(.data[[mRScol]]))
  } else {
    dftemp <- dftemp %>%
      mutate(mrs = NA_integer_)
  }

  if(!is.null(goscol)){
    dftemp <- dftemp %>%
      mutate(gos = as.integer(.data[[goscol]]))
  } else {
    dftemp <- dftemp %>%
      mutate(gos = NA_integer_)
  }

  if(!is.null(maxFUtimecol)){
    dftemp <- dftemp %>%
      mutate(maxFUtime = .data[[maxFUtimecol]])
  } else {
    dftemp <- dftemp %>%
      mutate(maxFUtime = NA_integer_)
  }

  if(!is.null(termreascol)){
    dftemp <- dftemp %>%
      mutate(termreas = .data[[termreascol]])
  } else {
    dftemp <- dftemp %>%
      mutate(termreas = NA_character_)
  }

  ### Initial Checks
  # Check if all patients are unique
  if(sum(is.na(dftemp$ID_2)) == 0){
    message("Checking if all patients are unique based on ID_2...\n")
    duplicated_patients <- duplicated(dftemp$ID_2)
    df_dupl <- dftemp[which(duplicated_patients),]
    df_dupl <- df_dupl[order(df_dupl$ID_2),]
  } else if(sum(is.na(dftemp$ID_1)) == 0){
    message("Checking if all patients are unique based on ID_1...\n")
    duplicated_patients <- duplicated(dftemp$ID_1)
    df_dupl <- dftemp[which(duplicated_patients),]
    df_dupl <- df_dupl[order(df_dupl$ID_1),]
  } else {
    warning(simpleWarning("Don't know how to identify patients."))
    stop(simpleError("Can not guarantee that all patients are unique."))
  }

  if(sum(duplicated_patients) !=0){
    e <- simpleError("Duplicate patient IDs!")
    tryCatch(stop(e),
             finally = message(
               paste0(capture.output( # print in table format
                 df_dupl),
                 collapse = "\n")
             )
    )
  }

  # Check Age values
  message("Checking age values...\n")
  if (any(as.numeric(dftemp$age) > 130, na.rm = TRUE)) {
    warning(simpleWarning("Unrealistic high values for patients age at diag detected."))
  } else if (any(as.numeric(dftemp$age) < 0, na.rm = TRUE)) {
    stop(simpleError("Patient age below 0 detected."))
  }

  # Check IAnum values
  message("Checking number of IAs...\n")
  if (any(as.numeric(dftemp$IAnum) > 10, na.rm = TRUE)) {
    warning(simpleWarning("More than 10 IAs detected in at least one patient."))
  } else if (any(as.numeric(dftemp$IAnum) < 1, na.rm = TRUE)) {
    stop(simpleError("Number of IAs <1 detected."))
  }

  # Check Size values
  message("Checking IA size values...\n")
  if (any(as.numeric(dftemp$IAsize) > 70, na.rm = TRUE)) {
    warning(simpleWarning("Unrealistic high values for IA size detected (>70)."))
  }
  if (any(as.numeric(dftemp$IAsize) < 1, na.rm = TRUE)) {
    stop(simpleError("IA size < 1 detected."))
  }

  ### Harmonisation
  dftemp <- dftemp %>%
    mutate(
      across(.cols = gender, .fns = harmonize_gender, .names = "{.col}"),
      across(.cols = basis_of_recruitment, .fns = harmonize_bor, .names = "{.col}"),
      across(.cols = age, .fns = harmonize_age, .names = "{.col}"),
      across(.cols = posfamhist, .fns = harmonize_posfamhist, .names = "{.col}"),
      across(.cols = IAnum, .fns = harmonize_IAnum, .names = "{.col}"),
      across(.cols = IAloc, .fns = harmonize_loc, .names = "{.col}"),
      across(.cols = IAmult, .fns = harmonize_IAmult, .names = "{.col}"),
      across(.cols = hbp, .fns = harmonize_hbp, .names = "{.col}"),
      across(.cols = hbptreat, .fns = harmonize_hbptreat, .names = "{.col}"),
      across(.cols = IArupt, .fns = harmonize_IArupt, .names = "{.col}"),
      across(.cols = IAsize, .fns = harmonize_IAsize, .names = "{.col}"),
      across(.cols = mrs, .fns = harmonize_mrs, .names = "{.col}"),
      across(.cols = gos, .fns = harmonize_gos, .names = "{.col}"),
      across(.cols = maxFUtime, .fns = harmonize_maxFUtime, .names = "{.col}"),
      across(.cols = termreas, .fns = harmonize_termreas, .names = "{.col}")
    )

  # Harmonize smoking variable depending on provided level of detail
  if (!is.null(smokingcol) & is.null(smokingCNcol)){
    # if only CFN is provided and CN is null (the case where both are provided was catched earlier)
    # harmonize CFN
    dftemp <- dftemp %>%
      mutate(across(.cols = smoking, .fns = harmonize_smokingCFN, .names = "smokingCFN")) %>%
      # build wrapper CN
      mutate(across(.cols = smokingCFN, .fns = wrap_smokingCFN, .names = "smokingCN"))
  } else if (is.null(smokingcol) & !is.null(smokingCNcol)){
    # if only CN is provided and CFN is null
    # harmonize CN
    dftemp <- dftemp %>%
      mutate(across(.cols = smoking, .fns = harmonize_smokingCN, .names = "smokingCN")) %>%
      # make CFN all NAs
      mutate(smokingCFN = NA_character_)
  } else if (is.null(smokingcol) & is.null(smokingCNcol)){
    # if none of them is provided, mark all missing
    dftemp <- dftemp %>%
      mutate(smokingCN = smoking,
             smokingCFN = smoking)
  }

  # Detect undefined values
  detectUndefinedValue(dftemp, "gender", gendcol)
  detectUndefinedValue(dftemp, "basis_of_recruitment", borcol)
  detectUndefinedValue(dftemp, "age", agecol)
  detectUndefinedValue(dftemp, "posfamhist", posfamhistcol)
  detectUndefinedValue(dftemp, "smokingCFN", smokingcol)
  detectUndefinedValue(dftemp, "smokingCN", smokingCNcol)
  detectUndefinedValue(dftemp, "IAnum", IAnumcol)
  detectUndefinedValue(dftemp, "hbp", hbpcol)
  detectUndefinedValue(dftemp, "hbptreat", hbptreatcol)
  detectUndefinedValue(dftemp, "IAloc", IAloccol)
  detectUndefinedValue(dftemp, "IArupt", IAruptcol)
  detectUndefinedValue(dftemp, "mrs", mRScol)
  detectUndefinedValue(dftemp, "gos", goscol)
  detectUndefinedValue(dftemp, "maxFUtime", maxFUtimecol)
  detectUndefinedValue(dftemp, "termreas", termreascol)

  ### Build Wrapper Variables
  dftemp <- dftemp %>%
    mutate(
      across(.cols = basis_of_recruitment, .fns = wrap_bor, .names = "basis_of_recruitment_CaseControl"),
      across(.cols = IAsize, .fns = wrap_IAsize, .names = "IAsize_group")
    )

  if(is.null(multIAcol)){
    # built IAmult from IA number only if IAmultcol not provided.
    dftemp <- dftemp %>%
      mutate(
        across(.cols = IAnum, .fns= wrap_IAnum, .names = "IAmult")
        )
    message("IA multiplicity is built from reported number of IAs.\n")
  } else if(!is.null(multIAcol)){
    message("IA multiplicity is taken from original source.\n")
  } else{
    message("IA multiplicity not properly handeled.\n")
  }

  ### Final Checks
  # Detect undefined values
  detectUndefinedValue(dftemp, "IAmult", IAnum)
  detectUndefinedValue(dftemp, "IAsize_group", IAsize)

  ### Final Variable Construction
  dffinal <- dftemp %>%
    select(ID_1, ID_2, source, gender, basis_of_recruitment, basis_of_recruitment_CaseControl, age, posfamhist, smokingCFN, smokingCN, IAnum, IAmult, hbp, hbptreat, IAloc, IArupt, IAsize, IAsize_group, mrs, gos, maxFUtime, termreas) %>%
    mutate(across(.cols = c("age", "IAnum", "IAsize", "mrs", "gos", "maxFUtime"), as.numeric)) %>%
    mutate(across(where(is.character), as.factor)) %>%
    rename(ID_1 = ID_1,
           ID_2 = ID_2,
           source = source,
           gender = gender,
           basis_of_recruitment = basis_of_recruitment,
           basis_of_recruitment_CaseControl = basis_of_recruitment_CaseControl,
           age = age,
           positive_famillial_history = posfamhist,
           smoking_current_former_no = smokingCFN,
           smoking_current_notcurrent = smokingCN,
           number_of_IAs = IAnum,
           multipleIAs = IAmult,
           hypertension = hbp,
           hypertension_treatment = hbptreat,
           IAlocation = IAloc,
           IAruptured = IArupt,
           IAsize_at_diag = IAsize,
           IAsize_at_diag_grouped = IAsize_group,
           mRS_afterSAH = mrs,
           gos_afterSAH = gos,
           maxFUtime = maxFUtime,
           termination_reason = termreas
    )
  # Return selection of variables
  if (!returnall) {
    return(dffinal)
  } else{
    return(dftemp)
  }
}

#' Harmonization of CRIM 4.0 and 5.0 ADB scheme to ISGC
#'
#' Query the ADB for all relevant features to be harmonized into ISGC.
#'
#' @param df data.frame of raw data set.
#' @param IDcol1 character string with indiviudal patient identifier or label for link with genomes of the respective study source
#' @param IDcol2 optional other label to identify cases in original study
#' @param gendcol character string of column name for biological sex
#' @param borcol character string of column name for reason of patient's recruitment
#' @param agecol character string of column name for age of patient at time of intracranial aneurysm diagnosis: if Ruptured_IA = Yes -> "Age" refers to the patient age at time of rupture. If Ruptured_IA = No -> "Age" refers to the patient age at the time of her first diagnosed IA (oldest IA).
#' @param posfamhistcol character string of column name for positive familial history
#' @param smokingcol character string of column name for smoking behaviour
#' @param IAnumcol character string of column name for total number of diagnosed IAs per patient
#' @param multIAcol character string of column name for wrapper of number of IAs.
#' @param hbpcol character string of column name for hypertension y/n
#' @param hbptreatcol character string of column name for type of hypertension treatment
#' @param IAloccol character string of column name for IA location by vessel
#' @param IAruptcol character string of column name for IA rupture status
#' @param IAsizecol character string of column name for IA size
#' @param mRScol character string of column name for for modified ranking score
#' @param goscol character string of column name for Glasgow outcome scale
#' @param maxFUtimecol character string of column name for max. follow-up time
#' @param termreascol character string of column name for follow-up termination reason
#' @param source Study source abbreviation / Description
#' @param returnall if TRUE the original variables remain in the returned data frame.
#'
#' @return data.frame of variables in ISGC structure
#'
#' @importFrom purrr is_empty
#' @importFrom dplyr group_by mutate distinct ungroup case_when rename select
#' @importFrom tidyr fill
#' @importFrom tidyselect everything
#' @importFrom lubridate time_length interval
#' @importFrom magrittr %>%
#'
#' @export
crim2ISGC <- function(df = aneuquest.raw,
                        IDcol1="patientID",
                        IDcol2="aneuUniqueID",
                        gendcol="patSex",
                        borcol="recruBasis",
                        agecol = "aneuReportPatAge",
                        posfamhistcol="prf_PosFamHis",
                        smokingcol = "prf_SmokedTobacco",
                        IAnumcol="IAnum",
                        multIAcol=NULL,
                        hbpcol="prf_HyperTension",
                        hbptreatcol="prf_HyperTension",
                        IAloccol = "aneuLoca",
                        IAruptcol="ruptureStatus",
                        IAsizecol="maxDiam",
                        mRScol=NULL,
                        goscol=NULL,
                        maxFUtimecol=NULL,
                        termreascol=NULL,
                        source="geneva_aneuquest",
                        returnall=FALSE){
  ### rename columns for down-stream consistency. If name of column is NA build an empty (NA) column
  # Required variables
  dftemp <- df %>%
    mutate(ID_1 = .data[[IDcol1]],
           source = source,
           gender = .data[[gendcol]],
           basis_of_recruitment = .data[[borcol]],
           age = .data[[agecol]],
           posfamhist = .data[[posfamhistcol]],
           smoking = .data[[smokingcol]],
           IAnum = .data[[IAnumcol]],
           hbp = .data[[hbpcol]],
           IAloc = .data[[IAloccol]],
           IArupt = .data[[IAruptcol]],
           IAsize = .data[[IAsizecol]]
    )

  # Optional or wrapper variables
  if(!is.null(IDcol2)){
    dftemp <- dftemp %>%
      mutate(ID_2 = .data[[IDcol2]])
  } else {
    dftemp <- dftemp %>%
      mutate(ID_2 = NA_character_)
  }

  if(!is.null(multIAcol)){
    dftemp <- dftemp %>%
      mutate(IAmult = .data[[multIAcol]])
  } else {
    dftemp <- dftemp %>%
      mutate(IAmult = NA_character_)
  }

  if(!is.null(hbptreatcol)){
    dftemp <- dftemp %>%
      mutate(hbptreat = .data[[hbptreatcol]])
  } else {
    dftemp <- dftemp %>%
      mutate(hbptreat = NA_character_)
  }

  if(!is.null(mRScol)){
    dftemp <- dftemp %>%
      mutate(mrs = as.integer(.data[[mRScol]]))
  } else {
    dftemp <- dftemp %>%
      mutate(mrs = NA_integer_)
  }

  if(!is.null(goscol)){
    dftemp <- dftemp %>%
      mutate(gos = as.integer(.data[[goscol]]))
  } else {
    dftemp <- dftemp %>%
      mutate(gos = NA_integer_)
  }

  if(!is.null(maxFUtimecol)){
    dftemp <- dftemp %>%
      mutate(maxFUtime = .data[[maxFUtimecol]])
  } else {
    dftemp <- dftemp %>%
      mutate(maxFUtime = NA_integer_)
  }

  if(!is.null(termreascol)){
    dftemp <- dftemp %>%
      mutate(termreas = .data[[termreascol]])
  } else {
    dftemp <- dftemp %>%
      mutate(termreas = NA_character_)
  }

  ### Initial Checks
  # Check if all patients are unique
  # if(sum(is.na(dftemp$ID_2)) == 0){
  #   message("Checking if all patients are unique based on ID_2...\n")
  #   duplicated_patients <- duplicated(dftemp$ID_2)
  #   df_dupl <- dftemp[which(duplicated_patients),]
  #   df_dupl <- df_dupl[order(df_dupl$ID_2),]
  # } else if(sum(is.na(dftemp$ID_1)) == 0){
  #   message("Checking if all patients are unique based on ID_1...\n")
  #   duplicated_patients <- duplicated(dftemp$ID_1)
  #   df_dupl <- dftemp[which(duplicated_patients),]
  #   df_dupl <- df_dupl[order(df_dupl$ID_1),]
  # } else {
  #   warning(simpleWarning("Don't know how to identify patients."))
  #   stop(simpleError("Can not guarantee that all patients are unique."))
  # }
  #
  # if(sum(duplicated_patients) !=0){
  #   e <- simpleError("Duplicate patient IDs!")
  #   tryCatch(stop(e),
  #            finally = message(
  #              paste0(capture.output( # print in table format
  #                df_dupl),
  #                collapse = "\n")
  #            )
  #   )
  # }

  # Check Age values
  message("Checking age values...\n")
  if (any(as.numeric(dftemp$age) > 130, na.rm = TRUE)) {
    warning(simpleWarning("Unrealistic high values for patients age at diag detected."))
  } else if (any(as.numeric(dftemp$age) < 0, na.rm = TRUE)) {
    stop(simpleError("Patient age below 0 detected."))
  }

  # Check IAnum values
  message("Checking number of IAs...\n")
  if (any(as.numeric(dftemp$IAnum) > 10, na.rm = TRUE)) {
    warning(simpleWarning("More than 10 IAs detected in at least one patient."))
  } else if (any(as.numeric(dftemp$IAnum) < 1, na.rm = TRUE)) {
    stop(simpleError("Number of IAs <1 detected."))
  }

  # Check Size values
  message("Checking IA size values...\n")
  if (any(as.numeric(dftemp$IAsize) > 70, na.rm = TRUE)) {
    warning(simpleWarning("Unrealistic high values for IA size detected (>70)."))
  }
  if (any(as.numeric(dftemp$IAsize) <= 0, na.rm = TRUE)) {
    stop(simpleError("IA size <= 0 detected."))
  }

  ### Harmonisation
  # Harmonize from CRIM4.0 to ISGC
  # ---------
  crim.mod <- dftemp
  # This part is moved to the vignette for visibility
  # %>%
    # # If multiple entries for the same IA (by aneuUniqueID) merge the entries (Fill NAs for same IA)
    # group_by(ID_2, aneuUniqueID) %>%
    # select(-c(ID)) %>%
    # fill(everything(), .direction = "updown") %>%
    # distinct() %>%
    # ungroup() %>%
    #
    # # If same aneuUniqueID but different maxDiam, keep the larger
    # group_by(ID_2, aneuUniqueID) %>%
    # mutate(IAsize = max(IAsize)) %>%
    # distinct() %>%
    # ungroup()

  # Build ISGC variables
  # ---------
  # Patient Age
  if (!is.null(agecol)) {
    # if patientage is provided from the data base, keep this
    crim.mod <- crim.mod %>%
      mutate(age = as.numeric(age))
  } else {
    # if not provided by data base, reanonymize age
    warning("Age variable is reanonymized. Verify reanonymization process!")
    crim.mod <- crim.mod %>%
      mutate(age = trunc(lubridate::time_length(
        lubridate::interval(patDOB, aneuReportDate), "years"
      ))) # Age at last anniversary
  }

  # Check value of age
  if (any(crim.mod$age > 130, na.rm = TRUE)) {
    warning("Unrealistic high values for patients age at diag detected.")
  } else if (any(crim.mod$age < 0, na.rm = TRUE)) {
    stop("Patient age below 0 detected.")
  }

  dftemp <- crim.mod %>%
    mutate(
      across(.cols = gender, .fns = harmonize_gender, .names = "{.col}"),
      across(.cols = basis_of_recruitment, .fns = harmonize_bor, .names = "{.col}"),
      across(.cols = age, .fns = harmonize_age, .names = "{.col}"),
      across(.cols = posfamhist, .fns = harmonize_posfamhist, .names = "{.col}"),
      across(.cols = smoking, .fns = harmonize_smokingCFN, .names = "smokingCFN"),
      across(.cols = IAnum, .fns = harmonize_IAnum, .names = "{.col}"),
      across(.cols = IAloc, .fns = harmonize_loc, .names = "{.col}"),
      across(.cols = IAmult, .fns = harmonize_IAmult, .names = "{.col}"),
      across(.cols = hbp, .fns = harmonize_hbp, .names = "{.col}"),
      across(.cols = hbptreat, .fns = harmonize_hbptreat, .names = "{.col}"),
      across(.cols = IArupt, .fns = harmonize_IArupt, .names = "{.col}"),
      across(.cols = IAsize, .fns = harmonize_IAsize, .names = "{.col}"),
      across(.cols = mrs, .fns = harmonize_mrs, .names = "{.col}"),
      across(.cols = gos, .fns = harmonize_gos, .names = "{.col}"),
      across(.cols = maxFUtime, .fns = harmonize_maxFUtime, .names = "{.col}"),
      across(.cols = termreas, .fns = harmonize_termreas, .names = "{.col}")
    )

  # Detect undefined values
  detectUndefinedValue(dftemp, "gender", gendcol)
  detectUndefinedValue(dftemp, "basis_of_recruitment", borcol)
  detectUndefinedValue(dftemp, "age", agecol)
  detectUndefinedValue(dftemp, "posfamhist", posfamhistcol)
  detectUndefinedValue(dftemp, "smokingCFN", smokingcol)
  detectUndefinedValue(dftemp, "IAnum", IAnumcol)
  detectUndefinedValue(dftemp, "hbp", hbpcol)
  detectUndefinedValue(dftemp, "hbptreat", hbptreatcol)
  detectUndefinedValue(dftemp, "IAloc", IAloccol)
  detectUndefinedValue(dftemp, "IArupt", IAruptcol)
  detectUndefinedValue(dftemp, "mrs", mRScol)
  detectUndefinedValue(dftemp, "gos", goscol)
  detectUndefinedValue(dftemp, "maxFUtime", maxFUtimecol)
  detectUndefinedValue(dftemp, "termreas", termreascol)

  ### Build Wrapper Variables
  dftemp <- dftemp %>%
    mutate(
      across(.cols = basis_of_recruitment, .fns = wrap_bor, .names = "basis_of_recruitment_CaseControl"),
      across(.cols = smokingCFN, .fns = wrap_smokingCFN, .names = "smokingCN"),
      across(.cols = IAnum, .fns = wrap_IAnum, .names = "IAmult"),
      across(.cols = IAsize, .fns = wrap_IAsize, .names = "IAsize_group")
    ) %>%

    # Group age variable
    mutate(
      age.grouped = ifelse(is.na(age) & IArupt == "no", 200, as.numeric(age))) %>%
    mutate(
      age.grouped = cut(
        age.grouped,
        breaks = c(0,
                   seq(40, 65, 5),
                   199, max(age.grouped, na.rm = TRUE)),
        include.lowest = TRUE,
        right = FALSE
      )
    ) %>%
    mutate(age.grouped = factor(age.grouped, labels = LETTERS[1:length(levels(age.grouped))]))

  # Group IA number in multiple IA Y/N
  # Different approach for aneuquest and aneux. Aneuquest has no dedicated variable
  # for the total number of IAs.
  if(is.null(multIAcol) & all(is.na(dftemp$IAnum)) & all(dftemp$source == "geneva_aneuquest")){
    dftemp <- dftemp %>%
      # IA Multiplicity according to aneuUniqueID.: Multiple IAs, if anything bigger than only 1. Equal to aneuUniqueID.
      group_by(ID_1) %>%
      mutate(IAmult = case_when(any(ID_2 >= 2) ~ "yes",
                                all(ID_2 == 1) ~ "no",
                                is.na(IAnum) ~ NA_character_,
                                TRUE ~ paste("UNDEFINED:", ID_2))) %>%
      ungroup()
    } else if(is.null(multIAcol) & !all(is.na(dftemp$IAnum)) & all(dftemp$source == "geneva_aneux")){
      dftemp <- dftemp %>%
        # IA Multiplicity according to noAnevrisme
        group_by(ID_1) %>%
        mutate(IAmult = case_when(IAnum > 1 ~ "yes",
                                  IAnum == 1 ~"no",
                                  is.na(IAnum) ~ NA_character_,
                                  TRUE ~paste("UNDEFINED:", IAnum))) %>%
        ungroup()
    } else {
      warning(simpleWarning("Not sure how to build 'IA multiplicity' based on the provided variable for the number of IAs. Check manually!"))
    }

  dftemp <- dftemp %>%
    group_by(ID_1) %>%
    # Keep only the most important IA (according to surgeons opinion)
    # filter(aneuUniqueID == 1)
    filter(case_when(n() > 1 ~ ID_2 == 1,
                     TRUE ~ ID_2 == ID_2)) %>%
    ungroup()

  ### Final Checks
  # Detect undefined values
  detectUndefinedValue(dftemp, "smokingCN", smokingCFN)
  detectUndefinedValue(dftemp, "IAmult", IAnum)
  detectUndefinedValue(dftemp, "IAsize_group", IAsize)

  ### Final Variable Construction
  dffinal <- dftemp %>%
    select(ID_1, ID_2, source, gender, basis_of_recruitment, basis_of_recruitment_CaseControl, age, posfamhist, smokingCFN, smokingCN, IAnum, IAmult, hbp, hbptreat, IAloc, IArupt, IAsize, IAsize_group, mrs, gos, maxFUtime, termreas) %>%
    mutate(across(.cols = c("age", "IAnum", "IAsize", "mrs", "gos", "maxFUtime"), as.numeric)) %>%
    mutate(across(where(is.character), as.factor)) %>%
    rename(ID_1 = ID_1,
           ID_2 = ID_2,
           source = source,
           gender = gender,
           basis_of_recruitment = basis_of_recruitment,
           basis_of_recruitment_CaseControl = basis_of_recruitment_CaseControl,
           age = age,
           positive_famillial_history = posfamhist,
           smoking_current_former_no = smokingCFN,
           smoking_current_notcurrent = smokingCN,
           number_of_IAs = IAnum,
           multipleIAs = IAmult,
           hypertension = hbp,
           hypertension_treatment = hbptreat,
           IAlocation = IAloc,
           IAruptured = IArupt,
           IAsize_at_diag = IAsize,
           IAsize_at_diag_grouped = IAsize_group,
           mRS_afterSAH = mrs,
           gos_afterSAH = gos,
           maxFUtime = maxFUtime,
           termination_reason = termreas
    )
  # Return selection of variables
  if (!returnall) {
    return(dffinal)
  } else{
    return(dftemp)
  }
}



#' Harmonization of CRIM 5.0 ADB scheme to ISGC
#'
#' Query the ADB for all relevant features to be harmonized into ISGC.
#'
#' @param data data.frame of extracted variables of ADB
#' @param returnall binary. If TRUE a all original variables is returned additionally to the ISGC features¨.
#'
#' @return data.frame of variables in ISGC structure
#' @export
crim5toISGC <- function(data, returnall = FALSE) {
  # Perform various checks
  # ---------
  # Assert if required variables for harmonization to ISGC are provided
  necessary_feats <-
    c(
      "patDOB",
      "aneuReportDate",
      "prf_SmokedTobacco",
      "aneuLoca",
      "maxDiam",
      "prf_HyperTension",
      "ruptureStatus",
      "recruBasis",
      "patientID",
      "patSex",
      "prf_PosFamHis",
      "aneuUniqueID",
      "noAnevrisme"
    )
  missing_feats <- c()
  for (feat in necessary_feats) {
    if (!(feat %in% names(data))) {
      missing_feats <- c(missing_feats, feat)
    }
  }
  if (!is_empty(missing_feats)) {
    stop(
      "Not all required features are provided by ",
      deparse(substitute(data)),
      ". Missing features: ",
      missing_feats
    )
  }

  # Harmonize from CRIM5.0 to ISGC
  # ---------
  crim.mod <- data %>%
    # If multiple entries for the same IA (by aneuUniqueID) merge the entries (Fill NAs for same IA)
    group_by(patientID, aneuUniqueID) %>%
    select(-c(ID)) %>%
    fill(everything(), .direction = "updown") %>%
    distinct() %>%
    ungroup() %>%

    # If same aneuUniqueID but different maxDiam, keep the larger
    group_by(patientID, aneuUniqueID) %>%
    mutate(maxDiam = max(maxDiam)) %>%
    distinct() %>%
    ungroup() %>%

    # unite aneuReportDate (CRIM5.0 <= 2021_04) and initialDiagDate (CRIM5.0 >= 2021_05) into UnitedDiagDate
    mutate(UnitedDiagDate = if ('initialDiagDate' %in% names(.))
      coalesce(aneuReportDate, initialDiagDate)
      else
        aneuReportDate)

  # Build ISGC variables
  # ---------
  # Patient Age
  if ("aneuReportPatAge" %in% names(data)) {
    # if patientage is provided from the data base, keep this
    crim.mod <- crim.mod %>%
      mutate(age.at.time.of.diagnosis = as.numeric(aneuReportPatAge))
  } else {
    # if not provided by data base, reanonymize age
    warning("Age variable is reanonymized. Verify reanonymization process!")
    crim.mod <- crim.mod %>%
      mutate(age.at.time.of.diagnosis = trunc(lubridate::time_length(
        lubridate::interval(patDOB, UnitedDiagDate), "years"
      ))) # Age at last anniversary
  }

  # Check value of age
  if (any(crim.mod$age.at.time.of.diagnosis > 130, na.rm = TRUE)) {
    warning("Unrealistic high values for patients age at diag detected.")
  } else if (any(crim.mod$age.at.time.of.diagnosis < 0, na.rm = TRUE)) {
    stop("Patient age below 0 detected.")
  }

  # Other Variables
  crim.mod <- crim.mod %>%
    mutate(
      Source = "Geneva",

      # Smoking status
      Smoking_Current_Former_No = case_when(
        prf_SmokedTobacco == "Yes: quit smoking" ~ "Former",
        prf_SmokedTobacco == "Yes: still smoking" ~ "Current",
        prf_SmokedTobacco == "No" ~ "No",
        TRUE ~ prf_SmokedTobacco
      ),
      Smoking_Current_Not.Current = case_when(
        prf_SmokedTobacco == "Yes: quit smoking" ~ "Not Current",
        prf_SmokedTobacco == "Yes: still smoking" ~ "Current",
        prf_SmokedTobacco == "No" ~ "Not Current",
        TRUE ~ prf_SmokedTobacco
      ),

      # Restructure Location to available locations in ISGC
      IA_Location = case_when(
        aneuLoca == "Ant Communicating artery" ~ "Acom",
        aneuLoca == "Comm anterior CoA forward" ~ "Acom",
        aneuLoca == "Comm anterior CoA upward" ~ "Acom",
        aneuLoca == "Comm anterior CoA backward" ~ "Acom",
        aneuLoca == "Comm anterior CoA forward-upward" ~ "Acom",
        aneuLoca == "Comm anterior CoA down" ~ "Acom",
        aneuLoca == "Ant communicating artery" ~ "Acom",

        aneuLoca == "Sylvian bifurcation" ~ "MCA",
        aneuLoca == "M1 segment middle cerebral artery" ~ "MCA",
        aneuLoca == "M1 perforator artery," ~ "MCA",
        aneuLoca == "Middle cerebral bifurcation" ~ "MCA",
        aneuLoca == "Middle cerebral MCA main trunk" ~ "MCA",

        aneuLoca == "Posterior Comm" ~ "Pcom",
        aneuLoca == "Posterior Comm Segment - Inferior wall" ~ "Pcom",

        aneuLoca == "Carotid bifurcation" ~ "ICA",
        aneuLoca == "Anterior and superior wall carotid" ~ "ICA",
        aneuLoca == "Ant Choroidal segment carotid" ~ "ICA",
        aneuLoca == "Superior wall ICA" ~ "ICA",
        aneuLoca == "Lateral wall ICA CoP" ~ "ICA",
        aneuLoca == "Carotid bifurcation ICA bif" ~ "ICA",
        aneuLoca == "Lateral wall ICA ChA" ~ "ICA",
        aneuLoca == "Carotid Bifurcation" ~ "ICA",
        aneuLoca == "Ant Choroidal Segment - Inferior wall" ~ "ICA",

        aneuLoca == "Ophthalmic artery" ~ "OphtICA",
        aneuLoca == "Ophthalmic segment carotid" ~ "OphtICA",
        aneuLoca == "Medial wall carotid" ~ "OphtICA",
        aneuLoca == "Medial wall ICA ophtalmic" ~ "OphtICA",
        aneuLoca == "Medial wall ICA distal" ~ "OphtICA",
        aneuLoca == "Inferior wall ICA" ~ "OphtICA",
        aneuLoca == "Ophtalmic Segment - Medial wall" ~ "OphtICA",
        aneuLoca == "Ophtalmic Segment - Lateral wall" ~ "OphtICA",

        aneuLoca == "Basilar Tip" ~ "Basilar",
        aneuLoca == "Basilar tip" ~ "Basilar",
        aneuLoca == "basilar artery bifurcation" ~ "Basilar",
        aneuLoca == "Basilar bifurcation" ~ "Basilar",
        aneuLoca == "P 1 posterior cerebral" ~ "Basilar",
        aneuLoca == "P1 Posterior cerebral artery" ~ "Basilar",

        aneuLoca == "AICA" ~ "V-B",
        aneuLoca == "PICA" ~ "V-B",
        aneuLoca == "Superior cerebellar artery" ~ "V-B",
        aneuLoca == "Superior Cerebellar artery  (SCA)" ~ "V-B",
        aneuLoca == "V4 segment vertebral artery" ~ "V-B",
        aneuLoca == "Basilar trunk" ~ "V-B",
        aneuLoca == "AICA" ~ "V-B",
        aneuLoca == "PICA" ~ "V-B",
        aneuLoca == "Vertebral" ~ "V-B",
        aneuLoca == "Vertebral trunk" ~ "V-B",
        aneuLoca == "Basilar others" ~ "V-B",
        aneuLoca == "Basilar SCA" ~ "V-B",
        aneuLoca == "Vertebral = PICA origin" ~ "V-B",
        aneuLoca == "PICA distal" ~ "V-B",
        aneuLoca == "SCA Segment of Basilar Trunk" ~ "V-B",
        aneuLoca == "PICA Segment of Vertebral Artery" ~ "V-B",
        aneuLoca == "Superior Cerebellar artery (SCA)" ~ "V-B",
        aneuLoca == "AICA Segment of Basilar Trunk" ~ "V-B",

        aneuLoca == "A2 segment ant" ~ "A2",
        aneuLoca == "A2 segment ant cerebral artery" ~ "A2",
        aneuLoca == "Pericallosal cerebral artery" ~ "A2",
        aneuLoca == "Pericallosal proximal" ~ "A2",
        aneuLoca == "Pericallosal typical" ~ "A2",

        aneuLoca == "A1 segment ant" ~ "A1 segment ant",
        aneuLoca == "A1 segment ant cerebral artery" ~ "A1 segment ant",

        aneuLoca == "P1-P2 junction posterior cerebral artery" ~ "PC",
        aneuLoca == "P2 posterior cerebral artery" ~ "PC",
        aneuLoca == "P 2 posterior cerebral" ~ "PC",
        aneuLoca == "P 1/2 posterior cerebral" ~ "PC",

        aneuLoca == "Distal posterior cerebral artery" ~ "Other",
        aneuLoca == "Ant Choroidal segment carotid" ~ "Other",
        aneuLoca == "Distal to sylvian bifurcation" ~ "Other",
        aneuLoca == "M1 segment middle cerebral artery" ~ "Other",
        aneuLoca == "Distal ant cerebral artery" ~ "Other",
        aneuLoca == "Middle cerebral peripheral" ~ "Other",
        aneuLoca == "Pericallosal distal" ~ "Other",
        aneuLoca == "P 3 posterior cerebral distal" ~ "Other",
        aneuLoca == "Ant Choroidal Segment - Superior wall" ~ "Other",
        aneuLoca == "Posterior Comm Segment - Superior wall" ~ "Other",

        aneuLoca == "Intracavernous internal carotid" ~ "CavICA",
        aneuLoca == "Intracavernous Internal carotid" ~ "CavICA",

        aneuLoca == "Infundibulum" ~ "Infundibulum",

        aneuLoca == "Cervical internal carotid" ~ "Removed",
        aneuLoca == "Petrous pyramidal" ~ "Removed",

        is.na(aneuLoca) ~ NA_character_,
        TRUE ~ paste("UNDEFINED:", aneuLoca)
      ),

      # Group size according to maxDiam.
      IA_size.at.time.of.SAH.groups = case_when(
        maxDiam <= 7 ~ "A",
        maxDiam > 7 &
          maxDiam <= 12 ~ "B",
        maxDiam > 12 &
          maxDiam <= 25 ~ "C",
        maxDiam >= 12 ~ "D",
        is.na(maxDiam) ~ "E"
      ),

      # Hypertension is selfreported in ISGC
      Hypertension = case_when(
        prf_HyperTension == "Yes - Treated and Controlled" ~ "AnyType",
        prf_HyperTension == "Yes - Not Treated" ~ "AnyType",
        prf_HyperTension == "Yes - Treated and Poorly Controlled" ~ "AnyType",
        prf_HyperTension == "No" ~ "Never",
        is.na(prf_HyperTension) ~ NA_character_,
        TRUE ~ paste("UNDEFINED:", prf_HyperTension)
      ),

      # Rupture status
      Ruptured_IA = case_when(
        ruptureStatus == "Ruptured" ~ "Yes",
        ruptureStatus == "Fully treated" ~ "Yes",
        ruptureStatus == "Symptomatic" ~ "Yes",
        ruptureStatus == "Unruptured" ~ "No",
        is.na(ruptureStatus) ~ NA_character_,
        TRUE ~ paste("UNDEFINED:", ruptureStatus)
      ),

      # Reason for recruitment
      Basis.of.recruitment_CaseControl = case_when(
        recruBasis == "Control" ~ "Control",
        recruBasis == "Sub Arachnoid Hemorrhage" ~ "Case",
        recruBasis == "Symptomatic Intracranial Aneurysm" ~ "Case",
        recruBasis == "Incidental Intracranial Aneurysm" ~ "Case",
        recruBasis == "Retrospective Sub Arachnoid Hemorrhage" ~ "Case",
        recruBasis == "Retrospective Symptomatic Intracranial Aneurysm" ~ "Case",
        recruBasis == "Retrospective Incidental Intracranial Aneurysm'" ~ "Case",
        recruBasis == "Genetically linked family member" ~ "Case",
        recruBasis == "Retrospective Incidental Intracranial Aneurysm" ~ "Case",
        is.na(recruBasis) ~ NA_character_,
        TRUE ~ paste("UNDEFINED:", recruBasis)
      )
    )


  # Check ISGC variables
  # ---------
  cat("\nChecking new variables:\n")

  detectUndefinedValue(crim.mod, "Smoking_Current_Former_No", "prf_SmokedTobacco")
  detectUndefinedValue(crim.mod, "IA_Location", "aneuLoca")
  detectUndefinedValue(crim.mod, "Hypertension", "prf_HyperTension")
  detectUndefinedValue(crim.mod, "Ruptured_IA", "ruptureStatus")
  detectUndefinedValue(crim.mod, "Basis.of.recruitment_CaseControl", "recruBasis")


  # Build wrapped/grouped ISGC variables
  # ---------
  # Group age variable
  crim.mod <- crim.mod %>%
    mutate(
      Age.at.diag.grouped = case_when(
        is.na(age.at.time.of.diagnosis) & Ruptured_IA == "No" ~ 200,
        TRUE ~ age.at.time.of.diagnosis
      )
    ) %>%
    mutate(
      Age.at.diag.grouped = cut(
        Age.at.diag.grouped,
        breaks = c(0,
                   seq(40, 65, 5),
                   199, max(Age.at.diag.grouped, na.rm = TRUE)),
        include.lowest = TRUE,
        right = FALSE
      )
    ) %>%
    mutate(Age.at.diag.grouped = factor(Age.at.diag.grouped, labels = LETTERS[1:length(levels(Age.at.diag.grouped))])) %>%

    # IA Multiplicity according to noAnevrisme: Multiple IAs, if anything bigger than only 1. Equal to aneuUniqueID.
    group_by(patientID) %>%
    mutate(Multiple.IAs = case_when(2 %in% noAnevrisme ~ "Yes",
                                    TRUE ~ "No")) %>%
    ungroup() %>%

    # Rename columns accordingly
    rename(
      ID_2 = patientID,
      Gender = patSex,
      Basis.of.recruitment = recruBasis,
      Positive.famillial.history = prf_PosFamHis,
      IA_size.at.time.of.SAH = maxDiam
    ) %>%

    # Keep only the most important IA (according to surgeons opinion)
    filter(aneuUniqueID == 1)

  # Return selection of variables
  if (!returnall) {
    return(
      select(
        .data = crim.mod,
        ID_2,
        IA_Location,
        Basis.of.recruitment_CaseControl,
        Source,
        Gender,
        Smoking_Current_Former_No,
        Multiple.IAs,
        Hypertension,
        Ruptured_IA,
        Positive.famillial.history,
        # Age.at.time.of.SAH,
        age.at.time.of.diagnosis,
        Age.at.diag.grouped,
        IA_size.at.time.of.SAH.groups,
        IA_size.at.time.of.SAH
      )
    )
  } else{
    return(crim.mod)
  }
}
