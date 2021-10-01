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
#' @importFrom dplyr filter
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

#' Harmonization of CRIM 4.0 ADB scheme to ISGC
#'
#' Query the ADB for all relevant features to be harmonized into ISGC.
#'
#' @param data data.frame of extracted variables of ADB
#' @param returnall binary. If TRUE a all original variables is returned additionally to the ISGC features¨.
#'
#' @return data.frame of variables in ISGC structure
#'
#' @importFrom dplyr group_by mutate distinct ungroup case_when rename
#' @importFrom tidyr fill
#' @importFrom tidyselect everything
#' @importFrom lubridate time_length interval
#'
#' @export
crim4toISGC <- function(data, returnall = FALSE) {
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
      "aneuUniqueID"
    )
  missing_feats <- c()
  for (feat in necessary_feats) {
    if (!(feat %in% names(data))) {
      missing_feats <- c(missing_feats, feat)
    }
  }
  if (!purrr::is_empty(missing_feats)) {
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
    ungroup()

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
        lubridate::interval(patDOB, aneuReportDate), "years"
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
        is.na(prf_SmokedTobacco) ~ NA_character_,
        TRUE ~ paste("UNDEFINED:", prf_SmokedTobacco)
      ),
      Smoking_Current_Not.Current = case_when(
        prf_SmokedTobacco == "Yes: quit smoking" ~ "Not Current",
        prf_SmokedTobacco == "Yes: still smoking" ~ "Current",
        prf_SmokedTobacco == "No" ~ "Not Current",
        is.na(prf_SmokedTobacco) ~ NA_character_,
        TRUE ~ paste("UNDEFINED:", prf_SmokedTobacco)
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
    mutate(Multiple.IAs = case_when(2 %in% aneuUniqueID ~ "Yes",
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


#' Harmonization of CRIM 5.0 ADB scheme to ISGC
#'
#' Query the ADB for all relevant features to be harmonized into ISGC.
#'
#' @param data data.frame of extracted variables of ADB
#' @param returnall binary. If TRUE a all original variables is returned additionally to the ISGC features¨.
#'
#' @return data.frame of variables in ISGC structure
#'
#' @importFrom dplyr group_by mutate distinct ungroup case_when rename
#' @importFrom tidyr fill
#' @importFrom tidyselect everything
#' @importFrom lubridate time_length interval
#'
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
  if (!purrr::is_empty(missing_feats)) {
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
