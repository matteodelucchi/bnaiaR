#' Aneurysm Data Base Raw
#'
#' Aneurysm data from Geneva (AneuQuest + AneuX) harmonized to ISGC variables.
#' Harmonization was performed according the harmonization matrix from Morel et al. 2021
#' and is implemented in \link[bnaiaR]{crim4toISGC} and  \link[bnaiaR]{crim5toISGC} respectively.
#' Contains missing values.
#' For details consider the vignette
#' \code{vignette("raw_data_harmonization", package = "bnaiaR")}.
#'
#' @format A data frame with 1248 obs. of  15 variables.
#' \describe{
#'   \item{ID_2}{Internal Identifier}
#'   \item{IA_Location}{Location of the detected IA}
#'   \item{Basis.of.recruitment_CaseControl}{Reason for data collection (case, control)}
#'   \item{Source}{Study Source  (Geneva)}
#'   \item{Gender}{Patients biological sex (male, female)}
#'   \item{Smoking_Current_Former_No}{Patient classified as current, former or non-smoker. See details.}
#'   \item{Multiple.IAs}{Detection of multiple IAs, binary (yes, no)}
#'   \item{Hypertension}{Known hypertension, binary (no, AnyType). See details.}
#'   \item{Ruptured_IA}{IA rupture status (yes, no)}
#'   \item{Positive.famillial.history}{Familiar incident known (yes, no, probably). See details.}
#'   \item{age.at.time.of.diagnosis}{Age at the time of IA report (aneuReportDate) in years}
#'   \item{Age.at.diag.grouped}{age.at.time.of.diagnosis grouped. See details for explanation.}
#'   \item{IA_size.at.time.of.SAH.groups}{IA_size.at.time.of.SAH grouped. See details.}
#'   \item{IA_size.at.time.of.SAH}{Measure for IA size [mm]}
#'   \item{db}{Data base source. CRIM4 = AneuQuest; CRIM5=AneuX.}
#' }
#'
#' @details
#' \itemize{
#'   \item Current smoker if smoked (> 300 cigarettes) and continues current smoking. If formerly smoked (>300 cigarettes) and stopped at least 6 months ago. Never smoked if < 300 cigarettes smoked ever.
#'
#'   \item Never had hypertension if blood pressure is less than 120/80mmHg. Hypertension of AnyType if ...
#'   \itemize{
#'     \item ... not treated BP >140/90mmHg and the patient does not take any antihypertensive treatment.
#'     \item ... treated and controlled BP >140/90mmHg and the patient takes antihypertensive treatment and BP is in normal range.
#'     \item ... treated and not controlled BP > 140/90mmHg, the patient takes antihypertensive treatment but the BP stay higher than normal BP values.
#'     }
#'
#'  \item Positive familial history of intracranial aneurysm if one or more first
#'  degree relative(s) were diagnosed with IA. Probably if the relative had a stroke
#'  but there is no definite diagnosis on the type of stroke.
#'
#'  \item Unruptured IAs with no age at time of IA report recorded are assigned to group H.
#'  Age at SAH is grouped in \eqn{A=[0,40), B=[40,45), C=[45,50), D=[50,55), E=[55,60), F=[60,65), G=[65,93), H=unruptured}.
#'
#'  \item Size grouped in \eqn{A=< 7mm}	Maximum diameter size of aneurysm less or equal to 7mm;
#'  \eqn{B=7-12mm}	Maximum diameter size of aneurysm between 7 and 12mm included;
#'  \eqn{C=13-25mm}	Maximum diameter size of aneurysm between 13 and 25mm included;
#'  \eqn{D=>25mm}	Maximum diameter size of aneurysm larger or equal to 25mm;
#'  \eqn{E=NA}	Unknown size
#' }
#'
#' @source Morel et al. 2021, in review, Neurology.
#'
"adb.raw"

#' Aneurysm Data Base Preprocessed
#'
#' Aneurysm data from Geneva (AneuQuest + AneuX) harmonized, preprocessed and transformed.
#' Complete data only.
#' For details consider the vignette
#' \code{vignette("data_preprocessing", package = "bnaiaR")}.
#'
#' @format A data frame with 790 obs. of  52 variables.
#'
#' @details
#' \itemize{
#'   \item Current smoker if smoked (> 300 cigarettes) and continues current smoking. If formerly smoked (>300 cigarettes) and stopped at least 6 months ago. Never smoked if < 300 cigarettes smoked ever.
#'
#'   \item Never had hypertension if blood pressure is less than 120/80mmHg. Hypertension of AnyType if ...
#'   \itemize{
#'     \item ... not treated BP >140/90mmHg and the patient does not take any antihypertensive treatment.
#'     \item ... treated and controlled BP >140/90mmHg and the patient takes antihypertensive treatment and BP is in normal range.
#'     \item ... treated and not controlled BP > 140/90mmHg, the patient takes antihypertensive treatment but the BP stay higher than normal BP values.
#'     }
#'
#'  \item Positive familial history of intracranial aneurysm if one or more first
#'  degree relative(s) were diagnosed with IA. Probably if the relative had a stroke
#'  but there is no definite diagnosis on the type of stroke.
#'
#'  \item Age at time of diagnosis is grouped in steps of 5y in AgeDiag.group
#'  \eqn{A=[0,40), B=[40,45), C=[45,50), D=[50,55), E=[55,60), F=[60,65), G=[65,max. AgeDiag), H=unruptured}
#'  and in steps of 10y in AgeDiag.group.coarse
#'  \eqn{A=[0,40), B=[40,50), C=[50,60), D=[60,max. AgeDiag)}
#'
#'  \item Size grouped in \eqn{A=< 7mm}	Maximum diameter size of aneurysm less or equal to 7mm;
#'  \eqn{B=7-12mm}	Maximum diameter size of aneurysm between 7 and 12mm included;
#'  \eqn{C=13-25mm}	Maximum diameter size of aneurysm between 13 and 25mm included;
#'  \eqn{D=>25mm}	Maximum diameter size of aneurysm larger or equal to 25mm;
#'  \eqn{E=NA}	Unknown size
#' }
#'
"adb"

#' Additive BN Analysis Input Data
#'
#' Aneurysm data from Geneva harmonized, preprocessed and transformed.
#' A selection of variables from \code{data("adb", package = "bnaiaR")},
#' prior knowledge and additional required data-set depended information for
#' BN analysis with `bnlearn` and `abn`.
#' For details consider the vignette
#' \code{vignette("data_preparation_for_experiments", package = "bnaiaR")}.
#'
#' @format A list of 4 items:
#' \describe{
#'   \item{abndata}{data frame of	790 obs. of  13 variables, discrete and continuous.}
#'   \item{dist}{List of 13. Type of distribution for each variable in abndata.}
#'   \item{banned}{matrix of 13x13. Prior knowledge implementation as matrix of banned arcs for package `abn`.}
#'   \item{bl}{data frame. Prior knowledge implementation as blacklist for package `bnlearn`.}
#' }
#'
"exp06_dat"

#' Discrete BN Analysis Input Data
#'
#' Aneurysm data from Geneva harmonized, preprocessed and transformed.
#' A selection of variables from \code{data("adb", package = "bnaiaR")},
#' prior knowledge and additional required data-set depended information for
#' BN analysis with `bnlearn` and `abn`.
#' For details consider the vignette
#' \code{vignette("data_preparation_for_experiments", package = "bnaiaR")}.
#'
#' @format A list of 4 items:
#' \describe{
#'   \item{abndata}{data frame of	790 obs. of  9 variables, discrete and continuous.}
#'   \item{dist}{List of 9. Type of distribution for each variable in abndata.}
#'   \item{banned}{matrix of 9x9. Prior knowledge implementation as matrix of banned arcs for package `abn`.}
#'   \item{bl}{data frame. Prior knowledge implementation as blacklist for package `bnlearn`.}
#' }
#'
"exp11_dat"

#' Evidence Knowledge Graph
#'
#' For details consider the vignette
#' \code{vignette("evidenceknowledgegraph", package = "bnaiaR")}.
#'
#' @format A list of 3 items and class "bn".
#'
"ekg"

#' Discrete Bayesian Network Models
#'
#' For details consider the vignette
#' \code{vignette("discrete_BN_SL", package = "bnaiaR")}.
#'
#' @format A list of 2 lists with different "bn" objects and "bn.strength"
#' objects respectively.
#'
"discrete_bns"
