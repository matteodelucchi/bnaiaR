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

#' Aneurysm Data Base of multiple study centres harmonized according to ISGC.
#'
#' Processed raw data and harmonized according \link[ExplorDataISGC]{harmonize2isgc} in \code{vignette("combineISGC", package = "ExplorDataISGC")}
#' and \code{vignette("data_preprocessing", package = "bnaiaR")}.
#' This separation of the preprocessing is on top of the anonymised raw data,
#' an additional layer of security to ensure privacy of patient data.
#' While the package \code{ExplorDataISGC} will never be published, \code{bnaiaR} is intended for open access.
#'
#' @format data frame
#' \describe{
#'   \item{ID_1}{Internal Identifier 1}
#'   \item{ID_2}{Internal Identifier 2}
#'   \item{source}{Study source abbreviation}
#'   \item{gender}{Patients biological sex (male, female)}
#'   \item{basis_of_recruitment}{Reason for data collection}
#'   \item{basis_of_recruitment_CaseControl}{Reason for data collection (case, control)}
#'   \item{age}{Age in years of patient at time of intracranial aneurysm diagnosis: if Ruptured_IA = Yes -> "Age" refers to the patient age at time of rupture. If Ruptured_IA = No -> "Age" refers to the patient age at the time of her first diagnosed IA (oldest IA).}
#'   \item{positive_famillial_history}{Familiar incident known (yes, no, probably). See details.}
#'   \item{smoking_current_former_no}{Patient classified as current, former or non-smoker. See details.}
#'   \item{smoking_current_notcurrent}{Wrapper of smoking_current_former_no.}
#'   \item{number_of_IAs}{Total number of IAs diagnosed.}
#'   \item{multipleIAs}{Detection of multiple IAs, binary (yes, no). Wrapper of number_of_IAs.}
#'   \item{hypertension}{Known hypertension, binary (no, AnyType). See details.}
#'   \item{hypertension_treatment}{Known hypertension treatment. See details.}
#'   \item{IAlocation}{Location of the detected IA}
#'   \item{IAruptured}{IA rupture status (yes, no)}
#'   \item{IAsize_at_diag}{Measure for IA size [mm]}
#'   \item{IAsize_at_diag_grouped}{Wrapper of IAsize_at_diag grouped. See details.}
#'
#'
#'   \item{mRS_afterSAH}{modified ranking scale at 1y after ruptured IA}
#'   \item{gos_afterSAH}{Glasgow outcome scale at 1y after ruptured IA}
#'   \item{maxFUtime}{maximum follow-up time of patients diagnosed with unruptured IA}
#'   \item{termination_reason}{Follow-up termination reason of patients diagnosed with unruptured IA.	}
#' }
#'
#' @details
#' \itemize{
#'  \item basis_of_recruitment
#'  \itemize{
#'    \item Retrospective Symptomatic Intracranial Aneurysm	Patient diagnosed inititaly with a symptom associated with the intracranial aneurysm / Retrospectif diagnosed  prior to november 1st 2006
#'    \item Symptomatic Intracranial Aneurysm	Patient diagnosed inititaly with a symptom associated with the intracranial aneurysm
#'    \item Retrospective Incidental Intracranial Aneurysm	Patient incidentaly diagnosed with intracranial aneurysm / Retrospectif diagnosed prior to november 1st 2006
#'    \item Incidental Intracranial Aneurysm	Patient incidentaly diagnosed with intracranial aneurysm
#'    \item Retrospective Sub Arachnoid Hemorrhage	Patient diagnosed with subarachnoid bleed as a consequence of intracranial aneurysm rupture / Retrospectif diagnosed prior to november 1st 2006
#'    \item Sub Arachnoid Hemorrhage	Patient diagnosed with subarachnoid bleed as a consequence of intracranial aneurysm rupture
#'    \item Genetically linked family member	Patient screened in the context of a positive familial history initialy without intracranial aneurysm
#'    \item Control	Healthy volunteers no screening for intracranial aneurysm was performed
#'    \item Case	Patients diagnosed with intracranial aneurysm(s) with unknown status of rupture and Genetically linked family member
#'  }
#'  \item basis_of_recruitment_CaseControl
#'  \itemize{
#'    \item case	Retrospective Symptomatic Intracranial Aneurysm, Symptomatic Intracranial Aneurysm, Retrospective Incidental , Retrospective Sub Arachnoid Hemorrhage, Incidental Intracranial AneurysmIntracranial Aneurysm, Sub Arachnoid Hemorrhage, Case, Genetically linked family member (Number of IA >=1)
#'    \item control	Control, Genetically linked family member (Number of IA= NA)
#'    \item NA	not available
#'  }
#'  \item age
#'  \itemize{
#'    \item 0 to 130	age in years
#'    \item NA	not available
#'  }
#'  \item positive_famillial_history
#'  \itemize{
#'    \item no	No 1st degree relative with intracranial aneurysm
#'    \item yes	One or more 1st degree relative(s) with intracranial aneurysm
#'    \item probably	The relative had a stroke but there is no definite diagnosis on the type of stroke
#'    \item NA	Unknown familial history of intracranial aneurysm
#'  }
#'  \item smoking_current_former_no
#'  \itemize{
#'    \item No	Never smoked (no more than 300 cigarets ever)
#'    \item Former	Smoked (more than 300 cigarets) and stopped (at least 6 month ago)
#'    \item Current	Smoked (more than 300 cigarets) and continues current smoking
#'    \item NA	Smoker status not available
#'  }
#'  \item smoking_current_notcurrent
#'  \itemize{
#'    \item Current	Never smoked (no more than 300 cigarets ever)
#'    \item Not Current	No + Former.
#'    \item NA	Smoker status not available
#'  }
#'  \item number_of_IAs
#'  \itemize{
#'    \item 0:inf	Number of intracranial aneurysms diagnosed
#'    \item NA	Not available or Healthy volunteers that were not screened are considered with an unknown number of aneurysm
#'  }
#'  \item multipleIAs
#'  \itemize{
#'    \item yes	>1 number of IAs
#'    \item no	0 or 1 number of IAs
#'    \item NA	Number of IAs not available
#'  }
#'  \item hypertension
#'  \itemize{
#'    \item no	Blood pressure less than 120/80 mm Hg
#'    \item yes	Blood pressure greater than 140/90 mm Hg
#'    \item NA	Not available
#'  }
#'  \item hypertension_treatment
#'  \itemize{
#'    \item No	No antihypertensive treatment
#'    \item yes	Hypertensive treatment
#'    \item NA	Not available
#'  }
#'  \item IAlocation. For details see \link[ExplorDataISGC]{harmonize_loc}
#'  \itemize{
#'    \item Acom	Ant communicating artery, Comm anterior CoA forward, Comm anterior CoA forward-upward, Comm anterior CoA upward, Comm anterior CoA backward, Comm anterior CoA down
#'    \item MCA	Sylvian bifurcation, M1 segment middle cerebral artery, M1 perforator artery,, Middle cerebral bifurcation, Middle cerebral MCA main trunk
#'    \item Pcom	Posterior Comm
#'    \item ICA	Anterior and superior wall carotid, Carotid bifurcation, Ant Choroidal segment carotid, Superior wall ICA, Lateral wall ICA CoP, Carotid bifurcation ICA bif, Lateral wall ICA ChA
#'    \item OphtICA	Opthalmic artery, Ophthalmic segment carotid, Medial wall carotid, Medial wall ICA ophtalmic, Medial wall ICA distal, Inferior wall ICA
#'    \item Basilar	Basilar Tip, Basilar tip, basilar artery bifurcation, Basilar bifurcation, P 1 posterior cerebral, P1 Posterior cerebral artery
#'    \item V-B	Basilar trunk, V4 segment vertebral artery, AICA, Superior cerebellar artery, PICA, Vertebral, Vertebral trunk, Basilar others, Basilar SCA, Vertebral = PICA origin, PICA distal"
#'    \item A2	Pericallosal cerebral artery, A2 segment ant, Pericallosal proximal, Pericallosal typical
#'    \item A1 segment ant	A1 segment ant
#'    \item PC	P2 posterior cerebral artery, P1-P2 junction posterior cerebral artery, P 2 posterior cerebral, P 1/2 posterior cerebral
#'    \item Other	Distal to sylvian bifurcation, Distal ant cerebral artery, Distal posterior cerebral artery, Middle cerebral peripheral, Pericallosal distal, P 3 posterior cerebral distal
#'    \item CavICA	Intracavernous internal carotid
#'    \item Infundibulum
#'    \item Removed	Cervical internal carotid, Petrous pyramidal
#'    \item NA	Unknown
#'  }
#'  \item IAruptured
#'  \itemize{
#'    \item Yes	Patient known to have ruptured an intracranial aneurysm: Ruptured, Fully treated, Symptomatic
#'    \item No	Patient known to have a never ruptured aneurysm: Unruptured
#'    \item NA	Patient with unknown status of rupture of aneurysm(s)
#'  }
#'  \item IAsize_at_diag
#'  \itemize{
#'    \item 0 to inf	Maximum diameter size in mm single digit numerical display
#'    \item NA	Healthy volunteers that were not screened are considered with an unknown aneurysm size
#'  }
#'  \item IAsize_at_diag_grouped
#'  \itemize{
#'    \item A=< 7mm	Maximum diameter size of aneurysm less or equal to 7mm
#'    \item B=7-12mm	Maximum diameter size of aneurysm between 7 and 12mm included
#'    \item C=13-25mm	Maximum diameter size of aneurysm between 13 and 25mm included
#'    \item D=>25mm	Maximum diameter size of aneurysm larger or equal to 25mm
#'    \item E=NA	Unknown size
#'  }
#'  \item maxFUtime
#'  \itemize{
#'    \item 0 to inf	time in years
#'    \item NA	Not available
#'  }
#'  \item termination_reason
#'  \itemize{
#'    \item IAex	IA excluded and no longer under observation.
#'    \item aSAH	IA ruptured.
#'    \item lossFU	Loss on follow-up.
#'    \item deathOC	Patient died from other cause.
#'    \item NA	Not available
#'  }
#' }
#'
#'
"adbisgc"

#' Additive BN Analysis Input Data
#'
#' Aneurysm data from multiple centres harmonized, preprocessed and transformed.
#' A selection of variables from \code{data("adbisgc", package = "bnaiaR")},
#' prior knowledge and additional required data-set depended information for
#' BN analysis with `bnlearn` and `abn`.
#' For details consider the vignette
#' \code{vignette("data_preparation_for_experiments", package = "bnaiaR")}.
#'
#' @format A list of 4 items:
#' \describe{
#'   \item{abndata}{data frame of	7481 obs. of  10 discrete and continuous variables.}
#'   \item{dist}{List of 10. Type of distribution for each variable in abndata.}
#'   \item{banned}{matrix of 10x10. Prior knowledge implementation as matrix of banned arcs for package `abn`.}
#'   \item{bl}{data frame. Prior knowledge implementation as blacklist for package `bnlearn`.}
#'   \item{retain}{matrix of 10x10. Prior knowledge implementation as matrix of retained arcs for package `abn`.}
#'   \item{wl}{data frame. Prior knowledge implementation as whitelist for package `bnlearn`.}
#' }
#'
"exp4_dat"

#' Additive BN Analysis Input Data, whitelisted study_source
#'
#' Variation of \code{data(exp4_dat, package = "bnaiaR")} with `study_source`
#' forced (whitelisted) as parent of all other variables.
#' For details consider the vignette
#' \code{vignette("data_preparation_for_experiments", package = "bnaiaR")}.
#'
#' @format A list of 4 items:
#' \describe{
#'   \item{abndata}{data frame of	7481 obs. of  10 discrete and continuous variables.}
#'   \item{dist}{List of 10. Type of distribution for each variable in abndata.}
#'   \item{banned}{matrix of 10x10. Prior knowledge implementation as matrix of banned arcs for package `abn`.}
#'   \item{bl}{data frame. Prior knowledge implementation as blacklist for package `bnlearn`.}
#'   \item{retain}{matrix of 10x10. Prior knowledge implementation as matrix of retained arcs for package `abn`.}
#'   \item{wl}{data frame. Prior knowledge implementation as whitelist for package `bnlearn`.}
#' }
#'
"exp44_dat"

#' Discrete BN Analysis Input Data
#'
#' Aneurysm data from multiple centres harmonized, preprocessed and transformed.
#' A selection of variables from \code{data("adbisgc", package = "bnaiaR")},
#' prior knowledge and additional required data-set depended information for
#' BN analysis with `bnlearn` and `abn`.
#' For details consider the vignette
#' \code{vignette("data_preparation_for_experiments", package = "bnaiaR")}.
#'
#' @format A list of 4 items:
#' \describe{
#'   \item{abndata}{data frame of	7481 obs. of  10 discrete variables.}
#'   \item{dist}{List of 10. Type of distribution for each variable in abndata.}
#'   \item{banned}{matrix of 10x10. Prior knowledge implementation as matrix of banned arcs for package `abn`.}
#'   \item{bl}{data frame. Prior knowledge implementation as blacklist for package `bnlearn`.}
#'   \item{retain}{matrix of 10x10. Prior knowledge implementation as matrix of retained arcs for package `abn`.}
#'   \item{wl}{data frame. Prior knowledge implementation as whitelist for package `bnlearn`.}
#' }
#'
"exp1_dat"

#' Discrete Bayesian Network Models
#'
#' For details consider the vignette
#' \code{vignette("discrete_BN_SL", package = "bnaiaR")}.
#'
#' @format A list of 2 lists with different "bn" objects and "bn.strength"
#' objects respectively.
#'
"discrete_bns"

#' BnaiaR variables without NAs in different transformations
#'
#' This is the basis on which the experiment-data was prepared.
#'
#' For details consider the vignette
#' \code{vignette("data_preparation_for_experiments", package = "bnaiaR")}.
#'
#' @format A data.frame
#'
"df_bnaiar_compl"
