phenotype_labels <- c(
  ADULT_ISCH = "Adult ischemic",
  ADULT_HEM = "Adult hemorrhagic",
  ADULT_MIX = "Adult mixed",
  PED_ISCH = "Pediatric ischemic",
  PED_MIX = "Pediatric mixed MMD/MMS",
  ASYM_STABLE = "Asymptomatic/stable",
  MMS_SCD = "SCD-associated MMS",
  MMS_NF1 = "NF1-associated MMS",
  MMS_OTHER = "Other MMS",
  PREG = "Pregnancy",
  MIXED_UNSEP = "Mixed/unseparated"
)

comparison_labels <- c(
  C00 = "No formal comparison",
  C01 = "Surgery vs conservative",
  C02 = "Direct vs indirect",
  C03 = "Combined vs indirect",
  C04 = "Direct vs combined",
  C05 = "Pediatric direct/combined vs indirect",
  C06 = "Hemorrhagic MMD: revascularization comparison",
  C07 = "Early safety vs late benefit",
  C08 = "Asymptomatic: surgery vs surveillance",
  C09 = "Antiplatelet vs none/alternative",
  C10 = "Inhalational vs intravenous anesthesia",
  C11 = "Remote ischemic conditioning vs control",
  C12 = "Indirect technique subtypes",
  C13 = "Double-barrel vs benchmark",
  C14 = "SCD-MMS: surgery + transfusion",
  C15 = "Adult direct/combined vs indirect"
)

outcome_labels <- c(
  O01 = "Early ischemic stroke",
  O02 = "Late ischemic stroke",
  O03 = "TIA",
  O04 = "Early intracranial hemorrhage",
  O05 = "Late hemorrhage/rebleeding",
  O06 = "Favorable functional outcome",
  O07 = "Mortality",
  O08 = "Hyperperfusion syndrome",
  O09 = "Postoperative seizure",
  O10 = "Matsushima/collateralization",
  O11 = "Bypass patency/capacity",
  O12 = "CBF/CVR/OEF/perfusion",
  O13 = "Cognition/neuropsychology",
  O14 = "QoL/school/work/headache",
  O15 = "Failure/reoperation",
  O16 = "Any/recurrent stroke",
  O99 = "Other/composite"
)

horizon_labels <- c(
  T0 = "Intraoperative",
  T1 = "Early/perioperative",
  T2 = "Intermediate",
  T3 = "Late/long-term",
  T4 = "Mixed/unspecified follow-up",
  T9 = "Not reported/not analytic"
)

unit_labels <- c(
  U01 = "Patient",
  U02 = "Hemisphere",
  U03 = "Procedure",
  U04 = "Bypass/anastomosis",
  U05 = "Patient-year",
  U06 = "Pregnancy",
  U07 = "Mixed/unclear"
)

measure_labels <- c(
  EM_OR = "OR", EM_RR = "RR", EM_HR = "HR",
  EM_MD = "MD", EM_WMD = "WMD",
  EM_PROPORTION = "Proportion", EM_RATE = "Rate",
  EM_COUNT = "Count", EM_MIXED_RATIO = "Mixed ratio",
  EM_CONTINUOUS_OTHER = "Continuous",
  EM_DESCRIPTIVE = "Descriptive", EM_OTHER = "Other"
)

label_code <- function(x, dictionary) {
  out <- unname(dictionary[as.character(x)])
  ifelse(is.na(out), as.character(x), out)
}
