# Tests for utils_classify.R — study design, research category, primary procedure

library(testthat)
source(here::here("R", "utils_classify.R"))

# ============================================================
# classify_study_design
# ============================================================

test_that("RCT detected from flag", {
  expect_equal(classify_study_design("any text", is_rct = TRUE), "rct")
})

test_that("systematic reviews detected", {
  expect_equal(classify_study_design("a systematic review of laparoscopic outcomes"), "systematic_review")
  expect_equal(classify_study_design("we performed a meta-analysis of 12 trials"), "systematic_review")
  expect_equal(classify_study_design("this scoping review examined"), "systematic_review")
})

test_that("prospective cohort detected", {
  expect_equal(classify_study_design("prospective cohort study of 200 patients"), "prospective_cohort")
  expect_equal(classify_study_design("prospective observational study"), "prospective_cohort")
  expect_equal(classify_study_design("a longitudinal study over 5 years"), "prospective_cohort")
})

test_that("retrospective cohort detected from multiple patterns", {
  expect_equal(classify_study_design("retrospective cohort study"), "retrospective_cohort")
  expect_equal(classify_study_design("we performed a chart review"), "retrospective_cohort")
  expect_equal(classify_study_design("a database study using claims data"), "retrospective_cohort")
  expect_equal(classify_study_design("we reviewed charts of all patients"), "retrospective_cohort")
  expect_equal(classify_study_design("retrospective analysis of outcomes"), "retrospective_cohort")
})

test_that("national databases classified as retrospective cohort", {
  expect_equal(classify_study_design("using the ACS-NSQIP database"), "retrospective_cohort")
  expect_equal(classify_study_design("data from NIS nationwide inpatient sample"), "retrospective_cohort")
  expect_equal(classify_study_design("we queried the SEER database"), "retrospective_cohort")
  expect_equal(classify_study_design("the national cancer database NCDB was used"), "retrospective_cohort")
})

test_that("case-control detected", {
  expect_equal(classify_study_design("case-control study comparing outcomes"), "case_control")
  expect_equal(classify_study_design("a matched case control analysis"), "case_control")
})

test_that("case series detected", {
  expect_equal(classify_study_design("a case series of 15 patients"), "case_series")
  expect_equal(classify_study_design("we present a single-case report"), "case_series")
  expect_equal(classify_study_design("video presentation of a novel technique"), "case_series")
})

test_that("cross-sectional and survey detected", {
  expect_equal(classify_study_design("cross-sectional analysis of prevalence"), "cross_sectional")
  expect_equal(classify_study_design("we distributed a survey to residents"), "cross_sectional")
  expect_equal(classify_study_design("an online questionnaire was administered"), "cross_sectional")
})

test_that("quality improvement detected", {
  expect_equal(classify_study_design("a quality improvement project"), "quality_improvement")
  expect_equal(classify_study_design("implementing a PDSA cycle"), "quality_improvement")
})

test_that("simulation and lab studies detected", {
  expect_equal(classify_study_design("a simulation study using porcine tissue"), "simulation_lab")
  expect_equal(classify_study_design("cadaver dissection for anatomy training"), "simulation_lab")
  expect_equal(classify_study_design("bench-top testing of suture techniques"), "simulation_lab")
})

test_that("cost analysis detected", {
  expect_equal(classify_study_design("cost-effectiveness analysis of robotic surgery"), "cost_analysis")
  expect_equal(classify_study_design("an economic evaluation comparing approaches"), "cost_analysis")
})

test_that("validation studies detected", {
  expect_equal(classify_study_design("validation study of a new instrument"), "validation")
  expect_equal(classify_study_design("we validated the scoring system"), "validation")
})

test_that("unclassifiable returns other", {
  expect_equal(classify_study_design("the sky is blue"), "other")
  expect_equal(classify_study_design(NA), "other")
  expect_equal(classify_study_design(""), "other")
})

# ============================================================
# classify_research_category
# ============================================================

test_that("basic science detected", {
  expect_equal(classify_research_category("gene expression analysis of endometrial tissue"), "basic_science")
  expect_equal(classify_research_category("molecular pathways in cell proliferation"), "basic_science")
  expect_equal(classify_research_category("protein biomarker levels in serum"), "basic_science")
})

test_that("education detected", {
  expect_equal(classify_research_category("simulation training for laparoscopic skills"), "education")
  expect_equal(classify_research_category("surgical curriculum for residents"), "education")
  expect_equal(classify_research_category("virtual reality warm-up before surgery"), "education")
})

test_that("education not triggered for RCTs about training", {
  # When study_design is rct, the education gate is bypassed
  result <- classify_research_category("simulation training for laparoscopic skills assessment", study_design = "rct")
  expect_true(result != "education")
})

test_that("quality improvement detected", {
  expect_equal(classify_research_category("quality improvement in surgical safety checklist"), "quality_improvement")
  expect_equal(classify_research_category("ERAS protocol implementation reduced length of stay"), "quality_improvement")
})

test_that("health services detected", {
  expect_equal(classify_research_category("racial disparities in hysterectomy approach"), "health_services")
  expect_equal(classify_research_category("cost and utilization of robotic surgery"), "health_services")
  expect_equal(classify_research_category("insurance coverage and access to care"), "health_services")
})

test_that("device and technology detected", {
  expect_equal(classify_research_category("robotic platform for hysterectomy"), "device_technology")
  expect_equal(classify_research_category("artificial intelligence model for image recognition"), "device_technology")
  expect_equal(classify_research_category("a novel surgical instrument for suturing"), "device_technology")
})

test_that("clinical is the broad fallback", {
  expect_equal(classify_research_category("patient outcomes after laparoscopic surgery"), "clinical")
  expect_equal(classify_research_category("surgical complications and operative time"), "clinical")
})

test_that("other when nothing matches", {
  expect_equal(classify_research_category("the weather today is sunny"), "other")
  expect_equal(classify_research_category(NA), "other")
})

# ============================================================
# classify_primary_procedure
# ============================================================

test_that("hysterectomy detected", {
  expect_equal(classify_primary_procedure("total laparoscopic hysterectomy outcomes"), "hysterectomy")
  expect_equal(classify_primary_procedure("robotic-assisted hysterectomy for benign disease"), "hysterectomy")
})

test_that("myomectomy detected", {
  expect_equal(classify_primary_procedure("laparoscopic myomectomy for large fibroids"), "myomectomy")
})

test_that("endometriosis detected", {
  expect_equal(classify_primary_procedure("deep infiltrating endometriosis excision"), "endometriosis")
  expect_equal(classify_primary_procedure("surgical management of endometriosis"), "endometriosis")
})

test_that("sacrocolpopexy detected over pelvic_floor", {
  # sacrocolpopexy is more specific and should win
  expect_equal(classify_primary_procedure("robotic sacrocolpopexy for prolapse"), "sacrocolpopexy")
})

test_that("pelvic floor detected", {
  expect_equal(classify_primary_procedure("midurethral sling for stress incontinence"), "pelvic_floor")
  expect_equal(classify_primary_procedure("pelvic organ prolapse repair"), "pelvic_floor")
})

test_that("gyn oncology detected", {
  expect_equal(classify_primary_procedure("staging for endometrial cancer"), "gynecologic_oncology")
  expect_equal(classify_primary_procedure("sentinel lymph node mapping in cervical malignancy"), "gynecologic_oncology")
})

test_that("adnexal surgery detected", {
  expect_equal(classify_primary_procedure("laparoscopic salpingo-oophorectomy"), "adnexal_surgery")
})

test_that("fibroids detected separately from myomectomy", {
  expect_equal(classify_primary_procedure("uterine artery embolization for leiomyoma"), "fibroids")
})

test_that("sterilization detected", {
  expect_equal(classify_primary_procedure("laparoscopic tubal sterilization"), "sterilization")
  expect_equal(classify_primary_procedure("Essure device placement"), "sterilization")
})

test_that("NA when no procedure mentioned", {
  expect_equal(classify_primary_procedure("survey of resident training"), NA_character_)
  expect_equal(classify_primary_procedure(NA), NA_character_)
})

test_that("priority order: specific beats general", {
  # myomectomy > hysterectomy (more specific)
  expect_equal(classify_primary_procedure("myomectomy vs hysterectomy outcomes"), "myomectomy")
  # sacrocolpopexy > pelvic_floor
  expect_equal(classify_primary_procedure("sacrocolpopexy for pelvic organ prolapse"), "sacrocolpopexy")
})
