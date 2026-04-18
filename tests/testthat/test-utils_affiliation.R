# Tests for utils_affiliation.R — practice type, subspecialty, career stage

library(testthat)
source(here::here("R", "utils_affiliation.R"))

# ============================================================
# classify_practice_type
# ============================================================

test_that("military/VA detected first", {
  expect_equal(classify_practice_type("Walter Reed National Military Medical Center"), "military_va")
  expect_equal(classify_practice_type("VA Medical Center, Portland, OR"), "military_va")
  expect_equal(classify_practice_type("Department of Surgery, Veterans Affairs Hospital"), "military_va")
})

test_that("research institutes without university are classified", {
  expect_equal(classify_practice_type("Endometriosis Research Foundation, Zurich"), "research_institute")
  expect_equal(classify_practice_type("National Cancer Research Institute"), "research_institute")
})

test_that("academic institutions classified via ACGME cross-reference", {
  expect_equal(classify_practice_type("Methodist Dallas Medical Center, Dallas, TX"), "academic")
  expect_equal(classify_practice_type("Mayo Clinic, Rochester, MN"), "academic")
})

test_that("academic institutions classified via regex fallback", {
  expect_equal(classify_practice_type("Department of OB/GYN, University of Michigan"), "academic")
  expect_equal(classify_practice_type("Harvard Medical School, Boston, MA"), "academic")
  expect_equal(classify_practice_type("School of Medicine, Stanford University"), "academic")
})

test_that("private practice classified", {
  expect_equal(classify_practice_type("Women's Health Associates LLC, Dallas, TX"), "private_practice")
  expect_equal(classify_practice_type("Gynecology Group Practice, Chicago"), "private_practice")
})

test_that("community hospital classified when no university signal and not ACGME", {
  expect_equal(classify_practice_type("Small Town Hospital, Nowhere, KS"), "community")
})

test_that("community upgraded to academic when all_aff has university", {
  expect_equal(
    classify_practice_type("Community Regional Medical Center",
                           "Community Regional Medical Center | University of California San Francisco"),
    "academic"
  )
})

test_that("department-only defaults to academic", {
  expect_equal(classify_practice_type("Department of Obstetrics and Gynecology"), "academic")
  expect_equal(classify_practice_type("Division of Minimally Invasive Surgery"), "academic")
})

test_that("NA and short strings return NA", {
  expect_equal(classify_practice_type(NA), NA_character_)
  expect_equal(classify_practice_type(""), NA_character_)
  expect_equal(classify_practice_type("abc"), NA_character_)
})

# ============================================================
# classify_subspecialty
# ============================================================

test_that("MIGS detected", {
  expect_equal(classify_subspecialty("Division of Minimally Invasive Gynecologic Surgery"), "MIGS")
  expect_equal(classify_subspecialty("Department of Gynecologic Surgery and Endoscopy"), "MIGS")
})

test_that("REI detected", {
  expect_equal(classify_subspecialty("Division of Reproductive Endocrinology and Infertility"), "REI")
  expect_equal(classify_subspecialty("IVF Center, Department of OB/GYN"), "REI")
})

test_that("GYN_ONC detected", {
  expect_equal(classify_subspecialty("Division of Gynecologic Oncology"), "GYN_ONC")
})

test_that("FPMRS detected", {
  expect_equal(classify_subspecialty("Female Pelvic Medicine and Reconstructive Surgery"), "FPMRS")
  expect_equal(classify_subspecialty("Section of Urogynecology, Department of OB/GYN"), "FPMRS")
})

test_that("MFM detected", {
  expect_equal(classify_subspecialty("Division of Maternal-Fetal Medicine"), "MFM")
})

test_that("general OB/GYN as fallback", {
  expect_equal(classify_subspecialty("Department of Obstetrics and Gynecology"), "general_OBGYN")
})

test_that("surgery_other for non-gyn surgery", {
  expect_equal(classify_subspecialty("Department of General Surgery"), "surgery_other")
})

test_that("urology detected", {
  expect_equal(classify_subspecialty("Department of Urology"), "urology")
})

test_that("NA for non-clinical", {
  expect_equal(classify_subspecialty("Department of Chemistry"), NA_character_)
  expect_equal(classify_subspecialty(NA), NA_character_)
})

# ============================================================
# classify_career_stage
# ============================================================

test_that("resident detected", {
  expect_equal(classify_career_stage("PGY-3 Resident, Department of OB/GYN"), "resident")
  expect_equal(classify_career_stage("Residency Program in Obstetrics"), "resident")
})

test_that("fellow detected", {
  expect_equal(classify_career_stage("MIGS Fellow, Department of OB/GYN"), "fellow")
  expect_equal(classify_career_stage("Fellowship in Reproductive Endocrinology"), "fellow")
})

test_that("student detected", {
  expect_equal(classify_career_stage("Medical Student, University of Michigan"), "student")
})

test_that("senior faculty detected", {
  expect_equal(classify_career_stage("Professor and Chair, Department of OB/GYN"), "faculty_senior")
  expect_equal(classify_career_stage("Director, Division of MIGS"), "faculty_senior")
})

test_that("junior faculty detected", {
  expect_equal(classify_career_stage("Assistant Professor, Department of OB/GYN"), "faculty_junior")
})

test_that("NA when not determinable", {
  expect_equal(classify_career_stage("Department of OB/GYN, University of X"), NA_character_)
  expect_equal(classify_career_stage(NA), NA_character_)
})

# ============================================================
# is_teaching_hospital
# ============================================================

test_that("known teaching hospitals detected", {
  # These should be in the ACGME list if teaching_hospital_names.txt is loaded
  if (length(TEACHING_HOSPITAL_NAMES) > 0) {
    expect_true(is_teaching_hospital("Brigham and Womens Hospital, Boston"))
    expect_true(is_teaching_hospital("Johns Hopkins Hospital"))
  }
})

test_that("non-hospitals return FALSE", {
  expect_false(is_teaching_hospital(NA))
  expect_false(is_teaching_hospital(""))
  expect_false(is_teaching_hospital("My House"))
})
