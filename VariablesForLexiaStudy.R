# Student Level Data for Lexia research project
# https://docs.google.com/document/d/1KhWGApj-uEPp9rx25JJ6-JauxObq-iNTTPgx4vpdMkw/edit?usp=sharing

# Load necessary libraries ----
library(janitor)
library(odbc)
library(DBI)
library(readxl)
library(tidyverse)

# connect to SQL database ----
con <- dbConnect(odbc(),
                 Driver = "SQL Server",
                 Server = "qdc-soars-tst",
                 trusted_connection = "true",
                 Port = 1433)

sort(unique(odbcListDrivers()[[1]]))

# Student Variables for Lexia Study ----
## Query DIBELS8 ----
qryDibels8 <- odbc::dbGetQuery(con, "
SELECT  
    vDIBELS8BenchmarkStudentList.PersonID
    ,vDIBELS8BenchmarkStudentList.Grade
    ,vDIBELS8BenchmarkStudentList.GradeID
    ,vDIBELS8BenchmarkStudentList.READStatus
    ,vDIBELS8BenchmarkStudentList.CalculatedLanguageProficiency
    ,vDIBELS8BenchmarkStudentList.IEP
    ,vDIBELS8BenchmarkStudentList.RawScore
    ,vDIBELS8BenchmarkStudentList.Percentile
    ,vDIBELS8BenchmarkStudentList.ProficiencyLongDescription
    ,vDIBELS8BenchmarkStudentList.StudentTestDate
    ,vDIBELS8BenchmarkStudentList.EndYear
    ,vDIBELS8BenchmarkStudentList.TestingPeriodName
    ,vDIBELS8BenchmarkStudentList.StudentNumber
FROM   
    dbSoars.dibels8.vDIBELS8BenchmarkStudentList
WHERE  
    TestTestPartLongDescription = 'DIBELS 8 Composite Score'
    AND vDIBELS8BenchmarkStudentList.EndYear = 2026
")

## Query CMAS results ----
qryCmas <- dbGetQuery(con, 
                       "
SELECT 
                      vPARCCStudentList.PersonID
                      ,vPARCCStudentList.StudentNumber
                      ,vPARCCStudentList.[SASID]
                      ,vPARCCStudentList.GradeDescription
                      ,vPARCCStudentList.Grade
                      ,vPARCCStudentList.FRLStatus
                      ,vPARCCStudentList.READStatus
                      ,vPARCCStudentList.CalculatedLanguageProficiency
                      ,vPARCCStudentList.[504Plan] AS X504Plan
                      ,vPARCCStudentList.GT
                      ,vPARCCStudentList.Ethnicity
                      ,vPARCCStudentList.Gender
                      ,vPARCCStudentList.GenderDescription
                      ,vPARCCStudentList.IEP
                      ,vPARCCStudentList.IEPStatus
                      ,vPARCCStudentList.PrimaryDisability
                      ,vPARCCStudentList.ScaleScore
                      ,vPARCCStudentList.ProficiencyLongDescription
                      ,vPARCCStudentList.ProficiencyDescription
                      ,vPARCCStudentList.TestID
                      ,vPARCCStudentList.TestName
                      ,vPARCCStudentList.TestTypeName
                      ,vPARCCStudentList.WindowStartDate
                      ,vPARCCStudentList.WindowEndDate
                      ,vPARCCStudentList.EndYear
                      ,vPARCCStudentList.ContentName
                      ,vPARCCStudentList.ContentGroupName
                      ,vPARCCStudentList.TestingPeriodName
                      ,vPARCCStudentList.TestTestPartID
                      ,vPARCCStudentList.TestTestPartLongDescription
                      ,vPARCCStudentList.TestTestPartShortDescription
                      ,vPARCCStudentList.TestTestPartDescription
                      ,vPARCCStudentList.RangeBottom
                      ,vPARCCStudentList.RangeTop
                      ,vPARCCStudentList.StandardLevelID
                      ,vPARCCStudentList.StandardLevelName
                      ,vPARCCStudentList.OverallFlag
                      ,vPARCCStudentList.CDESchoolNumber
                      ,vPARCCStudentList.School
                      ,vGetPARCCGrowthStudentList.GrowthPercentile
                      ,vGetPARCCGrowthStudentList.ProficiencyLongDescription as ProficiencyLongDescriptionGrowth
                      ,vGetPARCCGrowthStudentList.RangeBottom as RangeBottomGrowth
                      ,vGetPARCCGrowthStudentList.RangeTop as RangeTopGrowth
                      ,vGetPARCCGrowthStudentList.IsIncludedSchoolAccountability
                      ,vGetPARCCGrowthStudentList.IsIncludedDistrictAccountability  
FROM 
    dbSoars.parcc.vPARCCStudentList  WITH (NOLOCK)
    LEFT JOIN dbSoars.parcc.vGetPARCCGrowthStudentList WITH (NOLOCK) ON dbSoars.parcc.vGetPARCCGrowthStudentList.PersonID = dbSoars.parcc.vPARCCStudentList.PersonID AND
       dbSoars.parcc.vGetPARCCGrowthStudentList.TestID = dbSoars.parcc.vPARCCStudentList.TestID
WHERE 
  dbSoars.parcc.vPARCCStudentList.OverallFlag = 1
  AND
    vPARCCStudentList.EndYear = 2026
  AND
    vPARCCStudentList.ContentGroupName = 'READING'
                        ")

## Query MAP----
qryMap <- dbGetQuery(con, "
SELECT
      vMapStudentList.PersonID
      ,vMapStudentList.Grade
      ,vMapStudentList.READStatus
      ,vMapStudentList.CalculatedLanguageProficiency
      ,vMapStudentList.IEP
      ,vMapStudentList.ProficiencyLongDescription
      ,vMapStudentList.RITScore
      ,vMapStudentList.TestID
      ,vMapStudentList.TestName
      ,vMapStudentList.TestTypeName
      ,vMapStudentList.TestStartDate
      ,vMapStudentList.EndYear
      ,vMapStudentList.ContentName
      ,vMapStudentList.TestingPeriodName
      ,vMapStudentList.Percentile AS AchievementPercentile
      ,vMapStudentList.LexileScore
      ,vMapStudentList.TestedAtSchool
      ,vMapStudentList.TestedAtSchoolNumber
      ,StudentDemographic.StudentNumber
FROM dbSoars.map.vMapStudentList WITH(NOLOCK)
  JOIN achievementdw.dim.StudentDemographic WITH(NOLOCK) ON vMapStudentList.PersonID = StudentDemographic.PersonID
  LEFT JOIN dbImport.dbo.tMapImport WITH(NOLOCK) ON tMapImport.PersonID = vMapStudentList.PersonID
       AND tMapImport.TestID = vMapStudentList.TestID
	LEFT JOIN dbSoars.map.vMAPGrowthStudentList (nolock) ON vMapStudentList.PersonID = vMAPGrowthStudentList.PersonID
	  AND vMapStudentList.TestID = vMAPGrowthStudentList.MAPTestID
	  AND vMapStudentList.TestingPeriodID = vMAPGrowthStudentList.TestingPeriodID
	  AND vMapStudentList.ContentID = vMAPGrowthStudentList.ContentID
WHERE 
  vMapStudentList.EndYear >=2026
  AND 
    vMapStudentList.OverallFlag = 1
	AND 
    vMapStudentList.ContentName = 'READING'
	   	")

## Query of READ Plan Flag in Campus ----
qryFlags <- odbc::dbGetQuery(con,
                                "
SELECT
  V_ProgramParticipation.name
 ,V_ProgramParticipation.personID
 ,V_ProgramParticipation.startDate
 ,V_ProgramParticipation.endDate
 ,Enrollment.Grade
 ,Enrollment.CampusSchoolName
 ,Enrollment.EnrollmentStartDate
 ,Enrollment.EnrollmentEndDate
 ,Enrollment.EndYear
 ,Enrollment.CalendarName
 ,Enrollment.EnrollmentType
 ,studentdemographic.frlstatus
 ,studentdemographic.calculatedlanguageproficiency
 ,studentdemographic.gt
 ,studentdemographic.ethnicity
 ,studentdemographic.genderdescription as Gender
 ,studentdemographic.iep
 ,studentdemographic.primarydisability
 ,studentdemographic.programtype AS ELLProgram
 ,studentdemographic.StudentNumber
FROM
  Jeffco_IC.dbo.V_ProgramParticipation (NOLOCK)
  JOIN AchievementDW.dim.Enrollment (NOLOCK) ON
   Enrollment.PersonID = V_ProgramParticipation.PersonID
  LEFT JOIN AchievementDW.dim.StudentDemographic WITH (NOLOCK) ON
    V_ProgramParticipation.PersonID = StudentDemographic.PersonID
WHERE
 name = 'READ'
AND
  StudentDemoGraphic.LatestRecord = 1
AND
  V_ProgramParticipation.active = 1
AND
  Enrollment.DeletedInCampus = 0
AND
  Enrollment.LatestRecord = 1
--AND
 -- Enrollment.EnrollmentType = 'Primary'
--AND
 -- endDate > '2024-01-01 00:00:00'
"
)

## Load Lookup Tables ----
iepLookup <- data.frame(
  stringsAsFactors = FALSE,
  iep = c("No IEP", "Exited IEP", "IEP"),
  iepBin = c(0L, 0L, 1L),
  iepLabels = c("No IEP", "No IEP", "IEP")
)

mlLookup <- data.frame(
  stringsAsFactors = FALSE,
  calculatedlanguageproficiency = c("Not ELL",
                                    "NEP",
                                    "LEP",
                                    "FEP M1",
                                    "FEP M2",
                                    "FEP T3+",
                                    "FELL",
                                    # "PHLOTE",
                                    "Prior to Feb 2013","FEP E1",
                                    "FEP E2"),
  mlBin = c(0L,1L,1L,
            1L,1L,1L,0L,
            # NA,
            0L,1L,
            1L),
  mlLabels = c("Not ML",
               "Multilingual Learner",
               "Multilingual Learner",
               "Multilingual Learner",
               "Multilingual Learner",
               "Multilingual Learner",
               "Not ML",
               "Not ML",
               # "Not ELL",
               "Multilingual Learner",
               "Multilingual Learner")
)


### Transform READ Flags ----
flagStart <- qryFlags %>% 
  mutate(startDate = as_date(startDate), # format as date
         endDate = as_date(endDate),
         PlanStartDate = ymd(startDate), # format as Year, Month, Day
         planEndDate = ymd(endDate), 
         EnrollmentStartDate = ymd(EnrollmentStartDate), 
         EnrollmentEndDate = ymd(EnrollmentEndDate),
         enrollmentInterval = interval(EnrollmentStartDate, EnrollmentEndDate), #create time interval
         planStartInterval = PlanStartDate %within% enrollmentInterval, #determine if plan dates is within time interval
         planEndInterval = planEndDate %within% enrollmentInterval,
         planStart = ifelse(planStartInterval == TRUE, paste0(Grade, "- Start plan"), NA), #report if plan was started
         planEnd = ifelse(planEndInterval == TRUE, paste0(Grade, "- Exit plan"), NA)) %>% #report if plan was ended
  select(StudentNumber, Grade, CampusSchoolName, CalendarName, EnrollmentStartDate, PlanStartDate, planStart, EnrollmentEndDate, 
         planEndDate, planEnd, EndYear, EnrollmentType,  calculatedlanguageproficiency, gt, 
         ethnicity, iep, primarydisability, ELLProgram) %>%
  filter(EnrollmentEndDate < '2026-06-30' | is.na(EnrollmentEndDate)) %>% #exclude enrollments in the 2027 school year
  mutate(gradeInt = case_when(
    Grade == 'K' ~ 0, 
    Grade == 'PK' ~ -1, 
    Grade == 'It' ~ -2, 
    TRUE ~ as.numeric(Grade)
  )) %>% # convert grade grade to numeric value to arrange/sort by
  filter(gradeInt < 9) %>% 
  group_by(StudentNumber) %>%
  arrange(desc(planStart), .by_group = TRUE) %>% 
  fill(planStart, .direction = 'down') %>% 
group_by(StudentNumber) %>%
  arrange(desc(planEnd), .by_group = TRUE) %>%
  fill(planEnd, .direction = "up") %>%
  ungroup() |> 
  filter(EnrollmentType == 'Primary') %>% 
  full_join(mlLookup) %>% 
  full_join(iepLookup) %>% 
  filter(!is.na(StudentNumber)) %>% 
  select(-c(ethnicity, iep, primarydisability, ELLProgram, CalendarName, EnrollmentStartDate, 
            EnrollmentEndDate, EnrollmentType)) |> 
  filter(EndYear == 2026) |> 
  select(studentNumber = StudentNumber, Grade, gradeInt, planStart, planEnd, MLStatus = calculatedlanguageproficiency, 
      mlBin, iepBin) |> 
  distinct(studentNumber, .keep_all = TRUE) |> 
  select(studentNumber, readPlanEnd = planEnd, mlStatus = MLStatus, mlBin, iepBin) |> 
  mutate(readPlanEnd = str_remove(readPlanEnd, "- Exit plan")) |> 
  mutate(readPlanEnd = ifelse(readPlanEnd == "K", 0, readPlanEnd)) |>
  mutate(readPlanEnd = factor(readPlanEnd))

### Transform DIBELS ----
dibels <- qryDibels8 |> 
  clean_names('lower_camel') |> 
  select(studentNumber, testingPeriodName, rawScore, 
          achievementPercentile = percentile, 
          profDescription = proficiencyLongDescription) |> 
  distinct(studentNumber, testingPeriodName, .keep_all = TRUE) |> 
  pivot_wider(id_cols = studentNumber, 
             names_from = testingPeriodName, 
             values_from = rawScore:profDescription, 
             names_prefix = "DIBELS_")

### Transform CMAS ----
cmas <- qryCmas |> 
  clean_names('lower_camel') |> 
  select(studentNumber, 
    scaleScore_CMAS = scaleScore, 
    profLevel_CMAS = proficiencyLongDescription)

### Transform MAP ----
map <- qryMap |> 
  clean_names('lower_camel') |> 
  select(studentNumber, testingPeriodName, ritScore, achievementPercentile, profDescription = proficiencyLongDescription) |> 
  distinct(studentNumber, testingPeriodName, .keep_all = TRUE) |> 
  mutate(testingPeriodName = factor(testingPeriodName, 
    levels = c("Fall", "Winter", "Spring"))) |>
  arrange(studentNumber, testingPeriodName) |>
  pivot_wider(id_cols = studentNumber, 
             names_from = testingPeriodName, 
             values_from = ritScore:profDescription,
             names_prefix = "MAP_")

## Load Lexia Use Data ----
lexiaUse <- read.csv("g:\\Shared drives\\Research & Assessment Design (RAD)\\L1 Projects\\Early Learning\\Lexia\\data\\Core5LexiaYearEndData_Jeffco_Public_Schools_2025-08-18to2026-07-29.csv")

### Join all data sets ----
lexia <- lexiaUse |> 
  clean_names('lower_camel') |> 
  select(studentNumber = username, firstName, mi, lastName, grade) |> 
  left_join(flagStart) |> 
  left_join(cmas) |> 
  left_join(dibels) |> 
  left_join(map) |> 
  mutate(endYear = 2026) |> 
  mutate(endYear = factor(endYear)) |> 
  arrange(readPlanEnd)

# School Variables for Lexia Study ----
#School-level average prior achievement (not just program users), School-level SES, % chronic absenteeism rate

## Load Assessment Data ----
filePath <- "C:/Users/sswitzer/Documents/GitHub/SchoolInsightsPowerBI/School Insights 2.0.01/InsightsDataProcessing/data/"

### DIBELS ----
dibelsSchool <- readRDS(paste0(filePath, "dibels/dibelsDemos.rds")) |>
  filter(subcategory == "all") |> 
  ungroup() |>
  filter(proficiencyDescription %in% c("At Benchmark")) |>
  select(cdeSchoolNumber, testingPeriodName, pctMeetExceed) |>
  distinct(cdeSchoolNumber, testingPeriodName, .keep_all = TRUE) |>
  pivot_wider(id_cols = cdeSchoolNumber,
              names_from = testingPeriodName, 
              values_from = pctMeetExceed, 
              names_prefix = "pctAtOrAbove_DIBELS_")
### CMAS ----
cmasSchool <- readRDS(paste0(filePath, "cmas/statewideAllLevelsComplete.rds")) |>
  filter(category == "all") |> 
  filter(contentName == "LANGUAGE ARTS") |> 
  ungroup() |> 
  distinct(cdeSchoolNumber, pctMeetExceedCategory) |> 
  select(cdeSchoolNumber, pctMeetExceed_CMAS = pctMeetExceedCategory) 

### MAP ----
mapSchools <- readRDS(paste0(filePath, "map/mapDemos.rds")) |>
  filter(subcategory == "all") |> 
  ungroup() |>
  filter(proficiencyDescription == "High Average") |>
  select(cdeSchoolNumber, testingPeriodName, pctMeetExceed) |>
  distinct(cdeSchoolNumber, testingPeriodName, .keep_all = TRUE) |>
  pivot_wider(id_cols = cdeSchoolNumber,
              names_from = testingPeriodName, 
              values_from = pctMeetExceed, 
              names_prefix = "pctHiAvgHi_MAP_")
## Load chronic absenteeism data ----
path <- "G:/Shared drives/Research & Assessment Design (RAD)/L1 Projects/Comparable Districts/compDistricts R/data/attendance/cde/ChronicAbsenteeism/"

#Chronic Absenteeism Data ----
chronAb26 <- readxl::read_excel(file.path(path, "2025-2026_Attendance_SCHOOL_ChronicAbsenteeism_Truancy_AttendanceRates (1).xlsx")) %>% 
  clean_names("lower_camel") |> 
  filter(districtCode == 1420) |> 
  select(cdeSchoolNumber = schoolCode, schoolName, chronicallyAbsentRate) |>
  distinct(cdeSchoolNumber, .keep_all = TRUE)

## Load FRL data ----
frl <- readRDS(paste0(filePath, "masterSchoolData.rds")) |>
  ungroup() |>
  select(cdeSchoolCode, pctFRL = percentFreeAndReduced) |>
  distinct(cdeSchoolCode, .keep_all = TRUE)

### Join all data sets ----
schoolData <- dibelsSchool |> 
  left_join(cmasSchool) |> 
  left_join(mapSchools) |> 
  left_join(chronAb26) |> 
  left_join(frl, by = c("cdeSchoolNumber" = "cdeSchoolCode")) |>
  mutate(endYear = 2026) |> 
  mutate(endYear = factor(endYear)) |>
  select(cdeSchoolNumber, schoolName, everything())

# Save files for Lexia Study ----
write.csv(lexia, "g:/Shared drives/Research & Assessment Design (RAD)/L1 Projects/Early Learning/Lexia/data/lexiaStudentData.csv", row.names = FALSE)
write.csv(schoolData, "g:/Shared drives/Research & Assessment Design (RAD)/L1 Projects/Early Learning/Lexia/data/lexiaSchoolData.csv", row.names = FALSE)

#create a data dictionary for the student and school data sets
dataDictionaryStudent <- data.frame(
  variable = names(lexia),
  description = c(
    "Student's unique identifier",
    "Student's first name",
    "Student's middle initial",
    "Student's last name",
    "Student's grade level (Kindergarten = 0, 1st grade = 1, etc.)",
    "Indicates grade level of student when their READ plan ended",
    "Detailed indicator for detailed multilingual learner status (NEP, LEP, FEP))",
    "Binary indicator for multilingual learner status (1 = ML, 0 = Not ML)",
    "Binary indicator for IEP status (1 = IEP, 0 = Not IEP)",
    "CMAS scale score for reading",
    "CMAS proficiency level for reading",
    "DIBELS Fall raw score",
    "DIBELS Winter raw score",
    "DIBELS Spring raw score",
    "DIBELS Fall achievement percentile",
    "DIBELS Winter achievement percentile",
    "DIBELS Spring achievement percentile",
    "DIBELS Fall proficiency description",
    "DIBELS Winter proficiency description",
    "DIBELS Spring proficiency description",
    "MAP Fall RIT score",
    "MAP Winter RIT score",
    "MAP Spring RIT score",
    "MAP Fall achievement percentile",
    "MAP Winter achievement percentile",
    "MAP Spring achievement percentile",
    "MAP Fall proficiency description",
    "MAP Winter proficiency description",
    "MAP Spring proficiency description",
    "End year of data collection"), 
    varType = map_chr(lexia, ~ class(.x)[1]), 
    examples = map_chr(lexia, ~ paste(head(.x, 3), collapse = ", "))
  )

dataDictionarySchool <- data.frame(
  variable = names(schoolData),
  description = c(
    "School's unique identifier",
    "School's name",
    "DIBELS school-level data (percent of students at or above benchmark) Fall",
    "DIBELS school-level data (percent of students at or above benchmark) Winter",
    "DIBELS school-level data (percent of students at or above benchmark) Spring",
    "CMAS school-level data (percent of students meeting or exceeding expectations for reading)",
    "MAP school-level data (percent of students in Hi or High Average level) Fall",
    "MAP school-level data (percent of students in Hi or High Average level) Winter",
    "MAP school-level data (percent of students in Hi or High Average level) Spring",
    "Chronic absenteeism rate",
    "Free/reduced lunch rate",
    "End year of data collection"
  ),
  varType = map_chr(schoolData, ~ class(.x)[1]), 
  examples = map_chr(schoolData, ~ paste(head(.x, 3), collapse = ", "))
)
# Save data dictionaries
write.csv(dataDictionaryStudent, "g:/Shared drives/Research & Assessment Design (RAD)/L1 Projects/Early Learning/Lexia/data/dataDictionaryStudent.csv", row.names = FALSE)
write.csv(dataDictionarySchool, "g:/Shared drives/Research & Assessment Design (RAD)/L1 Projects/Early Learning/Lexia/data/dataDictionarySchool.csv", row.names = FALSE)
