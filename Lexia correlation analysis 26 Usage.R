# Lexia correlation analysis

# Load necessary libraries ----
library(emmeans)
library(janitor)
library(odbc)
library(DBI)
library(readxl)
library(rstatix)
library(stats)
library(tidyverse)

# connect to SQL database ----
con <- dbConnect(odbc(),
                 Driver = "SQL Server",
                 Server = "qdc-soars-tst",
                 trusted_connection = "true",
                 Port = 1433)

sort(unique(odbcListDrivers()[[1]]))

# Query DIBELS8 ----
# holds data from 2023-2024 +
# some students repeated due to multiple tests for single window due to re-testing and testing in Spanish
qryDibels8 <- odbc::dbGetQuery(con, "

SELECT  studentdemographic.personid AS 'personId'
      ,studentdemographic.studentnumber
     -- ,studentdemographic.frlstatus
      ,studentdemographic.[readstatus]
    --  ,studentdemographic.calculatedlanguageproficiency
   --   ,studentdemographic.gt
  --    ,studentdemographic.ethnicity
  --    ,studentdemographic.Gender
  --    ,studentdemographic.iep
  --    ,studentdemographic.primarydisability
  --    ,studentdemographic.programtype AS ELLProgram
      ,ttest.gradeid AS 'GradeID'
      ,trange.proficiencylongdescription  AS 'ProficiencyLongDescription'
       ,ttesttype.testtypename AS testTypeName
       ,CONVERT(DATE, CONVERT(VARCHAR(10), DIBELS8Benchmark.assessmentdatekey)) AS StudentTestDate
       ,SchoolYear.endyear  AS EndYear
       ,tcontent.contentname
       ,ttestingperiod.testingperiodid AS testingPeriodNumeric
       ,ttestingperiod.testingperiodname AS 'TestingPeriodName'
       ,tcontentgroup.contentgroupname AS 'contentGroupName'
       ,school.cdeschoolnumber AS cdeSchoolNumber
       ,school.school AS schoolName
       ,calendar.calendarname

FROM   achievementdw.fact.DIBELS8Benchmark WITH (nolock)
       JOIN achievementdw.dim.studentdemographic WITH (nolock)
         ON studentdemographic.studentdemographickey =
            DIBELS8Benchmark.studentdemographickey
            AND studentdemographic.isinvalid = 0
       JOIN achievementdw.dim.assessmentingredient WITH (nolock)
         ON assessmentingredient.assessmentingredientkey =
           DIBELS8Benchmark.assessmentingredientkey
        JOIN achievementdw.dim.school WITH (nolock)
         ON school.schoolkey = DIBELS8Benchmark.schoolkey
       JOIN achievementdw.dim.calendar WITH (nolock)
         ON calendar.calendarkey = DIBELS8Benchmark.calendarkey
       JOIN dbsoars.uckie.ttesttestpart WITH (nolock)
         ON ttesttestpart.testtestpartid = assessmentingredient.testtestpartid
       JOIN dbsoars.uckie.ttestpart WITH (nolock)
         ON ttestpart.testpartid = ttesttestpart.testpartid
       JOIN dbsoars.uckie.tstandardlevel WITH (nolock)
         ON tstandardlevel.standardlevelid = ttestpart.standardlevelid
       JOIN dbsoars.uckie.ttest WITH (nolock)
         ON ttest.testid = ttesttestpart.testid
       JOIN dbsoars.uckie.ttestingperiod WITH (nolock)
         ON ttestingperiod.testingperiodid = ttest.testingperiodid
       JOIN dbsoars.uckie.ttesttype WITH (nolock)
         ON ttesttype.testtypename = 'DIBELS 8'
       JOIN dbsoars.uckie.ttestuse WITH (nolock)
         ON ttestuse.testuseid = ttest.testuseid
       JOIN dbsoars.dbo.tschoolyear SchoolYear WITH (nolock)
         ON SchoolYear.schoolyearid = ttest.schoolyearid
       JOIN dbsoars.uckie.tcontent WITH (nolock)
         ON tcontent.contentid = ttestpart.contentid
       JOIN dbsoars.uckie.tcontentgroup WITH (nolock)
         ON tcontentgroup.contentgroupid = tcontent.contentgroupid
       LEFT OUTER JOIN achievementdw.dim.assessmentscoringrange WITH (nolock)
                    ON assessmentscoringrange.assessmentscoringrangekey =
                       DIBELS8Benchmark.assessmentscoringrangekey
      LEFT OUTER JOIN dbsoars.uckie.trange WITH (nolock)
                    ON trange.rangeid = assessmentscoringrange.rangeid
                       AND trange.rangetypeid = 1
      LEFT OUTER JOIN dbsoars.uckie.tproficiency WITH (nolock)
                    ON tproficiency.proficiencyid = trange.proficiencyid
       LEFT OUTER JOIN dbsoars.uckie.tproficiencycolor WITH (nolock)
                    ON tproficiencycolor.proficiencyid =
                       tproficiency.proficiencyid
                       AND tproficiencycolor.colortypenameid = 'DIBELS 8'
       LEFT OUTER JOIN dbsoars.uckie.tcolor WITH (nolock)
                    ON tcolor.colorid = tproficiencycolor.colorid
       LEFT OUTER JOIN dbsoars.isr.tscoretypetesttype WITH (nolock)
                    ON tscoretypetesttype.testtypeid = ttesttype.testtypeid
                       AND tscoretypetesttype.usedonstudentprofile = 1
       --  BOEScoreTypeColumnName IS NOT NULL
       LEFT OUTER JOIN dbsoars.isr.tscoretype WITH (nolock)
                    ON tscoretype.scoretypeid = tscoretypetesttype.scoretypeid
WHERE  standardlevelname = 'Overall'
")

# Query CMAS results ----
qryCmas <- dbGetQuery(con, 
                       "
                  SELECT vPARCCStudentList.PersonID
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
                       
                       FROM dbSoars.parcc.vPARCCStudentList  WITH (NOLOCK)
                       LEFT JOIN dbSoars.parcc.vGetPARCCGrowthStudentList WITH (NOLOCK) ON dbSoars.parcc.vGetPARCCGrowthStudentList.PersonID = dbSoars.parcc.vPARCCStudentList.PersonID AND
                       dbSoars.parcc.vGetPARCCGrowthStudentList.TestID = dbSoars.parcc.vPARCCStudentList.TestID
                       
                       WHERE (dbSoars.parcc.vPARCCStudentList.OverallFlag = 1)
                        
                        
                        ")

#Load Lexia Use Data
## Student Number Used to identify students
  lexiaUse <- read.csv("g:\\Shared drives\\Research & Assessment Design (RAD)\\L1 Projects\\Early Learning\\Lexia\\data\\Core5LexiaYearEndData_Jeffco_Public_Schools_2025-08-18to2026-07-29.csv")

#Prepare datasets for analysis ----
## CMAS ----
cmas <- qryCmas |> 
  clean_names('lower_camel') |> 
  filter(contentName == 'LANGUAGE ARTS') |> 
  filter(endYear == 2026) |> 
  filter(!is.na(scaleScore)) |>
  mutate(studentNumber = as.numeric(studentNumber), 
        scaleScore = as.numeric(scaleScore), 
      grade = as.numeric(grade)) |>
  select(studentNumber, grade, readStatus, scaleScore) |> 
  mutate(studentNumber = as.character(studentNumber)) 

lexia <- lexiaUse |> 
  clean_names('lower_camel') |> 
  filter(!is.na(currentStatus)) |> 
  mutate(currentStatus = as.numeric(currentStatus)) |>
  select(studentNumber = username, everything()) |> 
  filter(lastUse <= '2026-05-29') |> 
  mutate(dosageMet = case_when(
    percentWeeksMetUsage >= 0.50 & weeksOfUse >= 20 ~ "Met", 
    percentWeeksMetUsage < 0.50 & weeksOfUse >= 20 ~ "Did Not Meet", 
     weeksOfUse < 20 ~ "Did Not Use for Sufficient Weeks", 
     TRUE ~ "Unknown")) |>
  mutate(dosageMet = factor(dosageMet,
                     levels = c("Met", "Did Not Meet", "Did Not Use for Sufficient Weeks", "Unknown")))
 # mutate(dosageMet = factor(if_else(percentWeeksMetUsage >= 0.50 & weeksOfUse >= 20, "Met", "Not Met"), 
                       #    levels = c("Met", "Not Met")))

# Find students that are in both datasets
studentsCmasLexia <- intersect(cmas$studentNumber, lexia$studentNumber)

# Correlation between cmas scaleScore and lexia currentStatus
  correlationData <- cmas |>
   filter(studentNumber %in% studentsCmasLexia) |>
   inner_join(lexia, by = c("studentNumber", "grade")) 

correl <- cor.test(correlationData$scaleScore, correlationData$currentStatus, method = "pearson")

correl$estimate
correl$p.value

# find the effect size of scaleScore and currentStatus
effect_size <- correlationData |>
  rstatix::cor_test(scaleScore, currentStatus, method = "pearson") |>
  rstatix::add_significance() 

cohensD <- rstatix::cohens_d(correlationData, scaleScore ~ currentStatus, paired = FALSE)
cohensD

### Plot Correlation ----
ggplot(correlationData, 
  aes(x = scaleScore, y = currentStatus)) +
    geom_jitter(alpha = 0.5) +
  geom_smooth(method = "lm", col = "red", se = TRUE) +
  annotate("text", x = 675, y = 4, label = paste0("r = ", correl$estimate), 
           size = 5, fontface = "bold", color = "blue") +
  labs(title = "Correlation Plot between CMAS Performance and Lexia EOY Status", 
        x = "CMAS Scale Score", 
        y = "Lexia EOY Status") +
  ylim(-6,4) +
  xlim(650, 850) +
  theme_classic()
### Add Lexia dosage as moderator ----
fit_map <- lm(scaleScore ~ currentStatus * dosageMet + readStatus , data = correlationData)

# Display summary results
summary(fit_map)

#calculate estimated marginal means for currentStatus
# View means broken down by both interacting variables
emm_interaction <- emmeans(fit_map, ~ currentStatus * dosageMet + readStatus )
emm_interaction


# Calculates Cohen's d for currentStatus at each level of dosageMet
effOutputs <- eff_size(emm_interaction, sigma = sigma(fit_map), edf = df.residual(fit_map))

df <- as.data.frame(effOutputs)

ggplot(correlationData, aes(x = currentStatus, y = scaleScore, color = dosageMet)) +
  geom_jitter(alpha = 0.3) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Moderating Effect of Lexia Dosage on CMAS Scores",
    x = "Lexia Core5 End Level",
    y = "CMAS Scale Score",
    color = "Dosage Status"
  ) +
  scale_color_manual(values = c("Met" = "blue", "Not Met" = "orange")) +
  facet_wrap(~ readStatus) +
  theme_minimal() +
  theme(legend.position = "top")

## DIBELS ----

dibels <- qryDibels8 |> 
  clean_names('lower_camel') |> 
  filter(testingPeriodName == 'End') |> 
  filter(endYear == 2026) |> 
  mutate(profNumeric = case_when(proficiencyLongDescription == 'Well Below Benchmark' ~ 1,
                                 proficiencyLongDescription == 'Below Benchmark' ~ 2,
                                 proficiencyLongDescription == 'At Benchmark' ~ 3,
                                 proficiencyLongDescription == 'Above Benchmark' ~ 4)) |>
  filter(!is.na(profNumeric)) |>
  mutate(studentNumber = as.numeric(studentnumber), 
        profNumeric = as.numeric(profNumeric), 
      gradeId = as.numeric(gradeId)) |>
  select(studentNumber, grade = gradeId, readStatus = readstatus, profNumeric) |> 
  mutate(studentNumber = as.character(studentNumber))

# Find students that are in both datasets
studentsDibelsLexia <- intersect(dibels$studentNumber, lexia$studentNumber)

# Correlation between dibels scaleScore and lexia currentStatus
library(stats)
  correlationData <- dibels |>
  filter(studentNumber %in% studentsDibelsLexia) |>

  inner_join(lexia, by = c("studentNumber", "grade")) 

correl <- cor.test(correlationData$profNumeric, correlationData$currentStatus, method = "pearson")

correl$estimate
correl$p.value

# find the effect size of profNumeric and currentStatus
effect_size <- correlationData |>
  rstatix::cor_test(profNumeric, currentStatus, method = "pearson") |>
  rstatix::add_significance() 


# Create the scatter plot with the r value annotated
ggplot(correlationData, 
  aes(x = profNumeric, y = currentStatus)) +
    geom_jitter(alpha = 0.5) +
  geom_smooth(method = "lm", col = "red", se = TRUE) +
  annotate("text", x = 1.4, y = 4, label = paste0("r = ", correl$estimate), 
           size = 5, fontface = "bold", color = "blue") +
  labs(title = "Correlation Plot between DIBELS EOY Status and Lexia EOY Status", 
        x = "DIBELS EOY Status (1=Well Below, 2=Below, 3=At, 4=Above)", 
        y = "Lexia EOY Status") +
  ylim(-6,4) +
  xlim(1, 4) +
  theme_classic()


#Lexia Use
lexiaUseSummary  <- lexia |> 
 # filter(studentNumber %in% studentsDibelsLexia) |> 
  group_by(grade) |> 
  mutate(total = n()) |>
  group_by(grade, dosageMet) |>
  summarise(
    n = n(),
    total = first(total),
    percent = round(n / first(total), 2)
  ) |>
  group_by(grade) |> 
  filter(grade <6) |>
  mutate(grade = ifelse(grade == 0, "Grade K", paste0("Grade ", grade))) |> 
  filter(dosageMet == "Met") 

library(gt)
gt(lexiaUseSummary 
) |> 
  cols_label(dosageMet = "Dosage Category", 
n = "Count", total = "Total", percent = "Percent") |>
  fmt_number(columns = c(n, total), decimals = 0) |>
  fmt_percent(percent, decimals = 0) |> 
  cols_align(columns = c(dosageMet),
    align = "left") |> 
  cols_hide(columns = c(dosageMet)) |>
  #make column headers bold
  tab_header(
    title = "Lexia Core5 Usage Summary by Grade",
    subtitle = "Met = Students used for ≥20 weeks and met usage goals for ≥50% of weeks of usage"
  ) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = everything())
  ) |> 
  #make table compact
  tab_options(
    row_group.background.color = "lightgray",
    table.font.size = px(12),
    data_row.padding = px(2)
  )


# Student Characteristics of students that met dosage vs. those that did not meet dosage ----
## CMAS ----
cmasStudents <- qryCmas |> 
  clean_names('lower_camel') |> 
  filter(contentName == 'LANGUAGE ARTS') |> 
  filter(endYear == 2026) |> 
  filter(!is.na(scaleScore)) |>
  mutate(studentNumber = as.numeric(studentNumber), 
        scaleScore = as.numeric(scaleScore), 
      grade = as.numeric(grade)) |> 
    select(studentNumber, grade, scaleScore, school, readStatus, frlStatus, calculatedLanguageProficiency, gt, ethnicity, gender, iepStatus) |> 
  pivot_longer(cols = c(readStatus, frlStatus, calculatedLanguageProficiency, gt, ethnicity, gender, iepStatus), 
              names_to = "group", 
              values_to = "groupValue")


# Find students that are in both datasets
cmasLexiaIntersect <- intersect(cmasStudents$studentNumber, lexia$studentNumber)

## DIBELS ----
qryDibels <- odbc::dbGetQuery(con, "

SELECT  studentdemographic.personid AS 'PersonID'
      ,studentdemographic.studentnumber
      ,studentdemographic.firstname
      ,studentdemographic.lastname
      ,studentdemographic.frlstatus
      ,studentdemographic.[readstatus]
      ,studentdemographic.calculatedlanguageproficiency
      ,studentdemographic.[504plan]
      ,studentdemographic.gt
      ,studentdemographic.ethnicity
      ,studentdemographic.genderdescription as Gender
      ,studentdemographic.iep
      ,studentdemographic.iepstatus
      ,studentdemographic.primarydisability
      ,studentdemographic.programtype
      ,ttest.gradeid AS 'GradeID'
      ,trange.proficiencylongdescription                                AS
        'ProficiencyLongDescription'
		  ,tTestTestPart.TestTestPartLongDescription AS 'ProficiencyOrder'
       ,ttest.testid
       ,ttest.testname
       ,ttesttype.testtypename
       ,CONVERT(DATE, CONVERT(VARCHAR(10), DIBELS8Benchmark.assessmentdatekey)) AS
        StudentTestDate
       ,SchoolYear.endyear                                               AS
        EndYear
       ,tcontent.contentid
       ,tcontent.contentname
       ,ttestingperiod.testingperiodid
       ,ttestingperiod.testingperiodname AS 'TestingPeriodName'
       ,ttesttestpart.testtestpartid AS 'TestingPartID'
       ,ttesttestpart.testtestpartlongdescription
       ,ttesttestpart.testtestpartshortdescription
       ,ttesttestpart.testtestpartdescription
       ,trange.rangebottom
       ,trange.rangetop
       ,ttestpart.standardlevelid
       ,tstandardlevel.standardlevelname
       ,CASE tstandardlevel.standardlevelname
          WHEN 'Overall' THEN 1
          ELSE 0
        END                                                              AS
        OverallFlag
       ,tcontentgroup.contentgroupname
       ,school.school                                                    AS
        schoolName
       ,school.cdeschoolnumber                                           AS
        cdeSchoolNumber
       ,calendar.calendarname
  		 ,SchoolYear.ReportSchoolYear AS 'SchoolYear' 
       ,ttest.windowstartdate
       ,ttest.windowenddate
  
FROM   achievementdw.fact.DIBELS8Benchmark WITH (nolock)
       JOIN achievementdw.dim.studentdemographic WITH (nolock)
         ON studentdemographic.studentdemographickey =
            DIBELS8Benchmark.studentdemographickey
            AND studentdemographic.isinvalid = 0
       JOIN achievementdw.dim.assessmentingredient WITH (nolock)
         ON assessmentingredient.assessmentingredientkey =
           DIBELS8Benchmark.assessmentingredientkey
        JOIN achievementdw.dim.school WITH (nolock)
         ON school.schoolkey = DIBELS8Benchmark.schoolkey
       JOIN achievementdw.dim.calendar WITH (nolock)
         ON calendar.calendarkey = DIBELS8Benchmark.calendarkey
       JOIN dbsoars.uckie.ttesttestpart WITH (nolock)
         ON ttesttestpart.testtestpartid = assessmentingredient.testtestpartid
       JOIN dbsoars.uckie.ttestpart WITH (nolock)
         ON ttestpart.testpartid = ttesttestpart.testpartid
       JOIN dbsoars.uckie.tstandardlevel WITH (nolock)
         ON tstandardlevel.standardlevelid = ttestpart.standardlevelid
       JOIN dbsoars.uckie.ttest WITH (nolock)
         ON ttest.testid = ttesttestpart.testid
       JOIN dbsoars.uckie.ttestingperiod WITH (nolock)
         ON ttestingperiod.testingperiodid = ttest.testingperiodid
       JOIN dbsoars.uckie.ttesttype WITH (nolock)
         ON ttesttype.testtypename = 'DIBELS 8'
       JOIN dbsoars.uckie.ttestuse WITH (nolock)
         ON ttestuse.testuseid = ttest.testuseid
       JOIN dbsoars.dbo.tschoolyear SchoolYear WITH (nolock)
         ON SchoolYear.schoolyearid = ttest.schoolyearid
       JOIN dbsoars.uckie.tcontent WITH (nolock)
         ON tcontent.contentid = ttestpart.contentid
       JOIN dbsoars.uckie.tcontentgroup WITH (nolock)
         ON tcontentgroup.contentgroupid = tcontent.contentgroupid
       LEFT OUTER JOIN achievementdw.dim.assessmentscoringrange WITH (nolock)
                    ON assessmentscoringrange.assessmentscoringrangekey =
                       DIBELS8Benchmark.assessmentscoringrangekey
      LEFT OUTER JOIN dbsoars.uckie.trange WITH (nolock)
                    ON trange.rangeid = assessmentscoringrange.rangeid
                       AND trange.rangetypeid = 1
      LEFT OUTER JOIN dbsoars.uckie.tproficiency WITH (nolock)
                    ON tproficiency.proficiencyid = trange.proficiencyid
       LEFT OUTER JOIN dbsoars.uckie.tproficiencycolor WITH (nolock)
                    ON tproficiencycolor.proficiencyid =
                       tproficiency.proficiencyid
                       AND tproficiencycolor.colortypenameid = 'DIBELS 8'
       LEFT OUTER JOIN dbsoars.uckie.tcolor WITH (nolock)
                    ON tcolor.colorid = tproficiencycolor.colorid
       LEFT OUTER JOIN dbsoars.isr.tscoretypetesttype WITH (nolock)
                    ON tscoretypetesttype.testtypeid = ttesttype.testtypeid
                       AND tscoretypetesttype.usedonstudentprofile = 1
       --  BOEScoreTypeColumnName IS NOT NULL
       LEFT OUTER JOIN dbsoars.isr.tscoretype WITH (nolock)
                    ON tscoretype.scoretypeid = tscoretypetesttype.scoretypeid
WHERE  standardlevelname = 'Overall'
       AND SchoolYear.endyear = 2026 

")

## Load lookup tables for student characteristics ----
#FRL Lookup table
frlLookup <- data.frame(
  stringsAsFactors = FALSE,
         frlstatus = c("No FRL", "Reduced Lunch", "Free Lunch"),
           frlBin = c(0L, 1L, 1L),
                         labels = c("Not Free or Reduced Lunch",
                                    "Free or Reduced Lunch Eligible",
                                    "Free or Reduced Lunch Eligible")
             )

gtLookup <- data.frame(
  stringsAsFactors = FALSE,
                gt = c("GT", "Not GT"),
            gtBin = c(1L, 0L),
            labels = c("GT", "Not GT")
            )

iepLookup <- data.frame(
  stringsAsFactors = FALSE,
         iepstatus = c("No IEP", "Exited IEP", "IEP"),
           iepBin = c(0L, 0L, 1L),
            labels = c("No IEP", "No IEP", "IEP")
             )
  
ellLookup <- data.frame(
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
                                     ellBin = c(0L,1L,1L,
                                                 1L,1L,1L,0L,
                                                 # NA,
                                                 0L,1L,
                                                 1L),
                                      labels = c("Not ELL",
                                                 "English Language Learner",
                                                 "English Language Learner",
                                                 "English Language Learner",
                                                 "English Language Learner",
                                                 "English Language Learner",
                                                 "Not ELL",
                                                 "Not ELL",
                                                 # "Not ELL",
                                                 "English Language Learner",
                                                 "English Language Learner")
             )
  
raceLookup <-data.frame(
  stringsAsFactors = FALSE,
         ethnicity = c("White","Hispanic","Multi",
                       "Asian","Black","Am. Indian","Pacific Islander"),
          raceBin = c(0L, 1L, 1L, 1L, 1L, 1L, 1L),
            labels = c("Not Ethnic/Racial Minority",
                       "Ethnic/Racial Minority","Ethnic/Racial Minority",
                       "Ethnic/Racial Minority","Ethnic/Racial Minority",
                       "Ethnic/Racial Minority","Ethnic/Racial Minority")
)

dibelsStudents <- qryDibels |> 
  clean_names('lower_camel') |> 
  filter(testingPeriodName == 'End') |> 
  filter(endYear == 2026) |>
  mutate(profNumeric = case_when(proficiencyLongDescription == 'Well Below Benchmark' ~ 1,
                                 proficiencyLongDescription == 'Below Benchmark' ~ 2,
                                 proficiencyLongDescription == 'At Benchmark' ~ 3,
                                 proficiencyLongDescription == 'Above Benchmark' ~ 4)) |>
  filter(!is.na(profNumeric)) |>
  mutate(studentNumber = as.numeric(studentnumber), 
        profNumeric = as.numeric(profNumeric), 
      gradeId = as.numeric(gradeId)) |> 
   select(studentNumber, grade = gradeId, profNumeric, school = schoolName, readStatus = readstatus, 
    frlStatus = frlstatus, calculatedlanguageproficiency = calculatedlanguageproficiency, gt, ethnicity, gender, iepstatus = iepstatus) |> 

    mutate(frlBin = case_when(
    frlStatus == 'Free Lunch' | frlStatus == 'Reduced Lunch' ~ 'Free or Reduced Lunch Eligible', 
    TRUE ~ 'Not Free or Reduced Lunch Eligible'
  )) %>% 
  mutate(iepBin = case_when(
    iepstatus == 'IEP' ~ 'Individualized Education Program', 
    TRUE ~ 'No Individualized Education Program'
  )) %>% 
  mutate(gtBin = case_when(
    gt == 'GT' ~ 'Gifted and Talented Program', 
    TRUE ~ 'No Gifted and Talented Program'
  )) %>% 
  mutate(raceBin = case_when(
    ethnicity == 'White' ~ 'White', 
    TRUE ~ 'Students of Color of Hispanic'
  )) %>% 
   mutate(ellBin = case_when(
    calculatedlanguageproficiency == 'Not ELL'| calculatedlanguageproficiency == 'FELL' | calculatedlanguageproficiency == 'Prior to Feb 2013' ~ 'Not ML', 
    TRUE ~ 'ML'
  )) %>% 
  mutate(all = 'all') |> 
  select(studentNumber, grade, profNumeric, school, readStatus, frlBin, iepBin, gtBin, raceBin, ellBin, all) |> 
  pivot_longer(cols = c(readStatus, frlBin, iepBin, gtBin, raceBin, ellBin, all), 
              names_to = "group", 
              values_to = "groupValue") |> 
  mutate(studentNumber = as.character(studentNumber)) |> 
  distinct(studentNumber, grade, group, .keep_all = TRUE)


dibelsLexiaIntersect <- intersect(dibelsStudents$studentNumber, lexia$studentNumber)

#Lexia Use
lexiaUseSummary  <- lexia |> 
  full_join(dibelsStudents, by = c("studentNumber", "grade")) |>
  filter(studentNumber %in% dibelsLexiaIntersect) |> 
  group_by(grade, group, groupValue) |>
  mutate(total = n()) |>
  group_by(grade, dosageMet, group, groupValue) |>
  summarise(
    n = n(),
    total = first(total),
    percent = round(n / first(total), 2)
  ) |>
  group_by(grade, group) |> 
  filter(grade <6) |>
  mutate(grade = ifelse(grade == 0, "Grade K", paste0("Grade ", grade))) |> 
  filter(dosageMet == "Met") 

library(gt)
gt(lexiaUseSummary 
) |> 
  fmt_number(columns = c(n, total), decimals = 0) |>
  fmt_percent(percent, decimals = 0) |> 
  cols_align(columns = c(dosageMet),
    align = "left") |> 
  #make column headers bold
  tab_header(
    title = "Lexia Core5 Usage Summary by Grade",
    subtitle = "Met = Students used for ≥20 weeks and met usage goals for ≥50% of weeks of usage"
  ) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = everything())
  ) |> 
  #make table compact
  tab_options(
    table.font.size = px(12),
    data_row.padding = px(2)
  )

# Create a plot to visualize the distribution of Lexia dosageMet across different student characteristics for lexiaUseSummary

lexiaFiltered <- lexiaUseSummary %>%
  filter(dosageMet == 'Met', 
grade == 'Grade 0') |> 
  mutate(color = ifelse(grepl("Not|No|Unknown|Exited|White", groupValue), "grey", "purple"))

ggplot(lexiaFiltered,
     aes(y = groupValue, 
      x = percent, 
      fill = color)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("grey" = "grey", "purple" = "purple")) +
  facet_wrap(~ group, scales = "free") +
  labs(title = "Distribution of Lexia Dosage Met Across Student Characteristics",
       x = "Student Characteristic",
       y = "Percentage of Students",
       fill = " ") +
  xlim(0, 1)+
  theme_minimal() +
  theme(axis.text.x = element_blank(), 
        legend.position = "none")


#Lexia Use by group
lexiaUseSummary  <- lexia |> 
  full_join(dibelsStudents, by = c("studentNumber", "grade")) |>
  filter(studentNumber %in% dibelsLexiaIntersect) |> 
    filter(grade <6) |>
  group_by(group, groupValue) |>
  mutate(total = n()) |>
  group_by(dosageMet, group, groupValue) |>
  summarise(
    n = n(),
    total = first(total),
    percent = round(n / first(total), 2)
  ) |>
  group_by( group) |> 
  #mutate(grade = paste0("Grade ", grade))
  filter(dosageMet == "Met") |> 
  mutate(group = str_replace(group, "Bin", "Group")) |> 
    mutate(color = ifelse(grepl("Not|No|Unknown|Exited|White", groupValue), "grey", "purple")) |> 
  mutate(group = case_when(
    group == "readStatus" ~ "READ Plan Status",
    group == "frlGroup" ~ "Free or Reduced Lunch Status",
    group == "iepGroup" ~ "IEP Status",
    group == "gtGroup" ~ "Gifted and Talented Status",
    group == "raceGroup" ~ "Race/Ethnicity",
    group == "ellGroup" ~ "Multilingual Learner Status",
    group == "all" ~ "All Students"
  ))

library(gt)
gt(lexiaUseSummary) |> 
  cols_label(dosageMet = "Dosage Category", 
groupValue = "Student Group",
n = "Count of students who met dosage criteria", total = "Total students", percent = "Percent of students who met dosage criteria") |>
  cols_hide(columns = c(dosageMet,color)) |>
  fmt_number(columns = c(n, total), decimals = 0) |>
  fmt_percent(percent, decimals = 0) |> 
  cols_align(columns = c(groupValue),
    align = "left") |> 
  cols_align(columns = c(n, total, percent),
    align = "center") |>
  #set width of groupValue column to 200px and n, total, percent columns to 100px
  cols_width(c("groupValue") ~ px(200)) |>
  cols_width(c("n", "total", "percent") ~ px(100)) |>
  tab_header(
    title = "Lexia Core5 Usage Summary by Group",
    subtitle = "Met = Students used for ≥20 weeks and met usage goals for ≥50% of weeks of usage"
  ) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = everything())
  ) |> 
  tab_options(
    row_group.background.color = "lightgray",
    table.font.size = px(12),
    data_row.padding = px(2)
  )

ggplot(lexiaUseSummary,
     aes(y = groupValue, 
      x = percent, 
      fill = color)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = scales::percent(percent, accuracy = 1)), 
            position = position_dodge(width = 0.9), 
            hjust = -0.1, size = 5) +
  scale_fill_manual(values = c("grey" = "#6c7070", "purple" = "#673785")) +
  facet_wrap(~ group, 
ncol = 1,
    scales = "free") +
  labs(title = "Distribution of Lexia Dosage Met Across Student Characteristics",
       x = " ",
       y = " ",
       fill = " ") +
  xlim(0, 1)+
  theme_minimal() +
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_text(size = 14, face = "bold"),
        legend.position = "none", 
        panel.grid = element_blank(),
      strip.background = element_rect(fill = "lightgrey",
                                     color = "white"),
      strip.placement = "outside",
      strip.text = element_text(size = 12, 
                                face = "bold", 
                              hjust = 0))

#Use by School
#Lexia Use by group
lexiaUseSchoolSummary  <- lexia |> 
  full_join(dibelsStudents, by = c("studentNumber", "grade")) |>
  filter(studentNumber %in% dibelsLexiaIntersect) |> 
    filter(grade <6) |>
  group_by(schoolName, group, groupValue) |>
  mutate(total = n()) |>
  group_by(schoolName, dosageMet, group, groupValue) |>
  summarise(
    n = n(),
    total = first(total),
    percent = round(n / first(total), 2)
  ) |>
  group_by( group) |> 
  #mutate(grade = paste0("Grade ", grade))
  filter(dosageMet == "Met") |> 
  filter(group == 'all') |> 
  mutate(group = str_replace(group, "Bin", "Group")) |> 
    mutate(color = ifelse(grepl("Not|No|Unknown|Exited|White", groupValue), "grey", "purple"))


dualSchools <- data.frame(schoolName = c("Edgewater Elementary",  "Eiber Elementary" , "Foster Dual Language PK-8 School", 
                                "Lasley Elementary", "Lumberg Elementary"), 
                 dualSchool = rep(1, 5))

titleSchool <- data.frame(schoolName = c("Bear Creek K-8", "Deane Elementary", "Edgewater Elementary" ,

 "Eiber Elementary", "Foothills Elementary", "Foster Dual Language PK-8 School",

"Lasley Elementary", "Lawrence Elementary", "Little Elementary",

"Lumberg Elementary",  "Patterson Elementary", "Rose Stein International Elementary",

"Secrest Elementary", "Slater Elementary",  "South Lakewood Elementary" ,  "Stevens Elementary",

"Swanson Elementary",  "Welchester Elementary", "Westgate Elementary"),

titleSchool = rep(1, 19))

lexiaSchoolSummary <- lexiaUseSchoolSummary |> 
  left_join(dualSchools, by = "schoolName") |> 
  left_join(titleSchool, by = "schoolName") 


highUse <- lexiaSchoolSummary |> 
  filter(percent >= 0.5) |> 
  select(schoolName, percent, n, total, dualSchool, titleSchool) |> 
  arrange(desc(percent)) |> 
ungroup() |> 
  select(-group) |> 
  mutate(schoolName = str_remove(schoolName, 'Elementary'))

gt(highUse) |> 
  cols_label(schoolName = "School Name", 
            percent = "Percent ", n = "Count",
            total = "Total Students", dualSchool = "Dual Language School", 
            titleSchool = "Title I School") |>
  fmt_number(columns = c(n, total), decimals = 0) |>
  fmt_percent(percent, decimals = 0) |> 
  cols_align(columns = c(schoolName),
    align = "left") |> 
  cols_align(columns = c(n, total, percent),
    align = "center") |>
    sub_missing(
    columns = everything(), # Targets all columns
    missing_text = "-"       # Replaces NA with a blank space
  ) |> 
    fmt_tf(
    columns = titleSchool,
    tf_style = "check-mark"
  )
  tab_header(
    title = "Schools with High Lexia Core5 Usage",
    subtitle = "High Usage: ≥50% of students met dosage criteria"
  ) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) |> 
opt_vertical_padding(scale = 0.25) |> 
  tab_options(
    row_group.background.color = "lightgray",
    table.font.size = px(10),
    data_row.padding = px(0)
  )

lowUse <- lexiaSchoolSummary |> 
  filter(percent < 0.4) |> 
  select(schoolName, percent, n, total, dualSchool, titleSchool) |> 
  arrange(desc(percent))
