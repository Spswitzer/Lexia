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

# # MAP Results
# qryMAP <- dbGetQuery(con, "
# SELECT 
# vMapStudentList.PersonID
#  ,studentdemographic.studentnumber
#  ,studentdemographic.firstname
#  ,studentdemographic.lastname
#  ,vMapStudentList.Grade
#  ,vMapStudentList.ProficiencyLongDescription
#  ,vMapStudentList.RITScore
#  ,vMapStudentList.EndYear
#  ,vMapStudentList.ContentName
#  ,vMapStudentList.TestingPeriodName
#  ,vMapStudentList.ContentGroupName
#  ,vMapStudentList.TestedAtSchool
#  ,vMapStudentList.TestedAtSchoolNumber
#   ,vMAPGrowthStudentList.GrowthPercentile
#  ,vMAPGrowthStudentList.ProficiencyLongDescription AS 'GrowthProficiencyDesc'
#  ,vMAPGrowthStudentList.TestTestPartDescription AS 'growthPeriod'
#   FROM dbSoars.map.vMapStudentList (nolock)
#   JOIN achievementdw.dim.studentdemographic WITH (nolock)
#          ON studentdemographic.PersonID =
#             vMapStudentList.PersonID
#   LEFT JOIN dbSoars.map.vMAPGrowthStudentList (nolock) 
#         ON vMapStudentList.PersonID = 
#             vMAPGrowthStudentList.PersonID
#   AND vMapStudentList.TestID = vMAPGrowthStudentList.MAPTestID
# 	AND vMapStudentList.TestingPeriodID = vMAPGrowthStudentList.TestingPeriodID
# 	AND vMapStudentList.ContentID = vMAPGrowthStudentList.ContentID
# 	AND vMapStudentList.ContentID IN (29, 11)
# 	AND vMapStudentList.OverallFlag = 1
#   AND vMapStudentList.TestingPeriodName = 'Spring'
# 	")

#Load Lexia Use Data
## Student Number Used to identify students
  lexiaUse <- read_excel("g:\\Shared drives\\Research & Assessment Design (RAD)\\L1 Projects\\TSST\\Platform Use for Comm Sup\\data\\Lexia\\lexiaUse_Jeffco.xlsx")

#Prepare datasets for analysis ----
## CMAS ----
cmas <- qryCmas |> 
  clean_names('lower_camel') |> 
  filter(contentName == 'LANGUAGE ARTS') |> 
  filter(endYear == 2024) |> 
  filter(!is.na(scaleScore)) |>
  mutate(studentNumber = as.numeric(studentNumber), 
        scaleScore = as.numeric(scaleScore), 
      grade = as.numeric(grade)) |>
  select(studentNumber, grade, readStatus, scaleScore)

lexia <- lexiaUse |> 
  clean_names('lower_camel') |> 
  filter(!is.na(currentStatus)) |> 
  mutate(currentStatus = as.numeric(currentStatus)) |>
  select(studentNumber = username, everything()) |> 
  mutate(dosageMet = factor(if_else(percentWeeksMetUsage >= 0.50 & weeksOfUse >= 20, "Met", "Not Met"), 
                            levels = c("Met", "Not Met")))

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
  filter(endYear == 2024) |> 
  mutate(profNumeric = case_when(proficiencyLongDescription == 'Well Below Benchmark' ~ 1,
                                 proficiencyLongDescription == 'Below Benchmark' ~ 2,
                                 proficiencyLongDescription == 'At Benchmark' ~ 3,
                                 proficiencyLongDescription == 'Above Benchmark' ~ 4)) |>
  filter(!is.na(profNumeric)) |>
  mutate(studentNumber = as.numeric(studentnumber), 
        profNumeric = as.numeric(profNumeric), 
      gradeId = as.numeric(gradeId)) |>
  select(studentNumber, grade = gradeId, readStatus = readstatus, profNumeric)

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
  filter(studentNumber %in% studentsCmasLexia) |>
  group_by(grade) |> 
  mutate(total = n()) |>
  group_by(grade, dosageMet) |>
  summarise(
    n = n(),
    total = first(total),
    percent = n / first(total)
  ) |>
  ungroup() |> 
  filter(dosageMet == "Met") 
