
library(tidyr)

organized_survey=read.csv("/Users/alanochoa/Desktop/DWQ_Survey_results_OrganizedData.csv",skip = 1, header = TRUE)

# Load necessary libraries
library(tidyverse)
library(stringr)

# Read in your data
survey_data <- organized_survey
names(survey_data)[40]="Q13_Other_Text"
# Assuming the responses are in a column named 'Q13_Other_Text'
# Load necessary libraries
library(tidyverse)
library(stringr)

# Read in your data
survey_data <- read.csv("/path/to/DWQ_Survey_results_OrganizedData.csv")

# Extract responses for Question 8
responses <- survey_data$X8..Are.there.any.lakes..rivers..streams..or.reservoirs.near.your.home.that.you.are.concerned.about.where.conditions.could.be.improved.with.restoration....Yes..list.all.waterbodies.you.re.concerned.about....Text

# Clean the responses
responses <- tolower(responses)
responses <- str_replace_all(responses, "[^a-z\\s,]", "") # Remove punctuation except commas

# Split responses that list multiple water bodies
water_bodies <- unlist(str_split(responses, ",\\s*"))

# Trim whitespaces and remove empty entries
water_bodies <- str_trim(water_bodies)
water_bodies <- water_bodies[water_bodies != ""]

# Count the frequency of each water body
water_body_counts <- table(water_bodies)
water_body_counts <- as.data.frame(water_body_counts)
names(water_body_counts) <- c("WaterBody", "Count")

# Sort by count in descending order for better visualization
water_body_counts <- water_body_counts %>% 
  arrange(desc(Count))

# Visualize the top N water bodies
top_n <- 10 # You can change this number as needed

# Assuming 'data' is your loaded dataset
# Define the columns that are not to be melted (i.e., demographic columns)
id_columns <- c("Basin_Residence",	"Residence.Type",	"Age",	"Education_Level",	"Gender",	"Race",	"Ethnicity")

# Define the columns that are to be pivoted (i.e., the survey questions)
response_columns = setdiff(names(organized_survey), id_columns)

# Use pivot_longer to melt all except the demographic/filter columns
long_data <- organized_survey %>%
  pivot_longer(
    cols = response_columns,
    names_to = "Question",
    values_to = "Response",
    values_transform = list(Response = as.character) # Force all responses to be character type
  )

# Separate numeric and text responses
long_data <- long_data %>%
  mutate(
    Response_Numeric = as.numeric(Response),
    Response_Text = ifelse(is.na(Response_Numeric), Response, NA_character_)
  )

#Add Question Number Column
long_data <- long_data %>%
  mutate(QuestionNumber = sub("^X([0-9]+).*", "Question \\1", Question),
         Question)

#Change Question value names: 
name_mapping <- c(
  "X1..Clean.sources.of.drinking.water" = "Clean sources of drinking water",
  "X1..Water.for.agricultural.uses" = "Water for agricultural uses",
  "X1..Water.that.supports.fish...wildlife.habitat" = "Water that supports fish & wildlife habitat",
  "X1..Waters.safe.for.recreation..e.g..swimming..boating." = "Waters safe for recreation (e.g. swimming, boating)",
  "X10..How.would.you.rate.your.level.of.knowledge.about.water.quality.issues.facing.your.area." = "How would you rate your level of knowledge about water quality issues facing your area?",
  "X11..Select.all.activities.you.have.participated.in.during.the.last.two.years....Other..please.specify....Text" = "Select all activities you have participated in during the last two years:",
  "X11..Select.all.activities.you.have.participated.in.during.the.last.two.years....Selected.Choice" = "Select all activities you have participated in during the last two years:",
  "X12..In.the.last.year..did.you.see..hear..or.read.information.about.water.quality.in.your.area." = "In the last year, did you see, hear, or read information about water quality in your area?",
  "X13..Where.do.you.get.information.about.water.quality.in.Utah...Select.all.that.apply....Other...Text" = "Where do you get information about water quality in Utah?",
  "X13..Where.do.you.get.information.about.water.quality.in.Utah...Select.all.that.apply....Selected.Choice" = "Where do you get information about water quality in Utah?",
  "X2..Agriculture" = "Agriculture",
  "X2..Aquatic.ecosystems...wildlife" = "Aquatic ecosystems & wildlife",
  "X2..Human.health" = "Human health",
  "X2..Recreation" = "Recreation",
  "X3..How.would.you.rate.local.water.quality.of.lakes..rivers..streams..and.reservoirs.in.your.area...i.e..in.your.city.or.county." = "How would you rate local water quality of lakes, rivers, streams, and reservoirs in your area? (i.e. in your city or county)",
  "X4..How.would.you.rate.statewide.water.quality.of.Utah.s.lakes..rivers..streams..and.reservoirs." = "How would you rate statewide water quality of Utah’s lakes, rivers, streams, and reservoirs?",
  "X5..Clean.sources.of.drinking.water" = "Clean sources of drinking water",
  "X5..Water.that.supports.fish...wildlife.habitat" = "Water that supports fish & wildlife habitat",
  "X5..Waters.safe.for.recreation..e.g..swimming..boating." = "Waters safe for recreation (e.g. swimming, boating)",
  "X5.Water.used.for.agriculture" = "Water used for agriculture",
  "X6..Agriculture" = "Agriculture",
  "X6..Drinking.Water" = "Drinking Water",
  "X6..Recreation" = "Recreation",
  "X7...Construction.runoff" = "Construction runoff",
  "X7..Abandoned.mine.drainage" = "Abandoned mine drainage",
  "X7..Cropland.runoff" = "Cropland runoff",
  "X7..Erosion.and.sedimentation" = "Erosion and sedimentation",
  "X7..Feedlot.runoff" = "Feedlot runoff",
  "X7..Industrial.and.chemical.pollution" = "Industrial and chemical pollution",
  "X7..Livestock.access.to.waterbodies" = "Livestock access to waterbodies",
  "X7..Loss.of.natural.areas" = "Loss of natural areas",
  "X7..Natural.disasters..e.g..floods..droughts..and.wildfires." = "Natural disasters (e.g. floods, droughts, and wildfires)",
  "X7..Onsite.septic.systems" = "Onsite septic systems",
  "X7..Pet.waste..human.waste.from.recreation..trash.litter" = "Pet waste, human waste from recreation, trash/litter",
  "X7..Sewage.treatment.plant.discharge" = "Sewage treatment plant discharge",
  "X7..Urban.and.residential.runoff" = "Urban and residential runoff",
  "X8..Are.there.any.lakes..rivers..streams..or.reservoirs.near.your.home.that.you.are.concerned.about.where.conditions.could.be.improved.with.restoration." = "Are there any lakes, rivers, streams, or reservoirs near your home that you are concerned about where conditions could be improved with restoration?",
  "X8..Are.there.any.lakes..rivers..streams..or.reservoirs.near.your.home.that.you.are.concerned.about.where.conditions.could.be.improved.with.restoration....Yes..list.all.waterbodies.you.re.concerned.about....Text" = "Are there any lakes, rivers, streams, or reservoirs near your home that you are concerned about where conditions could be improved with restoration? - Yes (list all waterbodies you’re concerned about) - Text",
  "X8.1..In.the.waterbody.or.waterbodies.that.you.indicated.in.the.previous.question..which.of.the.following.water.quality.issues.are.you.concerned.about...Check.all.that.apply." = "In the waterbody or waterbodies that you indicated in the previous question, which of the following water quality issues are you concerned about? (Check all that apply)",
  "X9..How.often.do.you.visit.rivers..streams..or.lakes.for.recreational.activities.such.as.swimming..fishing..or.kayaking." = "How often do you visit rivers, streams, or lakes for recreational activities such as swimming, fishing, or kayaking?"
)

##Removed Question8.1 and Rename Questions to cleaner format
long_data1 <- long_data %>% 
  mutate(Question=recode(Question,!!!name_mapping),
         Response_Text=ifelse(Response_Text=="",NA,Response_Text),
         delete=ifelse((is.na(Response_Text)&is.na(Response_Numeric)),1,0))%>%
  rename(Residence_Type=Residence.Type)%>%
  filter(!Question== "In the waterbody or waterbodies that you indicated in the previous question, which of the following water quality issues are you concerned about? (Check all that apply)",
         !QuestionNumber=="X",
         delete==0)%>%
  select(-Response,-delete)

long_data1=long_data1[,c("QuestionNumber", "Question" ,"Response_Numeric","Response_Text", "Basin_Residence","Residence_Type",
"Age","Education_Level","Gender", "Race","Ethnicity")]

write.csv(long_data1,"survey_longdata.csv", row.names = FALSE,na="")