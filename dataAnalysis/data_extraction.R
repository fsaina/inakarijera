##SCRIPT FOR CONVERTING DATA FROM SURVEY CSV TO R DATA STRUCTURES

#LOCAL path for csv files
pathCsv <- "/mnt/8C1A960B1A95F286/Bartol_work/INA_Career_LOGO/resources"

#Types of attributes in columns
columnTypes <- c("character","character","character","integer","integer","integer","integer",
                 "integer", "character", "character", "character", "character", "character", "character"
                 , "character", "character", "character", "character","character","character")

#Column names
columnNames <- c("ID.Hash","Sex","Age","Student.Status","Employer.Opinion","National.Identity",
                 "Employee.Satisfaction","Future.Employee","Work.Field","Industry","Creativity","Ecology",
                 "Science","Energetics","Croatia","Association.Career", "E.Mail", "Start.Date","End.Date",
                 "Network.ID")

#Load data
rawData <- read.csv(paste(pathCsv,"/sample_survey.csv", sep = ''),header = TRUE, na.strings = "",
                    colClasses = columnTypes, comment.char = "")
#Set column names
colnames(rawData) <- columnNames

