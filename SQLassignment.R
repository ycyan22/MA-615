library(tidyverse)
library(RSQLite)
library(DBI)
library(readxl)

Direct <- read_xlsx("~/Desktop/Top MA Donors 2016-2020.xlsx",
                    sheet = "Direct Contributions & JFC Dist")
JFC <- read_xlsx("~/Desktop/Top MA Donors 2016-2020.xlsx",
                 sheet = "JFC Contributions (DO NOT SUM W")

unique(Direct$cycle)
# "cycle" is ok
unique(Direct$contrib)
# We can see some letters after first name

# In order to clean "contrib", we just keep "LastName, FirstName" 
Direct['contrib']=gsub(Direct$contrib, pattern=", ", replacement=",")
Direct['contrib']=gsub(Direct$contrib, pattern="\\s\\w*", replacement="")

# To view result of cleaning
unique(Direct$contrib)

# There are still many problems in "contrib", including typo, symbols and missing name.


# Since the combination of "contribid" and "fam" could stand for a unique donor, 
# I created a new variable to see the total number of donors
Direct$donor <- paste0(Direct$contribid, Direct$fam)
unique(Direct$donor)
length(unique(Direct$donor))

# And then I choose the most time-costing way to clean "contrib"
# Find various editions of names for each donor and rename with LastName, FirstName

# Donor 1 = a00003754861
d1 <- Direct %>% filter(donor =="a00003754861") 
unique(d1$contrib)
# "SLIFKA, RICHARD B"     "SLIFKA, RICHARD"       "SLIFKA, RICHARD BARRY" "SLIFKA, RICHARD MR"
# These 4 names all stand for the same person
# Replace these names with SLIFKA, RICHARD
Direct$contrib[Direct$donor =="a00003754861"] <- "SLIFKA, RICHARD" 


d2 <- Direct %>% filter(donor =="a0000375486A") 
unique(d2$contrib)
Direct$contrib[Direct$donor =="a0000375486A"] <- "SLIFKA, ROSALYN" 


d3 <- Direct %>% filter(donor =="a00010464171")
unique(d3$contrib)
Direct$contrib[Direct$donor =="a00010464171"] <- "RASKY, LAWRENCE"

d4 <- Direct %>% filter(donor =="a0001046417A")
unique(d4$contrib)
Direct$contrib[Direct$donor =="a0001046417A"] <- "RASKY, CAROLYN"

d5 <- Direct %>% filter(donor =="g1100446477@")
unique(d5$contrib)
Direct$contrib[Direct$donor =="g1100446477@"] <- "CITRIN, ANNE"

d6 <- Direct %>% filter(donor =="g11004464771")
unique(d6$contrib)
Direct$contrib[Direct$donor =="g11004464771"] <- "CITRIN, JACOB"

d7 <- Direct %>% filter(donor =="h10011600731")
unique(d3$contrib)
Direct$contrib[Direct$donor =="h10011600731"] <- "RASKY, LAWRENCE"

d8 <- Direct %>% filter(donor =="h1001160073A")
unique(d8$contrib)

d9 <- Direct %>% filter(donor =="h10013349521")
unique(d9$contrib)
Direct$contrib[Direct$donor =="h10013349521"] <- "FEELEY, THOMAS"

d10 <- Direct %>% filter(donor =="h1001334952A")
unique(d10$contrib)
Direct$contrib[Direct$donor =="h1001334952A"] <- "FEELEY, JOAN"

d11 <- Direct %>% filter(donor =="h30012273541")
unique(d11$contrib)
Direct$contrib[Direct$donor =="h30012273541"] <- "SIMMONS, IAN"

d12 <- Direct %>% filter(donor =="h3001227354A")
unique(d12$contrib)
Direct$contrib[Direct$donor =="h3001227354A"] <- "SIMMONS, LIESEL"

d13 <- Direct %>% filter(donor =="U0000000110A")
unique(d13$contrib)
Direct$contrib[Direct$donor =="U0000000110A"] <- "SCHUSTER, ELAINE"

d14 <- Direct %>% filter(donor =="U00000001101")
unique(d14$contrib)

d15 <- Direct %>% filter(donor =="U00000003801")
unique(d15$contrib)
Direct$contrib[Direct$donor =="U00000003801"] <- "SABAN, HAIM"

d16 <- Direct %>% filter(donor =="U0000000380A")
unique(d16$contrib)
Direct$contrib[Direct$donor =="U0000000380A"] <- "SABAN, CHERYL"

d17 <- Direct %>% filter(donor =="U0000003040A")
unique(d17$contrib)
Direct$contrib[Direct$donor =="U0000003040A"] <- "BARZUN, BROOKE"

d18 <- Direct %>% filter(donor =="U00000030401")
unique(d18$contrib)
Direct$contrib[Direct$donor =="U00000030401"] <- "BARZUN, MATTHEW"

d19 <- Direct %>% filter(donor =="U00000030741")
unique(d19$contrib)
Direct$contrib[Direct$donor =="U00000030741"] <- "BEKENSTEIN, JOSHUA"

d20 <- Direct %>% filter(donor =="U0000003074A")
unique(d20$contrib)
Direct$contrib[Direct$donor =="U0000003074A"] <- "BEKENSTEIN, ANITA"

d21 <- Direct %>% filter(donor =="U0000003074B")
unique(d21$contrib)

d22 <- Direct %>% filter(donor =="U00000032511")
unique(d22$contrib)
Direct$contrib[Direct$donor =="U00000032511"] <- "RESNICK, STEWART"

d23 <- Direct %>% filter(donor =="U0000003251A")
unique(d23$contrib)
Direct$contrib[Direct$donor =="U0000003251A"] <- "RESNICK, LYNDA"

d24 <- Direct %>% filter(donor =="U00000032881")
unique(d24$contrib)
Direct$contrib[Direct$donor =="U00000032881"] <- "EGERMAN, PAUL"

d25 <- Direct %>% filter(donor =="U0000003288A")
unique(d25$contrib)
Direct$contrib[Direct$donor =="U0000003288A"] <- "EGERMAN, JOANNE"

d26 <- Direct %>% filter(donor =="U0000003288NA")
unique(d26$contrib)
Direct$contrib[Direct$donor =="U0000003288NA"] <- "EGERMANCOM, JOANNE"

d27 <- Direct %>% filter(donor =="U0000003431A")
unique(d27$contrib)
Direct$contrib[Direct$donor =="U0000003431A"] <- "LAVINE, JEANNIE"

d28 <- Direct %>% filter(donor =="U00000034311")
unique(d28$contrib)
Direct$contrib[Direct$donor =="U00000034311"] <- "LAVINE, JONATHAN"

d29 <- Direct %>% filter(donor =="U0000003740A")
unique(d29$contrib)
Direct$contrib[Direct$donor =="U0000003740A"] <- "HOSTETTER, BARBARA"

d30 <- Direct %>% filter(donor =="U00000037401")
unique(d30$contrib)
Direct$contrib[Direct$donor =="U00000037401"] <- "HOSTETTER, AMOS"

d31 <- Direct %>% filter(donor =="U0000003904A")
unique(d31$contrib)
Direct$contrib[Direct$donor =="U0000003904A"] <- "ABERLY, NAOMI"

d32 <- Direct %>% filter(donor =="U00000039041")
unique(d32$contrib)
Direct$contrib[Direct$donor =="U00000039041"] <- "LEBOWITZ, LAURENCE"

d33 <- Direct %>% filter(donor =="U00000042431")
unique(d33$contrib)
Direct$contrib[Direct$donor =="U00000042431"] <- "KRUPP, GEORGE"

d34 <- Direct %>% filter(donor =="U0000004243A")
unique(d34$contrib)
Direct$contrib[Direct$donor =="U0000004243A"] <- "KRUPP, LIZBETH"

d35 <- Direct %>% filter(donor =="U00000042591")
unique(d35$contrib)
Direct$contrib[Direct$donor =="U00000042591"] <- "FISH, JOHN"

d36 <- Direct %>% filter(donor =="U0000004259A")
unique(d36$contrib)
Direct$contrib[Direct$donor =="U0000004259A"] <- "FISH, CYNTHIA"

d37 <- Direct %>% filter(donor =="Y0000037803L")
unique(d37$contrib)
Direct$contrib[Direct$donor =="Y0000037803L"] <- "ATKINS, CHESTER"

d38 <- Direct %>% filter(donor =="Y0000037803S")
unique(d38$contrib)
Direct$contrib[Direct$donor =="Y0000037803S"] <- "ATKINS, CORINNE"

d39 <- Direct %>% filter(donor =="Y0000040039S")
unique(d39$contrib)
Direct$contrib[Direct$donor =="Y0000040039S"] <- "KANIN, CAROL"

d40 <- Direct %>% filter(donor =="Y0000040039L")
unique(d40$contrib)
Direct$contrib[Direct$donor =="Y0000040039L"] <- "KANIN, DENNIS"

d41 <- Direct %>% filter(donor =="Y0000046458L")
unique(d41$contrib)
Direct$contrib[Direct$donor =="Y0000046458L"] <- "SOLOMONT, ALAN"

d42 <- Direct %>% filter(donor =="Y0000046458S")
unique(d42$contrib)
Direct$contrib[Direct$donor =="Y0000046458S"] <- "SOLOMONT, SUSAN"

d43 <- Direct %>% filter(donor =="Y0000046458B")
unique(d43$contrib)


# Check if "contrib" is totally cleaned
unique(Direct$contrib)

unique(Direct$City)
unique(Direct$State)
unique(Direct$Zip)
# "City", "State", "Zip" all work

sort(unique(Direct$Fecoccemp))
# I give up this column

sort(unique(Direct$orgname))
# Case sentitive problem
# Put "orgname" in upper case
Direct$orgname <- toupper(Direct$orgname)


unique(Direct$ultorg)
unique(Direct$date)
unique(Direct$amount)
unique(Direct$recipid)
unique(Direct$party)
unique(Direct$recipcode)
unique(Direct$type)
unique(Direct$fectransid)
unique(Direct$cmteid)
# These columns are ok


sort(unique(Direct$lastname))
# Case sentitive problem
# Put "lastname" in lower case
Direct$lastname = tolower(Direct$lastname)


# Create an ephemeral in-memory RSQLite database
newdb <- dbConnect(SQLite(), "new-db.sqlite")

# Copy an R data frame into the database
dbWriteTable(newdb, "Direct", Direct)
dbWriteTable(newdb, "JFC", JFC)

# List tables in database
dbListTables(newdb)

# List column names of each table
dbListFields(newdb, "Direct")
dbListFields(newdb, "JFC")


dbDisconnect(newdb)





