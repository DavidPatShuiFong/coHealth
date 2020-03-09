library(openxlsx)
library(pipeR)
library(dplyr)
library(MASS)

xl1_name <- '20190401CSTPhase1Groups.xlsx'
xl2_name <- '20190401CSTPhase2Groups.xlsx'

sheet_names <- getSheetNames(xl1_name)
id1_1_25 <- read.xlsx(xl1_name, sheet = "Phase 1 Group 1 -  25") %>>%
  pull(INTERNALID)
id1_2_25 <- read.xlsx(xl1_name, sheet = "Phase 1 Group 2 -  25") %>>%
  pull(INTERNALID)
id1_2_30 <- read.xlsx(xl1_name, sheet = "Phase 1 Group 2 -  30") %>>%
  pull(INTERNALID)
id1_2_35 <- read.xlsx(xl1_name, sheet = "Phase 1 Group 2 -  35") %>>%
  pull(INTERNALID)

id1b <- c(id1_2_25, id1_2_30, id1_2_35) # 'phase1' group contact by BE
id1 <- c(id1_1_25, id1b) # total phase 1 group

id2_1_25 <- read.xlsx(xl2_name, sheet = "Phase 2 Group 1 -  25") %>>%
  pull(INTERNALID)
id2_1_30 <- read.xlsx(xl2_name, sheet = "Phase 2 Group 1 -  30") %>>%
  pull(INTERNALID)
id2_1_35 <- read.xlsx(xl2_name, sheet = "Phase 2 Group 1 -  35") %>>%
  pull(INTERNALID)
id2_2_25 <- read.xlsx(xl2_name, sheet = "Phase 2 Group 2 -  25") %>>%
  pull(INTERNALID)
id2_2_30 <- read.xlsx(xl2_name, sheet = "Phase 2 Group 2 -  30") %>>%
  pull(INTERNALID)
id2_2_35 <- read.xlsx(xl2_name, sheet = "Phase 2 Group 2 -  35") %>>%
  pull(INTERNALID)

id2 <- c(id2_1_25, id2_2_25, id2_1_30, id2_2_30, id2_1_35, id2_2_35)
# equivalent phase 2 group

id <- c(id1, id2)
df <- data.frame(InternalID = id)

df <- df %>>%
  mutate(Phase = InternalID %in% id1)

noCST1 <- read.csv("TelephoneCST_Phase1_NoCST.csv")
noCST2 <- read.csv("TelephoneCST_Phase2_NoCST.csv")

noCST1_id <- noCST1 %>>% pull(INTERNALID)
noCST2_id <- noCST2 %>>% pull(INTERNALID)
noCST_ID <- c(noCST1_id, noCST2_id)

df <- df %>>%
  mutate(CST = !(InternalID %in% noCST_ID))

seenby_Buddini <- read.csv("20190401CSTBuddini.csv")

seenby_Buddini_id <- seenby_Buddini %>>% pull(INTERNALID)

df <- df %>>%
  mutate(Group2 = InternalID %in% id1b,
         Buddini = InternalID %in% seenby_Buddini_id)

refugee_asylum <- read.csv("20190401CSTRefugeeAsylum.csv")

refugee_asylum_id <- refugee_asylum %>>% pull(INTERNALID)

df <- df %>>%
  mutate(refugee = InternalID %in% refugee_asylum_id)

model1 <- lm(CST ~ Phase + refugee + Buddini + Group2 + refugee * Phase + Buddini * Group2, data = df)
summary(model1)

model2 <- stepAIC(model1)
summary(model2)

model3 <- lm(CST ~ Phase + refugee + refugee * Phase, data = df)
summary(model3)