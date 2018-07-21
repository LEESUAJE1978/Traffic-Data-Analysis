require(data.table);require(tidyverse)
RESP<-fread('20180704-06.csv', stringsAsFactors =FALSE)
str(RESP)

##common column: ISSUE_DATE, OBU_ID, RSU_ID