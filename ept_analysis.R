ept_analysis <- function(){



#Below are server/DB names and filepaths that may be variable. Change as-needed.

#SQL Server Driver:
sql_driver <- "{SQL Server}"

#SQL server name for Fusion:
fusion_server <- '[Fusion server name]'

#SQL DB name for Fusion:
fusion_db <- '[Fusion DB name]'

#Filepath for downloaded Eplet registry (see Load_Eplets.R):
eplets_reg_path <- '[eplets_path defined in Load_Eplets script]/eplets.xlsx'




library(odbc)
library(stringr)
library(tibble)
library(dplyr)
library(tidyr)
library(gt)
library(openxlsx)

file_path <- eplets_reg_path 
wb <- loadWorkbook(file_path) 
epts <- as_tibble(readWorkbook(wb))

recipFusionCon <- dbConnect(odbc::odbc(), Driver   = sql_driver, Server   = fusion_server, Database = fusion_db, Port     = 1433)
PID <- readline(prompt = "Enter Patient Patient ID:")
Sample_Dt <- readline(prompt = "Sample Date to Analyze (YYYY-MM-DD):")
CI_CII <- readline(prompt = "Analyze Class I or II? (Enter 'I' or 'II'):")
cutoff <- readline(prompt = "Enter Desired Negative Cutoff (MFI) for Eplet Analysis:")

ifelse(CI_CII == 'I', (cat_id <- 'LS1A04'), ifelse(CI_CII == 'II', (cat_id <- 'LS2A01'), return(paste("Invalid entry for Class I/Class II:", CI_CII))))

Sample_Dt <- as.Date(Sample_Dt, format = "%Y-%m-%d")
cutoff <- as.numeric(cutoff)

query <- paste0("SELECT p.FirstName, p.LastName, s.SampleIDName, s.ShipmentDT, pd.BeadID, wd.NormalValue, CAST(pd.Specificity AS VARCHAR(50)) AS MolSpec, CAST(pd.SpecAbbr AS VARCHAR(50)) AS Spec FROM PATIENT p INNER JOIN SAMPLE s on p.PatientID = s.PatientID INNER JOIN WELL w on s.SampleID = w.SampleID INNER JOIN TRAY t on w.TrayID = t.TrayID INNER JOIN PRODUCT_DETAIL pd on t.CatalogID = pd.CatalogID INNER JOIN WELL_DETAIL wd on pd.BeadID = wd.BeadID and w.WellID = wd.WellID where p.PatientID =", "'", PID, "'", " AND t.CatalogID LIKE ", "'%", cat_id, "%'", " AND pd.BeadID NOT IN ('001','002') ORDER BY BeadID")
SAB_data <- dbGetQuery(recipFusionCon, query)
SAB_data <- as_tibble(SAB_data)
(SAB_data <- SAB_data
%>% mutate(Spec = gsub("-",'',gsub(",",'',Spec))))
(SAB_data <- SAB_data
%>% mutate(MolSpec= gsub("-",'',gsub(",",'',MolSpec))))

(SAB_data <- SAB_data
%>% mutate(SampleDate_Ship = as.Date(ShipmentDT, format = "%y-%m-%d"))
)

(SAB_data <- SAB_data
%>% mutate(SampleDate_Name = as.Date(format(
as.Date(str_extract(SampleIDName, " \\d{1,2}-\\d{1,2}-\\d{2}"), format = "%m-%d-%Y"), "20%y-%m-%d"))
))
(SAB_data <- SAB_data
%>% mutate(SampleDate = coalesce(SampleDate_Ship, SampleDate_Name))
)

(SAB_data <- SAB_data
%>% filter(SampleDate == as.Date(Sample_Dt))
)
(SAB_data <- SAB_data
%>% mutate(MolSpec = gsub("(?<=\\d)(?=[A-Za-z])", ",", MolSpec, perl = TRUE))
)
(SAB_data <- SAB_data
%>% separate_rows(MolSpec, sep = ",")
)

(SAB_data_neg <- SAB_data
 %>% filter(NormalValue < cutoff)
)

(SAB_data_pos <- SAB_data
 %>% filter(NormalValue > cutoff)
)
epts <- epts %>% mutate("MolSpec" = alleles)

SAB_ept_neg <- inner_join(SAB_data_neg, epts, by = "MolSpec", relationship = "many-to-many")

(SAB_ept_neg <- SAB_ept_neg 
%>% select("NormalValue","MolSpec","name","exposition")
)

SAB_ept_score <- inner_join(SAB_data_pos, epts, by = "MolSpec", relationship = "many-to-many")

(SAB_ept_score <- SAB_ept_score 
%>% select("NormalValue","MolSpec","name","exposition")
)

(SAB_ept_score <- SAB_ept_score 
%>% anti_join(SAB_ept_neg, by = "name")
)


ept_score_mean <- aggregate(SAB_ept_score$NormalValue, by=list(name=SAB_ept_score$name), FUN=mean)

eplets <- inner_join(ept_score_mean, epts, by = "name")

(eplets <- eplets
%>% mutate(Mean_Eplet_MFI = x)
)


(eplets <- eplets
%>% select("alleles","Mean_Eplet_MFI","exposition","name")
)

eplets <- eplets %>% arrange(desc(Mean_Eplet_MFI))

return(
(eplets |>
gt(
rowname_col = NULL,
groupname_col = "group"
)
%>% 
  tab_header(
    title = gt::html(paste("Antibody eplet analysis for ", PID, " based on reactivity on ", Sample_Dt, " sample. Used negative cutoff of ", cutoff, " MFI"))
  )
))


}


