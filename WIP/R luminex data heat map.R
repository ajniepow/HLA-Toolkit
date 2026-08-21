library(odbc)
library(immunogenetr)
library(stringr)
library(tibble)
library(dplyr)
library(ggplot2)
library(tidyr)
library(gt)
library(corrplot)
library(pheatmap)
recipFusionCon <- dbConnect(odbc::odbc(), Driver   = "{SQL Server}", Server   = "[Fusion server name]", Database = "[Fusion DB name]", Port     = 1433)
cat_id <- 'LS1A04'
locus <- 'A'


query <- paste0("SELECT p.FirstName, p.LastName, p.PatientID, s.SampleIDName, s.ShipmentDT, pd.BeadID, wd.NormalValue, CAST(pd.Specificity AS VARCHAR(50)) AS MolSpec, CAST(pd.SpecAbbr AS VARCHAR(50)) AS Spec FROM PATIENT p INNER JOIN SAMPLE s on p.PatientID = s.PatientID INNER JOIN WELL w on s.SampleID = w.SampleID INNER JOIN TRAY t on w.TrayID = t.TrayID INNER JOIN PRODUCT_DETAIL pd on t.CatalogID = pd.CatalogID INNER JOIN WELL_DETAIL wd on pd.BeadID = wd.BeadID and w.WellID = wd.WellID where pd.BeadID NOT IN ('001','002') AND t.CatalogID LIKE ", "'%", cat_id, "%'", " AND t.CatalogID NOT LIKE '%EX%' ORDER BY BeadID")
SAB_data <- dbGetQuery(recipFusionCon, query)


SAB_data <- as_tibble(SAB_data)
(SAB_data <- SAB_data
%>% mutate(Spec = gsub("-",'',gsub(",",'',Spec))))
(SAB_data <- SAB_data
%>% mutate(MolSpec= gsub("-",'',gsub(",",'',MolSpec))))

(SAB_data <- SAB_data
%>% mutate(Spec =  gsub("Bw6",'',gsub("Bw4",'',Spec))))

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
%>% select("MolSpec", "NormalValue", "SampleIDName", "PatientID", "SampleDate")
)

(SAB_data <- SAB_data
%>% filter(str_detect(MolSpec, paste0("^",locus)))
)

(SAB_data <- SAB_data
%>% mutate(avgMFI = ave(SAB_data$NormalValue, SAB_data$SampleIDName))
)

(SAB_data <- SAB_data
%>% group_by(PatientID) 
%>% filter(avgMFI == max(avgMFI))
)

SAB_data <- pivot_wider(
  SAB_data,
  id_cols = "SampleIDName",
  id_expand = FALSE,
  names_from = "MolSpec",
  names_prefix = "",
  names_sep = "_",
  names_glue = NULL,
  names_sort = FALSE,
  names_vary = "fastest",
  names_expand = FALSE,
  names_repair = "check_unique",
  values_from = "NormalValue",
  values_fill = NULL,
  values_fn = mean,
  unused_fn = NULL
)

(SAB_data <- SAB_data
%>% select(-"SampleIDName")
)

SAB_corr <- cor(SAB_data)


pheatmap(
  SAB_corr,
  clustering_distance_rows = "euclidean", 
  clustering_distance_cols = "euclidean", 
  clustering_method = "complete",        
  display_numbers = TRUE,                  
  number_format = "%.2f",                  
  main = "Clustered Correlation Matrix"
)

