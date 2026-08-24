
DSA2 <- function(){



#Below are API keys, server/DB names, etc that may be variable. Change as-needed.

#SQL Server Driver:
#(leave this one as-is unless you are experiencing issues, {SQL Server} driver will work in most cases)
sql_driver <- '{SQL Server}'

#SQL server name for Fusion:
fusion_server <- '[Fusion server name]'

#SQL DB name for Fusion:
fusion_db <- '[Fusion DB name]'

#SQL server name for HistoTrac:
ht_server <- '[HistoTrac server name]'

#SQL DB name for HistoTrac:
ht_db <- '[HistoTrac DB name]'

#UNOS API key:
api_key <- '[API Key]'

#UNOS API secret:
api_secret <- '[API Secret]'

#UNOS app redirect URI:
#(most likely can leave as-is unless you have defined a different redirect uri in your UNOS API app)
redirect_uri <- 'https://localhost'

#Filepath for UNOS antigen lookups:
unos_lookup_path <- '[unos_lookup_path defined in download_UNOS_lookups script]'





#import all necessary packages
library(odbc)
library(immunogenetr)
library(stringr)
library(tibble)
library(dplyr)
library(ggplot2)
library(tidyr)
library(gt)
library(httr2)
library(openxlsx)
library(base64)
library(openssl)



#begin series of user prompts for donor/recipient information
PID <- readline(prompt = "Enter Recipient Patient ID:")
DonorID <- readline(prompt = "Enter Donor ID:")
source <- readline(prompt = "Choose donor typing source: \n Enter 'm' for manual, 'h' for HistoTrac, or 'u' for UNOS:")


#function for defining donor typing manually, having the user enter the typing one antigen at a time
GetTypingManual <- function(){
	#define typing fields and merge into GL string
DonorA1 <- readline(prompt = "Enter Donor HLA-A1:")
DonorA2 <- readline(prompt = "Enter Donor HLA-A2:")
DonorB1 <- readline(prompt = "Enter Donor HLA-B1:")
DonorB2 <- readline(prompt = "Enter Donor HLA-B2:")
DonorC1 <- readline(prompt = "Enter Donor HLA-C1:")
DonorC2 <- readline(prompt = "Enter Donor HLA-C2:")
DonorDR1 <- readline(prompt = "Enter Donor HLA-DR1:")
DonorDR2 <- readline(prompt = "Enter Donor HLA-DR2:")
DonorDRw1 <- readline(prompt = "Enter Donor HLA-DR345(1):")
DonorDRw2 <- readline(prompt = "Enter Donor HLA-DR345(2):")
DonorDQ1 <- readline(prompt = "Enter Donor HLA-DQ1:")
DonorDQ2 <- readline(prompt = "Enter Donor HLA-DQ2:")
DonorDP1 <- readline(prompt = "Enter Donor HLA-DP1:")
DonorDP2 <- readline(prompt = "Enter Donor HLA-DP2:")
Donor_type <- paste0("HLA-A",DonorA1,"+HLA-A",DonorA2,"^HLA-B",DonorB1,"+HLA-B",DonorB2,"^HLA-Cw",DonorC1,"+HLA-Cw",DonorC2,"^HLA-DR",DonorDR1,"+HLA-DR",DonorDR2, "^HLA-DR",DonorDRw1,"+HLA-DR",DonorDRw2,"^HLA-DQ",DonorDQ1,"+HLA-DQ",DonorDQ2,"^HLA-DP",DonorDP1,"+HLA-DP",DonorDP2)

return(Donor_type)
}



#function for defining donor typing from HistoTrac
GetTypingHT <- function(DonorID){
#establish HistoTrac DB connection
donorHTCon <- dbConnect(odbc::odbc(), Driver   = sql_driver, Server   = ht_server, Database = ht_db, Port     = 1433)

	#define SQL query
query <- paste0("Select p.lastnm, p.firstnm, p.hospitalid, p.ssnbr as SSN, p.abocd, 
isnull (hlaa1.aequivalentcd,p.a1cd) as A1,  isnull(hlaa2.aequivalentcd,p.a2cd) as A2, isnull(hlab1.bequivalentcd,p.b1cd) as B1, isnull(hlab2.bequivalentcd,p.b2cd) as B2, 
case when p.mbw1cd = '' then p.bw1cd else p.mbw1cd end as Bw1, 
case when p.mbw2cd = '' then p.bw2cd else p.mbw2cd end as Bw2, isnull(hlac1.cequivalentcd,p.cw1cd) as Cw1, isnull(hlac2.cequivalentcd,p.cw2cd) as Cw2,  isnull(hladrb11.drb1equivalentcd,p.dr1cd) as DR1, isnull(hladrb12.drb1equivalentcd,p.dr2cd) as DR2, isnull(hladqb11.dqb1equivalentcd,p.dq1cd) as DQ1, isnull(hladqb12.dqb1equivalentcd,p.dq2cd) as DQ2, isnull(hladrb31.drb3equivalentcd,'') as DRB31, isnull(hladrb32.drb3equivalentcd,'') as DRB32, isnull(hladrb41.drb4equivalentcd,'') as DRB41, isnull(hladrb42.drb4equivalentcd,'') as DRB42, isnull(hladrb51.drb5equivalentcd,'') as DRB51, isnull(hladrb52.drb5equivalentcd,'') as DRB52, 
p.drw1cd as DRw1,
p.drw2cd as Drw2,
isnull(p.mdpb11cd,'') DPB1, isnull(p.mdpb12cd,'') DPB2, isnull(p.mdpa11cd,'') DPA1, isnull(p.mdpa12cd,'') DPA2, isnull(p.mdqa11cd,'') DQA1, isnull(p.mdqa12cd,'') DQA2
from Patient p
left join hlaa hlaa1 on hlaa1.acd = p.ma1cd left join hlaa hlaa2 on hlaa2.acd = p.ma2cd left join hlab hlab1 on hlab1.bcd = p.mb1cd left join hlab hlab2 on hlab2.bcd = p.mb2cd left join hlac hlac1 on hlac1.ccd = p.mc1cd left join hlac hlac2 on hlac2.ccd = p.mc2cd left join hladrb1 hladrb11 on hladrb11.drb1cd = p.mdrb11cd  left join hladrb1 hladrb12 on hladrb12.drb1cd = p.mdrb12cd left join hladqb1 hladqb11 on hladqb11.dqb1cd = p.mdqb11cd left join hladqb1 hladqb12 on hladqb12.dqb1cd = p.mdqb12cd left join hladrb3 hladrb31 on hladrb31.drb3cd = p.mdrb31cd left join hladrb3 hladrb32 on hladrb32.drb3cd = p.mdrb32cd left join hladrb4 hladrb41 on hladrb41.drb4cd = p.mdrb41cd left join hladrb4 hladrb42 on hladrb42.drb4cd = p.mdrb42cd left join hladrb5 hladrb51 on hladrb51.drb5cd = p.mdrb51cd left join hladrb5 hladrb52 on hladrb52.drb5cd = p.mdrb52cd WHERE p.UNOSId = ", "'", DonorID, "'")

	#execute query
DonorData <- dbGetQuery(donorHTCon, query)

	#define typing fields and merge into GL string
DonorA1 <- DonorData$A1
DonorA2 <- DonorData$A2
DonorB1 <- DonorData$B1
DonorB2 <- DonorData$B2
DonorC1 <- sub("^0", "", (DonorData$Cw1))
DonorC2 <- sub("^0", "", (DonorData$Cw2))
DonorDR1 <- DonorData$DR1
DonorDR2 <- DonorData$DR2
DonorDRB31 <- DonorData$DRB31
DonorDRB32 <- DonorData$DRB32
DonorDRB41 <- DonorData$DRB41
DonorDRB42 <- DonorData$DRB42
DonorDRB51 <- DonorData$DRB51
DonorDRB52 <- DonorData$DRB52
DonorDQ1 <- DonorData$DQ1
DonorDQ2 <- DonorData$DQ2
DonorDP1 <- sub("^0", "", (sub(":.*", "", DonorData$DPB1)))
DonorDP2 <- sub("^0", "", (sub(":.*", "", DonorData$DPB2)))
Donor_type <- paste0("HLA-A",DonorA1,"+HLA-A",DonorA2,"^HLA-B",DonorB1,"+HLA-B",DonorB2,"^HLA-Cw",DonorC1,"+HLA-Cw",DonorC2,"^HLA-DR",DonorDR1,"+HLA-DR",DonorDR2, "^HLA-DR",DonorDRB31,"+HLA-DR",DonorDRB32,"^HLA-DR",DonorDRB41,"+HLA-DR",DonorDRB42,"^HLA-DR",DonorDRB51,"+HLA-DR",DonorDRB52,"^HLA-DQ",DonorDQ1,"+HLA-DQ",DonorDQ2,"^HLA-DP",DonorDP1,"+HLA-DP",DonorDP2)

return(Donor_type)
}



#function for defining donor typing through UNOS API
GetTypingUNOS <- function(DonorID){

auth_url <- "https://api.unos.org/login/v1/oauth2/authorize"

	#custom base64 encoding function that is url-friendly
encode64 <- function(x){
x <- base64_encode(x)
x <- gsub("=+$", "", x)
x <- gsub("+", "-", x, fixed = TRUE)
x <- gsub("/", "_", x, fixed = TRUE)
x
}

	#create code challenge and verifier using sha256 and base64-url encoding
verifier <- encode64(rand_bytes(32))
challenge = encode64(sha256(charToRaw(verifier)))

auth_params <- list(	response_type='code',
		client_id=api_key,
		redirect_uri=redirect_uri,
		code_challenge=challenge, 
		code_challenge_method='S256'
		)

	#direct user to UNOS login screen in browser
auth_req <- request(auth_url) %>%
	req_url_query(q=!!!auth_params)
browseURL(auth_req$url)
auth_res <- readline(prompt = "Log in to UNOS in browser window. Copy/paste URL here after login:")

	#parse user-entered url after login to extract auth code
auth_res <- url_parse(auth_res)
auth_code <- auth_res$query$code

	#use auth code to request API token
token_url <- "https://api.unos.org/login/v1/oauth2/token"
params <- paste0("grant_type=authorization_code&code=",auth_code,"&redirect_uri=https%3A%2F%2Flocalhost&code_verifier=",verifier)
auth_key <- base64_encode(paste0(api_key,':',api_secret))
token_req <- request(token_url) %>%
		req_headers(
		'Authorization'=paste0('Basic ',auth_key),
		'Content-Type'='application/x-www-form-urlencoded')
token_req <- token_req %>%
		req_body_raw(params, type = 'application/x-www-form-urlencoded')
token_res <- req_perform(	token_req,
			path = NULL,
  			verbosity = 3,
  			mock = NULL
			)
token_res_raw <- resp_body_json(token_res)

	#use API token to request donor typing
url <- paste0('https://api.unos.org/deceased-donor/v1/donor-data/',DonorID,'/hla')
req <- request(url)
req <- req %>%
	req_headers(
		'X-Center-Code'='MEMC',
		'X-Center-Type'='TX1')
req <- req_auth_bearer_token(req, token_res_raw$access_token)
res <- req_perform(
  req,
  path = NULL,
  verbosity = 2,
  mock = getOption("httr2_mock", NULL)
)
resj <- resp_body_json(res)

	#function to open pre-downloaded UNOS lookups, see 'download_UNOS_lookups.R'
open_lookups <- function(locus){
filepath <- unos_lookup_path
lookup <- read.xlsx(
    xlsxFile = paste0(filepath,locus,'.xlsx'),
    sheet = 1,
    colNames = TRUE,
    skipEmptyRows = TRUE,
    detectDates = TRUE
  )
return(lookup)
}

	#processes all lookups into one list broken out by locus
loci <- list("a-locus", "b-locus", "c-locus", "dpa1-locus", "dpb1-locus", "dqa1-locus", "dqb1-locus", "dr-locus", "dr51-locus", "dr52-locus", "dr53-locus")
lookup_list <- list()
for (i in loci){
	lookup_list[[i]] <- open_lookups(i)
}
lookup_antigen <- function(Id, locus, lookup){
    antigen <- lookup[[locus]][lookup[[locus]]$Id == Id, "Code"]
    return(antigen)
}

	#function to convert drb345 alleles to their corresponding antigen, else populate as blanks (if null or not present)
sub_dr345 <- function(allele, locus){
	antigen <- ifelse(allele %in% list('0', 'N', 'NT', 'NullAllele'), '', ifelse(locus == 'drb3', '52', ifelse(locus == 'drb4', 53, ifelse(locus == 'drb5', '51', ''))))
}

	#define typing fields using lookups, substitue drb345 alleles for their antigens, and merge into GL string
DonorA1 <- lookup_antigen(resj$Value$A$Value1, 'a-locus', lookup_list)
DonorA2 <- lookup_antigen(resj$Value$A$Value2, 'a-locus', lookup_list)
DonorB1 <- lookup_antigen(resj$Value$B$Value1, 'b-locus', lookup_list)
DonorB2 <- lookup_antigen(resj$Value$B$Value2, 'b-locus', lookup_list)
DonorC1 <- sub("^0", "", (lookup_antigen(resj$Value$C$Value1, 'c-locus', lookup_list)))
DonorC2 <- sub("^0", "", (lookup_antigen(resj$Value$C$Value2, 'c-locus', lookup_list)))
DonorDR1 <- lookup_antigen(resj$Value$DR$Value1, 'dr-locus', lookup_list)
DonorDR2 <- lookup_antigen(resj$Value$DR$Value2, 'dr-locus', lookup_list)
DonorDRB31 <- sub_dr345(lookup_antigen(resj$Value$DR52$Value1, 'dr52-locus', lookup_list), 'drb3')
DonorDRB32 <- sub_dr345(lookup_antigen(resj$Value$DR52$Value2, 'dr52-locus', lookup_list), 'drb3')
DonorDRB41 <- sub_dr345(lookup_antigen(resj$Value$DR53$Value1, 'dr53-locus', lookup_list), 'drb4')
DonorDRB42 <- sub_dr345(lookup_antigen(resj$Value$DR53$Value2, 'dr53-locus', lookup_list), 'drb4')
DonorDRB51 <- sub_dr345(lookup_antigen(resj$Value$DR51$Value1, 'dr51-locus', lookup_list), 'drb5')
DonorDRB52 <- sub_dr345(lookup_antigen(resj$Value$DR51$Value2, 'dr51-locus', lookup_list), 'drb5')
DonorDQ1 <- lookup_antigen(resj$Value$DQB1$Value1, 'dqb1-locus', lookup_list)
DonorDQ2 <- lookup_antigen(resj$Value$DQB1$Value2, 'dqb1-locus', lookup_list)
DonorDP1 <- sub("^0", "", (sub(":.*", "", lookup_antigen(resj$Value$DPB1$Value1, 'dpb1-locus', lookup_list))))
DonorDP2 <- sub("^0", "", (sub(":.*", "", lookup_antigen(resj$Value$DPB1$Value2, 'dpb1-locus', lookup_list))))
Donor_type <- paste0("HLA-A",DonorA1,"+HLA-A",DonorA2,"^HLA-B",DonorB1,"+HLA-B",DonorB2,"^HLA-Cw",DonorC1,"+HLA-Cw",DonorC2,"^HLA-DR",DonorDR1,"+HLA-DR",DonorDR2, "^HLA-DR",DonorDRB31,"+HLA-DR",DonorDRB32,"^HLA-DR",DonorDRB41,"+HLA-DR",DonorDRB42,"^HLA-DR",DonorDRB51,"+HLA-DR",DonorDRB52,"^HLA-DQ",DonorDQ1,"+HLA-DQ",DonorDQ2,"^HLA-DP",DonorDP1,"+HLA-DP",DonorDP2)

return(Donor_type)

}

#determine source of typing and use corresponding function
ifelse(source == 'm', (Donor_type <- GetTypingManual()), (ifelse(source == 'h', (Donor_type <- GetTypingHT(DonorID)), ifelse(source == 'u', (Donor_type <- GetTypingUNOS(DonorID)), return(paste("invalid entry for donor typing source:",source)))))) 

#establish Histotrac DB connection for recipient info
recipHTCon <- dbConnect(odbc::odbc(), Driver   = "{SQL Server}", Server   = "histotracp", Database = "Histotrac2048", Port     = 1433)

#define query for recipient info
query <- paste0("Select p.LowRiskAntibodyTxt, p.ModerateRiskAntibodyTxt, p.UnacceptAntigenTxt, p.firstnm, p.lastnm from Patient p WHERE p.HospitalID  = ", "'", PID, "'")
RecipFinalAssmt <- as_tibble(dbGetQuery(recipHTCon, query))
(RecipFinalAssmt <- RecipFinalAssmt
%>% mutate(Low_Risk = gsub("\r\n",";   ",gsub(" ",",",LowRiskAntibodyTxt)))
%>% mutate(Moderate_Risk = gsub("\r\n",";   ",gsub(" ",",",ModerateRiskAntibodyTxt)))
%>% mutate(Unacceptable = gsub("\r\n",";   ",gsub(" ",",",UnacceptAntigenTxt)))
)
recipName <- paste0(RecipFinalAssmt$lastnm,", ", RecipFinalAssmt$firstnm)

#execute query for recipient info
recipFusionCon <- dbConnect(odbc::odbc(), Driver   = sql_driver, Server   = fusion_server, Database = fusion_db, Port     = 1433)
query <- paste0("SELECT p.FirstName, p.LastName, s.SampleIDName, s.ShipmentDT, pd.BeadID, wd.NormalValue, CAST(pd.Specificity AS VARCHAR(50)) AS MolSpec, CAST(pd.SpecAbbr AS VARCHAR(50)) AS Spec FROM PATIENT p INNER JOIN SAMPLE s on p.PatientID = s.PatientID INNER JOIN WELL w on s.SampleID = w.SampleID INNER JOIN TRAY t on w.TrayID = t.TrayID INNER JOIN PRODUCT_DETAIL pd on t.CatalogID = pd.CatalogID INNER JOIN WELL_DETAIL wd on pd.BeadID = wd.BeadID and w.WellID = wd.WellID where p.PatientID =", "'", PID, "'", " AND pd.BeadID NOT IN ('001','002') ORDER BY BeadID")
SAB_data <- dbGetQuery(recipFusionCon, query)

#format Fusion data
SAB_data <- as_tibble(SAB_data)
(SAB_data <- SAB_data
%>% mutate(Spec = gsub("-",'',gsub(",",'',Spec))))
(SAB_data <- SAB_data
%>% mutate(MolSpec= gsub("-",'',gsub(",",'',MolSpec))))
(SAB_data <- SAB_data
%>% mutate(Spec_full =  HLA_prefix_add(Spec, prefix = "HLA-")))
(SAB_data <- SAB_data
%>% mutate(Spec_full =  gsub("Bw6",'',gsub("Bw4",'',Spec_full))))
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

#filter Fusion data to only donor antigens
(SAB_DSA <- SAB_data
%>% filter(str_detect(Donor_type, GLstring_regex(Spec_full))))

#create pivot table of antigen MFIs by date
(SAB_DSA_summary <- SAB_DSA
%>% select("SampleDate", "NormalValue", "MolSpec", "Spec"))
(SAB_DSA_summary <- SAB_DSA_summary
%>% arrange(desc(SampleDate),MolSpec))
SAB_DSA_summary <- pivot_wider(
  SAB_DSA_summary,
  id_cols = c("MolSpec", "Spec"),
  id_expand = FALSE,
  names_from = "SampleDate",
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

#plot title for DSA graph
plot_title <- paste("DSA Graph for", recipName, "vs. Donor ID:",DonorID)


#return DSA graph and HTML of MFI pivot table
return(
list(
(ggplot(SAB_DSA, aes(x = SampleDate, y = NormalValue, color = Spec, group=Spec)) + geom_point() + stat_summary(aes(y = NormalValue), fun = mean, geom = "line") + geom_hline(yintercept = 1000, color = "yellow") + geom_hline(yintercept = 2000, color = "orange") + geom_hline(yintercept = 3000, color = "red") + labs(x = "Date", y = "MFI", title = plot_title, color = "Specificity") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
),

(SAB_DSA_summary |>
gt(
rowname_col = NULL,
groupname_col = "group")
)
%>% 
  tab_header(
    title = gt::html(paste("Raw Antibody Data for ", recipName, "vs. Donor ID:",DonorID))
  )
|> 
data_color(
    method = "bin",
    palette = c("green", "yellow", "red"),
	bins = c(0,500,1000,2000,3000,40000)
  )
)
)
}
