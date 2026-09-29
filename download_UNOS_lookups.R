download_UNOS_lookups <- function(){



#Below are API keys and filepaths that may be variable. Change as-needed.

#---------------------------------------------------------------------------------
#Set UNOS API Key as env var with: Sys.setenv(api_key = "[API Key]")
#Set UNOS API Secret as env var with: Sys.setenv(api_secret = "[API Secret]")
#---------------------------------------------------------------------------------

#Filepath for UNOS antigen lookups
unos_lookup_path <- '[desired file path for lookup tables. set the same path in DSA script]'






library(httr)
library(openxlsx)

app <- oauth_app("HlaLookups",
	key = Sys.getenv("api_key"), 
	secret = Sys.getenv("api_secret"), 
	redirect_uri = oauth_callback())

endpoint <- oauth_endpoint(request = NULL, authorize = "https://api.unos.org/login/v1/oauth2/authorize", access = "https://api.unos.org/login/v1/oauth2/token", base_url = NULL)

token <- oauth2.0_token(
  endpoint,
  app,
  scope = NULL,
  user_params = NULL,
  type = NULL,
  use_oob = TRUE,
  oob_value = 'https://localhost',
  as_header = TRUE,
  use_basic_auth = FALSE,
  cache = FALSE,
  config_init = list(),
  client_credentials = TRUE,
  credentials = NULL,
  query_authorize_extra = list()
)

token <- token$credentials$access_token

loci <- list("a-locus", "b-locus", "c-locus", "dpa1-locus", "dpb1-locus", "dqa1-locus", "dqb1-locus", "dr-locus", "dr51-locus", "dr52-locus", "dr53-locus")



get_lookups <- function(locus, token){

url <- paste0('https://api.unos.org/deceased-donor/v1/lookups/hla/',locus)

req <- request(url)

req <- req_auth_bearer_token(req, token)

lookup <- req_perform(
  req,
  path = NULL,
  verbosity = 0,
  mock = getOption("httr2_mock", NULL)
)

lookupj <- resp_body_json(lookup)

lookupdf <- do.call(rbind, lapply(lookupj[2]$Value, as.data.frame))

write.xlsx(lookupdf, file = paste0(unos_lookup_path,locus,".xlsx"),overwrite = TRUE)

}



for (i in loci) {
get_lookups(i, token)
}


}
