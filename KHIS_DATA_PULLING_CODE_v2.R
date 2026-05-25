#===============================================================================
# Load the required libraries
#===============================================================================
####https://hiskenya.dha.go.ke/dhis-web-commons/security/login.action
options(pkgType = "binary")
renv_file <- Sys.getenv("PROJECT_RENVIRON", unset = ".Renviron")
if (file.exists(renv_file)) {
  readRenviron(renv_file)
}

# Install pacman if not already installed
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
# Use pacman to load and install packages
pacman::p_load(rio, zoo, tidyverse, formattable, ggforce, ggthemes, RODBC, patchwork,tidytext, data.table, lubridate,
               httr,curl,readxl,openxlsx,foreach,writexl, doParallel, jsonlite, future.apply, progress,  pbapply, future, furrr, progressr, promises,
               purrr, DBI,RMySQL, RMariaDB,stringr)

#===============================================================================
# 2. DIRECTORIES & LOGGING
#===============================================================================

BASE_DIR <- Sys.getenv("KHIS_BASE_DIR", unset = ".")
OUTPUT_DIR <- file.path(BASE_DIR, "output")
LOG_DIR <- file.path(BASE_DIR, "logs")
EXTERNAL_OUTPUT_DIR <- Sys.getenv("EXTERNAL_DIR", unset = "./data_source")

dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(LOG_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(EXTERNAL_OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

LOG_FILE <- file.path(
  LOG_DIR,
  paste0("KHIS_LOG_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log")
)

log_message <- function(msg, type="INFO"){
  line <- paste0(
    "[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ",
    "[", type, "] ", msg
  )
  cat(line, "\n")
  write(line, LOG_FILE, append=TRUE)
}

log_message("SCRIPT STARTED")

#===============================================================================
# 3. EMAIL ALERT FUNCTION
#===============================================================================

send_failure_email <- function(subject, body){
  try({
    email <- compose_email(body = md(body))
    smtp_send(
      email,
      from = Sys.getenv("EMAIL_FROM"),
      to   = Sys.getenv("EMAIL_TO"),
      subject = subject,
      credentials = creds(
        user = Sys.getenv("EMAIL_USER"),
        provider = "gmail"
      )
    )
  }, silent = TRUE)
}

#===============================================================================
# 4. SAFE EXECUTION WRAPPER
#===============================================================================

safe_run <- function(expr, step_name){
  
  tryCatch({
    
    log_message(paste("START:", step_name))
    result <- eval(expr)
    log_message(paste("SUCCESS:", step_name))
    return(result)
    
  }, error = function(e){
    
    log_message(paste("ERROR in", step_name, ":", e$message), "ERROR")
    
    send_failure_email(
      subject = paste("KHIS Weekly FAILED:", step_name),
      body = paste(
        "KHIS Weekly Job Failed\n\n",
        "Step:", step_name, "\n",
        "Error:", e$message, "\n\n",
        "Log File:", LOG_FILE
      )
    )
    
    quit(save="no", status=1)
  })
}

#===============================================================================
# create functions
#===============================================================================

# Metadata function
get_metadata<-function(metadata) {
  endpoint <- paste0(.url,'api/',metadata,'?fields=id,code,name,level,dataset,&paging=false')
  resp <- GET(endpoint, authenticate(.username,.password), httr::timeout(90000))
  
  if (httr::status_code(resp) < 200 || httr::status_code(resp) >= 300) {
    body_txt <- httr::content(resp, as = "text", encoding = "UTF-8")
    stop(
      sprintf(
        "Metadata request failed [%s] for %s. Response: %s",
        httr::status_code(resp),
        endpoint,
        substr(body_txt, 1, 500)
      )
    )
  }
  
  r <- tryCatch(
    content(resp, as = "parsed"),
    error = function(e) {
      stop(sprintf("Failed to parse metadata response for %s: %s", metadata, e$message))
    }
  )
  
  r <- map(r, bind_rows) %>% reduce(full_join, by = 'id')
}
# Data value function
get_data_analytics<-function(de){
  d.1=data.frame()
  for (i in 1:length(de)){ 
    
    url.base<-paste0(.url,"api/analytics.json?")
    url.dx<-paste0("dimension=dx:",de[i])
    url.ou<-paste0("&dimension=ou:",ou,level)
    url.pe<-paste0("&dimension=pe:",pe)
    url.co<-paste0("&dimension=co")
    url.ao<-paste0("&dimension=ao")
    url.meta<-"&displayProperty=NAME&skipMeta=true&outputIdScheme=NAME"
    url<-paste0(url.base,url.dx,url.ou,url.co,url.ao,url.pe,url.meta)
    resp <- httr::GET(url, httr::authenticate(.username,.password), httr::timeout(90000))
    resp_text <- httr::content(resp, "text", encoding = "UTF-8")
    
    if (httr::status_code(resp) < 200 || httr::status_code(resp) >= 300) {
      stop(
        sprintf(
          "Analytics request failed for dx=%s [%s]. URL: %s. Response: %s",
          de[i],
          httr::status_code(resp),
          url,
          substr(resp_text, 1, 500)
        )
      )
    }
    
    r <- tryCatch(
      jsonlite::fromJSON(resp_text, flatten = TRUE),
      error = function(e) {
        stop(sprintf("Failed to parse analytics response for dx=%s: %s", de[i], e$message))
      }
    )
    
    d<-data.frame(r$rows)
    if(length(d)==0){
      d<-data.frame(dx=NA_character_,
                    co=NA_character_,
                    ao=NA_character_,
                    ou=NA_character_,
                    pe=NA_character_,
                    value=NA
      )
    }else{
      names(d)<-r$headers$name
    }
    d.1=bind_rows(d.1,d)
    cat("\rFinished", i, "of", length(de))
    
  }
  return(d.1)
  
}
# Org-unit Function
get_dhis2_org_units <- function(.url, username, password) {
  
  # DHIS2 API endpoint for organization units
  api_endpoint <- "api/organisationUnits"
  
  # Full URL with query parameters to include hierarchy and the 'code' attribute
  url <- paste0(.url, api_endpoint, "?fields=id,name,level,path,code&paging=false")
  
  # Authenticate and get the data
  response <- GET(url, authenticate(username, password, type = "basic"))
  
  # Check if the request was successful
  if (response$status_code != 200) {
    body_txt <- content(response, "text", encoding = "UTF-8")
    stop(
      sprintf(
        "Failed to fetch organization units [%s]. URL: %s. Response: %s",
        response$status_code,
        url,
        substr(body_txt, 1, 500)
      )
    )
  }
  
  # Parse the JSON content
  content <- content(response, "text", encoding = "UTF-8")
  org_units <- fromJSON(content, flatten = TRUE)
  
  # Create a dataframe with organization units
  org_units_df <- as.data.frame(org_units$organisationUnits)
  
  ##keep country and count
  org_units_df = org_units_df[org_units_df$level %in% c(1,2,3,4,5),]
  
  
  # Split the path into individual levels and get their names
  hierarchy_df <- org_units_df %>%
    separate(path, into = paste0("Level_", 1:max(org_units_df$level, na.rm = TRUE)), sep = "/", fill = "right") %>%
    mutate(across(starts_with("Level_"), ~ ifelse(. == "", NA, .)))
  
  # Get a unique list of IDs to names
  id_name_lookup <- org_units_df %>% 
    dplyr::select(id, name) %>%
    distinct()
  
  # Replace IDs in hierarchy columns with names
  hierarchy_df <- hierarchy_df %>%
    mutate(across(starts_with("Level_"), ~ id_name_lookup$name[match(., id_name_lookup$id)]))
  
  # Combine the hierarchy columns with the original dataframe and include the 'code' attribute
  org_units_df <- org_units_df %>%
    dplyr::select(name, level, code,id) %>%
    cbind(hierarchy_df %>% dplyr::select(starts_with("Level_"))) %>%
    distinct()  # Remove any potential duplicates
  
  return(org_units_df)
}

#===============================================================================
# Set the parameters
#===============================================================================
.username<-Sys.getenv("DHIS2_USERNAME")
.password<-Sys.getenv("DHIS2_PASSWORD")

#.url = Sys.getenv("DHIS2_URL", "https://hiskenya.org/")
.url = Sys.getenv("DHIS3_URL")

# Error Handling
if (.username == "" || .password == "") {
  stop("
    Please create a .Renviron file in your project root with:
    DHIS2_USERNAME=your_username
    DHIS2_PASSWORD=your_password
    (Restart R after creating the file)
  ")
}

# Meta data
metadata = c('dataElements',"categoryOptionCombos",'dataSets','organisationUnits')

#*******************************************************************************************
# Set Data pull Period
#*******************************************************************************************
fy_year = 2026
s_date <- as.Date(paste0(fy_year - 1, "1001"), "%Y%m%d")
#e_date <- as.Date(paste0(fy_year  ,"0930"), "%Y%m%d")
e_date <- as.Date(format(Sys.Date(), "%Y-%m-01"))
pe<-paste0(format(seq(s_date,e_date,by="month"),format = "%Y%m"),collapse = ';')

#===============================================================================
# Get KHIS Health Facilities
#===============================================================================

counties_khis <- suppressWarnings(get_dhis2_org_units(.url, .username, .password)%>% 
                                    dplyr::select(County=name,id = id, level = level))

facilities_khis <- suppressWarnings(get_dhis2_org_units(.url, .username, .password)%>% 
                                      dplyr::select(id,mfl=code,facilityname=name,level,county=Level_3,subcounty=Level_4,ward=Level_5))

#===============================================================================
# Pull KHIS Meta Data
#===============================================================================
for (i in 1:length(metadata)){
  assign(paste0(metadata[i],'_khis'), get_metadata(metadata[i]))
}
ou='HfVjCurKxh2'

##old tool
##' MOH 711 New ANC clients f9vesk5d4IY
##' MOH 731 known positive at 1st ANC HV 02-10 oZc8MNc0nLZ
##' MOH 731 positive results L&D HV02-12  hn3aChn4sVx
##' MOH 731 Positive Results_ANC HV02-11 nwXS5vxrrr7
##' MOH 731 Positive Results_PNC<=6wks HV02-13 AfHArvGun12
##' MOH 731 Positive_PNC> 6weeks to 6 months HV02-14 hHLR1HP8xzI ##might miss in some cases no data
##' MOH 731 Initial test at ANC HV02-04 ETX9cUWF43c
##' MOH 731 Initial test at L&D HV02-05 mQz4DhBSv9V
##' MOH 731 Initial test at PNC_PNC<=6wks HV02-06 LQpQQP3KnU1
##' MOH 731 Tested_PNC> 6weeks to 6 months HV02-09 PXUzSsmeY0P
##' MOH 731 1st ANC Visits HV02-01 uSxBUWnagGg
##' MOH 731 Known Positive at 1st ANC HV02-03 qSgLzXh46n9
##' 
##' New tool
##' MOH 711 New ANC clients f9vesk5d4IY
##' MOH 731_EMTCT_Known Positive at 1st ANC_HV02-01 e9YgXAmC0qf
##' MOH 731_EMTCT_Tested at ANC_Initial_HV02-02 JNjdyMxJbrR
##' MOH 731_EMTCT_Positive Results_ANC_HV02-10 gmaBILMqfJ8
##' MOH 731_EMTCT_Tested at L&D_Initial_HV02-04 OWymB1SSCdJ
##' MOH 731_EMTCT_Positive Results _L&D_HV02-11 vk1y3YRXzBO
##' MOH 731_EMTCT_Tested at PNC_<=6 weeks_Initial_HV02-06 Qvr08fM3So0
##' MOH 731_EMTCT_Positive Results_PNC <=6weeks_HV02-12 yttRVTEafO4
##' MOH 731_EMTCT_Tested at PNC_>6 weeks_Initial_HV02-08 dWb5WVJBsCx
##' MOH 731_EMTCT_Positive PNC >6weeks_HV02-13 ZImJQtnhnnW
#level=";LEVEL-2" ##county
level=";LEVEL-5" ##facilty

##Pull old data
old_codes = c("f9vesk5d4IY","oZc8MNc0nLZ","hn3aChn4sVx", "nwXS5vxrrr7", "AfHArvGun12","hHLR1HP8xzI", "ETX9cUWF43c", "mQz4DhBSv9V",
              "LQpQQP3KnU1", "PXUzSsmeY0P", "uSxBUWnagGg", "qSgLzXh46n9")


old_khis= data.frame()
old_khis=bind_rows(old_khis,get_data_analytics(old_codes))

new_codes = c("e9YgXAmC0qf","JNjdyMxJbrR", "gmaBILMqfJ8","OWymB1SSCdJ", "vk1y3YRXzBO","Qvr08fM3So0",
              "yttRVTEafO4","dWb5WVJBsCx", "ZImJQtnhnnW")

new_khis= data.frame()
new_khis=bind_rows(new_khis,get_data_analytics(new_codes))
#===============================================================================
# Merge Data Values and Meta data
#===============================================================================

# 3.0 KHIS
tbl_khis<-old_khis %>% 
  left_join(dataElements_khis[,c(3,2)],by=c('dx'='id')) %>% 
  dplyr::rename(dataelement=name) %>% 
  left_join(organisationUnits_khis[,c(1,2,3)],by=c('ou'='id'))%>% 
  #dplyr::rename(facilityname=name,mfl=code) %>%
  left_join(categoryOptionCombos_khis[,c(1,2)],by=c('co'='id')) %>%
  #dplyr::rename(category=name) %>% 
  left_join(categoryOptionCombos_khis[,c(1,2)],by=c('ao'='id')) %>%
  dplyr::rename(uid_khis=ou) 


##bring inc county, sub county
tbl_khis = tbl_khis %>% left_join(., facilities_khis, by = c('uid_khis'='id'))

##extract year and month

tbl_khis <- tbl_khis %>%
  mutate(
    Year = substr(pe, 1, 4),
    Month_Num = as.numeric(substr(pe, 5, 6)),
    Month = month.name[Month_Num]
  ) %>%
  dplyr::select(-Month_Num)

names(tbl_khis)|> dput()

tbl_khis = dplyr::select(tbl_khis, c( "dataelement", "facilityname", "mfl","uid_khis",
                                      "county", "subcounty", "ward", "Year", "Month", "value"))

tbl_khis$dataelement = as.factor(tbl_khis$dataelement)
tbl_khis$value = as.numeric(tbl_khis$value)
nums = tbl_khis %>% group_by(dataelement) %>% summarise(Total = sum(value))

# 3.0 New KHIS
tbl_new_khis<-new_khis %>% 
  left_join(dataElements_khis[,c(3,2)],by=c('dx'='id')) %>% 
  dplyr::rename(dataelement=name) %>% 
  left_join(organisationUnits_khis[,c(1,2,3)],by=c('ou'='id'))%>% 
  #dplyr::rename(facilityname=name,mfl=code) %>%
  left_join(categoryOptionCombos_khis[,c(1,2)],by=c('co'='id')) %>%
  #dplyr::rename(category=name) %>% 
  left_join(categoryOptionCombos_khis[,c(1,2)],by=c('ao'='id')) %>%
  dplyr::rename(uid_khis=ou) 

tbl_new_khis = tbl_new_khis %>% left_join(., facilities_khis, by = c('uid_khis'='id'))

tbl_new_khis <- tbl_new_khis %>%
  mutate(
    Year = substr(pe, 1, 4),
    Month_Num = as.numeric(substr(pe, 5, 6)),
    Month = month.name[Month_Num]
  ) %>%
  dplyr::select(-Month_Num)

names(tbl_new_khis)|> dput()

tbl_new_khis = dplyr::select(tbl_new_khis, c( "dataelement", "facilityname","mfl","uid_khis", 
                                              "county", "subcounty", "ward", "Year", "Month", "value"))

tbl_new_khis$dataelement = as.factor(tbl_new_khis$dataelement)
tbl_new_khis$value = as.numeric(tbl_new_khis$value)
nums = tbl_new_khis %>% group_by(dataelement) %>% summarise(Total = sum(value))

###final data

final_data = bind_rows(tbl_khis, tbl_new_khis)

####tables

cat("Processing ANC data...\n")
cleaned_data <- final_data %>%
  # Clean county names
  mutate(
    county = case_when(
      county == "Elgeyo Marakwet" ~ "Elgeyo-Marakwet",
      county == "Muranga" ~ "Murang'a",
      county == "Tharaka Nithi" ~ "Tharaka-Nithi",
      TRUE ~ county
    )
  ) %>%
  arrange(county)


# Step 1: Pivot data from long to wide format
wide_data <- cleaned_data %>%
  # Ensure Year and Month are properly formatted
  mutate(
    Year = as.character(Year),
    Month = as.character(Month)
  ) %>%
  # Pivot to wide format
  pivot_wider(
    names_from = dataelement,
    values_from = value,
    values_fill = 0,        # Fill NAs with 0
    names_repair = "unique" # Handle duplicate column names if any
  )

# Guard against schema drift: create missing indicators as zero to avoid hard crashes.
required_indicator_cols <- c(
  "MOH 711 New ANC clients",
  "MOH 731 1st ANC Visits HV02-01",
  "MOH 731 Known Positive at 1st ANC HV02-03",
  "MOH 731 Initial test at ANC HV02-04",
  "MOH 731 Positive Results_ANC HV02-11",
  "MOH 731 Initial test at L&D HV02-05",
  "MOH 731 Positive Results _L&D HV02-12",
  "MOH 731 Initial test at PNC_PNC<=6wks HV02-06",
  "MOH 731 Tested_PNC> 6weeks to 6 months HV02-09",
  "MOH 731 Positive Results_PNC<=6wks HV02-13",
  "MOH 731 Positive_PNC> 6weeks to 6 months HV02-14",
  "MOH 731_EMTCT_Known Positive at 1st ANC_HV02-01",
  "MOH 731_EMTCT_Tested at ANC_Initial_HV02-02",
  "MOH 731_EMTCT_Positive Results_ANC_HV02-10",
  "MOH 731_EMTCT_Tested at L&D_Initial_HV02-04",
  "MOH 731_EMTCT_Positive Results _L&D_HV02-11",
  "MOH 731_EMTCT_Tested at PNC_<=6 weeks_Initial_HV02-06",
  "MOH 731_EMTCT_Positive Results_PNC <=6weeks_HV02-12",
  "MOH 731_EMTCT_Tested at PNC_>6 weeks_Initial_HV02-08",
  "MOH 731_EMTCT_Positive PNC >6weeks_HV02-13"
)

missing_indicator_cols <- setdiff(required_indicator_cols, names(wide_data))
if (length(missing_indicator_cols) > 0) {
  warning(
    "Missing DHIS indicator columns were defaulted to 0: ",
    paste(missing_indicator_cols, collapse = ", ")
  )
  for (nm in missing_indicator_cols) {
    wide_data[[nm]] <- 0
  }
}

# Step 2: Calculate summary statistics
dash_values <- wide_data %>%
  group_by(facilityname, Year, Month) %>% reframe(
    County    = first(county),
    SubCounty = first(subcounty),
    uid_khis = first(uid_khis),
    mfl = first(mfl),
    # October 2025 - ANC calculations
    tst_anc1 = `MOH 731 Initial test at ANC HV02-04` + 
      `MOH 731_EMTCT_Tested at ANC_Initial_HV02-02`,
    
    MOH711NewANCclients = `MOH 711 New ANC clients`,
    MOH7311stANCVisitsHV0201 = `MOH 731 1st ANC Visits HV02-01`,
    
    # att_anc1 = ifelse(tst_anc1 > `MOH 711 New ANC clients`, ##no clients attending first ANC clinic
    #                   `MOH 731 1st ANC Visits HV02-01` + `MOH 711 New ANC clients`, NA),## fix pick the maximum of `MOH 731 1st ANC Visits HV02-01` , `MOH 711 New ANC clients
    # 
    
    att_anc1 = ifelse(
      tst_anc1 > `MOH 711 New ANC clients`,
      pmax(`MOH 731 1st ANC Visits HV02-01`,
           `MOH 711 New ANC clients`,
           na.rm = TRUE),
      NA
    ),
    
    attending_anc1_rev = `MOH 711 New ANC clients`,
    attending_anc1_rev = ifelse(tst_anc1 > `MOH 711 New ANC clients`, 
                                att_anc1, attending_anc1_rev),##use att_anc1 value if tst_anc1 > `MOH 711 New ANC clients`
    
    num_attending_anc1 = attending_anc1_rev,
    num_known_pos = `MOH 731 Known Positive at 1st ANC HV02-03` + 
      `MOH 731_EMTCT_Known Positive at 1st ANC_HV02-01`,
    
    num_tested_anc1 = `MOH 731 Initial test at ANC HV02-04` + 
      `MOH 731_EMTCT_Tested at ANC_Initial_HV02-02`, ##intial test old and new version
    
    num_pos_anc1 = `MOH 731 Positive Results_ANC HV02-11` + 
      `MOH 731_EMTCT_Positive Results_ANC_HV02-10`,## positive results old and new version, currently feeding dashboard
    
    ##QC done on both the old and new versions, no simultenaous reporting
    num_neg_anc1 = num_tested_anc1 - num_pos_anc1,
    num_unk_status = num_attending_anc1 - (num_pos_anc1 + num_neg_anc1),
    
    # L&D calculations
    num_tst_labor = `MOH 731 Initial test at L&D HV02-05` + 
      `MOH 731_EMTCT_Tested at L&D_Initial_HV02-04`,# number of initial tests at L&D
    
    num_pos_labor = `MOH 731 Positive Results _L&D HV02-12` + 
      `MOH 731_EMTCT_Positive Results _L&D_HV02-11`,# number pos at L&D currently feeding dashboard
    
    num_neg_labor = num_tst_labor - num_pos_labor,
    
    # PNC calculations
    num_tested_pnc = `MOH 731 Initial test at PNC_PNC<=6wks HV02-06` + 
      `MOH 731_EMTCT_Tested at PNC_<=6 weeks_Initial_HV02-06` + 
      `MOH 731 Tested_PNC> 6weeks to 6 months HV02-09` + 
      `MOH 731_EMTCT_Tested at PNC_>6 weeks_Initial_HV02-08`,
    
    num_pos_pnc = `MOH 731 Positive Results_PNC<=6wks HV02-13` + 
      `MOH 731_EMTCT_Positive Results_PNC <=6weeks_HV02-12` + 
      `MOH 731 Positive_PNC> 6weeks to 6 months HV02-14` + 
      `MOH 731_EMTCT_Positive PNC >6weeks_HV02-13`, ##number pos at pnc feeds dashboard
    
    num_neg_pnc = num_tested_pnc - num_pos_pnc,
    
    # Total calculations
    total_pos = num_pos_anc1 + num_pos_labor + num_pos_pnc, ## feeds dashboard (denominator for PMTCT-NP)
    
    # Additional metrics
    total_tested = num_tested_anc1 + num_tst_labor + num_tested_pnc,
    total_negative = num_neg_anc1 + num_neg_labor + num_neg_pnc,
    
    .groups = 'drop'  # Remove grouping structure
  ) %>%
  arrange(facilityname, Year, Month)

# View the results
print(head(dash_values))

# Clean mfl_code
dash_values <- dash_values %>%
  mutate(
    mfl = str_replace(mfl, "^#.", ""),
    mfl = as.numeric(mfl)
  )

# Manual corrections
dash_values <- dash_values %>%
  mutate(
    mfl = ifelse(mfl == 8463 & facilityname == "Kapsaos Eldoret Dispensary", 28463, mfl),
    mfl = ifelse(is.na(mfl) & facilityname == "Reale Hospital Eldoret", 18983, mfl),
    mfl = ifelse(is.na(mfl) & facilityname == "Kosoiywo Dispensary", 25415, mfl)
  )

# # Handle duplicates (keep max num_attending_anc1)
# # -------------------------------
# 
# 
# 
dash_values <- dash_values %>%
  group_by(mfl, Year, Month, uid_khis) %>%
  arrange(mfl, Year, Month,uid_khis, desc(num_attending_anc1)) %>%
  slice(1) %>%
  ungroup()

uid_khis = c("SuSjUQpc8Fm", "SuSjUQpc8Fm", "SuSjUQpc8Fm", "P6JsnQp2JcX", 
             "P6JsnQp2JcX", "P6JsnQp2JcX", "zGxvoDhCgz9", "zGxvoDhCgz9", "zGxvoDhCgz9", 
             "DlNyyqjfQud", "DlNyyqjfQud", "DlNyyqjfQud", "KBDEcxMhdSN", "KBDEcxMhdSN", 
             "h6D72jpQh9p", "h6D72jpQh9p", "h6D72jpQh9p", "YiSC9JwNVW0", "YiSC9JwNVW0", 
             "YiSC9JwNVW0", "uhQItfgfpco", "dasSyaLQpkF", "dasSyaLQpkF", "dasSyaLQpkF", 
             "UeH6G4BMZrn", "UeH6G4BMZrn", "UeH6G4BMZrn", "TVSIXHVmG8q", "TVSIXHVmG8q", 
             "TVSIXHVmG8q", "uKQ00J1SYx7", "uKQ00J1SYx7", "uKQ00J1SYx7", "FQAtZMXSkd6", 
             "FQAtZMXSkd6", "FQAtZMXSkd6")

mfl = c(20535, 20535, 20535, 25212, 25212, 25212, 24993, 24993, 24993, 
        28630, 28630, 28630, 23523, 23523, 19117, 19117, 19117, 30788, 
        30788, 30788, 29642, 19891, 19891, 19891, 22265, 22265, 22265, 
        20601, 20601, 20601, 24073, 24073, 24073, 28463, 28463, 28463
)

fixed_mfl = data.frame(uid_khis,mfl)

# Find matching indices
matches <- match(dash_values$uid_khis, fixed_mfl$uid_khis)
#Replace mfl where matches exist
dash_values$mfl <- ifelse(!is.na(matches), 
                          fixed_mfl$mfl[matches], 
                          dash_values$mfl)

###analytics data


analytics = dash_values %>% group_by(County, SubCounty, facilityname, uid_khis, mfl) %>% 
  summarise(total_attending_anc1 = sum(attending_anc1_rev, na.rm = TRUE), total_known_pos = sum(num_known_pos, na.rm = TRUE),
            total_tested_anc1 = sum(tst_anc1, na.rm = TRUE), total_tested_labour = sum(num_tst_labor, na.rm = TRUE),
            total_tested_pnc = sum(num_tested_pnc, na.rm = TRUE), total_new_pos = sum(total_pos, na.rm = TRUE),
            total_new_pos_anc1 = sum(num_pos_anc1, na.rm = TRUE),
            total_positive = total_known_pos+total_new_pos,
            total_positive_anc1 = total_known_pos + total_new_pos_anc1,
            total_known_serostatus_anc1 = total_tested_anc1 + total_known_pos,
            total_negative_anc1 = sum(num_neg_anc1, na.rm = TRUE), total_negative_labour = sum(num_neg_labor, na.rm = TRUE),
            total_negative_pnc = sum(num_neg_pnc, na.rm = TRUE),total_unk_status = sum(num_unk_status, na.rm = TRUE),
            .groups = "drop")

# Persist analytics for downstream incidence script runs in clean sessions.
analytics_out_path <- file.path(OUTPUT_DIR,"analytics_latest.rds")
# analytics_out_path <- Sys.getenv("ANC_ANALYTICS_RDS", unset = file.path("documents", "analytics_latest.rds"))
dir.create(dirname(analytics_out_path), showWarnings = FALSE, recursive = TRUE)
saveRDS(analytics, analytics_out_path)
log_message(paste("Saved analytics dataset to", analytics_out_path))

#writexl::write_xlsx(analytics, "analytics.xlsx")

##by month

dash_values$month_year = paste0(dash_values$Month, dash_values$Year)

np_monthly = dash_values %>% group_by(County, SubCounty, facilityname, uid_khis, mfl, month_year) %>%
  summarise(total_new_pos = sum(total_pos, na.rm = TRUE), .groups = "drop")

##remove county

np_monthly$County <- gsub(" County$", "", np_monthly$County)
np_monthly$SubCounty <- gsub(" Sub County$", "", np_monthly$SubCounty)

###convert month
np_monthly$month_year <- format(
  as.Date(paste0("01-", np_monthly$month_year), format = "%d-%B%Y"),
  "%Y%m"
)

###find and replace

np_monthly$County[np_monthly$County == "Muranga"] <- "Murang'a"
np_monthly$County[np_monthly$County == "Elgeyo Marakwet"] <- "Elgeyo-Marakwet"
np_monthly$County[np_monthly$County == "Tharaka Nithi"] <- "Tharaka-Nithi"

colnames(np_monthly) = c("County", "SubCounty", "facilityname", "uid_khis", "mfl", "YearMonth", 
                         "Value")

#writexl::write_xlsx(np_monthly, "np_monthly.xlsx")

#===============================================================================
# 8. EXPORT OUTPUT
#===============================================================================

safe_run(
  quote(writexl::write_xlsx(
    np_monthly,
    path = file.path(EXTERNAL_OUTPUT_DIR, "np_monthly.xlsx")
  )),
  "Write Excel Output"
)

# wide_data_monthly <- np_monthly %>%
#   # Pivot to wide format
#   pivot_wider(
#     names_from = month_year,
#     values_from = total_new_pos,
#     values_fill = 0,        # Fill NAs with 0
#     names_repair = "unique" # Handle duplicate column names if any
#   )
# 
# 
# ##order columns
# 
# order_month_year_cols <- function(df) {
# 
#   cn <- names(df)
# 
#   # Identify month–year columns
#   month_cols <- grepl("^[A-Za-z]+\\s*\\d{4}$", cn)
# 
#   # Convert column names to dates
#   parsed_dates <- as.Date(
#     paste0("1 ", cn[month_cols]),
#     format = "%d %B %Y"
#   )
# 
#   # Ordered indices of month columns
#   month_idx <- which(month_cols)
#   month_idx_ord <- month_idx[order(parsed_dates)]
# 
#   # Final column order
#   new_order <- c(
#     setdiff(seq_along(cn), month_idx),  # all other columns unchanged
#     month_idx_ord                       # reordered month columns
#   )
# 
#   df[, new_order, drop = FALSE]
# }
# 
# 
# wide_data_monthly_ordered <- order_month_year_cols(wide_data_monthly)
# 
# openxlsx::write.xlsx(wide_data_monthly_ordered, "monthly.xlsx")
# 
# # writexl::write_xlsx(wide_data_monthly_ordered,"monthly.xlsx")
# 
# names(df_ordered)
# 
# 
# # Count facilities and months
# summary_stats <- dash_values %>%
#   summarise(
#     n_facilities = n_distinct(facilityname),
#     n_months = n_distinct(paste(Year, Month)),
#     total_positives = sum(total_pos, na.rm = TRUE),
#     avg_positives_per_facility = mean(total_pos, na.rm = TRUE)
#   )
# 
# print(summary_stats)
# 
# #############################################################################
# # SAVE ANALYTICS DATASET
# #############################################################################
# 
# cat("Saving analytics dataset...\n")
# analytics_data <- cleaned_data %>%
#   select(
#     county, sub_county, facility_name, facility_id, mfl_code,
#     total_attending_anc1, total_known_pos, total_tested_anc1,
#     total_tested_labour, total_tested_pnc, total_new_pos,
#     total_positive, total_negative_anc1, total_negative_labour,
#     total_negative_pnc, total_unk_status
#   )
# 
# # Save analytics dataset
# saveRDS(analytics_data, file.path(clean_data, "anc/pmtct_hiv_testing_data_county_analytics.rds"))
# write.xlsx(
#   analytics_data,
#   file.path(clean_data, "anc/pmtct_hiv_testing_data_county_analytics_09122025.xlsx")
# )
# 
# #############################################################################
# # SAVE DASHBOARD DATASET
# #############################################################################
# 
# cat("Saving dashboard dataset...\n")
# dashboard_data <- cleaned_data %>%
#   select(
#     county, sub_county, facility_name, facility_id, mfl_code,
#     np_october_2025 = total_pos_oct25,
#     np_november_2025 = total_pos_nov25,
#     overall_np = total_new_pos
#   )
# 
# # Save dashboard dataset
# saveRDS(dashboard_data, file.path(clean_data, "anc/pmtct_hiv_testing_data_county_his.rds"))
# write.xlsx(
#   dashboard_data,
#   file.path(clean_data, "anc/pmtct_hiv_testing_data_county_his_09122025.xlsx")
# )
# 
# 
