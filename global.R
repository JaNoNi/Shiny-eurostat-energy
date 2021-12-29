# INSTALL DEPENDENCIES ---------------------------------------------------------

source('dependencies.R')
# load all packages
lapply(required_packages, require, character.only = TRUE)

# Load env variables -----------------------------------------------------------
readRenviron(".env")
db_user <- Sys.getenv('MYSQL_USER')
db_password <- Sys.getenv('MYSQL_PASSWORD')
db_host <- Sys.getenv('MYSQL_HOST')
db_name <- Sys.getenv('MYSQL_DB')

# Extract port from db_host if present,
# otherwise use DB_PORT environment variable.
host_args <- str_split(db_host, pattern = ":")
if (lengths(host_args) == 1) {
  db_hostname <- db_host
  db_port <- Sys.getenv["DB_PORT"]
} else {
  db_hostname <- host_args[[1]][1]
  db_port <- host_args[[1]][2]
}

# Helper for getting new connection to SQL Server ------------------------------
getSqlConnection <- function(){
  con <-
    dbConnect(
      RMySQL::MySQL(),
      username = db_user,
      password = db_password,
      host = db_hostname,
      port = db_port %>% as.integer(),
      dbname = db_name
    )
  return(con)
}

# Function for getting data ----------------------------------------------------
getData <- function(eurostatcode){
  data <- tryCatch({
    # Downloads data from my SQL Server.
    # The Eurostat API is much too slow. Therefore I'm saving all the required
    # data on my own Server, which allows faster data access.
    conn <- getSqlConnection()
    query <- paste("select * from ", eurostatcode, sep = "")
    res   <- dbSendQuery(conn, query)
    data  <- dbFetch(res, n = -1)
    data$time <- as.Date(data$time)
    dbDisconnect(conn)
    data
  },
  error=function(cond) {
    # Should an error with the connection occur or the data is not on my server
    # the Eurostat API is called.
    data <- get_eurostat(eurostatcode)
    data <- label_eurostat(data, code = c("geo"))
    path <- paste("data/", eurostatcode, sep = "")
    write_csv(data, path)
    message("Here's the original error message:")
    message(cond)
    data
  })
  return(data)
}

data_list <- c("nrg_ind_ren", # Share of energy from renewable sources
               "nrg_ind_id", # Energy imports dependency
               "nrg_bal_c", 
               # Final energy consumption by product
               # Final energy consumption by sector
               # Final energy consumption in households by fuel type
               # Final energy consumption in industry by fuel type
               # Final energy consumption in transport by fuel type
               # Final energy consumption in services by fuel type
               "ilc_mdes01") # Population unable to keep home adequately warm

# Load Data
nrg_ind_ren <- getData("nrg_ind_ren")
nrg_ind_id <- getData("nrg_ind_id")
#nrg_bal_c <- getData("nrg_bal_c")
ilc_mdes01 <- getData("ilc_mdes01")

df <- data.frame(code = c("EU27_2020", "EA19"),
                 name = c("European Union - 27 countries (from 2020",
                          "Euro area - 19 countries  (from 2015)"),
                 label = c("European Union - 27 countries (from 2020",
                           "Euro area - 19 countries  (from 2015)"))
eu_country_label <- rbind(df, eu_countries %>% arrange(name))


# d12 <- nrg_ind_id %>% filter(geo_code == "BE")
# d12_siec <- d12 %>% distinct(siec) %>% arrange(siec)
# buttonlist <- vector("list", nrow(d12_siec))
# for (row in 1:nrow(d12_siec)) {
#   visiblelist <- rep(F, nrow(d12_siec))
#   visiblelist[row] <- T
#   buttonlist[[row]] <- list(method="restyle",
#                           args=list("visible", visiblelist),
#                           label=d12_siec[[1]][row])
#  }
# visiblelist[nrow(d12_siec)]
# nrow(d12_siec)-1
