version <- readLines("VERSION", warn=FALSE)
date <- Sys.Date()
year <- substring(date, 1, 4)
writeLines(paste0("
@book{murrell-hdvw-", date, ",
    title = {How Data Visualisation Works},
    author = {Murrell, Paul},
    note = {Version ", version, " (build ", date, ")},
    year = ", year, ",
    publisher = {The University of Auckland}
}"),
"how-to-cite.bib")

