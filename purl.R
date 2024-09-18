
master <- readLines("how-data-vis-works.qmd")

includes <- grep("{{< include ", master, fixed=TRUE)

getInclude <- function(i) {
    include <- gsub("[{][{][<] include | [>][}][}]", "", master[i])
    readLines(include)
}

includesContent <- lapply(includes, getInclude)

modified <- c(master[1:(includes[1] - 1)],
              unlist(includesContent),
              master[(includes[length(includes)] + 1):length(master)])

writeLines(modified, "how-data-vis-works-full.qmd")

knitr::purl("how-data-vis-works-full.qmd", "how-data-vis-works.R")
