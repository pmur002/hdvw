
pkgs <- readLines("packages.txt")
invisible(lapply(pkgs, library, character.only=TRUE))
