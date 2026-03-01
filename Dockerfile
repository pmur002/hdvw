
# Base image
FROM ubuntu:22.04
MAINTAINER Paul Murrell <paul@stat.auckland.ac.nz>

# Install R 
# https://cran.stat.auckland.ac.nz/bin/linux/ubuntu
# update indices
RUN apt update -qq
# install helper packages we need
RUN apt install -y --no-install-recommends software-properties-common dirmngr
RUN apt install -y --no-install-recommends wget
# add the signing key (by Michael Rutter) for these repos
# To verify key, run gpg --show-keys /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc 
# Fingerprint: E298A3A825C0D65DFD57CBB651716619E084DAB9
RUN wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc
# add the repo from CRAN -- lsb_release adjusts to 'noble' or 'jammy' or ... as needed
RUN add-apt-repository "deb https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/"
# install R itself
RUN apt install -y --no-install-recommends r-base

# For building packages from source
RUN apt install -y --no-install-recommends build-essential \
    xsltproc \
    libxml2-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    bibtex2html \
    subversion 
RUN apt install -y --no-install-recommends \
    libz-dev \
    libfontconfig1-dev \
    libfreetype-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev

# For building the report
RUN apt-get update && apt-get install -y \
    texlive-full \
    librsvg2-bin
RUN apt-get update && apt-get install -y \
    bibtex2html \
    w3m
RUN wget https://github.com/quarto-dev/quarto-cli/releases/download/v1.7.32/quarto-1.7.32-linux-amd64.deb
RUN dpkg -i quarto-1.7.32-linux-amd64.deb
RUN Rscript -e 'install.packages("quarto", repos="https://cran.rstudio.com/")'

# Packages used in the report
RUN apt-get update && apt-get install -y \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libwebp-dev
RUN apt-get update && apt-get install -y \
    libmagick++-dev
RUN apt-get update && apt-get install -y \
    gfortran \
    cmake \
    libudunits2-dev \
    libabsl-dev \
    libgdal-dev
# Use {renv} to restore from local renv.lock
RUN Rscript -e 'install.packages("renv", repos="https://cran.rstudio.com/")'
COPY renv.lock .
RUN Rscript -e 'renv::restore()'

# For snapshots of rgl plots in PDF output
# Use google-chrome instead of chromium because the latter requires snap 
# and that does not work within Docker container blah blah blah
# RUN apt-get update && apt-get install -y \
#     chromium-browser
# Froze "current" google-chrome on 2026-01-07
# RUN wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb
COPY google-chrome-stable_143.0.7499.192_amd64.deb /tmp/
RUN apt-get update && apt-get install -y \
    fonts-liberation \
    libasound2 \
    libvulkan1
RUN dpkg -i /tmp/google-chrome-stable_143.0.7499.192_amd64.deb
ENV RGL_USE_NULL=true

RUN apt-get install -y locales && locale-gen en_US.UTF-8
ENV LANG=en_US.UTF-8

ENV TERM=dumb

