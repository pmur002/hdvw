
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
RUN Rscript -e 'install.packages("devtools", repos="https://cran.rstudio.com/")'
RUN Rscript -e 'library(devtools); install_version("png", "0.1.8", repos="https://cran.rstudio.com/")'
RUN Rscript -e 'library(devtools); install_version("dplyr", "1.1.4", repos="https://cran.rstudio.com/")'
RUN Rscript -e 'library(devtools); install_version("reshape2", "1.4.4", repos="https://cran.rstudio.com/")'
RUN Rscript -e 'library(devtools); install_version("tidyr", "1.3.1", repos="https://cran.rstudio.com/")'
RUN Rscript -e 'library(devtools); install_version("knitr", "1.50", repos="https://cran.rstudio.com/")'
RUN Rscript -e 'library(devtools); install_version("kableExtra", "1.4.0", repos="https://cran.rstudio.com/")'
RUN Rscript -e 'library(devtools); install_version("ggplot2", "4.0.1", repos="https://cran.rstudio.com/")'
RUN Rscript -e 'library(devtools); install_version("gggrid", "0.2.0", repos="https://cran.rstudio.com/")'
RUN Rscript -e 'library(devtools); install_version("scales", "1.4.0", repos="https://cran.rstudio.com/")'
RUN Rscript -e 'library(devtools); install_version("colorspace", "2.1.2", repos="https://cran.rstudio.com/")'
RUN Rscript -e 'library(devtools); install_version("rcartocolor", "2.1.1", repos="https://cran.rstudio.com/")'
RUN Rscript -e 'library(devtools); install_version("ggsci", "3.2.0", repos="https://cran.rstudio.com/")'
RUN Rscript -e 'library(devtools); install_version("GGally", "2.4.0", repos="https://cran.rstudio.com/")'
RUN Rscript -e 'library(devtools); install_version("ggforce", "0.5.0", repos="https://cran.rstudio.com/")'
RUN Rscript -e 'library(devtools); install_version("ggChernoff", "0.3.0", repos="https://cran.rstudio.com/")'
RUN apt-get update && apt-get install -y \
    libmagick++-dev
RUN Rscript -e 'library(devtools); install_version("ggimage", "0.3.3", repos="https://cran.rstudio.com/")'
RUN Rscript -e 'library(devtools); install_version("ggh4x", "0.3.1", repos="https://cran.rstudio.com/")'
RUN apt-get update && apt-get install -y \
    gfortran \
    cmake \
    libudunits2-dev \
    libabsl-dev \
    libgdal-dev
RUN Rscript -e 'library(devtools); install_version("sf", "1.0.21", repos="https://cran.rstudio.com/")'
RUN Rscript -e 'library(devtools); install_version("grImport", "0.9.7", repos="https://cran.rstudio.com/")'
RUN Rscript -e 'library(devtools); install_version("grImport2", "0.3.3", repos="https://cran.rstudio.com/")'
RUN Rscript -e 'library(devtools); install_version("rsvg", "2.6.2", repos="https://cran.rstudio.com/")'
RUN Rscript -e 'library(devtools); install_version("ggtext", "0.1.2", repos="https://cran.rstudio.com/")'
RUN Rscript -e 'library(devtools); install_version("xdvir", "0.1.3", repos="https://cran.rstudio.com/")'
RUN Rscript -e 'library(devtools); install_version("vwline", "0.2.4", repos="https://cran.rstudio.com/")'
RUN Rscript -e 'library(devtools); install_version("gridGeometry", "0.4.0", repos="https://cran.rstudio.com/")'
RUN Rscript -e 'library(devtools); install_version("gridtext", "0.1.5", repos="https://cran.rstudio.com/")'
RUN Rscript -e 'library(devtools); install_version("rayshader", "0.37.3", repos="https://cran.rstudio.com/")'
RUN Rscript -e 'library(devtools); install_version("rgl", "1.3.24", repos="https://cran.rstudio.com/")'
RUN Rscript -e 'library(devtools); install_version("latticeExtra", "0.6.31", repos="https://cran.rstudio.com/")'
RUN Rscript -e 'library(devtools); install_version("lvplot", "0.2.2", repos="https://cran.rstudio.com/")'
RUN Rscript -e 'library(devtools); install_version("ggokabeito", "0.1.0", repos="https://cran.rstudio.com/")'
RUN Rscript -e 'library(devtools); install_version("bullseye", "1.0.1", repos="https://cran.rstudio.com/")'

COPY drewcurves_1.0.tar.gz /tmp/
RUN R CMD INSTALL /tmp/drewcurves_1.0.tar.gz

RUN apt-get install -y locales && locale-gen en_US.UTF-8
ENV LANG en_US.UTF-8

ENV TERM dumb

