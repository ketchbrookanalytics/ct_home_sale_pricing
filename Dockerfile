FROM eddelbuettel/r2u:22.04

# Install {remotes}
RUN install2.r remotes

# Create a temporary directory to build the package
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone

# Install our package
RUN R -e 'remotes::install_local(upgrade="never")'

RUN R -e 'install.packages("languageserver")'
RUN R -e 'install.packages("httpgd")'

# Once installed, remove the temporary directory
RUN rm -rf /build_zone
