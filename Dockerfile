FROM ghcr.io/e-kotov/spanishoddata-paper-supplement:4.4.2

# Copy everything to /home/rstudio
COPY --chown=rstudio . /home/rstudio

# Remove the files related to renv, so that renv does not take over the project
# We have (hopefully) all the packages already installed and burned into the container image
RUN rm -rf /home/rstudio/renv \
           /home/rstudio/renv.lock \
           /home/rstudio/.Rprofile
