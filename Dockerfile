FROM ghcr.io/e-kotov/spanishoddata-paper-supplement:4.4.2

# Copy everything to /home/rstudio
COPY --chown=rstudio . /home/rstudio

# Remove the files related to renv, so that renv does not take over the project
# We have (hopefully) all the packages already installed and burned into the container image
# Rename .Rprofile-binder to .Rprofile to disable renv and print suggestions in console
RUN rm -rf /home/rstudio/renv \
           /home/rstudio/renv.lock \
           /home/rstudio/.Rprofile \
    && mv /home/rstudio/.Rprofile-binder /home/rstudio/.Rprofile \
    && chown rstudio:rstudio /home/rstudio/.Rprofile
