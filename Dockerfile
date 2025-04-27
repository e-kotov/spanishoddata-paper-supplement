FROM ghcr.io/e-kotov/spanishoddata-paper-supplement:4.4.2

# Copy everything to /home/rstudio
COPY --chown=rstudio . /home/rstudio
