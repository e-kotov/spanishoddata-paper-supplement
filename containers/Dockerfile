FROM rocker/binder:4.4.2

# temporarily switch to root
# otherwise binder image will not allow us to install packages
USER root

# Install ssh server and client,
# so that this container could be remotly
# accessed when launched on an iteractive HPC node.
# also include a few convenience packages
RUN apt-get -y update && apt-get -y install \
  openssh-server \
  openssh-client \
  wget \
  gnupg \
  git \
  htop \
  screen \
  tree \
  nano \
  --no-install-recommends && \
  apt-get clean && \
  rm -rf /var/lib/apt/lists/*

# Install Chrome, required for creating png snapshots of html diagrams.
RUN wget -q -O - https://dl-ssl.google.com/linux/linux_signing_key.pub | apt-key add - \ 
    && echo "deb http://dl.google.com/linux/chrome/deb/ stable main" >> /etc/apt/sources.list.d/google.list
RUN apt-get update && apt-get -y install google-chrome-stable
# also see https://github.com/rstudio/chromote/issues/180#issue-2537005943

# switch back to rstudio user
USER rstudio

# create temporary project directory
RUN mkdir -p /home/rstudio/project && \ 
  chown -R rstudio:rstudio /home/rstudio/project
WORKDIR /home/rstudio/project

# copy files required for renv
COPY --chown=rstudio:rstudio ../renv.lock renv.lock
RUN mkdir -p renv
COPY --chown=rstudio:rstudio ../.Rprofile .Rprofile
COPY --chown=rstudio:rstudio ../renv/activate.R renv/activate.R
COPY --chown=rstudio:rstudio ../renv/settings.json renv/settings.json

# copy sources of non-released fix of osfr package
# that supports pulling files from view only osf.io links
COPY --chown=rstudio:rstudio ../rpkg rpkg

# install R packages straight into the system library
# this prevents dependency on repositories being online
# and saves time later for package installation
RUN R -e "renv::restore(library = '/usr/local/lib/R/site-library')"

# remove the temporary project directory

# cleanup
WORKDIR /home/rstudio
RUN chmod -R u+w /home/rstudio/project && rm -rf /home/rstudio/project

USER root
RUN rm -rf /var/lib/apt/lists/*
RUN rm -rf /tmp/downloaded_packages

USER rstudio

EXPOSE 8888
CMD ["jupyter", "lab", "--ip", "0.0.0.0", "--no-browser"]
