FROM r-base

ENV HOME /root
ENV APP_HOME /application

# create our work directory
RUN mkdir -p $APP_HOME
WORKDIR $APP_HOME

# create directory for images
RUN mkdir $APP_HOME/images

# copy dependencies script and twitter script
COPY ./rscripts $APP_HOME

# install dependencies
RUN apt-get update && apt-get install -y\
    build-essential \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev

# install packages
RUN Rscript $APP_HOME/packages.R
