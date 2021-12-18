FROM opencpu/ubuntu-18.04 

#install packages
RUN apt-get update && apt-get install -y\
  libxml2-dev 

RUN R -e "install.packages(c('stringi','dplyr','devtools','purrr','httr','RSQLite','tidyr','shiny'))"

ARG rstudio_pass
#change default password for user: opencpu
RUN echo "opencpu:${rstudio_pass}" | chpasswd

#allow DB access from package 
RUN mkdir /ctsgetr
WORKDIR /ctsgetr
COPY . ./

RUN echo "/root/ctsgetr/ rw,\n/root/ctsgetr/** rwkmix,\n" > /etc/apparmor.d/opencpu.d/custom
  
RUN Rscript -e "devtools::install('.',upgrade='never')"

RUN echo "opencpu:${rstudio_pass}" | chpasswd