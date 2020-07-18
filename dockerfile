FROM opencpu/ubuntu-18.04 


RUN Rscript -e "install.packages(c('devtools','dplyr','purrr','httr','RSQLite'))"

ARG rstudio_pass

#change default password for user: opencpu
RUN \
  echo "opencpu:$rstudio_pass" | chpasswd

#allow DB access from package 
RUN mkdir ctsgetr
RUN echo -e "/ctsgetr/ rw,\n/ctsgetr/** rwkmix,\n" >> /etc/apparmor.d/opencpu.d/custom
  


COPY ./ ./ctsgetr
RUN Rscript -e "devtools::install('ctsgetr',upgrade='never')"