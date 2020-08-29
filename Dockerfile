FROM opencpu/ubuntu-18.04 


RUN Rscript -e "install.packages(c('devtools','tidyr','dplyr','purrr','httr','RSQLite'))"


#allow DB access from package 
RUN mkdir ctsgetr
RUN echo -e "/ctsgetr/ rw,\n/ctsgetr/** rwkmix,\n" >> /etc/apparmor.d/opencpu.d/custom
  

COPY ./ ./ctsgetr
RUN Rscript -e "devtools::install('ctsgetr',upgrade='never')"
