FROM rocker/tidyverse:4.0.3
RUN install2.r rsconnect shiny reactable remotes htmltools Rcpp openxlsx
RUN Rscript -e "remotes::install_github('mitchelloharawild/icons')"
WORKDIR /home/abda-test
COPY app.R app.R
COPY exportExcel.R exportExcel.R
COPY logo_stzh_stat_sw_pos_1.png logo_stzh_stat_sw_pos_1.png
COPY icons/calendar.svg icons/calendar.svg
COPY prepareData.R prepareData.R
COPY sszDownload.R sszDownload.R
COPY sszTheme.css sszTheme.css
COPY Titelblatt.xlsx Titelblatt.xlsx
COPY deploy.R deploy.R
CMD Rscript deploy.R