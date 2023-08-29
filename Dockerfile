FROM rocker/tidyverse:4.2.1
RUN install2.r shiny remotes htmltools Rcpp openxlsx reactable shyinycssloaders
RUN Rscript -e "remotes::install_github('mitchelloharawild/icons')"
RUN Rscript -e "remotes::install_github('StatistikStadtZuerich/zuericssstyle')"
RUN Rscript -e "remotes::install_github('StatistikStadtZuerich/zuericolors')"
RUN Rscript -e "remotes::install_github('daattali/shinycssloaders')" # install from github as otherwise not available for this R version
RUN Rscript -e "remotes::install_github('rstudio/rsconnect')" # make sure we have latest version here as well despite old R version, otherwise not accepted by shinyapps.io
WORKDIR /home/abda-test
COPY app.R app.R
COPY R/get_data.R R/get_data.R
COPY R/get_main_reactable.R R/get_main_reactable.R
COPY R/get_second_reactable.R R/get_second_reactable.R
COPY R/ssz_download_excel.R R/ssz_download_excel.R
COPY www/logo_stzh_stat_sw_pos_1.png www/logo_stzh_stat_sw_pos_1.png
COPY www/icons/calendar.svg www/icons/calendar.svg
COPY www/icons/external-link.svg www/icons/external-link.svg
COPY www/icons/download.svg www/icons/download.svg
COPY www/sszThemeShiny.css www/sszThemeShiny.css
COPY www/ABDATheme.css www/ABDATheme.css
COPY www/Titelblatt.xlsx www/Titelblatt.xlsx
COPY deploy.R deploy.R
CMD Rscript deploy.R