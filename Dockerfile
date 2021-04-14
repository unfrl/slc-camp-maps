FROM plotly/heroku-docker-r:3.6.3_heroku18

COPY ./apt-packages /app/

# look for /app/apt-packages and if it exists, install the packages contained
RUN if [ -f '/app/apt-packages' ]; then apt-get update -q && cat apt-packages | xargs apt-get -qy install && rm -rf /var/lib/apt/lists/*; fi;              

COPY ./init.R /app/

# look for /app/init.R and if it exists, execute it
RUN if [ -f '/app/init.R' ]; then /usr/bin/R --no-init-file --no-save --quiet --slave -f /app/init.R; fi; 

COPY . /app/

# here app.R needs to match the name of the file which contains your app              
CMD cd /app && /usr/bin/R --no-save -f /app/camp_dash.R