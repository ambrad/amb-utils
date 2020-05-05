str=`echo $RANDOM`
countries="BEL+BRA+CAN+FRA+DEU+IND+ITA+JPN+MEX+NLD+NOR+RUS+SGP+ESP+SWE+CHE+TUR+GBR+USA+OWID_WRL"
sed -e s/STRING/$str/ -e s/COUNTRIES/$countries/ covid19.html.template > covid19.html
