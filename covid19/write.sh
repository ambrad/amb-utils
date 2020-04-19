str=`echo $RANDOM`
countries="BEL+BRA+CAN+FRA+DEU+ITA+MEX+NLD+NOR+ESP+SWE+CHE+TUR+GBR+USA+OWID_WRL"
sed -e s/STRING/$str/ -e s/COUNTRIES/$countries/ covid19.html.template > covid19.html
