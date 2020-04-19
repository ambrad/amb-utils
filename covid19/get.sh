mv daily.json daily.json.tmp; wget https://covidtracking.com/api/v1/states/daily.json
head -c 600 daily.json
echo
diff daily.json daily.json.tmp | wc
