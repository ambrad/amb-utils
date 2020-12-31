exe="bash /home/ambrad/bin/limcpu.sh hy"
$exe cov19.hy p1 png
$exe cov19.hy p1a png
#hy cov19.hy p1b png
$exe cov19.hy plot-county-data png
$exe cov19.hy glance png
ls -ltrh covid19/
