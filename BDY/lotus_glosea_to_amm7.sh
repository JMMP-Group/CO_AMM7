#!/bin/bash
#SBATCH --partition=short-serial
#SBATCH --mem=20000
#SBATCH -o LOGS/%A_%a.out
#SBATCH -e LOGS/%A_%a.err
#SBATCH --time=02:30:00
#SBATCH --ntasks=1
module add jaspy
source activate pybdy

export JAVA_HOME=/usr/lib/jvm/java-1.8.0-openjdk-1.8.0.412.b08-1.el7_9.x86_64/jre/
export JVM_PATH=/usr/lib/jvm/java-1.8.0-openjdk-1.8.0.412.b08-1.el7_9.x86_64/jre/lib/amd64/server/libjvm.so
for month in  02 03 04 05 06 07 08 09 10 11 12; do
	#month=04
	year=1994

	export TARGET_DIR=/home/users/jelt/GitHub/pyBDY/inputs/SYMLINK_DIR/${year}/${month}/
	export SOURCE_DIR=/gws/nopw/j04/jmmp/MASS/GloSea6/Daily/
	filebase="glosea6_grid_" # e.g. glosea6_grid_V_19930101.nc

	# Clear TARGET_DIR
	mkdir -p $TARGET_DIR
	#rm -f $TARGET_DIR/*

	# Find new files
	for grd in T U V; do
	for file in $SOURCE_DIR/$filebase${grd}_${year}${month}* ; do
	  ln -s $file $TARGET_DIR/.
	done
	done

	# Prepare namelist*bdy file
	namelist_template="/home/users/jelt/GitHub/pyBDY/inputs/namelist_local_glosea6_template.bdy"
	namelist="/home/users/jelt/GitHub/pyBDY/inputs/namelist_local_glosea6_"${year}${month}".bdy"

	# ncml file is referenced in the namelist
	ncml_template="/home/users/jelt/GitHub/pyBDY/inputs/src_data_local_glosea6_TEMPLATE_regexp.ncml"
	ncml="/home/users/jelt/GitHub/pyBDY/inputs/src_data_local_glosea6_"${year}${month}"_regexp.ncml"

	sed -e "s/__YEAR__/${year}/g" -e "s/__MONTH__/${month}/g" $namelist_template > $namelist
	sed -e "s/__YEAR__/${year}/g" -e "s/__MONTH__/${month}/g" $ncml_template > $ncml


	# pass integer year and month
	pybdy -s ${namelist}
done
