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


pybdy -s namelist_local_glosea6.bdy
