# AMM7_zgr
AMM7 configuration for testing various vertical coordinate options

##Quick Start:

```
git clone git@github.com:JMMP-Group/AMM7_zgr.git
./AMM7_zgr/scripts/setup/amm7_setup_archer -w $PWD/test -x $PWD/test -s $PWD/AMM7_zgr
cd test/nemo/cfgs/AMM7/EXP00
```
Edit the project code in  `runscript.pbs` then:
```
qsub -q short runscript.pbs
```

~to follow~: setting up the ensemble experiments
