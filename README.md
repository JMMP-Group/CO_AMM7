# CO_AMM7
CO AMM7 configuration, currently set up for testing various vertical coordinate options

## Quick Start:

```
git clone git@github.com:JMMP-Group/CO_AMM7.git
./CO_AMM7/scripts/setup/amm7_setup_archer -w $PWD/test -x $PWD/test -s $PWD/CO_AMM7
cd test/nemo/cfgs/AMM7/EXP00
```
Edit the project code in  `runscript.pbs` then:
```
qsub -q short runscript.pbs
```

## ENSEMBLE Runs:

*as above, but after installation*:

```
cd test/nemo/cfgs/AMM7/ENSEMBLE_CONTROL
```
Edit the `ensemble_hpg.pbs` script to add/remove model ensemble members from the list, then:
```
qsub ensemble_hpg.pbs
```
NB. If you alter the number of ensemble members you will have to change the number of cores requested, e.g. `#PBS -l select=21` for 3 members etc.

For reference, a list of current ensemble members can be found [here](https://github.com/JMMP-Group/AMM7_zgr/blob/master/scripts/setup/model_list)

### Forcing data:

[JMMP-COLLAB](http://gws-access.ceda.ac.uk/public/jmmp_collab/)
