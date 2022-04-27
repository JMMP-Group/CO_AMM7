## CO AMM7 (Coastal Ocean Atlantic Margin Model 7km) 
[![DOI](https://zenodo.org/badge/235544712.svg)](https://zenodo.org/badge/latestdoi/235544712)

The Joint Marine Modelling Programme [(JMMP)](https://www.metoffice.gov.uk/research/approach/collaboration/joint-marine-modelling-programme) provides world-class and easily accessible national capability, ocean modelling infrastructure and configurations to support the UK’s scientific research and operational prediction systems for ocean, weather and climate. It is partnership between the Met Office and British Antarctic Survey, National Oceanography Centre and Centre for Polar Observation and Modelling.

Model configurations are underpinned by the Nucleus for European Modelling of the Ocean [(NEMO)](https://www.nemo-ocean.eu) framework. JMMP works closely with the NEMO consortium to develop the underpinning model capability. 

---

_CO AMM7 configuration, currently set up for testing various vertical coordinate options_

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
