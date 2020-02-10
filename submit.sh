#!/bin/bash -l


# Hard coded settings for resources
# time limit
export ttime=4:00
# memory per job
export mem_per_gpu=10000

export JOB_NAME='gies_test'

# load r
module load new gmp mpfr r/3.6.0     

# bsub -J $JOB_NAME -n 1 -W $ttime -R "rusage[mem=$mem_per_gpu]" \
# "R CMD BATCH --no-restore --no-save $HOME/code/scl_replicate/experiments/experiments_vary_p_gies.R out_gies.Rout" 

# bsub -J $JOB_NAME -n 1 -W $ttime -R "rusage[mem=$mem_per_gpu]" \
# "R CMD BATCH --no-restore --no-save $HOME/code/scl_replicate/experiments/experiments_vary_p_LITERATURE.R out_LIT.Rout" 

bsub -J $JOB_NAME -n 1 -W $ttime -R "rusage[mem=$mem_per_gpu]" \
"R CMD BATCH --no-restore --no-save $HOME/code/scl_replicate/experiments/experiments_vary_p_NEW.R out_NEW.Rout" 