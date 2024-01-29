 '''
 # @ Author: feng
 # @ Create Time: 2022-10-18 10:52:42
 # @ Modified by: feng
 # @ Modified time: 2022-10-18 10:53:55
 # @ Description: Submit grid search of xgboost.
 '''

import os
from os.path import join as opj
from glob import glob
import pandas as pd
import re
import time
import subprocess
import logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s: %(message)s'
)

def func_JobNumber(serverName):
    cmd = f'qstat | grep {serverName}'
    return_cmd = os.popen(cmd).readlines()
    return len(return_cmd)

def func_JobIsExist(jobName):
    cmd = f'qstat | grep {jobName}'
    return_cmd = os.popen(cmd).readlines()
    if len(return_cmd) == 0:
        return False
    else:
        return True

def func_submit(projPath, rScriptPath, wkPath, dsPath, idxResample, idxGridSearch, resPath, senvPath, jobName):
    cli = [
        '#! /bin/bash\n',
        'set -ex\n',
        'export PATH=/usr/local/singularity-2.4.6/bin:$PATH\n',
        f'mv {wkPath}/{dsPath}/{resPath}/{jobName}.submited {wkPath}/{dsPath}/{resPath}/{jobName}.running\n',
        f'singularity exec -e {senvPath} Rscript {rScriptPath} {wkPath} {dsPath} {idxResample} {idxGridSearch} {resPath}\n',
        f'mv {wkPath}/{dsPath}/{resPath}/{jobName}.running {wkPath}/{dsPath}/{resPath}/{jobName}.finished\n',
        'echo "Done."\n'
    ]
    
    cli = ''.join(cli)
    # logging.info(cli)
    try:
        p = subprocess.run('bash', input=cli, encoding='utf-8', shell=True, check=True, stdout=subprocess.PIPE)
        logging.info(p.stdout)
    except subprocess.CalledProcessError as err:
        logging.error('Error: ', err)
        
if __name__ == '__main__':
    proj = '/brain/babri_in/sangf/Projects/S_task-SexDifferences'
    work = opj(proj, 'work')
    
    senvPath = '/brain/babri_in/sangf/Envs/renv.simg'
    jobPrefix = 'xgb'
    resPostfix = 'xgboost_GridSearch_v2'
    rScriptPath = opj(proj, 'analysis', 'a02_desc-hcpmmp', '_code', 'core_GridSearch.R')
    
    params = pd.read_csv(opj(work, 'xgboost_hyperparameters_v2.csv'), header=0)
    nParams = len(params)
    
    for i in sorted(glob(opj(work, 'thickness_rodelay_Male', 'dtrain*.csv'))):
        tmpDataSet, tmpResampleName = os.path.split(i)
        tmpDataSetName = os.path.split(tmpDataSet)[-1]

        tmpIdxResample = int(re.sub(r'\D', '', tmpResampleName))
        tmpMea, tmpCog, tmpSex = tmpDataSetName.split('_')
        
        tmpResPath = opj(tmpDataSet, resPostfix)
        if not os.path.exists(tmpResPath):
            os.makedirs(tmpResPath)
                    
        for j in range(nParams):
            # tmpJobName = f'{jobPrefix}_{tmpMea.upper()[0:1]}{tmpCog.upper()[0:1]}{tmpSex.upper()[0:1]}_r{tmpIdxResample}_g{j+1}'
            tmpJobName = f'{tmpMea.upper()[0:1]}{tmpCog.upper()[0:1]}{tmpSex.upper()[0:1]}r{tmpIdxResample}g{j+1}t'
            
            if (os.path.exists(opj(tmpResPath, 'ResultCvModel_resample-%06d_GridSearch-%06d.csv' % (tmpIdxResample, j + 1))) or 
                os.path.exists(opj(tmpResPath, tmpJobName + '.running')) or
                os.path.exists(opj(tmpResPath, tmpJobName + '.finished')) or
                os.path.exists(opj(tmpResPath, tmpJobName + '.submited'))):
                continue
            
            logging.info(tmpJobName)
            with open(opj(tmpResPath, tmpJobName + '.submited'), 'w') as f:
                f.writelines('')
            func_submit(
                projPath=proj,
                rScriptPath=rScriptPath,
                wkPath=work,
                dsPath=tmpDataSetName,
                idxResample=tmpIdxResample,
                idxGridSearch=j+1,
                resPath=resPostfix,
                senvPath=senvPath,
                jobName=tmpJobName
            )
            time.sleep(2)
