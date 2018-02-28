#!/usr/bin/env python
import glob
import subprocess as sub
import os,sys
from datetime import datetime
#mcflirtdefault='-plots -sinc_final'
#'betfunc':"'bet %s/bold_mcf.nii.gz %s/bold_mcf_brain.nii.gz -F
#'bet %s %s -f 0.3 -R,

basedir='/projects/niblab/data/eric_data/W1/subjects/'

os.chdir(basedir)

for nifti in glob.glob('cs*/BOLD/*.nii.gz'):
	print("Starting BET on "+nifti)
	INPUT=os.path.join(basedir,nifti)
	output=INPUT.strip('.nii.gz')
	BET_OUTPUT=os.path.join(basedir,output+'_brain')
	print(BET_OUTPUT)
	#subprocess.call(["/usr/share/Modules/software/RHEL-6.5/fsl/5.0.9/bin/bet",INPUT,OUTPUT,"-F"])
