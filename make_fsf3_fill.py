import os
import glob
import sys

outpath='/projects/niblab/data/eric_data/W1/milkshake/level3_grace_edit'
script_path='/projects/niblab/scripts/milkshake/feat_scripts'
level3_path='/projects/niblab/data/eric_data/design_files/milkshake/grace_edit/level3/'
def fill(repl_dict):
	for x in range(1,29):
		cope='cope'+str(x)
		print(cope)
		repl_dict.update({'COPE':cope})
		print(repl_dict)
		fsf_name=cope+'_risk_race.fsf'
		name=fsf_name.strip('.fsf')
		output=os.path.join(outpath,name)
		repl_dict.update({'OUTPUT':output})
		with open(os.path.join(script_path,'design3_risk_race.fsf'),'r') as infile:
                	tempfsf=infile.read()
                	for key in repl_dict:
                        	tempfsf = tempfsf.replace(key, repl_dict[key])
                        	with open(os.path.join(level3_path,fsf_name),'w') as outfile:
                                	outfile.write(tempfsf)


def main():
#globals
  repl_dict={}
#function called
  fill(repl_dict)
main()
