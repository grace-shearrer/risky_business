import sys
import os
from subprocess import check_output
import fnmatch
import glob

path='/projects/niblab/data/eric_data/W1/milkshake/level1_grace_edit/'
outpath='/projects/niblab/data/eric_data/W1/milkshake/level2_grace_edit/'
script_path='/projects/niblab/scripts/milkshake/feat_scripts'
level2_path='/projects/niblab/data/eric_data/design_files/milkshake/grace_edit/level2'
def test(repl_dict):
  os.chdir(path)
  for dir in glob.glob('cs*.feat'):
	sub0=dir.split('.')
	sub1=sub0[0].split('_')
	sub=sub1[0]
	pattern=sub+'*.feat'
	allfiles=os.listdir('.')
	files=fnmatch.filter(allfiles, pattern)
	featdir1=os.path.join(path,files[0])
	featdir2=os.path.join(path,files[1])
    	repl_dict.update({'FEATDIR1':featdir1})
    	repl_dict.update({'FEATDIR2':featdir2})
    	print(repl_dict)
	output=os.path.join(outpath,sub)
    	repl_dict.update({'OUTPUT':output})
	fsf_name=sub+'_level2.fsf'
    	with open(os.path.join(script_path,'design2.fsf'),'r') as infile:
      		tempfsf=infile.read()
      		for key in repl_dict:
        		tempfsf = tempfsf.replace(key, repl_dict[key])
        		with open(os.path.join(level2_path,fsf_name),'w') as outfile:
          			outfile.write(tempfsf)
          			#os.chdir(os.path.join())

def main():
#globals
  repl_dict={}
#function called
  test(repl_dict)
main()
