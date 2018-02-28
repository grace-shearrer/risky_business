import os
import glob
import sys
import fnmatch

basedir='/projects/niblab/scripts/milkshake'
#list_name=os.path.join(basedir, 'feat_scripts', 'list_files')
slope_name=os.path.join(basedir,'race.txt')
#set fmri(evg3.3) -0.54
#3 is the intercept
#4 is the slopei
g=open('race_input.txt','w')
h=open('race_cov.txt','w')
j=open('no_cov.txt','w')
k=open('risk_cov.txt','w')
i=0 
for dir in glob.glob('/projects/niblab/data/eric_data/W1/milkshake/level2_grace_edit/cs*.gfeat'):
	sub0=dir.split('/')
	sub=sub0[8].strip('.gfeat')
	with open(slope_name, 'r') as search2:
		for line2 in search2:
			line2 = line2.split('\t')
			name2 = line2[0]
#			print(name2)
			race = line2[1]
			race = race.strip('\n')
#			print(risk)
			risk = line2[3]
			risk = risk.strip('\n')
			norisk = line2[2]
			norisk = norisk.strip('\n')
			print(name2+' is '+race+' race, column 1 is '+norisk+' column 2 is '+risk)
			if fnmatch.fnmatch(name2, sub):
				#print(name2)
				#print(sub)
				#print(line2)
				i=i+1
#				print('set feat_files('+str(i)+') '+'"'+dir+'/COPE.feat"')
				g.write('set feat_files('+str(i)+') '+'"/projects/niblab/data/eric_data/W1/milkshake/level2_grace_edit/'+name2+'.gfeat/COPE.feat"'+'\n')
				h.write('set fmri(evg'+str(i)+'.3) '+race+'\n')  
				j.write('set fmri(evg'+str(i)+'.1) '+norisk+'\n')  
				k.write('set fmri(evg'+str(i)+'.2) '+risk+'\n')  
#				print('set fmri(evg'+str(i)+'.2) '+risk[0])
				#print('set fmri(evg'+str(i)+'.3) '+line2[4])
				#print('set fmri(evg3.'+str(i)+') '+line2[4])
				#print('set fmri(evg1.'+str(i)+') 1')
g.close()
h.close()
