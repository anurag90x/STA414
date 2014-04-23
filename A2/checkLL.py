def checkLL():
	fileO = open("testoutput2","r")
	withoutP = "Without Penalty "
	withP = "With Penalty"
	withoutPList=[]
	withPList=[]
	withoutPF = False
	withPF = False
	for line in fileO:
		if line.find(withoutP)!=-1:
			withoutPF = True
			withPF = False
			continue
		if line.find(withP)!=-1:
			withoutPF = False
			withPF = True
			continue
		if line.find("[1]")!=-1:
			
			if withoutPF:
				tokens = line.split(" ")
				if len(tokens)>1:
					#print tokens[1].strip("\n")
					withoutPList.append(float((tokens[1]).strip("\n")))
			
			if withPF:
				tokens = line.split(" ")
				if len(tokens)>1:
					#print tokens[1].strip("\n")
					withPList.append(float((tokens[1]).strip("\n")))
	for i in range(1,len(withoutPList)):
		if withoutPList[i]<withoutPList[i-1]:
			print "Well that went down.."
			break
		
	print "******************"
	for i in range(1,len(withPList)):
		if withPList[i]<withPList[i-1]:
			print "Well that went down..FUCK"
			break
	#print withPList
	
	fileO.close()
	
checkLL()