################
#Maricella Foster-Molina, 2016-1-24
#for congressional data, 112th congress
#C:\Users\marciposafm\Documents\112th\createSponPassMatrices.py

#creates a comma delimited numberBills x numRepresentatives matrix of bills cosponsored
#       entry = 0 if representative did not cosponsor bill
#       entry = 1

#creates a comma delimited numberBills x numRepresentatives matrix of bill sponsors
#       entry = 0 if representative did not sponsor bill
#       entry = 1

#creates a comma delimited numberBills x numRepresentatives matrix of bills passing house
#       entry = 0 if representative did not sponsor a bill that passed the house
#       entry = 1

#creates a comma delimited numberBills x numRepresentatives matrix of bills enacted
#       entry = 0 if representative did not sponsor a bill that became law
#       entry = 1

#creates a comma delimited numberBills x numRepresentatives matrix of bill status
#       entry = 0 
#       entry = bill status, a string

#legislatorInfo: list of every MC in 112th Congress, district, id, other info
#           includes delegates from non-states-- DC, virgin islands, etc

#note: not appending bills bc machine reads bill 1,10,100, 1000,1001, ...,1009,101,1010
#       this is not in numerical order
##################
#consider reformatting matrix labels to be interpretable in csv format
#           currently CA 10 200101. Might want to put it CA-10 200101
#           titles might have commas

import sys
import os
from os.path import join, getsize
import re
import xml.etree.ElementTree as ET

errorDict = {}

def main():
    #congNum = 111
    firstStart = 93
    firstEnd = 97+1
    secondStart = 103
    secondEnd = 113+1
    #congRange = list(range(firstStart,firstEnd))+list(range(secondStart,secondEnd))    
    passDict = {'cosp':'cosponsorMatrix.txt','spon':'sponsorMatrix.txt',
                'passHouse':'passHouseMatrix.txt','passed':'enactedMatrix.txt',
                'all':'allStatusMatrix.txt'}
    congRange = range(110,114)
    for congNum in congRange:
        print(congNum)
        fileName = 'C:\\Users\\marciposafm\\Documents\\allCongress\\leg'+str(congNum)+'.txt'
        sponFile = open(fileName,'r')
        sponInfo = sponFile.readlines()
        sponFile.close()
        numMC = len(sponInfo)+1
        #rootdir = 'C:/Users/marciposafm/Documents/112th/bills/bills'
        rootdir = 'C:/Users/marciposafm/Documents/allCongress/'+str(congNum)+'/bills/bills'
        numBills = findNumBills(rootdir)+1
        ##    #writing matrix
        for passType in passDict:
            print(passDict[passType])
            writeFile = 'C:/Users/marciposafm/Documents/allCongress/'+ \
                        str(congNum)+'/'+passDict[passType]
            writeData = open(writeFile,'w')
            print(rootdir)
            statusData = createStatus(rootdir, sponInfo, numBills, numMC, passType, congNum)
            for bills in statusData:
                for sponsor in bills:
                    writeData.write("%s" % sponsor)
                    writeData.write('\t')  #gives one extra column, a blank sponsor
                writeData.write('\n') 
            writeData.close()

#dynamically discovering how many bills in a given congress       
def findNumBills(rootdir):
    numBills = 0
    for root, dirs, files in os.walk(rootdir):
        for name in files:
            if 'h' in name and 'hr' not in name and 'hj' not in name and 'hc' not in name:
                bill = 'bills/bills/'+name
                billNo = int(name.replace("h","").replace(".xml",""))
                #print(billNo)
                if billNo >numBills:
                    numBills = billNo
                    #print(numBills)
    return numBills

#puts a 1 in each column that satisfies the passType. Or puts overall status if all requested
def createStatus(rootdir, sponInfo, numBills, numMC, passType, congNum):
    billData = [[0]]*numBills
    billData[0] = [0]*numMC       #giving space at top for sponIDs
    billData[0:numBills][0] = 0
    i = 0
    #print(root)
    for root, dirs, files in os.walk(rootdir):
        #print(root, dirs, files[:10])
        #exit('Debugging quit')
        for name in files:
            if 'h' in name and 'hr' not in name and 'hj' not in name and 'hc' not in name:
                #bill = os.path.join(root, name)
                #print(type(root), type(name))
                bill = root+'/'+name
                #print(bill)
                #sys.exit('nice message')
                billNo = int(name.replace("h","").replace(".xml",""))
                #print(billNo)
                tree = ET.parse(bill)
                xmlRoot = tree.getroot()
                status = xmlRoot.find('state').text
                title = xmlRoot.find('titles').find('title').text
                title = title.replace("'","").replace('"','').replace("#","")
                spon = xmlRoot.find('sponsor')
                sponID = spon.get('id')
                if(sponID == '400586' and congNum ==93):
                    sponID = '404650'  #fixing same name members
                    print(sponID)
                #print(sponID)
                #print(name)
                #print(rootdir)
                #print(title)
                colNum = sponCol(sponInfo, sponID)
                colNumFix = colNum+ 1   #fix off by one error
                billData[billNo] = [0]*numMC
                billData[billNo][0] = ' '.join([str(billNo), title])   #going to be missing the missing bills
                billData[0][colNumFix] = sponInfo[colNum].split(',')[2]
                if(passType == 'passed'):
                    if ('ENACTED' in status):
                        i = i+1
                        #print(i)
                        billData[billNo][colNumFix] = 1
                if(passType == 'spon'):
                    billData[billNo][colNumFix] = 1
                if(passType == 'all'):
                    billData[billNo][colNumFix] = status
                if(passType == 'passHouse'):
                    if(passHouseTruth(status) == 'TRUE'):
                        billData[billNo][colNumFix] = 1                       
                        i = i+1
                        #print(i)
                if(passType == 'cosp'):
                    billData[billNo][colNumFix] = 1  #sponsor was also a cosponsor
                    for cosponsor in xmlRoot.find('cosponsors'):
                        cospID = cosponsor.get('id')
                        #print(cospID)
                        if(cospID == '400586' and congNum == 93):
                            cospID = '404650'  #fixing same name members
                        cospColNum = sponCol(sponInfo, cospID)+1 #fix off by one
                        billData[billNo][cospColNum] = 1
                #could pull bill summary with
                    # sum = root.find('summary') then something
    lazy = []
    for i in range(1,numMC):  #dealing with people who sponsored no bills
        if billData[0][i] == 0:
            lazy.append(i)
            billData[0][i] = sponInfo[i-1].split(',')[2]
            print(billData[0][i])
    for i in range(1,numBills): #dealing with bills reserved and not listed
        if billData[i][0] == 0:
            billData[i] = [0]*numMC
            billData[i][0] =i
    return(billData)   

#checking if it's passed the House of Reps
def passHouseTruth(status):
    if('ENACTED' in status or 'PROV_KILL:CLOTUREFAILED' in status
       or 'PASS_OVER:HOUSE' in status or 'PASSED:BILL' in status
       or 'FAIL:SECOND:SENATE' in status or 'PASS_BACK:HOUSE' in status
       or 'PASS_BACK:SENATE' in status or 'PROV_KILL:PINGPONGFAIL' in status
       or 'CONFERENCE:PASSED' in status or 'PROV_KILL:VETO' in status
       or 'VETOED' in status):
        didPass = 'TRUE'
    else:
        didPass = 'FALSE'
    return(didPass)


_passing_status = ['ENACTED', 'PROV_KILL:CLOTUREFAILED', ...]

def passedHouse(status):
    return status in _passing_status

#finding which row the relevant information for a MC is in the list of legislators
#called col bc i'm using the row number in legFile (plus 1) as the column number for
# the bill x sponsor matrix
def sponCol(sponInfo, idNo):
    idEntry = [s for s in sponInfo if idNo in s]
    idEntry = ''.join(idEntry)
    #print(idEntry)
    idEntry = idEntry.strip('[')
    rowNum = sponInfo.index(idEntry)
    return(rowNum)

#necessary for all python files
if __name__ == '__main__':
    main()

