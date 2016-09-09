
################
#Maricella Foster-Molina, 2016-1-22
#for congressional data
#C:\Users\marciposafm\Documents\allCongress\playLegislators.py

#creates a comma delimited legislatorCharacteristics x numRepresentatives matrix
#       each row: state, district, govId, lastName, firstName,middleName, age,
#               gender, party, str(numberTerms),str(timeServed.days)
#       has one extra blank for b/c there's a \n at the last entry
# creates a csv, number MCs x yearsUS. houseSeniority[0] lists years between 1789 and 2016
#          houseSeniority[][0] lists MC ID
#          houseSeniority[i][j] lists number of terms MC served in the year they entered that term
#                     of Congress. So if someone starts in 2014 for 2013-14 term, then
#                     houseSeniority[i][2014-1788] = 1, houseSeniority[i][2013] = 0.


#input list of all historical legislators
#       list of all current legislators, as of April 2015
########################

import sys
import re
import os
import yaml
import csv
import ast
from collections import namedtuple
from datetime import date

from errorReader1 import(error_idCong, error_newID, error_newICPSR, error_newDist,
                        error_oldStarted, error_revisedStarted, error_revisedEnded, 
                        error_reason, createErrorDict)
##from errorReader import(error_idCong, error_newID, error_oldStarted,
##                        error_revisedStarted, error_revisedEnded, error_reason,
##                        createErrorDict)

def main():
    fileCurrent = open('C:\\Users\\marciposafm\\Documents\\113th\\legislators-current.yaml')
    currentLeg = yaml.load(fileCurrent)
    fileCurrent.close()
    fileHist = open('C:\\Users\\marciposafm\\Documents\\113th\\legislators-historical.yaml')
    histLeg = yaml.load(fileHist)
    fileHist.close()
    errorDict = createErrorDict("missingICPSRmine.csv")
    
    print('hi',errorDict)
    
    maxLeg = 500
    firstCong = 108
    lastCong = 109 #off by one... only going to 113
    allDates = [[]]*maxLeg
    numberTerms = 0

    dateFile = open('C:\\Users\\marciposafm\\Documents\\allCongress\\dateList.txt')
    dates = dateFile.readline()
    dateFile.close()
    dateDict = eval(dates)
    writeInfo(allDates, dateDict, firstCong, lastCong, currentLeg,
              histLeg, maxLeg, errorDict)
    

#switching to recording seniority
##    yearsUS = 2017-1788 #1789 really, but 1788 will be where id is recorded
##    l = 1
##    maxHouse = 12000  #actually 11086 members of the House, including territories.
##                        # this is 723 more people than govtrack.us thinks exist
##    houseSeniority = [[]]*maxHouse   #number of all house members
##    houseSeniority[0] = list(range(1788,2017))
##    houseSeniority[0][0] = 'id'
##    
##    houseSeniority = getSeniority(fileCurrent, houseSeniority, l, yearsUS, 'rep', maxHouse)
##    l = houseSeniority[maxHouse-1] #tracking where we are from previous file to current file
##    houseSeniority = getSeniority(fileHist, houseSeniority, l, yearsUS, 'rep', maxHouse)
##    l = houseSeniority[maxHouse-1]
##    houseSeniority.pop()
##    print(houseSeniority[0:5])
##    writeFile = open('C:\\Users\\marciposafm\\Documents\\allCongress\\houseTerms.txt','w')
##    writeF(writeFile, houseSeniority)
##
##    maxSenate = 2000 #actually 1964. Govtrack thinks 1882
##    senateSeniority = [[]]*maxSenate   #number of all senators
##    senateSeniority[0] = list(range(1788,2017))
##    senateSeniority[0][0] = 'id'
##
##    l = 1
##    senateSeniority = getSeniority(fileCurrent, senateSeniority, l, yearsUS, 'sen', maxSenate)
##    l = senateSeniority[maxSenate-1] #tracking where we are from previous file to current file
##    senateSeniority = getSeniority(fileHist, senateSeniority, l, yearsUS, 'sen', maxSenate)
##    senateSeniority.pop()
##    writeFile = open('C:\\Users\\marciposafm\\Documents\\allCongress\\senateTerms.txt','w')
##    writeF(writeFile, senateSeniority)

#writing information about legislators to files, one for each congress
#also writing the number of legislators in each congress to 
def writeInfo(allDates, dateDict, firstCong, lastCong, fileCurrent, fileHist,
              maxLeg, errorDict):
    k = 0
    for i in range(firstCong,lastCong):
        fileName = 'C:\\Users\\marciposafm\\Documents\\allCongress\\leg'+str(i)+'.txt'
        #print(fileName)
        startSes = dateDict[i][0]
        endSes = dateDict[i][1]
        #print(startSes)
        #print(i)
        info = getInfo(fileCurrent, startSes, endSes, allDates, k, maxLeg, i, errorDict)
        k = info[maxLeg-1] #tracking where i am in the list of legislators      
        info = getInfo(fileHist, startSes, endSes,  info, k, maxLeg, i, errorDict)
        k = info[maxLeg-1]
        print(i, k)
        info.pop()  #wtf are allDates and info pointing to the same object?
        del info[k:len(info)]
        writeFile = open(fileName,'w')
        for item in info:
            writeFile.write("%s" % item)
            writeFile.write('\n')        
        k = 0
        allDates = [[]]*maxLeg

def getErrorDates(govId, congNum, started, ended, errorDict, terms):
    #todo return start end
    idCongHere = str(congNum) + '-' + str(govId)
    newStarted, newEnded = started, ended
    if(idCongHere in errorDict.keys()):
        #print(idCongHere, errorDict.keys(),errorDict[idCongHere][error_oldStarted], started)
        if(errorDict[idCongHere][error_oldStarted]== started):
            newStarted, newEnded = errorDict[idCongHere][error_revisedStarted], \
                                   errorDict[idCongHere][error_revisedEnded]
            terms = terms+1
            print(newStarted, newEnded, terms)
            
    return newStarted, newEnded, terms

#workhorse. Getting information for all legislators for relevant dates
def getInfo(file, startSes, endSes, allDates, k, maxLeg, congNum, errorDict):
    for j in range(0,len(file)):
        numberTerms =0
        for i in range(0,len(file[j]['terms'])):
            if(file[j]['terms'][i]['type']== 'rep'):
                start = file[j]['terms'][i]['start'].split('-')
                end = file[j]['terms'][i]['end'].split('-')
                numberTerms = numberTerms +1
                started = date(int(start[0]),int(start[1]),int(start[2]))
                ended = date(int(end[0]),int(end[1]),int(end[2]))
                govId = str(file[j]['id']['govtrack'])
                started, ended, numberTerms = getErrorDates(govId, congNum, started, ended,
                                               errorDict, numberTerms)                
                if( ((started <= startSes) and (startSes <= ended) )            
                   or  ((startSes <= started) and (started <= endSes) )): #if is true, then log person in ith congres
                    name = file[j]['name']
                    firstName = name['first']
                    lastName = name['last']
                    if 'middle' in file[j]['name']:
                        middleName = file[j]['name']['middle'].replace(",", "")
                    else:
                        middleName = ' '
                    gender = file[j]['bio']['gender']
                    startDate = file[j]['terms'][i]['start']
                    endDate = file[j]['terms'][i]['end']
                    #print(str(file[j]['id']['govtrack'])) 
                    startDay = startSes  #age is calculated at beginning of term
                    try:
                        born = file[j]['bio']['birthday']  #need to handle not having a bday listed, 96th cong
                    except Exception as e:
                        born = "1900-01-01"
                    born = born.split('-')
                    bday = date(int(born[0]),int(born[1]),int(born[2]))
                    age = str(startDay.year - bday.year - ((startDay.month, startDay.day)
                                                           < (bday.month, bday.day)))
                    timeServed = time(startDate, endDate)                   
                    term = file[j]['terms'][i]
                    party = term['party']
                    district = str(term['district'])
                    state = term['state']
                    govId = str(file[j]['id']['govtrack'])
                    try:
                        icpsr = str(file[j]['id']['icpsr'])
                    except Exception as e:
                        icpsr = ''
                    #print(k)
##                    if( state == 'MT' and (congNum == 103 or congNum == 104) ):
##                        print(govId, congNum, state, district)
##                        district = '0'  #MT became an at large district, but
##                                      #not updated in historical legislator file
##                    if(congNum == 94 and govId == '409946'):
##                        print(govId, congNum, district)
##                        district = '15'  #CA Bernice Sisk was redistricted from
##                                        # 16 to 15, but not updated in file
                    district, govId, icpsr = getOtherErrors(govId, congNum, district,
                                                            icpsr, errorDict)
                    allDates[k] = ','.join([state, district, govId, icpsr, lastName, firstName,
                                            middleName, age, gender, party, str(numberTerms),
                                            str(timeServed.days)])
                    numberTerms = 0 #resetting number of terms to 0 after passing current term
                    k = k+1
    allDates[maxLeg-1] = k
    return(allDates)

def getOtherErrors(govId, congNum, district, icpsr, errorDict):
    idCongHere = str(congNum) + '-' + str(govId)
    newDistrict, newGovId, newIcpsr = district, govId, icpsr
    if(idCongHere in errorDict.keys()):
        if(errorDict[idCongHere][error_newICPSR]!= ""):
            newIcpsr = errorDict[idCongHere][error_newICPSR]
            print(icpsr, newIcpsr)            
        if(errorDict[idCongHere][error_newDist]!= ""):
            newDistrict = errorDict[idCongHere][error_newDist]
            print(district, newDistrict)
        if(errorDict[idCongHere][error_newID]!= ""):
            newGovId = errorDict[idCongHere][error_newID]
            print(govId, newGovId)
    return newDistrict, newGovId, newIcpsr

#tracking amount of time between two string dates
def time(date1, date2):
    date1 = date1.split('-')
    date1N = date(int(date1[0]),int(date1[1]),int(date1[2]))
    date2 = date2.split('-')
    date2N = date(int(date2[0]),int(date2[1]),int(date2[2]))
    timeElapsed = date2N-date1N
    return(timeElapsed)
    #TODO: check
    start = datetime.strptime(date2, '%Y-%m-%d')
    end = datetime.strptime(date1, '%Y-%m-%d')
    return end - start

#create numReps x 2016-1788 for all members of congress in a particular house
#records how many terms they served. Each number of terms is recorded in the start year
# of that term.
def getSeniority(file, seniority, l,yearsUS, mcType, maxi):
    m= 0
    for j in range(0,len(file)):
        numberTerms = 0
        seniority[l] = [0]*yearsUS
        govID = str(file[j]['id']['govtrack'])
        seniority[l][0] = govID
        for i in range(0,len(file[j]['terms'])):
            if(file[j]['terms'][i]['type']== mcType):
                start = file[j]['terms'][i]['start']
                startYr = int(start.split('-')[0])
                numberTerms = numberTerms +1
                index = startYr - 1788
                seniority[l][index] = numberTerms
                m = 1   # dummy to only index to next legislator if they were mcType
                        # for at least one congressional session. Prevents recording an MC
                        # who has no seniority to record as house member bc was only senator
        if m == 1:
            l = l+1  #indexing to next ID if legislator was ever a mcType. Otherwise will overwrite
                     #non mcType row until get a mcType entry
        m = 0
    seniority[maxi-1] = l #tracking how many legislators recorded
    return(seniority)

#just writing for seniority
def writeF(writeFile, file):
    for legislator in file:
        for year in legislator:
            writeFile.write("%s" % year)
            writeFile.write(',')
        writeFile.write('\n')
    writeFile.close()


#necessary for all python files
if __name__ == '__main__':
    main()

