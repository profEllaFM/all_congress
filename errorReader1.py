from datetime import datetime, date

error_idCong = 'idCong'
error_newID = 'newID'
error_newICPSR = 'newICPSR'
error_newDist = 'newDist'
error_oldStarted = 'oldStarted'
error_revisedStarted = 'revisedStarted'
error_revisedEnded = 'revisedEnded'
error_reason = 'reason'

_placeHolderDate = date(1000,1,1)

def createErrorDict(fileName):
    errorDict = {}
    sep = ","
    names = [error_idCong, error_newID, error_newICPSR, error_newDist, error_oldStarted,
             error_revisedStarted, error_revisedEnded]
    with open(fileName) as source:
        _ = source.readline()   #adam told me to do this, skip header row
        for line in source:
            idCong, newID, newICPSR, newDist, oldStarted, revisedStarted, revisedEnded, reason = line.split(sep)  
            oldStarted = _processDate(oldStarted)
            revisedStarted = _processDate(revisedStarted)
            revisedEnded = _processDate(revisedEnded)
            errorDict[idCong] = dict(zip(names[1:], (newID, newICPSR, newDist, oldStarted,
                                                          revisedStarted, revisedEnded)))
    return errorDict

def _processDate(stringDate):
    if stringDate:
        return datetime.strptime(stringDate, '%Y-%m-%d').date()
    return _placeHolderDate
    

def _IamKindOfSecret(butnotreally):
    pass

def shadowing():
    list_ # because I wanted to use 'list' but "can't"
    
