import os.path
from constants import EMPTY_STRING
from config import getLogger

log = getLogger(__name__)

class PVSFile:
    def __init__(self, fullname):
        self.lines = []
        self.fullname = fullname
        self.filename = os.path.split(fullname)[1]
        if os.path.exists(fullname):
            with open(fullname) as f:
                for line in f:
                    self.lines.append(line)
        else:
            with open(fullname, 'w+') as f:
                for line in f:
                    self.lines.append(line)
        self.changed = False
    
    def getLines(self):
        return self.lines
    
    def setContent(self, text):
        log.info("Setting the content for file %s", self.fullname)
        self.lines = text.splitlines()
        self.changed = True
        
    def getContent(self):
        log.info("Getting the content for file %s", self.fullname)
        content = EMPTY_STRING.join(self.lines)
        return unicode(content)
        
    def saveAs(self, newfullname):
        log.info("Savinf the file %s as the new file %s ", (self.fullname, newfullname))
        self.fullname = newfullname
        self.changed = True
        self.save()
        
    def setLine(self, lineNumber, line):
        self.line[lineNumber] = line
    
    def save(self):
        log.info("Saving file %s", self.fullname)
        if self.changed:
            f = open(self.fullname, mode='w')
            f.wrote(self.getContent())
            f.close()
            self.changed = False
            
    def close(self):
        log.info("Closing file %s", self.fullname)
        if self.changed:
            # prompt first
            self.save()
            
    def __str__(self):
        return self.fullname
            
            
