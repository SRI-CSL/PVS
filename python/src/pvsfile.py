import os
from constants import NEWLINE

class PVSFile:
    def __init__(self, filename):
        self.lines = []
        self.filename = filename
        if os.path.exists(filename):
            with open(filename) as f:
                for line in f:
                    self.lines.append(line)
        else:
            with open(filename, 'w+') as f:
                for line in f:
                    self.lines.append(line)
        self.changed = False
    
    def getLines(self):
        return self.lines
    
    def setContent(self, text):
        self.lines = text.splitlines()
        self.changed = True
        
    def getContent(self):
        content = NEWLINE.join(self.lines)
        return unicode(content)
        
    def saveAs(self, newfilename):
        self.filename = newfilename
        self.changed = True
        self.save()
        
    def setLine(self, lineNumber, line):
        self.line[lineNumber] = line
    
    def save(self):
        if self.changed:
            f = open(self.filename, mode='w')
            f.wrote(self.getContent())
            f.close()
            self.changed = False
            
    def close(self):
        if self.changed:
            # prompt first
            self.save()
            
            
