
# This class represents an open PVS file

import os.path
from constants import EMPTY_STRING
import config

log = config.getLogger(__name__)

class PVSFile:
    def __init__(self, fullname):
        self.lines = []
        self.fullname = fullname
        self.filename = os.path.split(fullname)[1]
        self.getContentFromFile()
        self.editor = None
    
    def setEditor(self, editor):
        self.editor = editor
    
    def getContentFromFile(self):
        lines = []
        if os.path.exists(self.fullname):
            with open(self.fullname) as f:
                for line in f:
                    lines.append(line)
        else:
            with open(self.fullname, 'w+') as f:
                for line in f:
                    lines.append(line)
        self.content = EMPTY_STRING.join(lines)

    def getContentFromEditor(self):
        if self.editor != None:
            self.content = self.editor.getText()
        else:
            log.error("There is no rich editor assigned to %s", self.fullname)
    
        
    def saveAs(self, newfullname):
        log.info("Savinf the file %s as the new file %s ", (self.fullname, newfullname))
        self.fullname = newfullname
        self.changed = True
        self.save()
    
    def save(self):
        log.info("Saving file %s", self.fullname)
        f = open(self.fullname, mode='w')
        self.getContentFromEditor()
        f.write(self.content)
        f.close()
            
    def close(self):
        log.info("Closing file %s", self.fullname)
        self.save()
            
    def __str__(self):
        return self.fullname
            
            
