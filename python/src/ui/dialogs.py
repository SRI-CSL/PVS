import wx
import util
import constants

def showError(message):
    """Show a dialog for an error"""
    dlg = wx.MessageDialog(util.frame, message, constants.ERROR, wx.OK | wx.ICON_ERROR)
    dlg.ShowModal()
    dlg.Destroy()
    
def showWarning(message):
    """Show a dialog for a warning"""
    dlg = wx.MessageDialog(util.frame, message, constants.WARNING, wx.OK | wx.ICON_WARNING)
    dlg.ShowModal()
    dlg.Destroy()
    
def showMessage(message):
    """Show a dialog for a message"""
    dlg = wx.MessageDialog(util.frame, message, constants.MESSAGE, wx.OK | wx.ICON_INFORMATION)
    dlg.ShowModal()
    dlg.Destroy()
    
def askYesNoQuestion(question, title=constants.EMPTY_STRING):
    """Show a dialog box to ask a question with two possible answers: Yes and No"""
    dlg = wx.MessageDialog(util.frame, question, title, wx.YES_NO | wx.ICON_QUESTION)
    choice = dlg.ShowModal() # choice will be either wx.ID_YES or wx.ID_NO
    dlg.Destroy()
    return choice

def askYesNoCancelQuestion(question, title=constants.EMPTY_STRING):
    """Show a dialog box to ask a question with three possible answers: Yes, No, and Cancel"""
    dlg = wx.MessageDialog(util.frame, question, title, wx.YES_NO | wx.CANCEL | wx.ICON_QUESTION)
    choice = dlg.ShowModal() # choice will be either wx.ID_YES or wx.ID_NO or wx.ID_CANCEL
    dlg.Destroy()
    return choice

def chooseDirectory(message, defaultDirectory=constants.EMPTY_STRING):
    """Show a dialog to choose a directory"""    
    dialog = wx.DirDialog (util.frame, message = message, defaultPath=defaultDirectory)
    newPath = None
    if dialog.ShowModal() == wx.ID_OK:
        newPath = dialog.GetPath()
    dialog.Destroy()
    return newPath
