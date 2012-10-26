import wx
import common
import constants

def showError(message):
    """Show a dialog for an error"""
    dlg = wx.MessageDialog(common.frame, message, constants.ERROR, wx.OK | wx.ICON_ERROR)
    dlg.ShowModal()
    dlg.Destroy()
    
def showWarning(message):
    """Show a dialog for a warning"""
    dlg = wx.MessageDialog(common.frame, message, constants.WARNING, wx.OK | wx.ICON_WARNING)
    dlg.ShowModal()
    dlg.Destroy()
    
def showMessage(message):
    """Show a dialog for a message"""
    dlg = wx.MessageDialog(common.frame, message, constants.MESSAGE, wx.OK | wx.ICON_INFORMATION)
    dlg.ShowModal()
    dlg.Destroy()

def chooseDirectory(message, defaultDirectory=constants.EMPTY_STRING):
    """Show a dialog to choose a directory"""    
    dialog = wx.DirDialog (common.frame, message = message, defaultPath=defaultDirectory)
    newPath = None
    if dialog.ShowModal() == wx.ID_OK:
        newPath = dialog.GetPath()
    dialog.Destroy()
    return newPath
