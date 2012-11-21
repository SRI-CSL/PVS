import wx
import config
import constants

def showError(message):
    dlg = wx.MessageDialog(config.frame, message, constants.ERROR, wx.OK | wx.ICON_ERROR)
    dlg.ShowModal()
    dlg.Destroy()
    
def showWarning(message):
    dlg = wx.MessageDialog(config.frame, message, constants.WARNING, wx.OK | wx.ICON_WARNING)
    dlg.ShowModal()
    dlg.Destroy()
    
def chooseDirectory(message, defaultDirectory=constants.EMPTY_STRING):
    dialog = wx.DirDialog (config.frame, message = message, defaultPath=defaultDirectory)
    newPath = None
    if dialog.ShowModal() == wx.ID_OK:
        newPath = dialog.GetPath()
    dialog.Destroy()
    return newPath
