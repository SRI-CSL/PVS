import wx
import config

class PVSResultEvent(wx.PyEvent):
    """Simple event to carry arbitrary result data."""
    def __init__(self, message, data):
        """Init Result Event."""
        wx.PyEvent.__init__(self)
        self.SetEventType(config.EVT_RESULT_ID)
        self.message = message
        self.data = data