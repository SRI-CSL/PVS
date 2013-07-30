#!/usr/bin/env python
import wx
import time
from wx.lib.floatcanvas import NavCanvas, Resources
from wx.lib.floatcanvas import FloatCanvas as FC
from wx.lib.floatcanvas.Utilities import BBox
import numpy as N
import random

class ConnectorLine(FC.LineOnlyMixin, FC.DrawObject,):
	"""
	A Line that connects two objects -- it uses the objects to get its coordinates
	The objects must have a GetConnectPoint() method.
	"""
	def __init__(self, Object1, Object2, LineColor = "Black", LineStyle = "Solid", LineWidth	= 1, InForeground = False):
		FC.DrawObject.__init__(self, InForeground)
		self.Object1 =  Object1	   
		self.Object2 =  Object2	   
		self.LineColor = LineColor
		self.LineStyle = LineStyle
		self.LineWidth = LineWidth

		self.CalcBoundingBox()
		self.SetPen(LineColor,LineStyle,LineWidth)

		self.HitLineWidth = max(LineWidth,self.MinHitLineWidth)

	def CalcBoundingBox(self):
		self.BoundingBox = BBox.fromPoints((self.Object1.GetConnectPoint(), self.Object2.GetConnectPoint()))
		if self._Canvas:
			self._Canvas.BoundingBoxDirty = True


	def _Draw(self, dc , WorldToPixel, ScaleWorldToPixel, HTdc=None):
		p1 = self.Object1.GetConnectPoint()
		p2 = self.Object2.GetConnectPoint()
		half = NodeViewObject.RectangleSize[1]/2
		if self.Object1.Node._level > self.Object2.Node._level:
			p1[1] = p1[1] + half
			p2[1] = p2[1] - half
		else:
			pass
			p1[1] = p1[1] - half
			p2[1] = p2[1] + half
			
		Points = N.array((p1, p2))
		Points = WorldToPixel(Points)
		dc.SetPen(self.Pen) # dc is an instance of wx.MemoryDC
		dc.DrawLines(Points)
		if HTdc and self.HitAble:
			HTdc.SetPen(self.HitPen)
			HTdc.DrawLines(Points)


class NodeViewObject(FC.Group):
	"""
	A version of the moving group for nodes -- an ellipse with text on it.
	"""
	RectangleSize = (12, 4)
	
	def __init__(self, node, shape, InForeground = False, IsVisible = True):
		Label = node.name
		location = node.Point()

		BackgroundColor = "Green"
		TextColor = "Black"
			
		locationArray = N.asarray(location, N.float).reshape(2,)
		WH = N.asarray(NodeViewObject.RectangleSize, N.float).reshape(2,)
		#print "Node %s: Location = %s, WH = %s"%(Label, locationArray, WH)
		self.Node = node
		scaler = 1. + len(Label)/7.0
		self.Label = FC.ScaledText(Label, locationArray, Size = WH[1] / scaler, Color = TextColor, Position = 'cc',)
		if shape == "rectangle":
			self.Shape = FC.Rectangle( (locationArray - WH/2.0), WH, FillColor = BackgroundColor, LineStyle = None,)
		elif shape == "ellipse":
			self.Shape = FC.Ellipse( (locationArray - WH/2.0), WH, FillColor = BackgroundColor, LineStyle = None,)
		else:
			print "Error. Unknown shape: %s"%(shape,)
		FC.Group.__init__(self, [self.Shape, self.Label], InForeground, IsVisible)

	def GetConnectPoint(self):
		return self.BoundingBox.Center


class TreeNode:
	NEXTINROW = 15
	NEXTINCOLUMN = 10
	
	def __init__(self, _id, name=None, data={}):
		self._id = _id
		self.name = _id if name is None else name 
		self.data = data
		self.children = []
		self._level = 0
		self._location = 0
		self._delta = 0
		self._parent = None
		self._connector = None
		self._drawObject = None
		self._expanded = True
		
	def Point(self):
		return (self._location, -self._level * TreeNode.NEXTINCOLUMN)
		
	def subTreeAsList(self):
		kids = []
		for c in self.children:
			kids.append(c)
			kids.extend(c.subTreeAsList())
		return kids
	
	def __getitem__(self, key):
		return self.data[key]
	
	def __setitem__(self, key, value):
		self.data[key] = value
		
	def __str__(self):
		return self.name


class DynamicTreeView(NavCanvas.NavCanvas):
	def __init__(self, parent):
		NavCanvas.NavCanvas.__init__(self,parent,wx.ID_ANY,(500,500), ProjectionFun = None, Debug = 0, BackgroundColor = "White")
		self._levels = {}
		self._nodes = {}
		self.root = None
		self.setLeafProperties("Green", "Black")
		self.setNonLeafProperties("Blue", "White")
		self.shape = "rectangle"

	def setRoot(self, root):
		assert self.root == None
		self.root = root
		self.root._level = 0
		self.root._location = 0
		self.root._delta = 0
		self._nodes[root._id] = root
		self._levels[0] = []
		self._levels[0].append(root)
		rootObj = self._createNodeViewObject(root)
		self.Canvas.AddObjects((rootObj,))
		rootObj.Bind(FC.EVT_FC_LEFT_UP, self.onNodeLeftClicked)
		rootObj.Bind(FC.EVT_FC_RIGHT_UP, self.onNodeRightClicked)
		#rootObj.Bind(FC.EVT_FC_LEFT_DCLICK, self.onNodeDoubleClicked) 
		self._updateNode(root) 
		print "Root was set to %s"%self.root.name

	def addChild(self, parent, node, idx=None):
		if node._id in self._nodes:
			print "Error. There is already another node in the tree with ID = %s"%(node._id)
			return
		if not isinstance(parent, TreeNode):
			if not parent in self._nodes:
				print "Error. There is no current node in the tree with ID = %s"%(parent, )
				return
			parent = self._nodes[parent]
		if idx is None or idx > len(parent.children):
			idx = len(parent.children)
		if idx < 0:
			idx = 0
		node._level = parent._level + 1
		node._parent = parent
		node._delta = 0
		self._nodes[node._id] = node
		if not node._level in self._levels:
			self._levels[node._level] = []
		self._levels[node._level].append(node)
		parent.children.insert(idx, node)
		self._setLocation(node)
		node._drawObject = self._createNodeViewObject(node)
		node._connector = self._addConnector(parent, node)
		self.Canvas.AddObjects([node._drawObject, node._connector])
		node._drawObject.Bind(FC.EVT_FC_LEFT_DOWN, self.onNodeLeftClicked)
		node._drawObject.Bind(FC.EVT_FC_RIGHT_DOWN, self.onNodeRightClicked)
		if not parent._expanded:
			node._drawObject.Hide()
			node._connector.Hide()
		affectedNodes = self._updateNextSiblingLocations(node)
		affectedNodes.insert(0, node)
		#self.onNodeRelocated(node)
		print "Node %s was added to %s as child #%d. level %d and location %d"%(node.name, parent.name, idx, node._level, node._location)
		self._updateLocationOfAffectedParents(affectedNodes)
		self._updateNode(parent)

	def removeNode(self, node):
		if not isinstance(node, TreeNode):
			if not node in self._nodes:
				print "Error. There is no current node in the tree with ID = %s"%(node, )
				return
			node = self._nodes[node]
		self._levels[node._level].remove(node)
		del self._nodes[node._id]
		node._parent.children.remove(node)
		self._updateNode(node._parent)
		print "Node %s was removed"%node.name

	def onNodeRelocated(self, node):
		if node._delta != 0:
			currentPoint = node.Point()
			node._location = node._location + node._delta
			node._delta = 0
			newPoint = node.Point()
			delta = (newPoint[0] - currentPoint[0], newPoint[1] - currentPoint[1])
			#dxy = self.Canvas.ScalePixelToWorld(delta)
			print "%s was relocated. Delta is %f"%(node.name, node._delta)
			node._drawObject.Move(delta) 
			self.Canvas.Draw(True)

	def _updateLocationOfAffectedParents(self, affectedNodes):
		if affectedNodes:
			affectedParents = []
			for affn in affectedNodes:
				if not (affn._parent is None or affn._parent in affectedParents):
					affectedParents.append(affn._parent)
			for affp in affectedParents:
				newLoc = int(0.5 + (affp.children[0]._location + affp.children[-1]._location)/2) # Here you might wanna round it
				affpNext = self._nextInLevel(affp)
				if newLoc > affp._location:
					affpNext = self._nextInLevel(affp)
					if affpNext:
						newLoc = min(newLoc, affpNext._location - TreeNode.NEXTINROW)
				elif newLoc < affp._location:
						affpPrev = self._previousInLevel(affp)
						if affpPrev:
							newLoc = max(newLoc, affpPrev._location + TreeNode.NEXTINROW)
				if newLoc != affp._location:
					print "Relocating %s from %d to %d"%(affp.name, affp._location, newLoc)
					affp._delta = newLoc - affp._location
					self.onNodeRelocated(affp)
					self._updateNextSiblingLocations(affp)
			self._updateLocationOfAffectedParents(affectedParents)
		
	def _updateNextSiblingLocations(self, node):
		nxt = self._nextInLevel(node)
		result = []
		if nxt is not None:
			print "=> %s location: %d. Next %s location: %d"%(node.name, node._location, nxt.name, nxt._location)
			if nxt._location <= node._location + TreeNode.NEXTINROW:
				nxt._delta = node._location + TreeNode.NEXTINROW - nxt._location
				self.onNodeRelocated(nxt)
				print "  Pushing %s one step to the right"%nxt.name
				result = [nxt] + self._updateNextSiblingLocations(nxt)
			else:
				print "  Not Pushing %s"%nxt.name
		return result
				
	def _updatePreviousSiblingLocations(self, node):
		return # TODO: remove this?
		result = []
		prev = self._previousInLevel(node)
		if prev is not None:
			print "=> %s location: %d. Prev %s location: %d"%(node.name, node._location, prev.name, prev._location)
			if node._location <= prev._location + TreeNode.NEXTINROW:
				prev._delta = node._location - TreeNode.NEXTINROW + prev._location
				self.onNodeRelocated(prev)
				print "  Pushing %s one step to the right"%prev.name
				result = [prev] + self._updatePreviousSiblingLocations(prev)
			else:
				print "  Not Pushing %s"%prev.name
		return result

	def _setLocation(self, node):
		prev = self._previousInLevel(node)
		nxt = self._nextInLevel(node)
		if prev is None:
			if nxt is None:
				node._location = node._parent._location
			else:
				node._location = min(node._parent._location,  nxt._location - TreeNode.NEXTINROW)
		else:
			node._location = max(node._parent._location, prev._location + TreeNode.NEXTINROW)
				

	def _nextInLevel(self, node):
		if node == self.root:
			return None
		nodeIndex = node._parent.children.index(node)
		if nodeIndex < len(node._parent.children) - 1:
			return node._parent.children[nodeIndex + 1] # next sibling
		nextSibling = None
		curr = node._parent
		while True:
			curr = self._nextInLevel(curr)
			if curr is None:
				break
			if curr.children:
				nextSibling = curr.children[0]
				break
		return nextSibling
	
	def _previousInLevel(self, node):
		if node == self.root:
			return None
		nodeIndex = node._parent.children.index(node)
		if nodeIndex > 0:
			return node._parent.children[nodeIndex - 1] # previous sibling
		previousibling = None
		curr = node._parent
		while True:
			curr = self._previousInLevel(curr)
			if curr is None:
				break
			if curr.children:
				previousibling = curr.children[-1]
				break
		return previousibling
	
	def __getitem__(self, item):
		return self.nodes[item]

	def display(self, node=None, indent=""):
		if node is None:
			node = self.root
		print "%s %s (lev: %d, loc: %d)"%(indent, node.name, node._level, node._location)
		for c in node.children:
			ind = "   " + indent
			self.display(c, ind)

	# _______________
	
	def _createNodeViewObject(self, node):
		node._drawObject = NodeViewObject(node, self.shape)
		return node._drawObject
		
	def _addConnector(self, node, child):
		child._connector = ConnectorLine(node._drawObject, child._drawObject, LineWidth=3, LineColor="Grey")
		return child._connector
		
	def onNodeRightClicked(self, obj):
		node = obj.Node
		print "%s was right clicked"%node.name

	def onNodeLeftClicked(self, obj):
		node = obj.Node
		print "%s was clicked"%node.name
		self.toggleExpansion(node)
	
	def onNodeDoubleClicked(self, obj):
		print "%s was double clicked"%node.name

	def toggleExpansion(self, node):
		if node._expanded:
			self.collapse(node)
		else:
			self.expand(node)
		node._expanded = not node._expanded
	
	def expand(self, node):
		removingObjects = self._getNodesAndObjectsBelow(node)[1]
		for obj in removingObjects:
			obj.Show()
		self.Canvas.Draw(True)
	
	def collapse(self, node):
		removingObjects = self._getNodesAndObjectsBelow(node)[1]
		for obj in removingObjects:
			obj.Hide()
		self.Canvas.Draw(True)
		
	def _getNodesAndObjectsBelow(self, node, includeThis=False, maxNumberOfLayers=-1):
		objects = []
		nodes = []
		if includeThis:
			nodes.append(node)
			if node._drawObject is not None:
				objects.append(node._drawObject)
			if node._connector is not None:
				objects.append(node._connector)
		if maxNumberOfLayers != 0:
			for c in node.children:
				(n, o) = self._getNodesAndObjectsBelow(c, True, maxNumberOfLayers-1)
				objects.extend(o)
				nodes.extend(n)
		return (nodes, objects)

	def setLeafProperties(self, color, textColor):
		self.leafColor = color
		self.leafTextColor = textColor
		
	def setNonLeafProperties(self, color, textColor):
		self.nonleafColor = color
		self.nonleafTextColor = textColor
		
	def setNodeShape(self, shape):
		shape = shape.lower()
		acceptableShapes = ("rectangle", "ellipse")
		if shape not in acceptableShapes:
			print "Error. Shape should be one of %s"%(acceptableShapes,)
		else:
			self.shape = shape
				
	def _updateNode(self, node):
		if node is not None:
			if len(node.children) > 0:
				self.setNodeColor(node, self.nonleafColor)
				node._drawObject.Label.SetColor(self.nonleafTextColor)
			else:
				self.setNodeColor(node, self.leafColor)
				node._drawObject.Label.SetColor(self.leafTextColor)
		self.Canvas.Draw()
			
	def setNodeColor(self, node, color):
		if isinstance(node, str):
			node = self[node]
		node._drawObject.SetFillColor(color)
			
	

def familyTree(tree):
	tree.setRoot(TreeNode("Akbar"))
	tree.addChild(tree.root, TreeNode("Hooshangiz"))
	tree.addChild(tree.root, TreeNode("Enayat"))
	tree.addChild(tree.root, TreeNode("Faraj"))
	tree.addChild(tree.root, TreeNode("Davood"))
	tree.addChild(tree.root, TreeNode("Shahnaz"))
	tree.addChild("Davood", TreeNode("Ruha"))
	tree.addChild("Davood", TreeNode("Neda"))
	tree.addChild("Davood", TreeNode("Hoda"))
	tree.addChild("Hooshangiz", TreeNode("Shahryar"))
	tree.addChild("Hooshangiz", TreeNode("Shahram"))
	tree.addChild("Hooshangiz", TreeNode("Narges"))	
	tree.addChild("Faraj", TreeNode("Salar"))
	tree.addChild("Faraj", TreeNode("Mahyar"))
	tree.addChild("Enayat", TreeNode("Shahrokh"))
	tree.addChild("Shahrokh", TreeNode("Aveesa"))
	tree.addChild("Enayat", TreeNode("Shahin"))
	tree.addChild("Davood", TreeNode("Maryam"))
	tree.addChild("Davood", TreeNode("Koosha"))

	#tree.addChild("Faraj", TreeNode("Salar"))
	#tree.display()
	
def randomTree(tree):
	root = TreeNode("Root")
	tree.setRoot(root)
	nodes = [root]
	for i in range(10):
		node = TreeNode("Node %s"%i)
		idx = random.randint(0, len(nodes)-1)
		tree.addChild(nodes[idx], node)
		nodes.append(node)
	#tree.display()


class DrawFrame(wx.Frame):
	def __init__(self, *args, **kwargs):
		wx.Frame.__init__(self, *args, **kwargs)
		
		sizer = wx.BoxSizer(wx.VERTICAL)
		self.CreateStatusBar()            
		self.tree = DynamicTreeView(self)
		self.tree.setLeafProperties("Green", "Black")
		self.tree.setNonLeafProperties("Red", "White")
		self.tree.setNodeShape("ellipse")
		button = wx.Button(self, wx.ID_ANY, "Add Node") #TODO: Change this to remove and test removeChild also.
		button.Bind(wx.EVT_BUTTON, self.onClick)
		sizer.Add(self.tree, 1, wx.EXPAND | wx.LEFT, 5)
		sizer.Add(button, 0, wx.EXPAND | wx.LEFT, 5)
		self.SetSizer(sizer)     
		#Canvas.Bind(FC.EVT_LEFT_UP, self.OnLeftUp) 
		self.Bind(wx.EVT_SIZE, self.onResize)

		randomTree(self.tree)
		
		self.Show(True)
		self.tree.Canvas.ZoomToBB()
		
	def onResize(self, event):
		self.tree.Canvas.ZoomToBB()
		event.Skip()
		
	def onClick(self, event):
		print "_________"
		nodes = self.tree._nodes.values()
		nnodes = len(nodes)
		node = TreeNode("Node %s"%(nnodes+1))
		idx = random.randint(0, nnodes-1)
		self.tree.addChild(nodes[idx], node)
		self.tree.Canvas.ZoomToBB()
		event.Skip()
		
if __name__ == "__main__":
	app = wx.PySimpleApp(0)
	DrawFrame(None, -1, "FloatCanvas Tree Demo App", wx.DefaultPosition, (700,700) )
	app.MainLoop()
