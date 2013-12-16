
#!/usr/bin/env python
import wx
import time
from wx.lib.floatcanvas import NavCanvas, Resources
from wx.lib.floatcanvas import FloatCanvas as FC
from wx.lib.floatcanvas.Utilities import BBox
import numpy as N
import sets
import random
#from sl import SortedList

class ConnectorLine(FC.LineOnlyMixin, FC.DrawObject,):
	"""
	A Line that connects two objects -- it uses the objects to get its coordinates
	The objects must have a GetConnectPoint() method.
	"""
	def __init__(self, Object1, Object2, config, LineWidth = 1, InForeground = True):
		FC.DrawObject.__init__(self, InForeground)
		self.Object1 =  Object1
		self.Object2 =  Object2
		self.Object1.connectors.add(self)
		self.Object2.connectors.add(self)
		self.LineColor = config.getLineColor()
		self.LineStyle = config.getLineStyle()
		self.LineWidth = config.getLineWidth()
		self.RectangleSize = config.getRectangleSize()

		self.CalcBoundingBox()
		self.SetPen(self.LineColor, self.LineStyle, self.LineWidth)

		self.HitLineWidth = max(self.LineWidth, self.MinHitLineWidth)

	def CalcBoundingBox(self):
		self.BoundingBox = BBox.fromPoints(self.getEndPoints())
		if self._Canvas:
			self._Canvas.BoundingBoxDirty = True

	def getEndPoints(self):
		p1 = self.Object1.GetConnectPoint()
		p2 = self.Object2.GetConnectPoint()
		half = self.RectangleSize[1]/2
		if self.Object1.Node._level > self.Object2.Node._level:
			p1[1] = p1[1] + half
			p2[1] = p2[1] - half
		else:
			pass
			p1[1] = p1[1] - half
			p2[1] = p2[1] + half
		return (p1, p2)

	def _Draw(self, dc , WorldToPixel, ScaleWorldToPixel, HTdc=None):
		#print "Drawing Line from %s to %s"%(self.Object1.Node, self.Object2.Node)
		Points = N.array(self.getEndPoints())
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
	def __init__(self, node, config, InForeground = False, IsVisible = True):
		Label = node.name
		location = config.Point(node)
		self.connectors = sets.Set([])

		BackgroundColor = config.getNodeColor(node)
		TextColor = config.getNodeTextColor(node)
		shape = config.getNodeShape(node)
		locationArray = N.asarray(location, N.float).reshape(2,)
		WH = N.asarray(config.getRectangleSize(), N.float).reshape(2,)
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
	
	def Move(self, Delta):
		#TODO: is this the right way to call this:
		FC.Group.Move(self, Delta)
		#super(NodeViewObject, self).Move(Delta)
		for connector in self.connectors:
			connector.CalcBoundingBox()
			
class DynamicTreeConfig:
	def __init__(self, lineColor="Grey", lineStyle="Solid", lineWidth=2, nodeWidth=15, levelHeight=20, nodeShape="rectangle", nodeColors=("Blue", "Green"), nodeTextColors=("White", "Black")):
		self.lineColor = lineColor
		self.lineStyle = lineStyle
		self.lineWidth = lineWidth
		self.nodeWidth = nodeWidth
		self.levelHeight = levelHeight
		if nodeShape in ("rectangle", "ellipse"):
			self.nodeShape = nodeShape
		else:
			self.nodeShape = "ellipse"
			print "Unknown Shape: %s. Using ellipse instead"
		self.nonLeafColor = nodeColors[0]
		self.leafColor = nodeColors[1]
		self.nonLeafTextColor = nodeTextColors[0]
		self.leafTextColor = nodeTextColors[1]
		self.tree = None # to be filled by the tree later
		
	
	def getLineColor(self):
		return self.lineColor
	
	def getLineStyle(self):
		return self.lineStyle
	
	def getLineWidth(self):
		return self.lineWidth
	
	def getRectangleSize(self):
		return (12, 4)
	
	def getNodeWidth(self):
		return self.nodeWidth
		
	def getLevelHeight(self):
		return self.levelHeight
		
	def getNodeShape(self, node):
		return self.nodeShape
	
	def getNodeColor(self, node):
		return self.leafColor if node.isLeaf() else self.nonLeafColor
	
	def getNodeTextColor(self, node):
		return self.leafTextColor if node.isLeaf() else self.nonLeafTextColor
	
	def Point(self, node):
		return (node._location, -node._level * self.getLevelHeight())
	
	def onNodeRightClicked(self, node):
		print "%s was right clicked"%node.name
	
	def onNodeLeftClicked(self, node):
		print "%s was left clicked"%node.name
		self.tree.toggleExpansion(node)
		
	def getTooltipText(self, node):
		return node.name

class TreeNode:
	def __init__(self, _id, name=None, data={}):
		self._id = _id
		self.name = _id if name is None else name 
		self.data = data
		self.children = []
		self._level = 0
		self._location = 0
		self._delta = 0
		self._parent = None
		self._left = None
		self._right = None
		self._connector = None
		self._drawObject = None
		self._expanded = True

	def isLeaf(self):
		return len(self.children) == 0
	
	def __delitem__(self, key):
		del self.data[key]
	
	def __getitem__(self, key):
		return self.data[key]
	
	def __setitem__(self, key, value):
		self.data[key] = value
				
	def __str__(self):
		return self.name

	def __repr__(self):
		return self.__str__()


class DynamicTreeView(NavCanvas.NavCanvas):
	def __init__(self, parent, config):
		NavCanvas.NavCanvas.__init__(self,parent,wx.ID_ANY,(500,500), ProjectionFun = None, Debug = 0, BackgroundColor = "White")
		self._nodes = {}
		self.root = None
		self.config = config # config is an instance of DynamicTreeConfig
		self.history = None
		self.tooltip = None
		config.tree = self

	def setRoot(self, root):
		assert self.root == None
		self.root = root
		self.root._level = 0
		self.root._location = 0
		self.root._delta = 0
		self.root._left = None
		self.root._right = None
		self._nodes[root._id] = root
		rootObj = self._createNodeViewObject(root)
		self.Canvas.AddObjects((rootObj,))
		rootObj.Bind(FC.EVT_FC_LEFT_UP, self.onNodeLeftClicked)
		rootObj.Bind(FC.EVT_FC_RIGHT_UP, self.onNodeRightClicked)
		rootObj.Bind(FC.EVT_FC_ENTER_OBJECT, self.onNodeWindowEntered)
		rootObj.Bind(FC.EVT_FC_LEAVE_OBJECT, self.onNodeWindowLeft)
		rootObj.Bind(FC.EVT_FC_LEFT_DCLICK, self.onNodeDoubleClicked) 
		self._updateNode(root) 
		print "Root was set to %s"%self.root.name
		self.history = "\ttree.setRoot(TreeNode(\"%s\"))\n"%root.name

	def addChild(self, parent, node, idx=None):
		print "__________"
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
		node._left = None
		node._right = None
		node._delta = 0
		self._nodes[node._id] = node
		parent.children.insert(idx, node)
		self._setLocation(node)
		node._drawObject = self._createNodeViewObject(node)
		node._connector = self._addConnector(parent, node)
		self.Canvas.AddObjects([node._drawObject,node._connector])
		node._drawObject.Bind(FC.EVT_FC_LEFT_DCLICK, self.onNodeDoubleClicked) 
		node._drawObject.Bind(FC.EVT_FC_LEFT_UP, self.onNodeLeftClicked)
		node._drawObject.Bind(FC.EVT_FC_RIGHT_UP, self.onNodeRightClicked)
		node._drawObject.Bind(FC.EVT_FC_ENTER_OBJECT, self.onNodeWindowEntered)
		node._drawObject.Bind(FC.EVT_FC_LEAVE_OBJECT, self.onNodeWindowLeft)
		if not parent._expanded:
			node._drawObject.Hide()
			node._connector.Hide()

		self._balancify(node)
		print "Node %s was added to %s. (%d, %d)"%(node.name, parent.name, node._level, node._location)
		self._updateNode(parent)
		self.Canvas.Draw(True)
		self.history = self.history + "\ttree.addChild(\"%s\", TreeNode(\"%s\"))\n"%(parent.name, node.name)
		
	def redrawTree(self):
		nodes = self._nodes.values()
		nodeLevel = {}
		for node in nodes:
			if not node._level in nodeLevel:
				nodeLevel[node._level] = []
			nodeLevel[node._level].append(node)
		lowestLevel = max(nodeLevel.keys())
		nodeLevel[lowestLevel].sort(key=lambda x: x._location)
		
		
		

	def removeNode(self, node, indent=""):
		#TODO: after removing a node, the tree layout should be redone
		if not isinstance(node, TreeNode):
			if not node in self._nodes:
				print "Error. There is no current node in the tree with ID = %s"%(node, )
				return
			node = self._nodes[node]
		print "%sRemoving %s and its %d children"%(indent, node, len(node.children))
		while node.children:
			self.removeNode(node.children[0], indent + "\t")
		leftNode = node._left
		rightNode = node._right
		if leftNode is not None:
			if rightNode is not None:
				leftNode._right = rightNode
				rightNode._left = leftNode
				
				#TODO: This is where I am trying to shrink the tree. 
				distance = self._subTreeDistance(leftNode, rightNode)
				if distance > self.config.getNodeWidth():
					rightNode._delta = self.config.getNodeWidth() - distance
					self.onNodeRelocated(rightNode)
					self._balancify(rightNode)
				#End of the todo above.
			else:
				leftNode._right = None
		else:
			if rightNode is not None:
				rightNode._left = None
		node._parent.children.remove(node)
		self.Canvas.RemoveObjects([node._drawObject,node._connector])
		self._updateNode(node._parent)
		del self._nodes[node._id]

	def onNodeRelocated(self, node):
		if node._delta != 0:
			currentPoint = self.config.Point(node)
			node._location = node._location + node._delta
			node._delta = 0
			newPoint = self.config.Point(node)
			delta = (newPoint[0] - currentPoint[0], newPoint[1] - currentPoint[1])
			node._drawObject.Move(delta) 
		
	def _balancify(self, node, updateLeft=False, updateRight=True, updateParent=True, updateFirstChild=True):
		#print "Balancifying for %s"%node
		if updateFirstChild and node.children:
			self.onParentMoved(node.children[0])
		if updateRight and node._right is not None:
			self.onLeftSiblingMoved(node._right)
		if updateLeft and node._left is not None:
			self.onRightSiblingMoved(node._left)
		if updateParent and node._parent is not None:
			self.onChildMoved(node._parent)
			
	def onRightSiblingMoved(self, node):
		newLocation = max(node._location, node._right._location - self.config.getNodeWidth())
		if newLocation != node._location:
			node._delta = newLocation - node._location
			self.onNodeRelocated(node)
			self._balancify(node, updateLeft=True, updateRight=False)
	
	def onLeftSiblingMoved(self, node):
		newLocation = max(node._location, node._left._location + self.config.getNodeWidth())
		if newLocation != node._location:
			node._delta = newLocation - node._location
			self.onNodeRelocated(node)
			self._balancify(node)
	
	def onChildMoved(self, node):
		newLocation = max(node._location, int(0.5 + (node.children[0]._location + node.children[-1]._location)/2.0))
		if newLocation != node._location:
			node._delta = newLocation - node._location
			self.onNodeRelocated(node)
			self._balancify(node, updateFirstChild=False)
	
	def onParentMoved(self, node):
		newLocation = max(node._location, node._parent._location - self.config.getNodeWidth() * (len(node._parent.children)-1) / 2.0)
		node._delta = newLocation - node._location
		self.onNodeRelocated(node)
		self._balancify(node, updateParent=False)
		
	def _subTreeDistance(self, leftNode, rightNode):
		if leftNode is None or rightNode is None:
			return self.config.getNodeWidth() * 100
		assert leftNode._level == rightNode._level
		assert leftNode._location <= rightNode._location
		distance1 = rightNode._location - leftNode._location
		distance2 = self._subTreeDistance(self._getFarthestChild(leftNode, True), self._getFarthestChild(rightNode, False))
		return min(distance1, distance2)
		
	def _getFarthestChild(self, node, right=True):
		if node.children:
			index = -1 if right else 0
			return node.children[index]
		return None		
		
	def _setLocation(self, node):
		leftNode  = None
		rightNode = None
		position = node._parent.children.index(node)
		if position > 0:
			leftNode = node._parent.children[position-1]
			node._location = leftNode._location + self.config.getNodeWidth()
			node._left = leftNode
		else:
			uncle = node._parent._left
			while uncle is not None:
				if uncle.children:
					leftNode = uncle.children[-1]
					break
				uncle = uncle._left
		if leftNode is not None:
			rightNode = leftNode._right
		else:
			if position < len(node._parent.children)-1:
				rightNode = node._parent.children[position+1]
			else:
				uncle = node._parent._right
				while uncle is not None:
					if uncle.children:
						rightNode = uncle.children[0]
						break
					uncle = uncle._right
		if leftNode is not None:
			node._location = max(node._parent._location, leftNode._location + self.config.getNodeWidth())
			node._left = leftNode
			leftNode._right = node
		else:
			node._location = node._parent._location
		if rightNode is not None:
			node._right = rightNode
			rightNode._left = node
		print "%s was located between %s and %s"%(node, node._left, node._right)
	
	def __getitem__(self, item):
		return self.nodes[item]
	
	def traverse(self, func):
		func(self)
		for child in (root.children):
			traverse(child, self)

	def _testNodeLocationOk(self, node):
		if node._left:
			if node._left._location + self.config.getNodeWidth() > node._location:
				print "BAAAD Left !!! %s, %s, %s"%(node._left, node, node._right)
				assert False
		if node._right:
			if node._location + self.config.getNodeWidth() > node._right._location:
				print "BAAAD Right !!! %s, %s, %s"%(node._left, node, node._right)
				assert False				
			
	def _testTree(self):
		for node in self._nodes.values():
			self._testNodeLocationOk(node)
	# _______________
	
	def _createNodeViewObject(self, node):
		node._drawObject = NodeViewObject(node, self.config)
		return node._drawObject
		
	def _addConnector(self, node, child):
		child._connector = ConnectorLine(node._drawObject, child._drawObject, self.config)
		child._connector.Visible = True
		return child._connector
		
	def onNodeWindowEntered(self, obj):
		node = obj.Node
		obj = node._drawObject
		#center = obj.GetConnectPoint()
		self.tooltip = wx.ToolTip(tip=self.config.getTooltipText(node))
		self.tooltip.SetDelay(100)
		self.SetToolTip(self.tooltip)
		self.tooltip.Enable(True)

	def onNodeWindowLeft(self, obj):
		#node = obj.Node
		self.tooltip.Enable(False)
		self.tooltip = None

	def onNodeRightClicked(self, obj):
		node = obj.Node
		self.config.onNodeRightClicked(node)

	def onNodeLeftClicked(self, obj):
		node = obj.Node
		self.config.onNodeLeftClicked(node)
	
	def onNodeDoubleClicked(self, obj):
		node = obj.Node
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
		
				
	def _updateNode(self, node):
		if node is not None:
			node._drawObject.Shape.SetFillColor(self.config.getNodeColor(node))
			node._drawObject.Label.SetColor(self.config.getNodeTextColor(node))
		self.Canvas.Draw(True)




def randomTree(tree):
	root = TreeNode("Root")
	tree.setRoot(root)
	nodes = [root]
	for i in range(200):
		node = TreeNode("Node %s"%i)
		idx = random.randint(0, len(nodes)-1)
		tree.addChild(nodes[idx], node)
		nodes.append(node)
	#tree.display()
	
def predefinedTree(tree):
	tree.setRoot(TreeNode("Root"))
	tree.addChild("Root", TreeNode("Node 0"))
	tree.addChild("Root", TreeNode("Node 1"))
	tree.addChild("Node 0", TreeNode("Node 2"))
	tree.addChild("Node 1", TreeNode("Node 3"))
	tree.addChild("Node 3", TreeNode("Node 4"))
	tree.addChild("Node 2", TreeNode("Node 5"))

class DrawFrame(wx.Frame):
	def __init__(self, *args, **kwargs):
		wx.Frame.__init__(self, *args, **kwargs)
		
		sizer = wx.BoxSizer(wx.VERTICAL)
		self.CreateStatusBar()            
		self.tree = DynamicTreeView(self, DynamicTreeConfig())
		addButton = wx.Button(self, wx.ID_ANY, "Add Node") #TODO: Change this to remove and test removeChild also.
		addButton.Bind(wx.EVT_BUTTON, self.onAddNode)
		removeButton = wx.Button(self, wx.ID_ANY, "Remove Random Node")
		removeButton.Bind(wx.EVT_BUTTON, self.onRemoveButton)
		sizer.Add(self.tree, 1, wx.EXPAND | wx.LEFT, 5)
		sizer.Add(addButton, 0, wx.EXPAND | wx.LEFT, 5)
		sizer.Add(removeButton, 0, wx.EXPAND | wx.LEFT, 5)
		self.SetSizer(sizer)     
		#Canvas.Bind(FC.EVT_LEFT_UP, self.OnLeftUp) 
		self.Bind(wx.EVT_SIZE, self.onResize)
		
		randomTree(self.tree)
		#predefinedTree(self.tree)
		
		self.Show(True)
		self.tree.Canvas.ZoomToBB()
		
	def onResize(self, event):
		self.tree.Canvas.ZoomToBB()
		event.Skip()
		
	def onRemoveButton(self, event):
		nodes = self.tree._nodes.values()
		nnodes = len(nodes)
		idx = random.randint(0, nnodes-1)
		node = nodes[idx]
		print "Removing %s"%node
		self.tree.removeNode(node)
		self.tree.Canvas.Draw(True)

	def onAddNode(self, event):
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

