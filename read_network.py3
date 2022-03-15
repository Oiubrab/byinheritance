import gi

gi.require_version("Gtk", "3.0")
from gi.repository import Gtk


think_thing = open("think2.txt")
think_read = think_thing.read()
think_net_line = []
think_net = []


for position in think_read:
	if position=='1' or position=='0':
		think_net_line+=[position]
	
	if position=='\n':
		think_net+=[think_net_line]
		think_net_line=[]

class GridWindow(Gtk.Window):

		
	def __init__(self):

		super().__init__(title="think2")
		

		
		grid = Gtk.Grid()
		boxer = Gtk.Box()
		
		self.start_button = Gtk.Button(label="Start Check",halign=Gtk.Align.START)
		self.start_button.connect("clicked", self.on_button_clicked)
		
		for rower,line in enumerate(think_net):
			for columner,datum in enumerate(line):
				labelx=Gtk.Label(label=" "+datum+" ")
				grid.attach(labelx,columner,rower,1,1)
		
		grid.attach(self.start_button,columner+1,rower+1,1,2)
				
		self.add(grid)

	def on_button_clicked(self, widget):
		think_thing = open("think2.txt")
		think_read = think_thing.read()
		think_net_line = []
		think_net = []


		for position in think_read:
			if position=='1' or position=='0':
				think_net_line+=[position]
			
			if position=='\n':
				think_net+=[think_net_line]
				think_net_line=[]
				
		grid = Gtk.Grid()	
		for rower,line in enumerate(think_net):
			for columner,datum in enumerate(line):
				labelx=Gtk.Label(label=" "+datum+" ")
				grid.attach(labelx,columner,rower,1,1)
		
		
winner = GridWindow()
winner.connect("destroy", Gtk.main_quit)
winner.show_all()
Gtk.main()		

