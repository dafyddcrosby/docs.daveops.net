# Python - PyGTK
@Python 

```python

 import pygtk
 pygtk.require('2.0')
 import gtk
 
 class PyGUI():
 def __init__(self):
 try:
 builder = gtk.Builder()
 builder.add_from_file("gui.glade") 
 except:
 self.stderr("Failed to load Glade template file")
 sys.exit(1)
 
 self.main_window = builder.get_object("main_window")
 
 builder.connect_signals(self)
 
 def main(self):
 self.main_window.show()
 gtk.main()
```
