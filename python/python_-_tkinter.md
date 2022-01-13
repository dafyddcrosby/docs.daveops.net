# Python - Tkinter

```python

 from Tkinter import *
 
 class MyDialog:
 def __init__(self, parent):
 self.top = Frame(parent)
 self.top.pack()
 
 Label(self.top, text="Value").pack()
 
 self.e = Entry(self.top)
 self.e.pack(padx=5)
 
 b = Button(self.top, text="OK", command=self.ok)
 b.pack(pady=5)
 
 def ok(self):
 print "value is", self.e.get()
 self.top.destroy()
 
 root = Tk()
 root.title("Some title!")
 d = MyDialog(root)
 root.wait_window(d.top)
```
