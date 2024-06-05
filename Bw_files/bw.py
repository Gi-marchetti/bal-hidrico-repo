from kivy.app import App
from kivy.uix.gridlayout import GridLayout
from kivy.uix.label import Label
from kivy.uix.button import Button
from kivy.uix.textinput import TextInput
import kivy.clock as CLOCK
from kivy.uix.widget import Widget
from kivy.uix.floatlayout import FloatLayout
from kivy.uix.image import Image
from rpy2.robjects import RObject

class waterBalance(Button):
    pass        


class bwapp(App):
    def build(self):
        self.window = GridLayout()
        self.window.cols = 1
        self.window.add_widget(Image(source = "background.png"))
            
       
if __name__ == '__main__':
    bwapp().run()