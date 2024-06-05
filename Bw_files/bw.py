from kivy.app import App
from kivy.uix.gridlayout import GridLayout
from kivy.uix.label import Label
from kivy.uix.button import Button
from kivy.uix.textinput import TextInput
import kivy.clock as CLOCK
from kivy.uix.widget import Widget
from kivy.uix.floatlayout import FloatLayout
from rpy2.robjects import RObject

class waterBalance(GridLayout):
    pass


class bwapp(App):
    def build(self):
        return waterBalance
            
       
if __name__ == '__main__':
    bwapp().run()