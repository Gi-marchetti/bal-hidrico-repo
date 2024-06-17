from kivy.app import App
from kivy.lang import Builder
from kivy.uix.gridlayout import GridLayout
from kivy.uix.label import Label
from kivy.uix.button import Button
from kivy.uix.textinput import TextInput
import kivy.clock as CLOCK
from kivy.uix.widget import Widget
from kivy.uix.floatlayout import FloatLayout
from kivy.uix.image import Image
from kivy.core.window import Window
from kivy.uix.screenmanager import Screen, ScreenManager
from kivy.core.audio import SoundLoader
import os
from rpy2.situation import get_r_home
os.environ["R_HOME"] = get_r_home()
from rpy2.robjects import RObject
audio = 0
Window.clearcolor = 0,1,1,1,

class HomeScreen(Screen):
    def play_sound(self,music):
        global sound
        sound = SoundLoader.load(music)
        if sound:
            sound.play()

            

class PrevTempo(Screen):
    pass

class PrevClim(Screen):
    pass

class BalHidro(Screen):
    pass

class WindowManager(ScreenManager):
    def stop_sound(self):
        global sound
        sound.stop()

kv = Builder.load_file("bw.kv")

class bwapp(App):
    def build(self):
        return kv
            
       
if __name__ == '__main__':
    bwapp().run()