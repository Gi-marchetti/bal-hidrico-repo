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
from kivy.uix.behaviors import ButtonBehavior
from kivy.properties import StringProperty
import os
from rpy2.situation import get_r_home
os.environ["R_HOME"] = get_r_home()
from rpy2.robjects import RObject
import requests  # for using API
import xml.etree.ElementTree as ET  # for parsing XML
Window.clearcolor = 0,1,1,1,


class HomeScreen(Screen):
    def play_sound(self,music):
        global sound
        sound = SoundLoader.load(music)
        if sound:
            sound.play()         

class PrevTempo(Screen):
    texto = StringProperty()
    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        info = []
        request_url = 'http://servicos.cptec.inpe.br/XML/cidade/7dias/244/previsao.xml'
        r = requests.get(request_url)
        root = ET.fromstring(r.content)
        siglas = {"ec": "Encoberto com Chuvas Isoladas",
                "ci": "Chuvas Isoladas",
                "c": "Chuva",
                "in": "Instável",
                "pp": "Poss. de Pancadas de Chuva",
                "cm": "Chuva pela Manhã",
                "cn": "Chuva a Noite",
                "pt": "Pancadas de Chuva a Tarde",
                "pm": "Pancadas de Chuva pela Manhã",
                "np": "Nublado e Pancadas de Chuva",
                "pc": "Pancadas de Chuva",
                "pn": "Parcialmente Nublado",
                "cv": "Chuvisco",
                "ch": "Chuvoso",
                "t": "Tempestade",
                "ps": "Predomínio de Sol",
                "e": "Encoberto",
                "n": "Nublado",
                "cl": "Céu Claro",
                "nv": "Nevoeiro",
                "g": "Geada",
                "ne": "Neve",
                "nd": "Não Definido",
                "pnt": "Pancadas de Chuva a Noite",
                "psc": "Possibilidade de Chuva",
                "pcm": "Possibilidade de Chuva pela Manhã",
                "pct": "Possibilidade de Chuva a Tarde",
                "pcn": "Possibilidade de Chuva a Noite",
                "npt": "Nublado com Pancadas a Tarde",
                "npn": "Nublado com Pancadas a Noite",
                "ncn": "Nublado com Possibilidade de Chuva a Noite",
                "nct": "Nublado com Possibilidade de Chuva a Tarde",
                "ncm": "Nublado com Possibilidade de Chuva pela Manhã",
                "npm": "Nublado com Pancadas pela Manhã",
                "npp": "Nublado com Possibilidade de Chuva",
                "vn": "Variação de Nebulosidade",
                "ct": "Chuva a Tarde",
                "ppn": "Possibilidade de Pancada de Chuva a Noite",
                "ppt": "Possibilidade de Pancada de Chuva a Tarde",
                "ppm": "Possibilidade de Pancada de Chuva pela Manhã"}
        for previsao in root.iter('previsao'):
            dia = previsao.find('dia').text
            tempo = previsao.find('tempo').text
            maxima = previsao.find('maxima').text
            minima = previsao.find('minima').text
            info.append(f'{dia}, {siglas.get(tempo)}, {maxima}ºC, {minima}ºC')
        
        self.texto = "    /    ".join(info)
        print(self.texto)
   

class PrevClim(Screen):
    pass

class BalHidro(Screen):
    pass

class ImageButton(ButtonBehavior,Image):
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