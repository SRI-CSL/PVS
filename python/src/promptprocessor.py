import re

allegro_prompt_regex = r"^((\[\d+i?c?\] |\[step\] )?((<?[-A-Za-z]* ?\d*>)|([-A-Za-z0-9]+\(\d+\):)|Rule\?|\(Y or N\)|\(Yes or No\)|Please enter.*:) )+"

ALLEGRO = "allegro"
CMU = "cmu"

allegroRe = re.compile(allegro_prompt_regex)

def getLispType():
    global ALLEGRO, CMU
    return ALLEGRO # or CMU

def isPrompt(text):
    matcher = allegroRe.match(text)
    return matcher is not None

def hasPrompt(text):
    found = allegroRe.search(text)
    return found is not None

