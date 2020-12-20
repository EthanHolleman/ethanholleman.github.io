import random

def make_teams():
    names = [
        "Ethan Holleman",
        "Gerald",
        "Hongru Hu",
        "Arielle Yoo",
        "Jing Lyu",
        "Lacey Walker",
        "Nelson",
        "Rita",
        "Xinzhe Li",
        "Yongin Choi"
    ]
    random.shuffle(names)
    midpoint = round(len(names) / 2)
    print('Team 1:', ', '.join(names[:midpoint]))
    print('Team 2:', ', '.join(names[midpoint:]))