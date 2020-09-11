from os import environ

# if you set a property in SESSION_CONFIG_DEFAULTS, it will be inherited by all configs
# in SESSION_CONFIGS, except those that explicitly override it.
# the session config can be accessed from methods in your apps as self.session.config,
# e.g. self.session.config['participation_fee']

SESSION_CONFIG_DEFAULTS = {
    'real_world_currency_per_point': 0.005,
    'participation_fee': 6.00,
    'doc': "",
}

SESSION_CONFIGS = [
    {
        'name': 'ESL1',
        'display_name': 'ESL1',
        'num_demo_participants': 8,
        'app_sequence': ['ESL1'],
    },
    {
        'name': 'ESL1_Ind',
        'display_name': 'ESL1_Ind',
        'num_demo_participants': 1,
        'app_sequence': ['ESL1_Ind'],
    },
]
# see the end of this file for the inactive session configs


# ISO-639 code
# for example: de, fr, ja, ko, zh-hans
LANGUAGE_CODE = 'de'

# e.g. EUR, GBP, CNY, JPY
REAL_WORLD_CURRENCY_CODE = 'EUR'
USE_POINTS = True
POINTS_DECIMAL_PLACES=0
REAL_WORLD_CURRENCY_DECIMAL_PLACES=2


ROOMS = [
    {
        'name': 'MPI_EVA_lab1',
        'display_name': 'MPI EVA lab1',
        'participant_label_file': '_rooms/LabelList1.txt',
    },
    {
        'name': 'MPI_EVA_lab2',
        'display_name': 'MPI EVA lab2',
        'participant_label_file': '_rooms/LabelList2.txt',
    }
]


ADMIN_USERNAME = 'MPI_EVA_Lab'
# for security, best to set admin password in an environment variable
ADMIN_PASSWORD = environ.get('ESL1')

#OTREE_AUTH_LEVEL = 'DEMO'

#OTREE_AUTH_LEVEL = environ.get('DEMO')

# the environment variable OTREE_PRODUCTION controls whether Django runs in
# DEBUG mode. If OTREE_PRODUCTION==1, then DEBUG=False

#if environ.get('OTREE_PRODUCTION') not in {None, '', '0'}:
#    DEBUG = True
#else:

DEBUG = False




DEMO_PAGE_INTRO_HTML = """
Here are various games implemented with 
oTree. These games are open
source, and you can modify them as you wish.
"""

# don't share this with anybody.
SECRET_KEY = '7u)=mkft878q0wkq&u2xxzb@lj*5#odq&(ln40(4n&9n^1761c'

# if an app is included in SESSION_CONFIGS, you don't need to list it here
INSTALLED_APPS = ['otree']

