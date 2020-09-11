from otree.api import Currency as c, currency_range
from otree.models import player

import random

from ._builtin import Page, WaitPage
from .models import Constants


class WelcomePage(Page):
    def is_displayed(self):
        return self.round_number == 1

class Survey(Page):
    def is_displayed(self):
        return self.round_number == 1

    form_model = 'player'
    form_fields = ['age', 'gender', 'lang', 'edu']

class FinalPage(Page):
    def is_displayed(self):
        return self.round_number == Constants.num_rounds

class WaitPageInstructions(WaitPage):
    def is_displayed(self):
        return self.round_number == 1

    def after_all_players_arrive(self):
        pass


class FirstRound (Page):
    def is_displayed(self):
        return self.round_number == 1

    def vars_for_template(self):
        image_order = ['Weizen', 'Kartoffeln', 'Reis', 'Mais']
        random.shuffle(image_order)
        return{
            'image1': image_order[0] + '.jpg',
            'image2': image_order[1] + '.jpg',
            'image3': image_order[2] + '.jpg',
            'image4': image_order[3] + '.jpg'
        }


    form_model = 'player'
    form_fields = ['crop_choice']

    def crop_choice_choices(self):
        choices = ['Weizen', 'Kartoffeln', 'Reis', 'Mais']
        random.shuffle(choices)
        return choices


class WaitPageFirst_Round(WaitPage):
    def is_displayed(self):
        return self.round_number == 1

    def after_all_players_arrive(self):
        for x in self.subsession.get_players():
            x.define_payoff()
            x.define_experience()




class ConsecutiveRounds (Page):
    def is_displayed(self):
        return self.round_number > 1

    def vars_for_template(self):

        crop_choice_agent = self.player.in_round(self.round_number - 1).crop_choice

        payoff_agent = self.player.in_round(self.round_number - 1).payoff

        experience_agent = self.player.in_round(self.round_number).experience

        return{'crop_choice_agent': crop_choice_agent+'.jpg',
               'payoff_agent': payoff_agent,
               'experience_agent': experience_agent,
               }

    form_model = 'player'
    form_fields = ['crop_choice',
                   'procdata']

    def crop_choice_choices(self):
        choices = ['Weizen', 'Kartoffeln', 'Reis', 'Mais']
        random.shuffle(choices)
        return choices


class WaitPageRound(WaitPage):
    def is_displayed(self):
        return self.round_number >= 1

    def after_all_players_arrive(self):
        for x in self.subsession.get_players():
            x.define_payoff()
            x.define_experience()


class Migration (Page):
    def is_displayed(self):
        return self.round_number in Constants.MigrationSch


class SeasonChange (Page):
    def is_displayed(self):
        return self.round_number in [25, 50, 75]


page_sequence = [
    WelcomePage,
    Survey,
    WaitPageInstructions,
    FirstRound,
    WaitPageRound,
    ConsecutiveRounds,
    WaitPageRound,
    SeasonChange,
    Migration,
    FinalPage
]
