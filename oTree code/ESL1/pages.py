from otree.api import Currency as c, currency_range
from otree.models import player

import random

from ._builtin import Page, WaitPage
from .models import Constants


# Here we define define when Participants are migrating
Times_of_Migration_1_5 = [5, 25, 45, 65, 85]
Times_of_Migration_2_6 = [10, 30, 50, 70, 90]
Times_of_Migration_3_7 = [15, 35, 55, 75, 95]
Times_of_Migration_4_8 = [20, 40, 60, 80, ]

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
    wait_for_all_groups = True

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
    wait_for_all_groups = True

    def is_displayed(self):
        return self.round_number == 1

    def after_all_players_arrive(self):
        for player in self.group.get_players():
            player.define_payoff()
            player.define_experience()


class ConsecutiveRounds (Page):
    def is_displayed(self):
        return self.round_number > 1

    def vars_for_template(self):

        crop_choice_agent = self.player.in_round(self.round_number - 1).crop_choice
        crop_choice_others = []

        others = self.player.get_others_in_group()
        random.shuffle(others)

        for p in others:
            if p.in_round(self.round_number).experience != 1:
                crop_choice_others.append(p.in_round(self.round_number - 1).crop_choice)
            else:
                crop_choice_others.append('New_Member')

        payoff_agent = self.player.in_round(self.round_number - 1).payoff
        payoff_others = []
        for p in others:
            payoff_others.append(p.in_round(self.round_number - 1).payoff)

        experience_agent = self.player.in_round(self.round_number).experience
        experience_others = []
        for p in others:
            experience_others.append(p.in_round(self.round_number).experience)

        # Store info about boxes in database
        self.player.in_round(self.round_number).Choice1 = crop_choice_others[0]
        self.player.in_round(self.round_number).Choice2 = crop_choice_others[1]
        self.player.in_round(self.round_number).Choice3 = crop_choice_others[2]

        self.player.in_round(self.round_number).Exp1 = experience_others[0]
        self.player.in_round(self.round_number).Exp2 = experience_others[1]
        self.player.in_round(self.round_number).Exp3 = experience_others[2]

        return{'crop_choice_agent': crop_choice_agent+'.jpg',
               'crop_choice_partner1': crop_choice_others[0]+'.jpg',
               'crop_choice_partner2': crop_choice_others[1]+'.jpg',
               'crop_choice_partner3': crop_choice_others[2]+'.jpg',
               'payoff_agent': payoff_agent,
               'payoff_1': payoff_others[0],
               'payoff_2': payoff_others[1],
               'payoff_3': payoff_others[2],
               'experience_agent': experience_agent,
               'experience_1': experience_others[0],
               'experience_2': experience_others[1],
               'experience_3': experience_others[2],
               }

    form_model = 'player'
    form_fields = ['crop_choice',
                   'procdata']

    def crop_choice_choices(self):
        choices = ['Weizen', 'Kartoffeln', 'Reis', 'Mais']
        random.shuffle(choices)
        return choices


class WaitPageRound(WaitPage):
    wait_for_all_groups = True

    def is_displayed(self):
        return self.round_number >= 1

    def after_all_players_arrive(self):
        for group in self.subsession.get_groups():

            for x in group.get_players():
                x.define_payoff()
                x.define_experience()


class Migration (Page):
    def is_displayed(self):
        if self.participant.id_in_session in [1, 5]:
            return self.round_number in Times_of_Migration_1_5
        if self.participant.id_in_session in [2, 6]:
            return self.round_number in Times_of_Migration_2_6
        if self.participant.id_in_session in [3, 7]:
            return self.round_number in Times_of_Migration_3_7
        if self.participant.id_in_session in [4, 8]:
            return self.round_number in Times_of_Migration_4_8


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
    Migration,
    SeasonChange,
    FinalPage
]
