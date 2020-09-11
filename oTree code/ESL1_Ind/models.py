
import random

from otree.api import (
    models, widgets, BaseConstants, BaseSubsession, BaseGroup, BasePlayer
)

author = 'Dominik Deffner'
doc = """
Your app description
"""

# Here we define things that are constant for all players #

class Constants(BaseConstants):
    name_in_url = 'ESL1_Ind'
    players_per_group = None
    num_rounds = 100
    # 4 Migration schedules
    # Defined as before, here we randomly select one schedule for individual learner
    Times_of_Migration_1_5 = [5, 25, 45, 65, 85]
    Times_of_Migration_2_6 = [10, 30, 50, 70, 90]
    Times_of_Migration_3_7 = [15, 35, 55, 75, 95]
    Times_of_Migration_4_8 = [20, 40, 60, 80, 100]

    Migration = [Times_of_Migration_1_5, Times_of_Migration_2_6, Times_of_Migration_3_7,
                 Times_of_Migration_4_8]

    random.shuffle(Migration)

    MigrationSch = Migration[0]




# Set payoff schedules for 4 different phases; each phase lasts 25 rounds

Options = ['Weizen', 'Kartoffeln', 'Reis', 'Mais']

random.shuffle(Options)
A = Options[0]
B = Options[1]
C = Options[2]
D = Options[3]


#Define 4 phases of experiment
Phase_1 = list(range(1, 26))
Phase_2 = list(range(26, 51))
Phase_3 = list(range(51, 76))
Phase_4 = list(range(76, 101))

# Choose which farms are relatively hard and which are relatively easy
Hard_Phases = random.sample(list(range(1, 5)), 2)
SD_Hard = 3
SD_Simple = 1.5

# Choose which farms give relatively high or low payoff
#Expected payoff in low-payoff farm is always 3 points lower
PayoffsBetter = random.sample([13, 15, 17, 19], 4)
PayoffsWorse = [x-3 for x in PayoffsBetter]


class Subsession(BaseSubsession):
    Payoff_Better = models.IntegerField()
    Payoff_Worse = models.IntegerField()
    SD_payoff = models.FloatField()
    Phase = models.IntegerField()
    Group = models.IntegerField()
    better_crop = models.CharField()

# Define average payoffs for 4 phases

    def creating_session(self):
        for m in Phase_1:
            self.in_round(m).Payoff_Better = PayoffsBetter[0]
            self.in_round(m).Payoff_Worse = PayoffsWorse[0]

        for m in Phase_2:
            self.in_round(m).Payoff_Better = PayoffsBetter[1]
            self.in_round(m).Payoff_Worse = PayoffsWorse[1]

        for m in Phase_3:
            self.in_round(m).Payoff_Better = PayoffsBetter[2]
            self.in_round(m).Payoff_Worse = PayoffsWorse[2]

        for m in Phase_4:
            self.in_round(m).Payoff_Better = PayoffsBetter[3]
            self.in_round(m).Payoff_Worse = PayoffsWorse[3]

#Assign Phase variable

        for m in Phase_1:
            self.in_round(m).Phase = 1
        for m in Phase_2:
            self.in_round(m).Phase = 2
        for m in Phase_3:
            self.in_round(m).Phase = 3
        for m in Phase_4:
            self.in_round(m).Phase = 4

#Assign group variable

        for m in list(range(1, 101)):
            if m <= min(Constants.MigrationSch):
                self.in_round(m).Group = 1
            if min(Constants.MigrationSch) < m <= Constants.MigrationSch[1]:
                self.in_round(m).Group = 2
            if Constants.MigrationSch[1] < m <= Constants.MigrationSch[2]:
                self.in_round(m).Group = 1
            if Constants.MigrationSch[2] < m <= Constants.MigrationSch[3]:
                self.in_round(m).Group = 2
            if Constants.MigrationSch[3] < m <= Constants.MigrationSch[4]:
                self.in_round(m).Group = 1
            if Constants.MigrationSch[4] < m <= max(Constants.MigrationSch):
                self.in_round(m).Group = 2
            if m > max(Constants.MigrationSch):
                self.in_round(m).Group = 1


# Define SDs for 4 phases

        if 1 in Hard_Phases:
            for m in Phase_1:
                self.in_round(m).SD_payoff = SD_Hard
        else:
            for m in Phase_1:
                self.in_round(m).SD_payoff = SD_Simple

        if 2 in Hard_Phases:
            for m in Phase_2:
                self.in_round(m).SD_payoff = SD_Hard
        else:
            for m in Phase_2:
                self.in_round(m).SD_payoff = SD_Simple

        if 3 in Hard_Phases:
            for m in Phase_3:
                self.in_round(m).SD_payoff = SD_Hard
        else:
            for m in Phase_3:
                self.in_round(m).SD_payoff = SD_Simple

        if 4 in Hard_Phases:
            for m in Phase_4:
                self.in_round(m).SD_payoff = SD_Hard
        else:
            for m in Phase_4:
                self.in_round(m).SD_payoff = SD_Simple

# Assign optimal crop for both groups

        for m in Phase_1:
            if self.in_round(m).Group == 1:
                self.in_round(m).better_crop = A
            if self.in_round(m).Group == 2:
                self.in_round(m).better_crop = B

        for m in Phase_2:
            if self.in_round(m).Group == 1:
                self.in_round(m).better_crop = C
            if self.in_round(m).Group == 2:
                self.in_round(m).better_crop = D

        for m in Phase_3:
            if self.in_round(m).Group == 1:
                self.in_round(m).better_crop = B
            if self.in_round(m).Group == 2:
                self.in_round(m).better_crop = A

        for m in Phase_4:
            if self.in_round(m).Group == 1:
                self.in_round(m).better_crop = D
            if self.in_round(m).Group == 2:
                self.in_round(m).better_crop = C


class Group(BaseGroup):
    pass

class Player(BasePlayer):

    age = models.IntegerField()
    gender = models.StringField()
    lang = models.StringField()
    edu = models.StringField()

    crop_choice = models.StringField(
        choices=['Weizen', 'Kartoffeln', 'Reis', 'Mais'],
        widget=widgets.RadioSelect
    )

    experience = models.IntegerField()

#String field for mouse-tracking data
    procdata = models.StringField(blank=True)


    def define_experience(self):
        x = Constants.MigrationSch
        if self.subsession.round_number <= min(x):
            self.experience = self.subsession.round_number
        else:
            last_migration = max([x for x in x if x < self.subsession.round_number])
            self.experience = self.subsession.round_number - last_migration


    def define_payoff(self):
        if self.round_number != 100:
            if self.crop_choice == self.subsession.better_crop:
                self.payoff = round(random.gauss(self.subsession.Payoff_Better,
                                            self.subsession.SD_payoff))
                while self.payoff < 0:
                    self.payoff = round(random.gauss(self.subsession.Payoff_Better,
                                               self.subsession.SD_payoff))
            else:
                self.payoff = round(random.gauss(self.subsession.Payoff_Worse,
                                                 self.subsession.SD_payoff))
                while self.payoff < 0:
                    self.payoff = round(random.gauss(self.subsession.Payoff_Worse,
                                               self.subsession.SD_payoff))

        else:
            if self.crop_choice == self.in_round(99).subsession.better_crop:
                self.payoff = round(random.gauss(self.subsession.Payoff_Better,
                                                 self.subsession.SD_payoff))
                while self.payoff < 0:
                    self.payoff = round(random.gauss(self.subsession.Payoff_Better,
                                               self.subsession.SD_payoff))
            else:
                self.payoff = round(random.gauss(self.subsession.Payoff_Worse,
                                                 self.subsession.SD_payoff))
                while self.payoff < 0:
                    self.payoff = round(random.gauss(self.subsession.Payoff_Better,
                                               self.subsession.SD_payoff))

