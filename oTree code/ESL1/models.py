
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
    name_in_url = 'ESL1'
    players_per_group = 4
    num_rounds = 100

# Set payoff schedules for 4 different phases; each phase lasts 25 rounds


Options = ['Weizen', 'Kartoffeln', 'Reis', 'Mais']

random.shuffle(Options)
A = Options[0]
B = Options[1]
C = Options[2]
D = Options[3]

# Here we define define when individual participants are migrating
Times_of_Migration_1_5 = [5, 25, 45, 65, 85]
Times_of_Migration_2_6 = [10, 30, 50, 70, 90]
Times_of_Migration_3_7 = [15, 35, 55, 75, 95]
Times_of_Migration_4_8 = [20, 40, 60, 80, ]

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
    Payoff_Better = models.FloatField()
    Payoff_Worse = models.FloatField()
    SD_payoff = models.FloatField()

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
            for f in self.get_groups():
                if f.id_in_subsession == 1:
                    f.in_round(m).better_crop = A
                if f.id_in_subsession == 2:
                    f.in_round(m).better_crop = B

        for m in Phase_2:
            for f in self.get_groups():
                if f.id_in_subsession == 1:
                    f.in_round(m).better_crop = C
                if f.id_in_subsession == 2:
                    f.in_round(m).better_crop = D

        for m in Phase_3:
            for f in self.get_groups():
                if f.id_in_subsession == 1:
                    f.in_round(m).better_crop = B
                if f.id_in_subsession == 2:
                    f.in_round(m).better_crop = A

        for m in Phase_4:
            for f in self.get_groups():
                if f.id_in_subsession == 1:
                    f.in_round(m).better_crop = D
                if f.id_in_subsession == 2:
                    f.in_round(m).better_crop = C

# Set group structure; every 4 rounds an individual moves from one region to the other

        if len(self.get_players()) == 8:
            if self.round_number in list(range(1, 6))+list(range(41, 46)) + list(range(81, 86)):
                afterMigration = [[1, 2, 3, 4], [5, 6, 7, 8]]
                self.set_group_matrix(afterMigration)

            if self.round_number in list(range(6, 11))+list(range(46, 51)) + list(range(86, 91)):
                afterMigration = [[5, 2, 3, 4], [1, 6, 7, 8]]
                self.set_group_matrix(afterMigration)

            if self.round_number in list(range(11, 16))+list(range(51, 56)) + list(range(91, 96)):
                afterMigration = [[5, 6, 3, 4], [1, 2, 7, 8]]
                self.set_group_matrix(afterMigration)

            if self.round_number in list(range(16, 21))+list(range(56, 61)) + list(range(96, 101)):
                afterMigration = [[5, 6, 7, 4], [1, 2, 3, 8]]
                self.set_group_matrix(afterMigration)

            if self.round_number in list(range(21, 26))+list(range(61, 66)):
                afterMigration = [[5, 6, 7, 8], [1, 2, 3, 4]]
                self.set_group_matrix(afterMigration)

            if self.round_number in list(range(26, 31))+list(range(66, 71)):
                afterMigration = [[1, 6, 7, 8], [5, 2, 3, 4]]
                self.set_group_matrix(afterMigration)

            if self.round_number in list(range(31, 36))+list(range(71, 76)):
                afterMigration = [[1, 2, 7, 8], [5, 6, 3, 4]]
                self.set_group_matrix(afterMigration)

            if self.round_number in list(range(36, 41))+list(range(76, 81)):
                afterMigration = [[1, 2, 3, 8], [5, 6, 7, 4]]
                self.set_group_matrix(afterMigration)


class Group(BaseGroup):
    better_crop = models.CharField()

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

#Define fields for information visible to participants from 3 neighbors

    Choice1 = models.StringField()
    Choice2 = models.StringField()
    Choice3 = models.StringField()

    Exp1 = models.IntegerField()
    Exp2 = models.IntegerField()
    Exp3 = models.IntegerField()



    def define_experience(self):
        if self.participant.id_in_session in [1, 5]:
            x = Times_of_Migration_1_5
            if self.subsession.round_number <= min(x):
                self.experience = self.subsession.round_number
            else:
                last_migration = max([x for x in x if x < self.subsession.round_number])
                self.experience = self.subsession.round_number - last_migration

        if self.participant.id_in_session in [2, 6]:
            x = Times_of_Migration_2_6
            if self.subsession.round_number <= min(x):
                self.experience = self.subsession.round_number
            else:
                last_migration = max([x for x in x if x < self.subsession.round_number])
                self.experience = self.subsession.round_number - last_migration

        if self.participant.id_in_session in [3, 7]:
            x = Times_of_Migration_3_7
            if self.subsession.round_number <= min(x):
                self.experience = self.subsession.round_number
            else:
                last_migration = max([x for x in x if x < self.subsession.round_number])
                self.experience = self.subsession.round_number - last_migration

        if self.participant.id_in_session in [4, 8]:
            x = Times_of_Migration_4_8
            if self.subsession.round_number <= min(x):
                self.experience = self.subsession.round_number
            else:
                last_migration = max([x for x in x if x < self.subsession.round_number])
                self.experience = self.subsession.round_number - last_migration

    def define_payoff(self):
        if self.round_number != 100:
            if self.crop_choice == self.group.better_crop:
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
            if self.crop_choice == self.in_round(99).group.better_crop:
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

