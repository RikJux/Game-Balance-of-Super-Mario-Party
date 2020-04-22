# Game-Balance-of-Super-Mario-Party

This is the main idea: Each one of the 6-faces dice have some common 'resources' that are: - the expected movement in each turn, i.e. the mean of movement faces -the expected coin gain/loss in each turn, i.e. the mean of coin faces -the different options of movement, i.e. the variety of movement faces (due to the many 'special events' squares and different routes, players with more options may be in advantage) The three resources above are favourable to the player, so are to be maximized.

To every mean there is a variance associated, which can be favourable (risk-seeking p.o.v.) or unfavorable (risk-adversion p.o.v.).

As long as we want the game to be balanced, we can't optimize all the resources values to infinity: there must occur trade-offs between resources so that in improving a resource an other is worsened.

For each pair of resources a die can be seen as a market basket and therefore compared with all the others in terms of 'die X is better in this trade-off than die Y'. Therefore for each trade-off we can make a trade-off dice ranking.

From the games we can have an idea of which die is better than the other through the frequency of victories for each character; basically a dice ranking based on victories, or a performance dice ranking.

The objective is: Determine which trade-off rankings are significant descriptors of the performance dice-ranking

FURTHER INFORMATION ABOUT DATA: https://www.kaggle.com/riccardogiussani/super-mario-party-dice
