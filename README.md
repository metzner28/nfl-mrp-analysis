# nfl-mrp-analysis
Estimating the Marginal Revenue Product of the NFL Quarterback

*Abstract*

The National Football League is the most popular professional sports league in the United States, both by viewership numbers and yearly revenue, and these numbers continue to grow each year despite the player-safety and political controversies the league has recently become involved in. The NFL’s 32 teams each operate as separate business entities with their own balance sheets, accruing revenue based on fan attendance and merchandise sales (although some proportion of the total revenue of the league is shared among the teams in a standard revenue sharing agreement similar to that of Major League Baseball or the National Basketball Association) (Norman, 2018). Generally, the quarterback is viewed as one of the most important drivers of success on each team, as the league has shifted in recent years toward an emphasis on offense and passing specifically. This trend has resulted in a monetary premium being placed on quarterback salaries -- in fact, as of 2018, four of the top 10 salaries in the league belong to quarterbacks, out of a possible 22 positions (Pro Football Reference, 2018). Even though football is much more of a "team sport" than baseball, for example, where individual contributions are much more easily quantified, the contributions of quarterbacks toward a team's success are relatively straightforward to measure given their role as the main offensive drivers of each team. Interestingly, though, advanced analytics capable of quantifying individual player performance have not emerged in football to the same extent as other sports such as baseball and basketball. 

In this work, we set out to determine the marginal revenue product of the quarterback, assuming that quarterbacks produce wins based on their statistical performance, and wins determine the yearly revenue of each team. Using a two-stage model built based on data from the past 10 seasons (2008-2017), we will show that a set of quarterback performance statistics is predictive of the wins added by each quarterback per season, from which revenue per season can then also be predicted using an autoregressive framework. Using the two stages of this model, we predict the marginal revenue product of the 32 starting quarterbacks for the 2018 season, comparing predicted values to real salaries to illustrate the presence or absence of the "quarterback premium" in 2018 and determine whether the NFL labor market exists in a perfectly competitive equilibrium.

Raw data is in the "data" folder, and the code written to aggregate and clean the data and build the models is in the "analysis-output" folder, along with the final products.
