##############################################
#FactorStudies from the match table
##############################################
#We have a total of 115 columns, most of them are player columns and the other most is bettings which we cover in:
#- bettingOdds.R 
#- playerData.R
#and in this doc we are going to study the following ones:
#- Goal #Deduce goal times for each match -> GOOD IDEA
#- Posession
#- ShotOn
#- ShotOff
#- FoulCommit
#- Card
#- Corner
#- Stage
#- Date
#The question for the previous values (except stage & date)
#Is this really something that statsbomb can't offer us?
#probably is just better to remove them and take statsbomb ones?


###############################################
# Goals: Does goals time condition victory? 
# if you win inthe first half, you end up winning?
###############################################
match$goal
exampleGoal <- unique(match$goal)[1000]
exampleGoal <- xmlParse(exampleGoal)

#<goal>
#...
  #<value>
  #   <comment>n</comment>
  #   <stats>
  #     <goals>1</goals>
  #     <shoton>1</shoton>
  #   </stats>
  #   <elapsed_plus>3</elapsed_plus>
  #   <event_incident_typefk>393</event_incident_typefk>
  #   <elapsed>90</elapsed>
  #   <player2>46353</player2>
  #   <subtype>shot</subtype>
  #   <player1>40755</player1>
  #   <sortorder>3</sortorder>
  #   <team>8586</team>
  #   <id>1566459</id>
  #   <n>275</n>
  #   <type>goal</type>
  #   <goal_type>n</goal_type>
  #</value>
#...
#</goal>




for(i in match$goal){
  if(!is.na(i)){
    xmlValue <- xmlParse(i)
    #print(xmlValue)
    print(getNodeSet(xmlValue, "//elapsed"))
  }
}

###############################################
# Posession: How posession affects the match?
###############################################
source(loadingDB.R)
match$posession
unique(match$possession)

#We can see two things:
#1- Many NA's, which is a bit surprising. We have to see in which leagues this happens.
#2- Wtf is this formatting that is given to us, kinda disgusting. We might need to see the scrapper.

###############################################
# ShotOn: What exactly is xd
###############################################

match$shoton
exampleShotOn <- unique(match$shoton)[1000]
exampleShotOn <- xmlParse(exampleShotOn)

#we can see the following structure:
#<shoton>
  #<value>
  #</value>
  #...
  #<value>
  #  <stats> #in one case this is missing.
  #     <blocked>1</blocked> can be also <shoton>1</shoton>
  #  </stats>
  #  <event_incident_typefk>61</event_incident_typefk>
  #  <elapsed>73</elapsed>
  #  <subtype>blocked_shot</subtype>
  #  <player1>160713</player1>
  #  <sortorder>3</sortorder>
  #  <team>8528</team>
  #  <n>238</n>
  #  <type>shoton</type>
  #  <id>1465886</id>
  #</value>
  #...
  #<value>
  #</value>
#</shoton>

###############################################
# ShotOff: What exactly is xd
###############################################
match$shotoff
exampleShotOff <- unique(match$shotoff)[1000]
exampleShotOff <- xmlParse(exampleShotOff)

#<shotoff>
  #<value>
  #</value>
  #...
  #<value>
  #  <stats>
  #     <shotoff>1</shotoff> can maybe be something else here??
  #  </stats>
  #  <elapsed_plus>2</elapsed_plus>
  #  <event_incident_typefk>96</event_incident_typefk>
  #  <elapsed>45</elapsed>
  #  <subtype>crossbar</subtype>
  #  <player1>35443</player1>
  #  <sortorder>0</sortorder>
  #  <team>8658</team>
  #  <n>195</n>
  #  <type>shotoff</type>
  #  <id>1465203</id>
  #</value>
  #...
  #<value>
  #</value>
#</shotoff>

###############################################
# FoulCommit
###############################################
match$foulcommit
exampleFoulcommit <- unique(match$foulcommit)[1000]
exampleFoulcommit <- xmlParse(exampleFoulcommit)

#similar format as before, we have something like:

#<foulcommit>
  #<value>
  #</value>
  ...#
  #<value>
    #<stats>
    #  <foulscommitted>1</foulscommitted>
    #</stats>
    #<event_incident_typefk>5</event_incident_typefk>
    #<elapsed>86</elapsed>
    #<player2>37799</player2>
    #<subtype>serious_foul</subtype>
    #<player1>111865</player1>
    #<sortorder>0</sortorder>
    #<team>8528</team>
    #<n>259</n>
    #<type>foulcommit</type>
    #<id>1466141</id>
  #</value>
#</foulcommit>


###############################################
# Card
###############################################
match$card
exampleCard <- unique(match$card)[1000]
exampleCard <- xmlParse( exampleCard)
exampleCard  

#this format seems slightly different tbh

#<card>
#  ...
#  <value>
#    <comment>y</comment>
#    <stats>
#       <ycards>1</ycards>
#    </stats>
#    <event_incident_typefk>73</event_incident_typefk>
#    <elapsed>89</elapsed>
#    <card_type>y</card_type>
#    <subtype>serious_fouls</subtype>
#    <player1>43247</player1>
#    <sortorder>0</sortorder>
#    <team>9879</team>
#    <n>379</n>
#    <type>card</type>
#    <id>1548440</id>
#  </value>
#  ...
#</card>

###############################################
# Cross
###############################################
match$cross
unique(match$cross)

exampleCross <- unique(match$cross)[1000]
exampleCross <- xmlParse(exampleCross)
exampleCross  

#<cross>
  #...
  #<value>
  #  <stats>
  #    <crosses>1</crosses>
  #  </stats>
  #  <elapsed_plus>4</elapsed_plus> (sometimes missing)
  #  <event_incident_typefk>7</event_incident_typefk>
  #  <elapsed>90</elapsed>
  #  <subtype>cross</subtype>
  #  <player1>23787</player1>
  #  <sortorder>1</sortorder>
  #  <team>8658</team>
  #  <n>262</n>
  #  <type>cross</type>
  #  <id>1466264</id>
  #</value>
  #...
#</cross>


###############################################
# Corner
###############################################
match$corner
unique(match$corner)




###############################################

###############################################




##############################################
# Stage: What is?  Crec que és la jornada.
##############################################
match$stage
max(match$stage)
min(match$stage)
#sembla la jornada, sí.
#would be interesting to see trends during stage like, 
#can we see if a team is going to win anyways maybe doesn't play hard that game


##############################################
# Date
##############################################
#I think we should do two variables:
#1- Days of resting of a previous match a team had, 
#2- Number of matches already that season maybe so teams have champions like it stacks up
unique(match$date)[0]
