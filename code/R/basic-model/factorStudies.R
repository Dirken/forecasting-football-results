##############################################
#FactorStudies from the match table
##############################################

##################################################################
# Goal, Posession,ShotOn, ShotOff, FoulCommit, Card, Cross,Corner
##################################################################
match$cross
unique(match$cross)

exampleCross <- unique(match$cross)[1000]
exampleCross <- xmlParse(exampleCross)
exampleCross  
#XML example:
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
#We need to convert this into tabular data.
#(based on https://www.kaggle.com/njitram/exploring-the-incident-data)
###############################################
#IncidentDataTransformation xml -> table
###############################################
#source(incidentDataTransformation.R)
readRDS("./rds/incidents.rds")

###############################################
# Incident Data Analysis
###############################################

source(factorCards.R)
source(factorGoals.R)
source(factorCorner.R)
source(factorShotOff.R)
source(factorShotOn.R)
source(factorCommit.R)
source(factorPosession.R)

##############################################
# Other data
##############################################


#######################
#Date
#######################
#I think we should do two variables:
#1- Days of resting of a previous match a team had, 
#2- Number of matches already that season maybe so teams have champions like it stacks up
unique(match$date)[0]
