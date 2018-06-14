########################################################################################################
# Bradley-Terry Simulation of the NCAA Men's Basketball Tournament, Based on Pre-set Strength Parameters
#
# AUTHOR: Brady T. West
# UPDATED: March 2018
########################################################################################################

# Enter vector of 64 strength parameters (after play-in games)

strengths64 <- c(63,3,44,30,51,23,49,18,36,20,48,6,31,38,58,11,53,1,29,34,43,17,55,13,45,24,54,14,40,22,57,5,64,2,32,27,56,
16,50,8,47,21,52,10,37,40,60,4,59,9,41,26,42,19,35,12,46,25,61,15,28,33,62,8)

# Function to simulate tournaments and save resulting expectations

winner64 <- function(ntourn,strengths) 
{
nowins <- data.frame(team = seq(1:64),win0 = rep(0,64),win1 = rep(0,64),win2 = rep(0,64),win3 = rep(0,64),win4 =  rep(0,64),win5 = rep(0,64),win6 = rep(0,64))

for (i in 1:ntourn)
{

################
# Region 1 Code
################

game33 <- data.frame(team1 = 0, team1rat = 0, team2 = 0, team2rat = 0)

game1 <- data.frame(team1 = 1, team1rat = strengths[1], team2 = 2, team2rat = strengths[2])
btprob1 <- game1$team1rat / (game1$team1rat + game1$team2rat)
test1 <- runif(1)
if (test1 <= btprob1)
{
   game33$team1 <- game1$team1
   game33$team1rat <- game1$team1rat
   nowins[game1$team2,2] <- nowins[game1$team2,2] + 1  
}
else
{
   game33$team1 <- game1$team2
   game33$team1rat <- game1$team2rat
   nowins[game1$team1,2] <- nowins[game1$team1,2] + 1 
}

game2 <- data.frame(team1 = 3, team1rat = strengths[3], team2 = 4, team2rat = strengths[4])
btprob2 <- game2$team1rat / (game2$team1rat + game2$team2rat)
test2 <- runif(1)
if (test2 <= btprob2)
{
   game33$team2 <- game2$team1
   game33$team2rat <- game2$team1rat
   nowins[game2$team2,2] <- nowins[game2$team2,2] + 1  
}
else
{
   game33$team2 <- game2$team2
   game33$team2rat <- game2$team2rat
   nowins[game2$team1,2] <- nowins[game2$team1,2] + 1 
}

game34 <- data.frame(team1 = 0, team1rat = 0, team2 = 0, team2rat = 0)

game3 <- data.frame(team1 = 5, team1rat = strengths[5], team2 = 6, team2rat = strengths[6])
btprob3 <- game3$team1rat / (game3$team1rat + game3$team2rat)
test3 <- runif(1)
if (test3 <= btprob3)
{
   game34$team1 <- game3$team1
   game34$team1rat <- game3$team1rat
   nowins[game3$team2,2] <- nowins[game3$team2,2] + 1  
}
else
{
   game34$team1 <- game3$team2
   game34$team1rat <- game3$team2rat
   nowins[game3$team1,2] <- nowins[game3$team1,2] + 1 
}

game4 <- data.frame(team1 = 7, team1rat = strengths[7], team2 = 8, team2rat = strengths[8])
btprob4 <- game4$team1rat / (game4$team1rat + game4$team2rat)
test4 <- runif(1)
if (test4 <= btprob4)
{
   game34$team2 <- game4$team1
   game34$team2rat <- game4$team1rat
   nowins[game4$team2,2] <- nowins[game4$team2,2] + 1  
}
else
{
   game34$team2 <- game4$team2
   game34$team2rat <- game4$team2rat
   nowins[game4$team1,2] <- nowins[game4$team1,2] + 1 
}

game35 <- data.frame(team1 = 0, team1rat = 0, team2 = 0, team2rat = 0)

game5 <- data.frame(team1 = 9, team1rat = strengths[9], team2 = 10, team2rat = strengths[10])
btprob5 <- game5$team1rat / (game5$team1rat + game5$team2rat)
test5 <- runif(1)
if (test5 <= btprob5)
{
   game35$team1 <- game5$team1
   game35$team1rat <- game5$team1rat
   nowins[game5$team2,2] <- nowins[game5$team2,2] + 1  
}
else
{
   game35$team1 <- game5$team2
   game35$team1rat <- game5$team2rat
   nowins[game5$team1,2] <- nowins[game5$team1,2] + 1 
}

game6 <- data.frame(team1 = 11, team1rat = strengths[11], team2 = 12, team2rat = strengths[12])
btprob6 <- game6$team1rat / (game6$team1rat + game6$team2rat)
test6 <- runif(1)
if (test6 <= btprob6)
{
   game35$team2 <- game6$team1
   game35$team2rat <- game6$team1rat
   nowins[game6$team2,2] <- nowins[game6$team2,2] + 1  
}
else
{
   game35$team2 <- game6$team2
   game35$team2rat <- game6$team2rat
   nowins[game6$team1,2] <- nowins[game6$team1,2] + 1 
}

game36 <- data.frame(team1 = 0, team1rat = 0, team2 = 0, team2rat = 0)

game7 <- data.frame(team1 = 13, team1rat = strengths[13], team2 = 14, team2rat = strengths[14])
btprob7 <- game7$team1rat / (game7$team1rat + game7$team2rat)
test7 <- runif(1)
if (test7 <= btprob7)
{
   game36$team1 <- game7$team1
   game36$team1rat <- game7$team1rat
   nowins[game7$team2,2] <- nowins[game7$team2,2] + 1  
}
else
{
   game36$team1 <- game7$team2
   game36$team1rat <- game7$team2rat
   nowins[game7$team1,2] <- nowins[game7$team1,2] + 1 
}

game8 <- data.frame(team1 = 15, team1rat = strengths[15], team2 = 16, team2rat = strengths[16])
btprob8 <- game8$team1rat / (game8$team1rat + game8$team2rat)
test8 <- runif(1)
if (test8 <= btprob8)
{
   game36$team2 <- game8$team1
   game36$team2rat <- game8$team1rat
   nowins[game8$team2,2] <- nowins[game8$team2,2] + 1  
}
else
{
   game36$team2 <- game8$team2
   game36$team2rat <- game8$team2rat
   nowins[game8$team1,2] <- nowins[game8$team1,2] + 1 
}

game49 <- data.frame(team1 = 0, team1rat = 0, team2 = 0, team2rat = 0)

btprob33 <- game33$team1rat / (game33$team1rat + game33$team2rat)
test33 <- runif(1)
if (test33 <= btprob33)
{
   game49$team1 <- game33$team1
   game49$team1rat <- game33$team1rat
   nowins[game33$team2,3] <- nowins[game33$team2,3] + 1
}
else
{
   game49$team1 <- game33$team2
   game49$team1rat <- game33$team2rat
   nowins[game33$team1,3] <- nowins[game33$team1,3] + 1
}

btprob34 <- game34$team1rat / (game34$team1rat + game34$team2rat)
test34 <- runif(1)
if (test34 <= btprob34)
{
   game49$team2 <- game34$team1
   game49$team2rat <- game34$team1rat
   nowins[game34$team2,3] <- nowins[game34$team2,3] + 1
}
else
{
   game49$team2 <- game34$team2
   game49$team2rat <- game34$team2rat
   nowins[game34$team1,3] <- nowins[game34$team1,3] + 1
}

game50 <- data.frame(team1 = 0, team1rat = 0, team2 = 0, team2rat = 0)

btprob35 <- game35$team1rat / (game35$team1rat + game35$team2rat)
test35 <- runif(1)
if (test35 <= btprob35)
{
   game50$team1 <- game35$team1
   game50$team1rat <- game35$team1rat
   nowins[game35$team2,3] <- nowins[game35$team2,3] + 1
}
else
{
   game50$team1 <- game35$team2
   game50$team1rat <- game35$team2rat
   nowins[game35$team1,3] <- nowins[game35$team1,3] + 1
}

btprob36 <- game36$team1rat / (game36$team1rat + game36$team2rat)
test36 <- runif(1)
if (test36 <= btprob36)
{
   game50$team2 <- game36$team1
   game50$team2rat <- game36$team1rat
   nowins[game36$team2,3] <- nowins[game36$team2,3] + 1
}
else
{
   game50$team2 <- game36$team2
   game50$team2rat <- game36$team2rat
   nowins[game36$team1,3] <- nowins[game36$team1,3] + 1
}

game57 <- data.frame(team1 = 0, team1rat = 0, team2 = 0, team2rat = 0)

btprob49 <- game49$team1rat / (game49$team1rat + game49$team2rat)
test49 <- runif(1)
if (test49 <= btprob49)
{
   game57$team1 <- game49$team1
   game57$team1rat <- game49$team1rat
   nowins[game49$team2,4] <- nowins[game49$team2,4] + 1
}
else
{
   game57$team1 <- game49$team2
   game57$team1rat <- game49$team2rat
   nowins[game49$team1,4] <- nowins[game49$team1,4] + 1
}

btprob50 <- game50$team1rat / (game50$team1rat + game50$team2rat)
test50 <- runif(1)
if (test50 <= btprob50)
{
   game57$team2 <- game50$team1
   game57$team2rat <- game50$team1rat
   nowins[game50$team2,4] <- nowins[game50$team2,4] + 1
}
else
{
   game57$team2 <- game50$team2
   game57$team2rat <- game50$team2rat
   nowins[game50$team1,4] <- nowins[game50$team1,4] + 1
}

game61 <- data.frame(team1 = 0, team1rat = 0, team2 = 0, team2rat = 0)

btprob57 <- game57$team1rat / (game57$team1rat + game57$team2rat)
test57 <- runif(1)
if (test57 <= btprob57)
{
   game61$team1 <- game57$team1
   game61$team1rat <- game57$team1rat
   nowins[game57$team2,5] <- nowins[game57$team2,5] + 1
}
else
{
   game61$team1 <- game57$team2
   game61$team1rat <- game57$team2rat
   nowins[game57$team1,5] <- nowins[game57$team1,5] + 1
}

###############
# Region 2 code
###############

game37 <- data.frame(team1 = 0, team1rat = 0, team2 = 0, team2rat = 0)

game9 <- data.frame(team1 = 17, team1rat = strengths[17], team2 = 18, team2rat = strengths[18])
btprob9 <- game9$team1rat / (game9$team1rat + game9$team2rat)
test9 <- runif(1)
if (test9 <= btprob9)
{
   game37$team1 <- game9$team1
   game37$team1rat <- game9$team1rat
   nowins[game9$team2,2] <- nowins[game9$team2,2] + 1  
}
else
{
   game37$team1 <- game9$team2
   game37$team1rat <- game9$team2rat
   nowins[game9$team1,2] <- nowins[game9$team1,2] + 1 
}

game10 <- data.frame(team1 = 19, team1rat = strengths[19], team2 = 20, team2rat = strengths[20])
btprob10 <- game10$team1rat / (game10$team1rat + game10$team2rat)
test10 <- runif(1)
if (test10 <= btprob10)
{
   game37$team2 <- game10$team1
   game37$team2rat <- game10$team1rat
   nowins[game10$team2,2] <- nowins[game10$team2,2] + 1  
}
else
{
   game37$team2 <- game10$team2
   game37$team2rat <- game10$team2rat
   nowins[game10$team1,2] <- nowins[game10$team1,2] + 1 
}

game38 <- data.frame(team1 = 0, team1rat = 0, team2 = 0, team2rat = 0)

game11 <- data.frame(team1 = 21, team1rat = strengths[21], team2 = 22, team2rat = strengths[22])
btprob11 <- game11$team1rat / (game11$team1rat + game11$team2rat)
test11 <- runif(1)
if (test11 <= btprob11)
{
   game38$team1 <- game11$team1
   game38$team1rat <- game11$team1rat
   nowins[game11$team2,2] <- nowins[game11$team2,2] + 1  
}
else
{
   game38$team1 <- game11$team2
   game38$team1rat <- game11$team2rat
   nowins[game11$team1,2] <- nowins[game11$team1,2] + 1 
}

game12 <- data.frame(team1 = 23, team1rat = strengths[23], team2 = 24, team2rat = strengths[24])
btprob12 <- game12$team1rat / (game12$team1rat + game12$team2rat)
test12 <- runif(1)
if (test12 <= btprob12)
{
   game38$team2 <- game12$team1
   game38$team2rat <- game12$team1rat
   nowins[game12$team2,2] <- nowins[game12$team2,2] + 1  
}
else
{
   game38$team2 <- game12$team2
   game38$team2rat <- game12$team2rat
   nowins[game12$team1,2] <- nowins[game12$team1,2] + 1 
}

game39 <- data.frame(team1 = 0, team1rat = 0, team2 = 0, team2rat = 0)

game13 <- data.frame(team1 = 25, team1rat = strengths[25], team2 = 26, team2rat = strengths[26])
btprob13 <- game13$team1rat / (game13$team1rat + game13$team2rat)
test13 <- runif(1)
if (test13 <= btprob13)
{
   game39$team1 <- game13$team1
   game39$team1rat <- game13$team1rat
   nowins[game13$team2,2] <- nowins[game13$team2,2] + 1  
}
else
{
   game39$team1 <- game13$team2
   game39$team1rat <- game13$team2rat
   nowins[game13$team1,2] <- nowins[game13$team1,2] + 1 
}

game14 <- data.frame(team1 = 27, team1rat = strengths[27], team2 = 28, team2rat = strengths[28])
btprob14 <- game14$team1rat / (game14$team1rat + game14$team2rat)
test14 <- runif(1)
if (test14 <= btprob14)
{
   game39$team2 <- game14$team1
   game39$team2rat <- game14$team1rat
   nowins[game14$team2,2] <- nowins[game14$team2,2] + 1  
}
else
{
   game39$team2 <- game14$team2
   game39$team2rat <- game14$team2rat
   nowins[game14$team1,2] <- nowins[game14$team1,2] + 1 
}

game40 <- data.frame(team1 = 0, team1rat = 0, team2 = 0, team2rat = 0)

game15 <- data.frame(team1 = 29, team1rat = strengths[29], team2 = 30, team2rat = strengths[30])
btprob15 <- game15$team1rat / (game15$team1rat + game15$team2rat)
test15 <- runif(1)
if (test15 <= btprob15)
{
   game40$team1 <- game15$team1
   game40$team1rat <- game15$team1rat
   nowins[game15$team2,2] <- nowins[game15$team2,2] + 1  
}
else
{
   game40$team1 <- game15$team2
   game40$team1rat <- game15$team2rat
   nowins[game15$team1,2] <- nowins[game15$team1,2] + 1 
}

game16 <- data.frame(team1 = 31, team1rat = strengths[31], team2 = 32, team2rat = strengths[32])
btprob16 <- game16$team1rat / (game16$team1rat + game16$team2rat)
test16 <- runif(1)
if (test16 <= btprob16)
{
   game40$team2 <- game16$team1
   game40$team2rat <- game16$team1rat
   nowins[game16$team2,2] <- nowins[game16$team2,2] + 1  
}
else
{
   game40$team2 <- game16$team2
   game40$team2rat <- game16$team2rat
   nowins[game16$team1,2] <- nowins[game16$team1,2] + 1 
}

game51 <- data.frame(team1 = 0, team1rat = 0, team2 = 0, team2rat = 0)

btprob37 <- game37$team1rat / (game37$team1rat + game37$team2rat)
test37 <- runif(1)
if (test37 <= btprob37)
{
   game51$team1 <- game37$team1
   game51$team1rat <- game37$team1rat
   nowins[game37$team2,3] <- nowins[game37$team2,3] + 1
}
else
{
   game51$team1 <- game37$team2
   game51$team1rat <- game37$team2rat
   nowins[game37$team1,3] <- nowins[game37$team1,3] + 1
}

btprob38 <- game38$team1rat / (game38$team1rat + game38$team2rat)
test38 <- runif(1)
if (test38 <= btprob38)
{
   game51$team2 <- game38$team1
   game51$team2rat <- game38$team1rat
   nowins[game38$team2,3] <- nowins[game38$team2,3] + 1
}
else
{
   game51$team2 <- game38$team2
   game51$team2rat <- game38$team2rat
   nowins[game38$team1,3] <- nowins[game38$team1,3] + 1
}

game52 <- data.frame(team1 = 0, team1rat = 0, team2 = 0, team2rat = 0)

btprob39 <- game39$team1rat / (game39$team1rat + game39$team2rat)
test39 <- runif(1)
if (test39 <= btprob39)
{
   game52$team1 <- game39$team1
   game52$team1rat <- game39$team1rat
   nowins[game39$team2,3] <- nowins[game39$team2,3] + 1
}
else
{
   game52$team1 <- game39$team2
   game52$team1rat <- game39$team2rat
   nowins[game39$team1,3] <- nowins[game39$team1,3] + 1
}

btprob40 <- game40$team1rat / (game40$team1rat + game40$team2rat)
test40 <- runif(1)
if (test40 <= btprob40)
{
   game52$team2 <- game40$team1
   game52$team2rat <- game40$team1rat
   nowins[game40$team2,3] <- nowins[game40$team2,3] + 1
}
else
{
   game52$team2 <- game40$team2
   game52$team2rat <- game40$team2rat
   nowins[game40$team1,3] <- nowins[game40$team1,3] + 1
}

game58 <- data.frame(team1 = 0, team1rat = 0, team2 = 0, team2rat = 0)

btprob51 <- game51$team1rat / (game51$team1rat + game51$team2rat)
test51 <- runif(1)
if (test51 <= btprob51)
{
   game58$team1 <- game51$team1
   game58$team1rat <- game51$team1rat
   nowins[game51$team2,4] <- nowins[game51$team2,4] + 1
}
else
{
   game58$team1 <- game51$team2
   game58$team1rat <- game51$team2rat
   nowins[game51$team1,4] <- nowins[game51$team1,4] + 1
}

btprob52 <- game52$team1rat / (game52$team1rat + game52$team2rat)
test52 <- runif(1)
if (test52 <= btprob52)
{
   game58$team2 <- game52$team1
   game58$team2rat <- game52$team1rat
   nowins[game52$team2,4] <- nowins[game52$team2,4] + 1
}
else
{
   game58$team2 <- game52$team2
   game58$team2rat <- game52$team2rat
   nowins[game52$team1,4] <- nowins[game52$team1,4] + 1
}

btprob58 <- game58$team1rat / (game58$team1rat + game58$team2rat)
test58 <- runif(1)
if (test58 <= btprob58)
{
   game61$team2 <- game58$team1
   game61$team2rat <- game58$team1rat
   nowins[game58$team2,5] <- nowins[game58$team2,5] + 1
}
else
{
   game61$team2 <- game58$team2
   game61$team2rat <- game58$team2rat
   nowins[game58$team1,5] <- nowins[game58$team1,5] + 1
}

############################
# West Regional Championship
############################

game63 <- data.frame(team1 = 0, team1rat = 0, team2 = 0, team2rat = 0)

btprob61 <- game61$team1rat / (game61$team1rat + game61$team2rat)
test61 <- runif(1)
if (test61 <= btprob61)
{
   game63$team1 <- game61$team1
   game63$team1rat <- game61$team1rat
   nowins[game61$team2,6] <- nowins[game61$team2,6] + 1
}
else
{
   game63$team1 <- game61$team2
   game63$team1rat <- game61$team2rat
   nowins[game61$team1,6] <- nowins[game61$team1,6] + 1
}




################
# Region 3 Code
################

game41 <- data.frame(team1 = 0, team1rat = 0, team2 = 0, team2rat = 0)

game17 <- data.frame(team1 = 33, team1rat = strengths[33], team2 = 34, team2rat = strengths[34])
btprob17 <- game17$team1rat / (game17$team1rat + game17$team2rat)
test17 <- runif(1)
if (test17 <= btprob17)
{
   game41$team1 <- game17$team1
   game41$team1rat <- game17$team1rat
   nowins[game17$team2,2] <- nowins[game17$team2,2] + 1  
}
else
{
   game41$team1 <- game17$team2
   game41$team1rat <- game17$team2rat
   nowins[game17$team1,2] <- nowins[game17$team1,2] + 1 
}

game18 <- data.frame(team1 = 35, team1rat = strengths[35], team2 = 36, team2rat = strengths[36])
btprob18 <- game18$team1rat / (game18$team1rat + game18$team2rat)
test18 <- runif(1)
if (test18 <= btprob18)
{
   game41$team2 <- game18$team1
   game41$team2rat <- game18$team1rat
   nowins[game18$team2,2] <- nowins[game18$team2,2] + 1  
}
else
{
   game41$team2 <- game18$team2
   game41$team2rat <- game18$team2rat
   nowins[game18$team1,2] <- nowins[game18$team1,2] + 1 
}

game42 <- data.frame(team1 = 0, team1rat = 0, team2 = 0, team2rat = 0)

game19 <- data.frame(team1 = 37, team1rat = strengths[37], team2 = 38, team2rat = strengths[38])
btprob19 <- game19$team1rat / (game19$team1rat + game19$team2rat)
test19 <- runif(1)
if (test19 <= btprob19)
{
   game42$team1 <- game19$team1
   game42$team1rat <- game19$team1rat
   nowins[game19$team2,2] <- nowins[game19$team2,2] + 1  
}
else
{
   game42$team1 <- game19$team2
   game42$team1rat <- game19$team2rat
   nowins[game19$team1,2] <- nowins[game19$team1,2] + 1 
}

game20 <- data.frame(team1 = 39, team1rat = strengths[39], team2 = 40, team2rat = strengths[40])
btprob20 <- game20$team1rat / (game20$team1rat + game20$team2rat)
test20 <- runif(1)
if (test20 <= btprob20)
{
   game42$team2 <- game20$team1
   game42$team2rat <- game20$team1rat
   nowins[game20$team2,2] <- nowins[game20$team2,2] + 1  
}
else
{
   game42$team2 <- game20$team2
   game42$team2rat <- game20$team2rat
   nowins[game20$team1,2] <- nowins[game20$team1,2] + 1 
}

game43 <- data.frame(team1 = 0, team1rat = 0, team2 = 0, team2rat = 0)

game21 <- data.frame(team1 = 41, team1rat = strengths[41], team2 = 42, team2rat = strengths[42])
btprob21 <- game21$team1rat / (game21$team1rat + game21$team2rat)
test21 <- runif(1)
if (test21 <= btprob21)
{
   game43$team1 <- game21$team1
   game43$team1rat <- game21$team1rat
   nowins[game21$team2,2] <- nowins[game21$team2,2] + 1  
}
else
{
   game43$team1 <- game21$team2
   game43$team1rat <- game21$team2rat
   nowins[game21$team1,2] <- nowins[game21$team1,2] + 1 
}

game22 <- data.frame(team1 = 43, team1rat = strengths[43], team2 = 44, team2rat = strengths[44])
btprob22 <- game22$team1rat / (game22$team1rat + game22$team2rat)
test22 <- runif(1)
if (test22 <= btprob22)
{
   game43$team2 <- game22$team1
   game43$team2rat <- game22$team1rat
   nowins[game22$team2,2] <- nowins[game22$team2,2] + 1  
}
else
{
   game43$team2 <- game22$team2
   game43$team2rat <- game22$team2rat
   nowins[game22$team1,2] <- nowins[game22$team1,2] + 1 
}

game44 <- data.frame(team1 = 0, team1rat = 0, team2 = 0, team2rat = 0)

game23 <- data.frame(team1 = 45, team1rat = strengths[45], team2 = 46, team2rat = strengths[46])
btprob23 <- game23$team1rat / (game23$team1rat + game23$team2rat)
test23 <- runif(1)
if (test23 <= btprob23)
{
   game44$team1 <- game23$team1
   game44$team1rat <- game23$team1rat
   nowins[game23$team2,2] <- nowins[game23$team2,2] + 1  
}
else
{
   game44$team1 <- game23$team2
   game44$team1rat <- game23$team2rat
   nowins[game23$team1,2] <- nowins[game23$team1,2] + 1 
}

game24 <- data.frame(team1 = 47, team1rat = strengths[47], team2 = 48, team2rat = strengths[48])
btprob24 <- game24$team1rat / (game24$team1rat + game24$team2rat)
test24 <- runif(1)
if (test24 <= btprob24)
{
   game44$team2 <- game24$team1
   game44$team2rat <- game24$team1rat
   nowins[game24$team2,2] <- nowins[game24$team2,2] + 1  
}
else
{
   game44$team2 <- game24$team2
   game44$team2rat <- game24$team2rat
   nowins[game24$team1,2] <- nowins[game24$team1,2] + 1 
}

game53 <- data.frame(team1 = 0, team1rat = 0, team2 = 0, team2rat = 0)

btprob41 <- game41$team1rat / (game41$team1rat + game41$team2rat)
test41 <- runif(1)
if (test41 <= btprob41)
{
   game53$team1 <- game41$team1
   game53$team1rat <- game41$team1rat
   nowins[game41$team2,3] <- nowins[game41$team2,3] + 1
}
else
{
   game53$team1 <- game41$team2
   game53$team1rat <- game41$team2rat
   nowins[game41$team1,3] <- nowins[game41$team1,3] + 1
}

btprob42 <- game42$team1rat / (game42$team1rat + game42$team2rat)
test42 <- runif(1)
if (test42 <= btprob42)
{
   game53$team2 <- game42$team1
   game53$team2rat <- game42$team1rat
   nowins[game42$team2,3] <- nowins[game42$team2,3] + 1
}
else
{
   game53$team2 <- game42$team2
   game53$team2rat <- game42$team2rat
   nowins[game42$team1,3] <- nowins[game42$team1,3] + 1
}

game54 <- data.frame(team1 = 0, team1rat = 0, team2 = 0, team2rat = 0)

btprob43 <- game43$team1rat / (game43$team1rat + game43$team2rat)
test43 <- runif(1)
if (test43 <= btprob43)
{
   game54$team1 <- game43$team1
   game54$team1rat <- game43$team1rat
   nowins[game43$team2,3] <- nowins[game43$team2,3] + 1
}
else
{
   game54$team1 <- game43$team2
   game54$team1rat <- game43$team2rat
   nowins[game43$team1,3] <- nowins[game43$team1,3] + 1
}

btprob44 <- game44$team1rat / (game44$team1rat + game44$team2rat)
test44 <- runif(1)
if (test44 <= btprob44)
{
   game54$team2 <- game44$team1
   game54$team2rat <- game44$team1rat
   nowins[game44$team2,3] <- nowins[game44$team2,3] + 1
}
else
{
   game54$team2 <- game44$team2
   game54$team2rat <- game44$team2rat
   nowins[game44$team1,3] <- nowins[game44$team1,3] + 1
}

game59 <- data.frame(team1 = 0, team1rat = 0, team2 = 0, team2rat = 0)

btprob53 <- game53$team1rat / (game53$team1rat + game53$team2rat)
test53 <- runif(1)
if (test53 <= btprob53)
{
   game59$team1 <- game53$team1
   game59$team1rat <- game53$team1rat
   nowins[game53$team2,4] <- nowins[game53$team2,4] + 1
}
else
{
   game59$team1 <- game53$team2
   game59$team1rat <- game53$team2rat
   nowins[game53$team1,4] <- nowins[game53$team1,4] + 1
}

btprob54 <- game54$team1rat / (game54$team1rat + game54$team2rat)
test54 <- runif(1)
if (test54 <= btprob54)
{
   game59$team2 <- game54$team1
   game59$team2rat <- game54$team1rat
   nowins[game54$team2,4] <- nowins[game54$team2,4] + 1
}
else
{
   game59$team2 <- game54$team2
   game59$team2rat <- game54$team2rat
   nowins[game54$team1,4] <- nowins[game54$team1,4] + 1
}

game62 <- data.frame(team1 = 0, team1rat = 0, team2 = 0, team2rat = 0)

btprob59 <- game59$team1rat / (game59$team1rat + game59$team2rat)
test59 <- runif(1)
if (test59 <= btprob59)
{
   game62$team1 <- game59$team1
   game62$team1rat <- game59$team1rat
   nowins[game59$team2,5] <- nowins[game59$team2,5] + 1
}
else
{
   game62$team1 <- game59$team2
   game62$team1rat <- game59$team2rat
   nowins[game59$team1,5] <- nowins[game59$team1,5] + 1
}

###############
# Region 4 code
###############

game45 <- data.frame(team1 = 0, team1rat = 0, team2 = 0, team2rat = 0)

game25 <- data.frame(team1 = 49, team1rat = strengths[49], team2 = 50, team2rat = strengths[50])
btprob25 <- game25$team1rat / (game25$team1rat + game25$team2rat)
test25 <- runif(1)
if (test25 <= btprob25)
{
   game45$team1 <- game25$team1
   game45$team1rat <- game25$team1rat
   nowins[game25$team2,2] <- nowins[game25$team2,2] + 1  
}
else
{
   game45$team1 <- game25$team2
   game45$team1rat <- game25$team2rat
   nowins[game25$team1,2] <- nowins[game25$team1,2] + 1 
}

game26 <- data.frame(team1 = 51, team1rat = strengths[51], team2 = 52, team2rat = strengths[52])
btprob26 <- game26$team1rat / (game26$team1rat + game26$team2rat)
test26 <- runif(1)
if (test26 <= btprob26)
{
   game45$team2 <- game26$team1
   game45$team2rat <- game26$team1rat
   nowins[game26$team2,2] <- nowins[game26$team2,2] + 1  
}
else
{
   game45$team2 <- game26$team2
   game45$team2rat <- game26$team2rat
   nowins[game26$team1,2] <- nowins[game26$team1,2] + 1 
}

game46 <- data.frame(team1 = 0, team1rat = 0, team2 = 0, team2rat = 0)

game27 <- data.frame(team1 = 53, team1rat = strengths[53], team2 = 54, team2rat = strengths[54])
btprob27 <- game27$team1rat / (game27$team1rat + game27$team2rat)
test27 <- runif(1)
if (test27 <= btprob27)
{
   game46$team1 <- game27$team1
   game46$team1rat <- game27$team1rat
   nowins[game27$team2,2] <- nowins[game27$team2,2] + 1  
}
else
{
   game46$team1 <- game27$team2
   game46$team1rat <- game27$team2rat
   nowins[game27$team1,2] <- nowins[game27$team1,2] + 1 
}

game28 <- data.frame(team1 = 55, team1rat = strengths[55], team2 = 56, team2rat = strengths[56])
btprob28 <- game28$team1rat / (game28$team1rat + game28$team2rat)
test28 <- runif(1)
if (test28 <= btprob28)
{
   game46$team2 <- game28$team1
   game46$team2rat <- game28$team1rat
   nowins[game28$team2,2] <- nowins[game28$team2,2] + 1  
}
else
{
   game46$team2 <- game28$team2
   game46$team2rat <- game28$team2rat
   nowins[game28$team1,2] <- nowins[game28$team1,2] + 1 
}

game47 <- data.frame(team1 = 0, team1rat = 0, team2 = 0, team2rat = 0)

game29 <- data.frame(team1 = 57, team1rat = strengths[57], team2 = 58, team2rat = strengths[58])
btprob29 <- game29$team1rat / (game29$team1rat + game29$team2rat)
test29 <- runif(1)
if (test29 <= btprob29)
{
   game47$team1 <- game29$team1
   game47$team1rat <- game29$team1rat
   nowins[game29$team2,2] <- nowins[game29$team2,2] + 1  
}
else
{
   game47$team1 <- game29$team2
   game47$team1rat <- game29$team2rat
   nowins[game29$team1,2] <- nowins[game29$team1,2] + 1 
}

game30 <- data.frame(team1 = 59, team1rat = strengths[59], team2 = 60, team2rat = strengths[60])
btprob30 <- game30$team1rat / (game30$team1rat + game30$team2rat)
test30 <- runif(1)
if (test30 <= btprob30)
{
   game47$team2 <- game30$team1
   game47$team2rat <- game30$team1rat
   nowins[game30$team2,2] <- nowins[game30$team2,2] + 1  
}
else
{
   game47$team2 <- game30$team2
   game47$team2rat <- game30$team2rat
   nowins[game30$team1,2] <- nowins[game30$team1,2] + 1 
}

game48 <- data.frame(team1 = 0, team1rat = 0, team2 = 0, team2rat = 0)

game31 <- data.frame(team1 = 61, team1rat = strengths[61], team2 = 62, team2rat = strengths[62])
btprob31 <- game31$team1rat / (game31$team1rat + game31$team2rat)
test31 <- runif(1)
if (test31 <= btprob31)
{
   game48$team1 <- game31$team1
   game48$team1rat <- game31$team1rat
   nowins[game31$team2,2] <- nowins[game31$team2,2] + 1  
}
else
{
   game48$team1 <- game31$team2
   game48$team1rat <- game31$team2rat
   nowins[game31$team1,2] <- nowins[game31$team1,2] + 1 
}

game32 <- data.frame(team1 = 63, team1rat = strengths[63], team2 = 64, team2rat = strengths[64])
btprob32 <- game32$team1rat / (game32$team1rat + game32$team2rat)
test32 <- runif(1)
if (test32 <= btprob32)
{
   game48$team2 <- game32$team1
   game48$team2rat <- game32$team1rat
   nowins[game32$team2,2] <- nowins[game32$team2,2] + 1  
}
else
{
   game48$team2 <- game32$team2
   game48$team2rat <- game32$team2rat
   nowins[game32$team1,2] <- nowins[game32$team1,2] + 1 
}

game55 <- data.frame(team1 = 0, team1rat = 0, team2 = 0, team2rat = 0)

btprob45 <- game45$team1rat / (game45$team1rat + game45$team2rat)
test45 <- runif(1)
if (test45 <= btprob45)
{
   game55$team1 <- game45$team1
   game55$team1rat <- game45$team1rat
   nowins[game45$team2,3] <- nowins[game45$team2,3] + 1
}
else
{
   game55$team1 <- game45$team2
   game55$team1rat <- game45$team2rat
   nowins[game45$team1,3] <- nowins[game45$team1,3] + 1
}

btprob46 <- game46$team1rat / (game46$team1rat + game46$team2rat)
test46 <- runif(1)
if (test46 <= btprob46)
{
   game55$team2 <- game46$team1
   game55$team2rat <- game46$team1rat
   nowins[game46$team2,3] <- nowins[game46$team2,3] + 1
}
else
{
   game55$team2 <- game46$team2
   game55$team2rat <- game46$team2rat
   nowins[game46$team1,3] <- nowins[game46$team1,3] + 1
}

game56 <- data.frame(team1 = 0, team1rat = 0, team2 = 0, team2rat = 0)

btprob47 <- game47$team1rat / (game47$team1rat + game47$team2rat)
test47 <- runif(1)
if (test47 <= btprob47)
{
   game56$team1 <- game47$team1
   game56$team1rat <- game47$team1rat
   nowins[game47$team2,3] <- nowins[game47$team2,3] + 1
}
else
{
   game56$team1 <- game47$team2
   game56$team1rat <- game47$team2rat
   nowins[game47$team1,3] <- nowins[game47$team1,3] + 1
}

btprob48 <- game48$team1rat / (game48$team1rat + game48$team2rat)
test48 <- runif(1)
if (test48 <= btprob48)
{
   game56$team2 <- game48$team1
   game56$team2rat <- game48$team1rat
   nowins[game48$team2,3] <- nowins[game48$team2,3] + 1
}
else
{
   game56$team2 <- game48$team2
   game56$team2rat <- game48$team2rat
   nowins[game48$team1,3] <- nowins[game48$team1,3] + 1
}

game60 <- data.frame(team1 = 0, team1rat = 0, team2 = 0, team2rat = 0)

btprob55 <- game55$team1rat / (game55$team1rat + game55$team2rat)
test55 <- runif(1)
if (test55 <= btprob55)
{
   game60$team1 <- game55$team1
   game60$team1rat <- game55$team1rat
   nowins[game55$team2,4] <- nowins[game55$team2,4] + 1
}
else
{
   game60$team1 <- game55$team2
   game60$team1rat <- game55$team2rat
   nowins[game55$team1,4] <- nowins[game55$team1,4] + 1
}

btprob56 <- game56$team1rat / (game56$team1rat + game56$team2rat)
test56 <- runif(1)
if (test56 <= btprob56)
{
   game60$team2 <- game56$team1
   game60$team2rat <- game56$team1rat
   nowins[game56$team2,4] <- nowins[game56$team2,4] + 1
}
else
{
   game60$team2 <- game56$team2
   game60$team2rat <- game56$team2rat
   nowins[game56$team1,4] <- nowins[game56$team1,4] + 1
}

btprob60 <- game60$team1rat / (game60$team1rat + game60$team2rat)
test60 <- runif(1)
if (test60 <= btprob60)
{
   game62$team2 <- game60$team1
   game62$team2rat <- game60$team1rat
   nowins[game60$team2,5] <- nowins[game60$team2,5] + 1
}
else
{
   game62$team2 <- game60$team2
   game62$team2rat <- game60$team2rat
   nowins[game60$team1,5] <- nowins[game60$team1,5] + 1
}

############################
# East Regional Championship
############################

btprob62 <- game62$team1rat / (game62$team1rat + game62$team2rat)
test62 <- runif(1)
if (test62 <= btprob62)
{
   game63$team2 <- game62$team1
   game63$team2rat <- game62$team1rat
   nowins[game62$team2,6] <- nowins[game62$team2,6] + 1
}
else
{
   game63$team2 <- game62$team2
   game63$team2rat <- game62$team2rat
   nowins[game62$team1,6] <- nowins[game62$team1,6] + 1
}

#######################
# National Championship
#######################

btprob63 <- game63$team1rat / (game63$team1rat + game63$team2rat)
test63 <- runif(1)
if (test63 <= btprob63)
{
   nowins[game63$team2,7] <- nowins[game63$team2,7] + 1
   nowins[game63$team1,8] <- nowins[game63$team1,8] + 1
}
else
{
   nowins[game63$team1,7] <- nowins[game63$team1,7] + 1
   nowins[game63$team2,8] <- nowins[game63$team2,8] + 1
}

print(i)

} # end of for loop

nowins$win0prob <- nowins$win0 / (nowins$win0 + nowins$win1 + nowins$win2 + nowins$win3 + nowins$win4 + nowins$win5 + nowins$win6)
nowins$win1prob <- nowins$win1 / (nowins$win0 + nowins$win1 + nowins$win2 + nowins$win3 + nowins$win4 + nowins$win5 + nowins$win6)
nowins$win2prob <- nowins$win2 / (nowins$win0 + nowins$win1 + nowins$win2 + nowins$win3 + nowins$win4 + nowins$win5 + nowins$win6)
nowins$win3prob <- nowins$win3 / (nowins$win0 + nowins$win1 + nowins$win2 + nowins$win3 + nowins$win4 + nowins$win5 + nowins$win6)
nowins$win4prob <- nowins$win4 / (nowins$win0 + nowins$win1 + nowins$win2 + nowins$win3 + nowins$win4 + nowins$win5 + nowins$win6)
nowins$win5prob <- nowins$win5 / (nowins$win0 + nowins$win1 + nowins$win2 + nowins$win3 + nowins$win4 + nowins$win5 + nowins$win6)
nowins$win6prob <- nowins$win6 / (nowins$win0 + nowins$win1 + nowins$win2 + nowins$win3 + nowins$win4 + nowins$win5 + nowins$win6)
nowins$expwins <- 1*nowins$win1prob + 2*nowins$win2prob + 3*nowins$win3prob + 4*nowins$win4prob + 
5*nowins$win5prob + 6*nowins$win6prob  

print(nowins)

}

# Examples

result <- winner64(100,strengths64)

result <- winner64(100000,strengths64)

# write.table(result,"H:\\cscar-xfer\\Papers\\March Madness\\results_18_long.csv",row.names=FALSE,sep=",")

# On Unix, after saving this file as .R file in IFS space

# system.time(source("simulation_Rcode_64.R"))
