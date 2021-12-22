# csv = read.csv("./Exercise 5/table_v2.csv")
csv = read.csv("./table_v2.csv")

scores_TA1 = csv$Score..TA_1.
scores_TA2 = csv$Score..TA_2.
scores_TA3 = csv$Score..TA_3.

q2a_set1 = c(scores_TA1[1:5])
q2a_set2 = c(scores_TA2[11:15])
q2a_set3 = c(scores_TA3[21:25])

# a
set1_2 = mean(q2a_set1) - mean(q2a_set2)
set1_3 = mean(q2a_set1) - mean(q2a_set3)
set2_3 = mean(q2a_set2) - mean(q2a_set3)

# b
cor1_2 = cor(q2a_set1, q2a_set2)
cor1_3 = cor(q2a_set1, q2a_set3)
cor2_3 = cor(q2a_set2, q2a_set3)

# c
group_TA1 = csv$Score..TA_1_G.
group_TA2 = csv$Score..TA_2_G.
group_TA3 = csv$Score..TA_3_G.
group_T = csv$Score..T_G.

group_TA1[is.na(group_TA1)] <- 0
group_TA2[is.na(group_TA1)] <- 0
group_TA3[is.na(group_TA1)] <- 0

cor1_T = cor(group_T, group_TA1)
cor2_T = cor(group_T, group_TA2)
cor3_T = cor(group_T, group_TA3)

# d

#	Group 1 (1 2)
gp1_c1 = cor(mean(csv$Score..TA_1.[1:5]), group_TA1[1])
gp1_c2 = cor(mean(csv$Score..TA_2.[1:5]), group_TA2[1])

#	Group 2 (1 2)
gp2_c1 = cor(mean(csv$Score..TA_1.[6:10]), group_TA1[6])
gp2_c2 = cor(mean(csv$Score..TA_2.[6:10]), group_TA2[6])

#	Group 3 (2 3)
gp3_c2 = cor(mean(csv$Score..TA_2.[11:15]), group_TA2[11])
gp3_c3 = cor(mean(csv$Score..TA_3.[11:15]), group_TA3[11])

#	Group 4 (2 3)
gp4_c2 = cor(mean(csv$Score..TA_2.[16:20]), group_TA2[16])
gp4_c3 = cor(mean(csv$Score..TA_3.[16:20]), group_TA3[16])

#	Group 5 (2 3)
gp5_c2 = cor(mean(csv$Score..TA_2.[21:25]), group_TA2[21])
gp5_c3 = cor(mean(csv$Score..TA_3.[21:25]), group_TA3[21])

