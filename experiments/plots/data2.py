e_cost = [55.75,61.75,58.0,73.9,83.9,61.15]
e_vc   = [86.25,85.0,83.75,77.5,78.75,86.25]

h_cost = [50.30,63.45,62.8,86.1,76.85,76.4,93.6,96.8,102.7]
h_vc   = [77.5,81.25,81.25,72.5,70.0,71.25,67.5,75.0,65.0]

costdiff = []
cdiffpercent = []
for i in range(len(e_cost)):
    costdiff.append(h_cost[i] - e_cost[i])
    cdiffpercent.append(costdiff[i]/e_cost[i]*100)

print(costdiff)
print(sum(costdiff)/len(costdiff))
print(cdiffpercent)
print(sum(cdiffpercent)/len(cdiffpercent))

vcdiff = []
vcdiffpercent = []
for i in range(len(e_vc)):
    vcdiff.append(h_vc[i] - e_vc[i])
    vcdiffpercent.append(vcdiff[i]/e_vc[i]*100)

print(vcdiff)
print(sum(vcdiff)/len(vcdiff))
print(vcdiffpercent)
print(sum(vcdiffpercent)/len(vcdiffpercent))


# [-5.450000000000003, 1.7000000000000028, 4.799999999999997, 12.199999999999989, -7.050000000000011, 15.250000000000007]
# [-9.775784753363235, 2.7530364372469682, 8.275862068965512, 16.50879566982407, -8.402860548271764, 24.93867538838922]
# 5.7162873771317955
# [-8.75, -3.75, -2.5, -5.0, -8.75, -15.0]
# [-10.144927536231885, -4.411764705882353, -2.9850746268656714, -6.451612903225806, -11.11111111111111, -17.391304347826086]
# -8.749299205190486
