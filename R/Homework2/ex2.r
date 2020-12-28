#P1- pr. na horata ot grada koito imat rak
#P2 - pr. hora ot oficite koito imat rak
#Ho: P1=P2
#H2: P1!=P2
prop.test(c(48,12), c(5000,520), alternative = "two.sided")
#p-value=0.00936 < 0.05 -> Othv. hipotezata
#-> Vliqe se!