#Petrale Sole
Rex.out <- SS_output("Rex sole/2_base_model/run/")
cbind(unique(Rex.out$cpue$Fleet_name), unique(Rex.out$cpue$Fleet))
Rex.Ind <- Index.method(
    Rex.out,
    fleet.in = 5,
    Index.RP = 0.25
)

Rex.out.Lts.fleet <- subset(Rex.out$len_comp_fit_table, Fleet == 5)
Rex.out.Lts.fleet.sex <- unique(Rex.out.Lts.fleet$Sexes)
Rex.MeanLt <- MeanLt.method(
    Rex.out,
    fleet.in = 5,
    Sex = 3,
    Ltm.RP = 0.25
)

#Petrale Sole
Petrale.out <- SS_output("Petrale sole/")
cbind(unique(Petrale.out$cpue$Fleet_name), unique(Petrale.out$cpue$Fleet))
Petrale.Ind <- Index.method(
    Petrale.out,
    fleet.in = 4,
    Index.RP = 0.25
)

Petrale.out.Lts.fleet <- subset(Petrale.out$len_comp_fit_table, Fleet == 4)
Petrale.out.Lts.fleet.sex <- unique(Petrale.out.Lts.fleet$Sexes)
Petrale.MeanLt <- MeanLt.method(
    Petrale.out,
    fleet.in = 4,
    Sex = 3,
    Ltm.RP = 0.25
)

#Lingcod North
Lcod.N.out <- SS_output("Lingcod north/")
cbind(unique(Lcod.N.out$cpue$Fleet_name), unique(Lcod.N.out$cpue$Fleet))
Lcod.N.Ind <- Index.method(
    Lcod.N.out,
    fleet.in = 7,
    Index.RP = 0.4
)

Lcod.N.out.Lts.fleet <- subset(Lcod.N.out$len_comp_fit_table, Fleet == 4)
Lcod.N.out.Lts.fleet.sex <- unique(Lcod.N.out.Lts.fleet$Sexes)
Lcod.N.MeanLt <- MeanLt.method(
    Lcod.N.out,
    fleet.in = 5,
    Sex = 3,
    Ltm.RP = 0.4
)

Lcod.S.out <- SS_output("Lingcod south/")
cbind(unique(Lcod.S.out$cpue$Fleet_name), unique(Lcod.S.out$cpue$Fleet))
Lcod.S.Ind <- Index.method(
    Lcod.S.out,
    Years.in = Lcod.S.out.index$Yr,
    fleet.in = 8,
    Index.RP = 0.4
)

#Lingcod South
Lcod.S.out.Lts.fleet <- subset(Lcod.S.out$len_comp_fit_table, Fleet == 8)
Lcod.S.out.Lts.fleet.sex <- unique(Lcod.S.out.Lts.fleet$Sexes)
Lcod.S.MeanLt.sex0 <- MeanLt.method(
    Lcod.S.out,
    fleet.in = 8,
    Sex = Lcod.S.out.Lts.fleet.sex[1],
    Ltm.RP = 0.4
)

Lcod.S.MeanLt.sex3 <- MeanLt.method(
    Lcod.S.out,
    fleet.in = 8,
    Sex = Lcod.S.out.Lts.fleet.sex[2],
    Ltm.RP = 0.4
)
