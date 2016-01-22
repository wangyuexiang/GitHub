select 
	count(rownum) Nbre,
	trajet1.dpbgq,
	trajet1.hmjp2 HeureT1,
	trajet2.hmjp2 HeureT2,
	trajet1.caree,
	trajet1.ob1tr1,
	trajet1.ob1tr3,	
	trajet1.ob1tr5,	
	trajet1.ob1tr6,	
	trajet1.ob1tr7	
from 
	(	select
			substr(conpis, 1,20) ID, -- identifiant
			caree,	-- classe reelle
			nugap,	-- numero gare peage
			nugae,	-- numero gare entree
			vopea,	-- no voie peage
			dpbgq,	-- date message journee quantieme
			hmjp2,	-- heure message journee quantieme
			ob1tr1,	-- type abonne tis gratuit
			ob1tr3,	-- type abonne int aut post pai
			ob1tr5,	-- type abonne carte b
			ob1tr6,	-- type abonne tis
			ob1tr7	-- type abonne amtech
		from
				TITRES_MON/AFTSPE2014	-- la BDD de 2014
		where
			nugap = 011	-- sortir a les adrets
			and vopea < 20 -- au sens vers italie
			and dpbgq <= 10 -- pr 1/1/14 - 10/1/14
	) trajet1
	inner join (	select
			substr(conpis, 1,20) ID, -- identifiant
			caree,	-- classe reelle
			nugap,	-- numero gare peage
			nugae,	-- numero gare entree
			vopea,	-- no voie peage
			dpbgq,	-- date message journee quantieme
			hmjp2,	-- heure message journee quantieme
			ob1tr1,	-- type abonne tis gratuit
			ob1tr3,	-- type abonne int aut post pai
			ob1tr5,	-- type abonne carte b
			ob1tr6,	-- type abonne tis
			ob1tr7	-- type abonne amtech
		from
				TITRES_MON/AFTSPE2014	-- la BDD de 2014
		where
			nugap = 014	-- sortir a antibes ouest
			and vopea >= 20	-- au sens vers italie
			and dpbgq <= 10	-- pr 1/1/14 - 10/1/14
	) trajet2
	on		
		trajet1.ID = trajet2.ID	-- meme identifiant de moyen de paiement
		and trajet1.dpbgq = trajet2.dpbgq	-- meme jr
		and (trajet2.hmjp2 - trajet1.hmjp2) <= 2 -- deux transactions se passent dans moins de 2 heures