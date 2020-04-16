responseH(Q,PQ,PR,[R]):-
	Q=[what,can,i,have,for,MT,that,contains,FI],
	prop(_,not,MT),
	prop(FI,is,_),
	once(responseO(Q,PQ,PR,LR)),
	once(bestMatches(LR,4,CR)),
	length(CR,N),
	N >= 1,
	getDiffAnswer(Q,PQ,PR,CR,R).
responseH(Q,PQ,PR,["I",told,you,that,before]):-
	Q=[what,can,i,have,for,MT,that,contains,FI],
	prop(_,not,MT),
	prop(FI,is,_),
	once(responseO(Q,PQ,PR,LR)),
	once(bestMatches(LR,4,CR)),
	length(CR,N),
	N >= 1,
	\+getDiffAnswer(Q,PQ,PR,CR,_).
responseH(Q,PQ,PR,["I",do,not,know]) :-
	Q = [what,can,i,have,for,MT,that,contains,FI],
	((((\+ prop(_,_,MT)); (\+prop(FI,_,_))); (\+getUnlikedIngredients(PQ,_))); (\+calculateCalories(PQ,PR,_))).
responseH(Q,PQ,PR,["Nothing",from,what,i,know]) :-
	Q=[what,can,i,have,for,MT,that,contains,FI],
	prop(_,not,MT),
	prop(FI,is,_),
	once(responseO(Q,PQ,PR,LR)),
	once(bestMatches(LR,4,CR)),
	length(CR,0).

responseO(Q,PQ,PR,LR):-
	Q=[what,can,i,have,for,MT,that,contains,FI],
	getUnlikedIngredients(PQ,Unliked),
	getUnlikedFoodType(Unliked,UnlikedFT),
	filterProp(contain,L1),
	filterProp(not,L2),
	matchSecond(FI,L1,ML1),
	formatList(UnlikedFT,UnlikedX),
	subtract1(UnlikedX,UnlikedY),
	matchSecond(MT,L2,ML2),
	formatList2(ML2,ML2Format),
	mergeMatchLists(ML2Format,UnlikedY,UnlikedNot),
	mergeMatchLists(UnlikedNot,ML1,LikedSuitContain),
	formatList3(PQ,PR,LikedSuitContain,FinalList),
	add2toAll(FinalList,LR).

getUnlikedFoodType(UnlikedIngredients,UnlikedFoodType):-
	findall(FT,(prop(FT,contain,FI),member(FI,UnlikedIngredients)),UnlikedFoodType).


formatList([],[]).
formatList([H|T],[F|T1]):-
	F=H-0,
	formatList(T,T1).

subtract1([],[]).
subtract1([H-X|T],[H-Y|T1]):-
	Y is X-1,
	subtract1(T,T1).

formatList2([],[]).
formatList2([E-X|T],[E-Y|T1]):-
	X=1,
	Y is X-2,
	formatList2(T,T1).
formatList2([E-0|T],[E-0|T1]):-
	formatList2(T,T1).


formatList3(_,_,[],[]).
formatList3(PQ,PR,[H1-E1|T1],[H1-E2|T2]):-
	calcCalories(H1,PQ,PR,C),
	C>=0,
	E2 is E1+1,
	formatList3(PQ,PR,T1,T2).
formatList3(PQ,PR,[H1-E1|T1],[H1-E1|T2]):-
	calcCalories(H1,PQ,PR,C),
	C<0,
	formatList3(PQ,PR,T1,T2).
add2toAll([],[]).
add2toAll([E-X|T],[E-Y|T1]):-
	Y is X+2,
	add2toAll(T,T1).
