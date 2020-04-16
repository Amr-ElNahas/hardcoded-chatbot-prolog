:- use_module('DCGblueprint.pl').
:- ensure_loaded(info_food).
:- use_module('read_sentence.pl').



readInputTillQuit:-
	write("> Welcome to your personal assistant"),
	PQ=[],
	PR=[],
	readInputTillQuit([PQ,PR]).
readInputTillQuit([PQ,PR]) :-
	nl,
	write("> "),
	res(Q),
	readInputTillQuit([PQ,PR],Q).
readInputTillQuit([PQ,PR],Q):-
	delete(Q,?,Y),
	delete(Y,.,X),
	isValid(X),
	Q\=[quit,.],
	response(X,PQ,PR,R),
	read_sentence:ws(R),
	append(PQ,[X],NPQ),
	append(PR,[R],NPR),
	readInputTillQuit([NPQ,NPR]).
	
readInputTillQuit(ACC,Q):-
	delete(Q,?,Y),
	delete(Y,.,X),
	\+isValid(X),
	Q\=[quit,.],
	write("> I can not understand you"),
	readInputTillQuit(ACC).

readInputTillQuit([PQ,PR],X):-
	delete(X,?,Y),
	delete(Y,.,Z),
	\+isValid(Z),
	X = [quit,.],
	foodForQuitbreakfast(PQ,PR,A),
	foodForQuitlunch(PQ,PR,B),
	foodForQuitdinner(PQ,PR,C),
	((length(A,N),N=1,write("you had "),'read_sentence':ws(A), write(" for breakfast"));(length(A,N),N>1,write("you had "),write(A), write(" for breakfast"));write("you had - for breakfast")),
	nl,
	((length(B,M),M=1,write("you had "), 'read_sentence':ws(B), write(" for lunch"));(length(B,M),M>1,write("you had "), write(B), write(" for lunch"));write("you had - for lunch")),
	nl,
	((length(C,O),O=1,write("you had "), 'read_sentence':ws(C), write(" for dinner"));(length(C,O),O>1,write("you had "), write(C), write(" for dinner"));write("you had - for dinner")),
	nl,
	write("Bye"),!.

totalCal(1800).
isValid(Q):-
	sentence(Q,[]).

filterProp(Z,L):-
	setof((X,Y),prop(X,Z,Y),L).


matchFirst(_,[],[]).
matchFirst(T1,[(T1,E)|T],[(E-1)|T2]):-
	matchFirst(T1,T,T2).
matchFirst(T1,[(X,E)|T],[(E-0)|T2]):-
	X\=T1,
	matchFirst(T1,T,T2).



matchSecond(_,[],[]).
matchSecond(T1,[(E,T1)|T],[(E-1)|T2]):-
	matchSecond(T1,T,T2).
matchSecond(T1,[(E,X)|T],[(E-0)|T2]):-
	X\=T1,
	matchSecond(T1,T,T2).

quiz(_,[],[]).
quiz(E,[(E-H)|T],[H|T1]):-
	quiz(E,T,T1).
quiz(E,[(H-E)|T],[H|T1]):-
	quiz(E,T,T1).
quiz(E,[(H-F)|T],L1):-
	E\=H,
	E\=F,
	quiz(E,T,L1).

mergeMatchLists(ML1,ML2,LM):-
	mergeMatchLists(ML1,ML2,[],LM).
mergeMatchLists([],[],ACC,ACC).
mergeMatchLists([],ML2,ACC,LM):-
	\+length(ML2,0),
	mergeMatchLists(ML2,[],ACC,LM).
mergeMatchLists([E1-T1|Tail1],ML2,ACC,LM):-	
	\+(memberchk(E1-_,ACC)),
	append([E1-T1],ACC,NewACC),
	mergeMatchLists(Tail1,ML2,NewACC,LM).
mergeMatchLists([E1-T1|Tail1],ML2,[EACC-TACC|TailACC],LM):-
	E1=EACC,
	R1 is T1+TACC,
	mergeMatchLists(Tail1,ML2,[EACC-R1|TailACC],LM).
mergeMatchLists([E1-T1|Tail1],ML2,ACC,LM):-
	memberchk(E1-T2,ACC),
	T3 is T1+T2,
	replace(E1-T2,ACC,E1-T3,NewACC),
	mergeMatchLists(Tail1,ML2,NewACC,LM).

replace(_, [], _, []).
replace(O, [O|T], R, [R|T2]) :- 
	replace(O, T, R, T2).
replace(O, [H|T], R, [H|T2]) :- 
	H \= O, 
	replace(O, T, R, T2).


maxScore([],-9).
maxScore([_-X|T],R):-
	maxScore(T,R1),
	R1>X,
	R is R1.
maxScore([_-X|T],R):-
	maxScore(T,R1),
	R1=<X,
	R is X.

bestMatches(L1,L2):-
	maxScore(L1,R),
	bestMatches(L1,R,L2).
bestMatches([],_,[]).
bestMatches([E-X|T1],R,[E|T2]):-
	X=R,
	bestMatches(T1,R,T2).
bestMatches([_-X|T1],R,T2):-
	X\=R,
	bestMatches(T1,R,T2).

foodCal(F,C):-
	prop(F,contain,C,cal).
foodCal(F,C):-
	typeIngredient(F,R),
	foodCalList(R,C).

typeIngredient(F,R):-
	setof(L, prop(F, contain, L),R).



foodCalList([],0).
foodCalList([H|T],C):-
	foodCal(H,C1),
	foodCalList(T,R),
	C is C1 + R.

calcCalories(F,PQ,PR,C):-
	totalCal(X),
	calcCalories(F,PQ,PR,X,C).
calcCalories(F,[],[],X,C):-
	foodCal(F,C1),
	C is X-C1.
calcCalories(F,[HPQ|TPQ],[HPR|TPR],X,C):-
	HPQ=[can,i,have,G,for,_],
	HPR=["You", can, have, G, for, _],
	foodCal(G,C1),
	X1 is X-C1,
	calcCalories(F,TPQ,TPR,X1,C).
calcCalories(F,[HPQ|TPQ],[HPR|TPR],X,C):-
	HPQ=[can,i,have,_,for,_],
	HPR \=["You", can, have, _, for, _],
	calcCalories(F,TPQ,TPR,X,C).
calcCalories(F,[HPQ|TPQ],[HPR|TPR],X,C):-
	HPQ=[i,ate,G,for,_],
	(HPR =["Ok"];HPR="Ok";HPR=[ok];HPR=ok;HPR=["ok."];HPR=["ok"];HPR="ok.";HPR="ok"),
	foodCal(G,C1),
	X1 is X-C1,
	calcCalories(F,TPQ,TPR,X1,C).
calcCalories(F,[HPQ|TPQ],[_|TPR],X,C):-
	HPQ\=[can,i,have,_,for,_],
	HPQ\=[i,ate,_,for,_],
	calcCalories(F,TPQ,TPR,X,C).


calculateCalories([],[],0).
calculateCalories([HPQ|TPQ],[HPR|TPR],C):-
	HPQ=[can,i,have,G,for,_],
	HPR =["You", can, have, G, for, _],
	foodCal(G,C1),
	calculateCalories(TPQ,TPR,C2),
	C is C1+C2.
calculateCalories([HPQ|TPQ],[_|TPR],C):-
	HPQ\=[can,i,have,_,for,_],
	HPQ\=[i,ate,_,for,_],
	calculateCalories(TPQ,TPR,C).
calculateCalories([HPQ|TPQ],[HPR|TPR],C):-
	HPQ=[can,i,have,_,for,_],
	HPR\=["You", can, have, _, for, _],
	calculateCalories(TPQ,TPR,C).
calculateCalories([HPQ|TPQ],[HPR|TPR],C):-
	HPQ=[i,ate,G,for,_],
	HPR =["Ok"],
	foodCal(G,C1),
	calculateCalories(TPQ,TPR,C2),
	C is C1+C2.


	
listOrderDesc(List,Sorted):-
	permutation(List,Sorted),
	is_sorted(Sorted),!.

is_sorted([]).
is_sorted([_]).
is_sorted([(_-Z1),(Y-Z2)|T]):-
	Z1>=Z2,
	is_sorted([(Y-Z2)|T]).

foodFromHistory([],[]).
foodFromHistory([HHL|THL],[F|TFL]):-
	HHL=[i,ate,F,for,_],
	foodFromHistory(THL,TFL).
foodFromHistory([HHL|THL],[F|TFL]):-
	HHL=[you,can,have,F,for,_],
	foodFromHistory(THL,TFL).
foodFromHistory([HHL|THL],FL):-
	HHL\=[you,can,have,_,for,_],
	HHL\=[i,ate,_,for,_],
	foodFromHistory(THL,FL).

getUnlikedIngredients([],[]).
getUnlikedIngredients([HPQ|TPQ],[HFL|TFL]):-
	HPQ=[i,do,not,eat,HFL],
	getUnlikedIngredients(TPQ,TFL).
getUnlikedIngredients([HPQ|TPQ],TFL):-
	\+HPQ=[i,do,not,eat,_],
	getUnlikedIngredients(TPQ,TFL).
	

getDiffAnswer(_,[],[],[H|_],H).
getDiffAnswer(Q,[HPQ|TPQ],[_|TPR],CR,R):-
	Q\=HPQ,
	getDiffAnswer(Q,TPQ,TPR,CR,R).
getDiffAnswer(Q,[HPQ|TPQ],[HPR|TPR],CR,R):-
	Q=HPQ,
	HPR=[FML],
	delete(CR,FML,NCR),
	getDiffAnswer(Q,TPQ,TPR,NCR,R).

foodForQuitbreakfast([],[],[]).
foodForQuitbreakfast([HPQ|TPQ],[_|TPR],[F|R]):-
	HPQ=[i, ate, F, for, breakfast],
	foodForQuitbreakfast(TPQ,TPR,R).
foodForQuitbreakfast([HPQ|TPQ],[HPR|TPR],[F|R]):-
	HPQ=[can, i, have, F, for, breakfast],
	HPR=["You",can,have,F,for,breakfast],
	foodForQuitbreakfast(TPQ,TPR,R).
foodForQuitbreakfast([HPQ|TPQ],[_|TPR],R):-
	HPQ\=[can, i, have, _, for, breakfast],
	HPQ\=[i, ate, _, for, breakfast],
	foodForQuitbreakfast(TPQ,TPR,R).
foodForQuitbreakfast([HPQ|TPQ],[HPR|TPR],R):-
	HPQ=[can, i, have, _, for, breakfast],
	HPR\=["You",can,have,_,for,breakfast],
	foodForQuitbreakfast(TPQ,TPR,R).

foodForQuitlunch([],[],[]).
foodForQuitlunch([HPQ|TPQ],[_|TPR],[F|R]):-
	HPQ=[i, ate, F, for, lunch],
	foodForQuitlunch(TPQ,TPR,R).
foodForQuitlunch([HPQ|TPQ],[HPR|TPR],[F|R]):-
	HPQ=[can, i, have, F, for, lunch],
	HPR=["You",can,have,F,for,lunch],
	foodForQuitlunch(TPQ,TPR,R).
foodForQuitlunch([HPQ|TPQ],[_|TPR],R):-
	HPQ\=[can, i, have, _, for, lunch],
	HPQ\=[i, ate, _, for, lunch],
	foodForQuitlunch(TPQ,TPR,R).
foodForQuitlunch([HPQ|TPQ],[HPR|TPR],R):-
	HPQ=[can, i, have, _, for, lunch],
	HPR\=["You",can,have,_,for,lunch],
	foodForQuitlunch(TPQ,TPR,R).

foodForQuitdinner([],[],[]).
foodForQuitdinner([HPQ|TPQ],[_|TPR],[F|R]):-
	HPQ=[i, ate, F, for, dinner],
	foodForQuitdinner(TPQ,TPR,R).
foodForQuitdinner([HPQ|TPQ],[HPR|TPR],[F|R]):-
	HPQ=[can, i, have, F, for, dinner],
	HPR=["You",can,have,F,for,dinner],
	foodForQuitdinner(TPQ,TPR,R).
foodForQuitdinner([HPQ|TPQ],[_|TPR],R):-
	HPQ\=[can, i, have, _, for, dinner],
	HPQ\=[i, ate, _, for, dinner],
	foodForQuitdinner(TPQ,TPR,R).
foodForQuitdinner([HPQ|TPQ],[HPR|TPR],R):-
	HPQ=[can, i, have, _, for, dinner],
	HPR\=["You",can,have,_,for,dinner],
	foodForQuitdinner(TPQ,TPR,R).




corresponding(E,[H|T],[_|T1],R):-
	E\=H,
	corresponding(E,T,T1,R).
corresponding(E,[E|_],[H1|_],H1).
	
responseF(Q,_,_,["I",do,not,know]) :-
	Q = [what,kind,of,FC,does,F,contain],
	((\+ prop(_,_,FC)); (\+prop(F,_,_))).
responseF(Q,_,_,["Nothing",from,what,i,know]) :-
	Q = [what,kind,of,FC,does,F,contain],
	prop(_,_,FC),
	prop(F,_,_),
	filterProp(contain,L1),
	filterProp(is,L2),
	matchFirst(F,L1,R1),
	matchSecond(FC,L2,R2),
	mergeMatchLists(R1,R2,L3),
	bestMatches(L3,2,CR),
	length(CR,0).
responseF(Q,PQ,PR,[R]) :-
	Q = [what,kind,of,FC,does,F,contain],
	prop(_,_,FC),
	prop(F,_,_),
	filterProp(contain,L1),
	filterProp(is,L2),
	matchFirst(F,L1,R1),
	matchSecond(FC,L2,R2),
	mergeMatchLists(R1,R2,L3),
	bestMatches(L3,2,CR),
	length(CR,N),
	N >= 1,
	getDiffAnswer(Q,PQ,PR,CR,R).
responseF(Q,PQ,PR,["I",told,you,that,before]) :-
	Q = [what,kind,of,FC,does,F,contain],
	prop(_,_,FC),
	prop(F,_,_),
	filterProp(contain,L1),
	filterProp(is,L2),
	matchFirst(F,L1,R1),
	matchSecond(FC,L2,R2),
	mergeMatchLists(R1,R2,L3),
	bestMatches(L3,2,CR),
	length(CR,N),
	N >= 1,
	\+getDiffAnswer(Q,PQ,PR,CR,_).

responseA(Q,PQ,_,["I",told,you,that,before]):-
	Q=[how,many,calories,does,_,contain],
	member(Q,PQ).
responseA(Q,PQ,_,[R1,"Calories"]):-
	Q=[how,many,calories,does,F,contain],
	\+member(Q,PQ),
	foodCal(F,R1).
responseA(Q,PQ,_,["I",do,not,know]):-
	Q=[how,many,calories,does,F,contain],
	\+member(Q,PQ),
	\+foodCal(F,_).

responseB(Q,_,_,["I",do,not,know]) :-
	Q = [what,does,F,contain],
	\+prop(F,contain,_).
responseB(Q,PQ,PR,[R]) :-
	Q = [what,does,F,contain],
	prop(F,_,_),
	filterProp(contain,L1),
	matchFirst(F,L1,R1),
	mergeMatchLists(R1,[],L3),
	bestMatches(L3,1,CR),
	getDiffAnswer(Q,PQ,PR,CR,R).
responseB(Q,PQ,PR,["I",told,you,that,before]):-
	Q = [what,does,F,contain],
	prop(F,_,_),
	filterProp(contain,L1),
	matchFirst(F,L1,R1),
	mergeMatchLists(R1,[],L3),
	bestMatches(L3,1,CR),
	\+getDiffAnswer(Q,PQ,PR,CR,_).

responseC(Q,PQ,PR,R):-
	Q = [can,i,have,F,for,M],
	\+prop(F,not,M),
	prop(F,_,_),
	prop(_,_,M),
	calcCalories(F,PQ,PR,C),
	C>=0,
	R = ["You",can,have,F,for,M].
responseC(Q,PQ,_,R):-
	Q = [can,i,have,F,for,M],
	\+member(Q,PQ),
	prop(F,not,M),
	R=[F,is,not,suitable,for,M].
responseC(Q,PQ,PR,["No"]) :-
	Q = [can,i,have,F,for,M],
	\+prop(F,not,M),
	prop(F,_,_),
	prop(_,_,M),
	calcCalories(F,PQ,PR,C),
	C<0.
responseC(Q,PQ,PR,["I",do,not,know]):-
	Q = [can,i,have,F,for,M],
	(((\+prop(_,_,M)); (\+prop(F,_,_))); (\+calcCalories(F,PQ,PR,_))).
responseC(Q,PQ,PR,["I",told,you,that,before]):-
	Q = [can,i,have,_,for,_],
	member(Q,PQ),
	corresponding(Q,PQ,PR,"No");
	corresponding(Q,PQ,PR,[_, "is", not, suitable, for, _]); 
	corresponding(Q,PQ,PR,["I",do,not,know]).

responseD(Q,PQ,_,R):-
	Q=[what,is,F],
	\+memberchk(Q,PQ),
	prop(F,is,F1),
	R=[F1].
responseD(Q,PQ,_,["I",told,you,that,before]):-
	Q=[what,is,_],
	memberchk(Q,PQ).
responseD(Q,PQ,_,["I",do,not,know]):-
	Q=[what,is,F],
	\+memberchk(Q,PQ),
	\+prop(F,is,_).

responseE(Q,PQ,PR,["I",told,you,that,before]):-
	Q=[how,many,calories,do,i,have,left],
	calculateCalories(PQ,PR,Y),
	totalCal(X),
	R1 is X-Y,
	R = [R1,"Calories"],
	member(Q,PQ),
	corresponding(Q,PQ,PR,R),!.
responseE(Q,PQ,PR,["I",do, not,know]):-
	Q=[how,many,calories,do,i,have,left],
	\+calculateCalories(PQ,PR,_).
responseE(Q,PQ,PR,R):-
	Q=[how,many,calories,do,i,have,left],
	calculateCalories(PQ,PR,Y),
	totalCal(X),
	R1 is X-Y,
	R =[R1, "Calories"].


responseG(Q,PQ,_,["Yes"]):-
	Q=[is,FI,a,FC,in,FT],
	\+member(Q,PQ),
	prop(FT,contain,FI),
	prop(FI,is,FC).
responseG(Q,PQ,_,["No"]):-
	Q=[is,FI,a,FC,in,FT],
	\+member(Q,PQ),
	prop(FI,_,_),
	prop(FT,_,_),
	prop(_,_,FC),
	((\+prop(FT,contain,FI)); (\+prop(FI,is,FC))).
responseG(Q,PQ,_,["I",told,you,that,before]):-
	Q=[is,_,a,_,in,_],
	member(Q,PQ).
responseG(Q,PQ,_,["I",do,not,know]):-
	Q=[is,FI,a,FC,in,FT],
	\+member(Q,PQ),
	(((\+prop(FI,_,_)); (\+prop(FT,_,_))); (\+prop(_,_,FC))).


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

	
response1(Q,_,_,["Ok"]):-
	Q=[i,ate,_,for,_].

response2(Q,_,_,["Ok"]):-
	Q=[i,do,not,eat,_].

response(Q,PQ,PR,R):-
	responseA(Q,PQ,PR,R).
response(Q,PQ,PR,R):-
	responseB(Q,PQ,PR,R).
response(Q,PQ,PR,R):-
	responseC(Q,PQ,PR,R).
response(Q,PQ,PR,R):-
	responseD(Q,PQ,PR,R).
response(Q,PQ,PR,R):-
	responseE(Q,PQ,PR,R).
response(Q,PQ,PR,R):-
	responseF(Q,PQ,PR,R).
response(Q,PQ,PR,R):-
	responseG(Q,PQ,PR,R).
response(Q,PQ,PR,R):-
	responseH(Q,PQ,PR,R).
response(Q,PQ,PR,R):-
	response1(Q,PQ,PR,R).
response(Q,PQ,PR,R):-
	response2(Q,PQ,PR,R).
