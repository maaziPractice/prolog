:- dynamic already_parsed_successfully/3.
:- dynamic stoch_already_parsed_successfully/4.
:- dynamic stoch_already_parsed_successfully1/4.
:- dynamic cantbe_parsed_successfully/2.

brule(s, np, vp).
brule(np, det, n).
brule(vp, vt, np).

brule(np, np, pp).
brule(pp, p, np).
brule(vp,vp,pp).

brule(np,np,rc).
brule(rc,rt,vp).

trule(rt, who).
trule(det, the).
trule(n, man).
trule(n, woman).
trule(vt, likes).

trule(vt, eats).
trule(n, he).
trule(n, sushi).
trule(n, tuna).
trule(p,with).

%%parse2(s,[the,man,likes,the,woman,who,eats,sushi,with,tuna])
parse2(X,S):- parse2original(X, S,_Output,0).

parse2original(X, [W],Output,_Level) :- trule(X, W),
							  append([X,'('],[W,')'],Output).

parse2original(X, S,Output,Level):- 
    brule(X, Y, Z),
    append(S1, S2, S), 
    S1 \== [], S2 \== [],
	Level1 is Level+1 ,
	parse2original(Y, S1,Output1,Level1),
	parse2original(Z, S2,Output2,Level1),
	append([X,'('],Output1,Output11),
	append(Output2,[')'],Output22),
	append(Output11,Output22,Output),
	( ( Level == 0 ) -> prettyFormat(Output,0) ; true ) .



% parse3(s,[the,man,eats,the,sushi,with,the,woman,who,eats,the,sushi,with,the,tuna]).
parse3(X,S):- 
retractall(cantbe_parsed_successfully(_,_)),
retractall(already_parsed_successfully(_,_,_)),
parse3original(X, S,_Output,0).

parse3original(X, [W],Output,_Level) :- trule(X, W),
							  append([X,'('],[W,')'],Output),
							  ( not(already_parsed_successfully(X,[W],_))->
										(assert(already_parsed_successfully(X,[W],Output)))						  
										;fail																		
							   ),fail .

parse3original(X, S,Output,Level):- 
	% the following line avoids reparsing of 
	% same string by same production rule again
	(( cantbe_parsed_successfully(X,S);already_parsed_successfully(X,S,_))-> fail ;true ),
	
    brule(X, Y, Z),
    append(S1, S2, S), 
    S1 \== [], S2 \== [],
	Level1 is Level+1 ,

	not(parse3original(Y, S1,_,Level1)),
	not(parse3original(Z, S2,_,Level1)),	

	((already_parsed_successfully(Y,S1,_),already_parsed_successfully(Z,S2,_))->
		(true);
		( (not(cantbe_parsed_successfully(X,S)) ,not(already_parsed_successfully(X,S,_))   )
		
				-> (assert(cantbe_parsed_successfully(X,S)), fail);	
			fail
		)
	),
	
	already_parsed_successfully(Y,S1,Output1),
	already_parsed_successfully(Z,S2,Output2),
	append([X,'('],Output1,Output11),
	append(Output2,[')'],Output22),
	append(Output11,Output22,Output),


	assert(already_parsed_successfully(X,S,Output)),
%	( cantbe_parsed_successfully(X,S)-> retractall(cantbe_parsed_successfully(X,S));true ),
	
	retractall(cantbe_parsed_successfully(X,S)),
	( ( Level \== 0 ) -> fail; (prettyFormat(Output,0) ) ) .

	
pbrule(s, np, vp, 1).
pbrule(np, det, n, 0.5).
pbrule(np,np,rc,0.2).
pbrule(vp, vt, np, 0.5).
pbrule(rc,rt,vp,1).
pbrule(vp, vp, pp, 0.2).
pbrule(pp, p, np, 0.1).
pbrule(np, np, pp, 0.3).

ptrule(rt, who,0.9).
ptrule(det, the, 0.5).
ptrule(n, man, 0.4).
ptrule(n, woman, 0.3).
ptrule(vt, likes, 0.2).
ptrule(vt, eats, 0.1).
ptrule(n, sushi, 0.3).
ptrule(n, tuna, 0.3).
ptrule(p, with, 0.7).



  



prob_parse(X, [W],Output,P,_Level) :- ptrule(X, W,P),  append([X,'('],[W,')'],Output).

prob_parse(X, S,Output,P,Level):- 
    pbrule(X, Y, Z,Px),
    append(S1, S2, S), 
    S1 \== [], S2 \== [],
	( ( stoch_already_parsed_successfully(Y,S1,_,_) ) ->	
	     (true,stoch_already_parsed_successfully(Y,S1,Output1,Py))
		;
	   (	Level1 is Level+1 ,prob_parse(Y, S1,Output1,Py,Level1),assert( stoch_already_parsed_successfully(Y,S1,Output1,Py))  )
	)		 , 
	( ( stoch_already_parsed_successfully(Z,S2,_,_) ) ->	
	     (true,stoch_already_parsed_successfully(Z,S2,Output2,Pz))
		;
	   (    Level1 is Level+1,prob_parse(Z, S2,Output2,Pz,Level1),assert( stoch_already_parsed_successfully(Z,S2,Output2,Pz))   )
	)		,  
	append([X,'('],Output1,Output11),
	append(Output2,[')'],Output22),
	append(Output11,Output22,Output),
	P is Px*Py*Pz.
	%( ( Level == 0 ) -> prettyFormat(Output,0) ; true ) .


stoch_parse_topTree(X, S):-
						prob_parse(X, S,Output,Attempted_P_tree,0),
						((	stoch_already_parsed_successfully1(X,S,_,P_treeOld)	)
						  ->   ((P_treeOld < Attempted_P_tree)-> (
												retractall(stoch_already_parsed_successfully1(X,S,_,_)),
												assert(stoch_already_parsed_successfully1(X,S,Output,Attempted_P_tree))
											   )
											   ;
											   (
												  (
												   (P_treeOld == Attempted_P_tree)->(assert(stoch_already_parsed_successfully1(X,S,Output,Attempted_P_tree)));
																					(true)
												  
												   )
											   )


								) 
							 ; (
								assert(stoch_already_parsed_successfully1(X,S,Output,Attempted_P_tree))	
							   )
						
						)
						,

						fail.



parse41(X, S, P_tree):- retractall(stoch_already_parsed_successfully(_,_,_,_)),
				       % retractall(stoch_already_parsed_successfully1(_,_,_,_)),
					   % not(stoch_parse_topTree( X, S)),
					    prob_parse(X, S,Output,P_tree,0),
					    prettyFormat(Output,0).
					   %stoch_already_parsed_successfully1(X,S,P_tree,Op),prettyFormat(Op,0).


parse4(X, S, P_tree):- retractall(stoch_already_parsed_successfully(_,_,_,_)),
				       retractall(stoch_already_parsed_successfully1(_,_,_,_)),
					   not(stoch_parse_topTree( X, S)),
					   % prob_parse(X, S,Output,P_tree,0),
					   % prettyFormat(Output,0).
					   stoch_already_parsed_successfully1(X,S,Op,P_tree),prettyFormat(Op,0).



%%	( ( Level == 0 ) -> prettyFormat(Output,0) ; true ) .

%%   the man who likes the woman eats sushi


prettyFormat([Head1,B|T],Level) :-
	write(Head1),
	(   Head1=='(' ->
	      (	  trule(_,B) -> write(B),prettyFormat(T,Level);
	                        nl,
		                OneLevelHigher is Level +1,
	                        spaceEmmiter(OneLevelHigher),
			        prettyFormat([B|T],OneLevelHigher))
	    ;
	     (	Head1==')' ->
	           (   B==')'  ->  OneLevelHigher is Level -1,
		                   prettyFormat([B|T],OneLevelHigher);
		                   nl,spaceEmmiter(Level),prettyFormat([B|T],Level));
		  prettyFormat([B|T],Level)
	    )
	).

prettyFormat([Head1],_):- write(Head1).


spaceEmmiter(H):-
	(   H >  0 ->
	write('  '),
	H1 is H-1,
	spaceEmmiter(H1);true).
% parse2(s,[the,man,eats,the,sushi,with,the,woman,who,eats,the,sushi,with,the,tuna]).
% parse4(s,[the,man,eats,the,sushi,with,the,woman,who,eats,the,sushi,with,the,tuna],P).