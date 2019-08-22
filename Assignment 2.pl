% Valid Questions
% Are flights arriving from 'ZURICH'?
% Are flights leaving from zurich?

% Do flights arrive from 'ZURICH'?
% Do flights leave from Zurich?

% Does flights arrive from 'ZURICH'
% Does flights leave from Zurich?

% Did flights arrive from 'ZURICH'?
% Did flights leave from Zurich?

% When does flight 'KM481' arrive?
% When does flight KM481 leave?

% Did the 'AIR MALTA' flight from 'ZURICH' arrive?
% Did the airmalta flight from Zurich leave?

% What time does the 'AIR MALTA' flight from 'ZURICH'arrive?
% What time does the 'AIR MALTA' flight from Zurich leave?

% What time does the flight from 'ZURICH'arrive?
% What time does the flight from Zurich leave?

% What flights arrive from 'ZURICH'?
% What flights leave from Zurich?

% What flights arrived from 'ZURICH'?
% What flights left from Zurich?

% How many flights are there from 'ZURICH'?

reduce(X^F,X,F).

process(Sent,Result) :- s(SEMREP,Sent,[]), interpret(SEMREP, Result).

interpret(a1(X),A) :- a1(X,A).
interpret(q1(Q),A) :- q1(Q,A).
interpret(q2(Q),A) :- q2(Q,A).


a1(SEM,ok) :- asserta(SEM).

q1(SEM,yes) :- call(SEM),!.
q1(_,no).

q2(T^SEM,T) :- call(SEM,T),!.
q2(_,no).

%obatining and parsing html and asserting database
:- use_module(library(sgml)).
:- use_module(library(http/http_open)).



http_load_html(URL, DOM) :-	setup_call_cleanup(http_open(URL, In,
                           [ timeout(60)]),
                           (
					 dtd(html, DTD),
                               load_structure(stream(In),DOM,
                                              [ dtd(DTD),
                                                dialect(sgml),
                                                shorttag(false),
                                                max_errors(-1),
                                                syntax_errors(quiet)
                                              ])
                           ),
                           close(In)).


scrape_no_error() :-catch(http_load_html("http://staff.um.edu.mt/mros1/NLPMT/arrivals.html", DOM), Error,
			(
				  print_message(warning, Error),
				 fail
			)), !,
		        process_dom(DOM).
scrape_no_error().

process_dom([_,element(div,[class='table-bottom-arrivals'],T)|_]):- process_table(T).
process_dom([_, element(_,_,Z)|_]):- process_dom(Z).
process_dom([_, element(_,_,_)|Tail]):- process_dom(Tail).
process_dom([element(_,_,Z)|_]):- process_dom(Z).
process_dom([element(_,_,_)|Tail]):- process_dom(Tail).

process_table([_,H|T]):- parse_table(H),process_table(T).
process_table(_).


parse_table(element(_,_,T)) :-
	airline(T,T1,[Airline|_]),
	place(T1,T2,[Place|_]),
	flightno(T2,T3,[Flightno|_]),
	craft(T3,T4,_),
	scheduledtime(T4,T5,[ScheduledTime|_]),
	arrivaltime(T5,T6,[ArrivalTime|_]),
	status(T6,_,[Status|_]),
	assertz(arrival(Airline,Place,Flightno,ScheduledTime,ArrivalTime,Status)).
parse_table([]).

airline([_,element(_,_,C)|T],T,C).
place([_,element(_,_,C)|T],T,C).
flightno([_,element(_,_,C)|T],T,C).
craft([_,element(_,_,C)|T],T,C).
scheduledtime([_,element(_,_,C)|T],T,C) .
arrivaltime([_,element(_,_,C)|T],T,C) .
status([_,element(_,_,C)|T],T,C).

% WH returns qtime or ptime
% I returns the flight or place or company or ptime

s(q1(S)) --> aux(T),np(NP,I,_),vp(T,I,VP), {reduce(VP,NP,S)}. %applying NP to VP
s(q2(S))--> wh(WH), aux(T), np(NP,_,WH), vp(T,WH,VP), {reduce(VP,NP,S)}.
s(q2(S))--> wh(_), np(NP,_,_), vp(_,flightno,VP), {reduce(VP,NP,S)}.
s(q2(S)) --> [how,many],np(NP,_,_),vp(_,count,VP),{reduce(VP,NP,S)}.


%noun phrases
np(N,place ,ptime) --> det, n(N).
np(N,qtime,qtime) --> f(N).
np(N,place,_) --> n(N).
np(N,comp,_) --> det, comp(N), n(_).

%nouns
n(flight) --> [flight].
n(flight) --> [flights].
n(_) --> [time].
n(X) --> [X].

%flight numbers
f(X) --> [flight,X].

%company
comp('AIR MALTA ') --> ['AIR MALTA'].
comp('RYANAIR') --> ['RYANAIR'].
comp('EASYJET') --> ['EASYJET'].

%verb phrases
vp(T,I,V)--> v(T,I,V).
vp(T,I,VP) --> v(T,I,V), pp(PP),{reduce(V,PP,VP)}.
vp(T,I,VP) --> pp(PP), v(T,I,V),{reduce(V,PP,VP)}.

%prepositions
pp(N) --> p(_),n(N).
p(from) --> [from].
p(to) --> [to].
p(for) --> [for].

%determinants
det --> [a].
det --> [the].

%auxiliaries
aux(present) --> [do].
aux(present) --> [does].
aux(past) --> [did].
aux(present) --> [are].

%question words
wh(qtime) --> [when].
wh(ptime) --> [what].
wh(ptime) --> [what,time].


%-------------------------ARRIVALS-----------------------------
%count
v(_,count,Z^X^Y^arrivecount(Z,Y,X)) --> [are,there].

%place
v(past,place,X^Y^arrived(Y,X)) --> [arrive].
v(present,place,X^Y^arrive(Y,X)) --> [arrive].
v(present,place,X^Y^arrive(Y,X)) --> [arriving].

v(past,place,X^Y^arrived(Y,X)) --> [land].
v(present,place,X^Y^arrive(Y,X)) --> [land].
v(present,place,X^Y^arrive(Y,X)) --> [landing].


%flight
v(past,flight,X^Y^arrivedflight(Y,X)) --> [arrive].
v(present,flight,X^Y^arriveflight(Y,X)) --> [arrive].
v(present,flight,X^Y^arriveflight(Y,X)) --> [arriving].

v(past,flight,X^Y^arrivedflight(Y,X)) --> [land].
v(present,flight,X^Y^arriveflight(Y,X)) --> [land].
v(present,flight,X^Y^arriveflight(Y,X)) --> [landing].

% flight no
v(_,flightno,Z^X^Y^arrivedflightno(Z,Y,X)) --> [arrived].
v(_,flightno,Z^X^Y^arriveflightno(Z,Y,X)) --> [arrive].

v(_,flightno,Z^X^Y^arrivedflightno(Z,Y,X)) --> [landed].
v(_,flightno,Z^X^Y^arriveflightno(Z,Y,X)) --> [land].

%time - flight
v(past,qtime,X^Y^arrivedtime(Y,X)) --> [arrive].
v(present,qtime,X^Y^arrivetime(Y,X)) --> [arrive].
v(present,qtime,X^Y^arrivetime(Y,X)) --> [arriving].

v(past,qtime,X^Y^arrivedtime(Y,X)) --> [land].
v(present,qtime,X^Y^arrivetime(Y,X)) --> [land].
v(present,qtime,X^Y^arrivetime(Y,X)) --> [landing].

%time - place
v(past,ptime,P^X^Y^arrivedtimeplace(P,Y,X)) --> [arrive].
v(present,ptime,P^X^Y^arrivetimeplace(P,Y,X)) --> [arrive].
v(present,ptime,P^X^Y^arrivetimeplace(P,Y,X)) --> [arriving].

v(past,ptime,Z^X^Y^arrivedtimeplace(Z,Y,X)) --> [land].
v(present,ptime,Z^X^Y^arrivetimeplace(Z,Y,X)) --> [land].
v(present,ptime,Z^X^Y^arrivetimeplace(Z,Y,X)) --> [landing].


%company
v(past,comp,X^Y^arrivedcomp(Y,X)) --> [arrive].
v(present,comp,X^Y^arrivecomp(Y,X)) --> [arrive].
v(present,comp,X^Y^arrivecomp(Y,X)) --> [arriving].

v(past,comp,X^Y^Z^arrivedcomp(Y,X,Z)) --> [land].
v(present,comp,X^Y^Z^arrivecomp(Y,X,Z)) --> [land].
v(present,comp,X^Y^Z^arrivecomp(Y,X,Z)) --> [landing].


%place
arrive(_,P) :- arrival(_,P,_,_,_,'DELAYED').
arrive(_,P) :- arrival(_,P,_,_,_,' ').
arrived(_,P) :- arrival(_,P,_,_,_,'LANDED ').

%flight number
arrivedflightno(P,_,_,F) :- arrival(_,P,F,_,_,'LANDED ').
arriveflightno(P,_,_,F) :- findall(F,arrival(_,P,F,_,_,' '),L1), findall(F,arrival(_,P,F,_,_,'DELAYED'),L2), append(L1,L2,F).


%flight
arriveflight(_,F) :- arrival(_,_,F,_,_,'DELAYED').
arriveflight(_,F) :- arrival(_,_,F,_,_,' ').
arrivedflight(_,F) :- arrival(_,_,F,_,_,'LANDED ').

%time - flight number
arrivetime(_,F,T) :- arrival(_,_,F,_,T,'DELAYED').
arrivetime(_,F,T) :- arrival(_,_,F,_,T,' ').
arrivedtime(_,F,T) :- arrival(_,_,F,_,T,'LANDED ').


%time - place
arrivetimeplace(P,_,_,T) :- arrival(_,P,_,_,T,'DELAYED').
arrivetimeplace(P,_,_,T) :- arrival(_,P,_,_,T,' ').
arrivedtimeplace(P,_,_,T) :- arrival(_,P,_,_,T,'LANDED ').

%company
arrivecomp(C,P) :- arrival(C,P,_,_,_,'DELAYED').
arrivecomp(C,P) :- arrival(C,P,_,_,_,' ').
arrivedcomp(C,P) :- arrival(C,P,_,_,_,'LANDED ').

%count
arrivecount(P,_,_,Sum) :- findall(P,arrival(_,P,_,_,_,_), List), listlen(List,Sum).
listlen([], 0).
listlen([_|T], L):- listlen(T, S), L is S + 1.


%database - arrivals
% airline - destination - flight number - scheduled time - estimated time - status
%arrival(airmalta,birmingham,fr391,10:45,11:00,delayed).
%arrival(ryanair,zurich,fr3901,1:35,11:32,landed).
%arrival(airmalta,zurich,fr22,15.45,10:12,airborne).
%arrival(easyjet,zurich,fr11,15.45,10:12,delayed).
%arrival(easyjet,gatwick,ezy8823,11:45,12:05,airborne).

%----------------DEPARTURES----------------------

%place
v(past,place,X^Y^departed(Y,X)) --> [leave].
v(present,place,X^Y^depart(Y,X)) --> [leave].
v(present,place,X^Y^depart(Y,X)) --> [leaving].


%flight
v(past,flight,X^Y^departedflight(Y,X)) --> [leave].
v(present,flight,X^Y^departflight(Y,X)) --> [leave].
v(present,flight,X^Y^departflight(Y,X)) --> [leaving].


% flight no
v(_,flightno,Z^X^Y^departedflightno(Z,Y,X)) --> [left].
v(_,flightno,Z^X^Y^departflightno(Z,Y,X)) --> [leave].

%time - flight
v(past,qtime,X^Y^departedtime(Y,X)) --> [leave].
v(present,qtime,X^Y^departtime(Y,X)) --> [leave].
v(present,qtime,X^Y^departtime(Y,X)) --> [leaving].

%time - place
v(past,ptime,Z^X^Y^departedtimeplace(Z,Y,X)) --> [leave].
v(present,ptime,Z^X^Y^departtimeplace(Z,Y,X)) --> [leave].
v(present,ptime,Z^X^Y^departtimeplace(Z,Y,X)) --> [leaving].


%company
v(past,comp,X^Y^Z^departedcomp(Y,X,Z)) --> [leave].
v(present,comp,X^Y^Z^departcomp(Y,X,Z)) --> [leave].
v(present,comp,X^Y^Z^departcomp(Y,X,Z)) --> [leaving].

%place
depart(_,P) :- departure(_,P,_,_,_,delayed).
depart(_,P) :- departure(_,P,_,_,_,airborne).
departed(_,P) :- departure(_,P,_,_,_,departed).

%flight number
departflightno(P,F,_) :-	departure(_,P,F,_,_,delayed).
departedflightno(P,F,_) :- findall(F,departure(_,P,F,_,_,departed),L1), findall(F,departed(_,P,F,_,_,airborne),L2), append(L1,L2,F).


%flight
departeflight(_,F) :- departure(_,_,F,_,_,delayed).
departflight(_,F) :- departure(_,_,F,_,_,airborne).
departedflight(_,F) :- departure(_,_,F,_,_,departed).

%time - flight number
departetime(T,F) :- departure(_,_,F,T,_,delayed).
departtime(T,F) :- departure(_,_,F,T,_,airborne).
departedtime(T,F) :- departure(_,_,F,T,_,departed).


%time - place
departetimeplace(P,T,_) :- departure(_,P,_,T,_,delayed).
departtimeplace(P,T,_) :- departure(_,P,_,T,_,airborne).
departedtimeplace(P,T,_) :- departure(_,P,_,T,_,departed).

%company
departcomp(C,P,T) :- departure(C,P,_,T,_,delayed).
departcomp(C,P,T) :- departure(C,P,_,T,_,airborne).
departedcomp(C,P,T) :- departure(C,P,_,T,_,departed).

%database - departures
%airline - origin - flight number - departure time - arrival time - status
departure(airmalta,rome,am2091,1:12,15:52,delayed).
departure(ryanair,london,ry211,16:35,18:34,departed).
departure(airmalta,paris,am932,13:45,15:42,airborne).
departure(easyjet,dubai,ezy8as23,12:45,16:05,airborne).
